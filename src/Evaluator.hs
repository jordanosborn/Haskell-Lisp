{-# LANGUAGE ExistentialQuantification #-}
module Evaluator where
import           Utilities.Types
import           Errors
import           Utilities.Tools
import           Control.Monad.Except
import           Data.IORef
import           Data.Maybe
import           Control.Applicative
import           System.IO
import           Parser
import           Text.ParserCombinators.Parsec

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
        ref <- newIORef value
        return (var, ref)


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left  err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow Parser.parseExpr

readExprList = readOrThrow (endBy Parser.parseExpr spaces)


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = Right $ f v
unaryOp f x   = throwError $ NumArgs 1 x

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False

numberp (Number _) = Bool True
numberp _          = Bool False

stringp (String _) = Bool True
stringp _          = Bool False

boolp (Bool _) = Bool True
boolp _        = Bool False

listp (List _        ) = Bool True
listp (DottedList _ _) = Bool True
listp _                = Bool False

numericBinop
    :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = Number . foldl1 op <$> mapM unpackNum params

boolBinop
    :: (LispVal -> ThrowsError a)
    -> (a -> a -> Bool)
    -> [LispVal]
    -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        left  <- unpacker $ head args
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _        = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

car :: [LispVal] -> ThrowsError LispVal
car value = case value of
    [List (x : xs)        ] -> return x
    [DottedList (x : xs) _] -> return x
    [badArg               ] -> throwError $ TypeMismatch "pair" badArg
    badArgList              -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr value = case value of
    [List (x : xs)        ] -> return $ List xs
    [DottedList [_     ] x] -> return x
    [DottedList (_ : xs) x] -> return $ DottedList xs x
    [badArg               ] -> throwError $ TypeMismatch "pair" badArg
    badArgList              -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons value = case value of
    [x1, List []            ] -> return $ List [x1]
    [x , List xs            ] -> return $ List $ x : xs
    [x , DottedList xs xlast] -> return $ DottedList (x : xs) xlast
    [x1, x2                 ] -> return $ DottedList [x1] x2
    badArgList                -> throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv value = case value of
    [Bool   arg1, Bool arg2  ] -> return $ Bool $ arg1 == arg2
    [Number arg1, Number arg2] -> return $ Bool $ arg1 == arg2
    [String arg1, String arg2] -> return $ Bool $ arg1 == arg2
    [Atom   arg1, Atom arg2  ] -> return $ Bool $ arg1 == arg2
    [DottedList xs x, DottedList ys y] ->
        eqv [List $ xs ++ [x], List $ ys ++ [y]]
    [l1@(List arg1), l2@(List arg2)] -> eqvList eqv [l1, l2]
    [_             , _             ] -> return $ Bool False
    badArgList                       -> throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do
            unpacked1 <- unpacker arg1
            unpacked2 <- unpacker arg2
            return $ unpacked1 == unpacked2
        `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal value = case value of
    [l1@(List arg1), l2@(List arg2)] -> eqvList equal [l1, l2]
    [DottedList xs x, DottedList ys y] ->
        equal [List $ xs ++ [x], List $ ys ++ [y]]
    [arg1, arg2] -> do
        primitiveEquals <- or <$> mapM
            (unpackEquals arg1 arg2)
            [ AnyUnpacker unpackNum
            , AnyUnpacker unpackStr
            , AnyUnpacker unpackBool
            ]
        eqvEquals <- eqv [arg1, arg2]
        return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
    badArgList -> throwError $ NumArgs 2 badArgList

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen value = case value of
    [String s ] -> Right $ Number $ fromIntegral $ length s
    [notString] -> throwError $ TypeMismatch "string" notString
    badArgList  -> throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef value = case value of
    [String s, Number k]
        | length s < k' + 1 -> throwError $ Default "Out of bound error"
        | otherwise         -> Right $ String [s !! k']
        where k' = fromIntegral k
    [String s , notNum] -> throwError $ TypeMismatch "number" notNum
    [notString, _     ] -> throwError $ TypeMismatch "string" notString
    badArgList          -> throwError $ NumArgs 2 badArgList

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc        func) args = func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
        then throwError $ NumArgs (num params) args
        else
            liftIO (bindVars closure $ zip params args)
            >>= bindVarArgs varargs
            >>= evalBody
  where
    remainingArgs = drop (length params) args
    num           = toInteger . length
    evalBody env = last Control.Applicative.<$> mapM (eval env) body
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing      -> return env
apply _ _ =
    throwError $ Default "Couldn't interpret statement"

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
    [ ("apply"            , applyProc)
    , ("open-input-file"  , makePort ReadMode)
    , ("open-output-file" , makePort WriteMode)
    , ("close-input-port" , closePort)
    , ("close-output-port", closePort)
    , ("read"             , readProc)
    , ("write"            , writeProc)
    , ("read-contents"    , readContents)
    , ("read-all"         , readAll)
    ]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"             , numericBinop (+))
    , ("-"             , numericBinop (-))
    , ("*"             , numericBinop (*))
    , ("/"             , numericBinop div)
    , ("mod"           , numericBinop mod)
    , ("quotient"      , numericBinop quot)
    , ("remainder"     , numericBinop rem)
    , ("symbol?"       , unaryOp symbolp)
    , ("string?"       , unaryOp stringp)
    , ("number?"       , unaryOp numberp)
    , ("bool?"         , unaryOp boolp)
    , ("list?"         , unaryOp listp)
    , ("symbol->string", unaryOp symbol2string)
    , ("string->symbol", unaryOp string2symbol)
    , ("="             , numBoolBinop (==))
    , ("<"             , numBoolBinop (<))
    , (">"             , numBoolBinop (>))
    , ("/="            , numBoolBinop (/=))
    , (">="            , numBoolBinop (>=))
    , ("<="            , numBoolBinop (<=))
    , ("&&"            , boolBoolBinop (&&))
    , ("||"            , boolBoolBinop (||))
    , ("string=?"      , strBoolBinop (==))
    , ("string<?"      , strBoolBinop (<))
    , ("string>?"      , strBoolBinop (>))
    , ("string<=?"     , strBoolBinop (<=))
    , ("string>=?"     , strBoolBinop (>=))
    , ("car"           , car)
    , ("cdr"           , cdr)
    , ("cons"          , cons)
    , ("eq?"           , eqv)
    , ("eqv?"          , eqv)
    , ("equal?"        , equal)
    , ("string-length" , stringLen)
    , ("string-ref"    , stringRef)
    ]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . flip writeIORef value)
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env      <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip
    bindVars
    (  map (makeFunc IOFunc)        ioPrimitives
    ++ map (makeFunc PrimitiveFunc) primitives
    )
    where makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body =
    return $ Func (map showVal params) varargs body env
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal


cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond env [List [Atom "else", value]     ] = eval env value
cond env (List [condition, value] : alts) = do
    result     <- eval env condition
    boolResult <- liftThrows (unpackBool result)
    if boolResult then eval env value else cond env alts
cond env (List a : _) = throwError $ NumArgs 2 a
cond env (a      : _) = throwError $ NumArgs 2 [a]
cond env _            = throwError $ Default "Not viable alternative in cond"

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _                   ) = return val
eval env val@(Number _                   ) = return val
eval env val@(Bool   _                   ) = return val
eval env val@(Atom   id                  ) = getVar env id
eval env (    List   [Atom "quote", val] ) = return val
eval env (    List   (Atom "cond" : alts)) = cond env alts
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "load", String filename]) =
    load filename >>= fmap last . mapM (eval env)
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body))
    = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
eval env form@(List (Atom "case" : key : clauses)) = if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
        List (Atom "else" : exprs) -> last <$> mapM (eval env) exprs
        List (List datums : exprs) -> do
            result   <- eval env key
            equality <- liftThrows (mapM (\x -> eqv [result, x]) datums)
            if Bool True `elem` equality
                then last <$> mapM (eval env) exprs
                else eval env $ List (Atom "case" : key : tail clauses)
        _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True  -> eval env conseq
        _          -> throwError $ TypeMismatch "bool" pred
eval env (List (function : args)) = do
    func    <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm =
    throwError $ BadSpecialForm "Unrecognised special form" badForm
