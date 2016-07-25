module SubsInterpreter
       ( runProg
       , Error (..)
       , Value(..)
       , arrayNew
       )
       where

import SubsAst as S

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


valExprMap :: Value -> Expr
valExprMap val =
  case val of
    IntVal i    -> Number i
    StringVal s -> String s
    TrueVal     -> TrueConst
    FalseVal    -> FalseConst
    _           -> Undefined

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", equal)
                       , ("<", lt)
                       , ("+", add)
                       , ("*", mult)
                       , ("-", sub)
                       , ("%", modulo)
                       , ("Array.new", arrayNew)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM (\c -> Right (x, fst c))
  (>>=) m f = SubsM (\c -> case runSubsM m c of
                   Left (Error s) -> Left (Error s)
                   Right (a, c')  -> runSubsM (f a) (c', snd c))
  fail s = SubsM (\_ -> Left $ Error s)

instance Functor SubsM where
  fmap = liftM

instance Applicative SubsM where
  pure = return
  (<*>) sf s = sf >>= \f -> fmap f s

stripChars :: String -> String -> String
stripChars = filter . flip notElem

arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(take n $ repeat UndefinedVal)
arrayNew _ = fail "Array.new called with wrong number of arguments"

modulo :: Primitive
modulo [IntVal x, IntVal y] = return $ IntVal $ mod x y
modulo [_, _] = fail "%: type mismatch"
modulo _ = fail "% called with wrong number of arguments"

mult :: Primitive
mult [IntVal x, IntVal y] = return $ IntVal $ x * y
mult [_, _] = fail "*: type mismatch"
mult _ = fail "* called with wrong number of arguments"

add :: Primitive
add [IntVal x, IntVal y] = return $ IntVal $ x + y
add [IntVal x, StringVal y] = return $ StringVal $ show x ++ stripChars "\'" y
add [StringVal x, IntVal y] = return $ StringVal $ stripChars "\'" x  ++ show y
add [StringVal x, StringVal y] = return $ StringVal $ stripChars "\'" x  ++ stripChars "\'" y
add [_, _] = fail "+: type mismatch"
add _ = fail "+ called with wrong number of arguments"

sub :: Primitive
sub [IntVal x, IntVal y] = return $ IntVal $ x - y
sub [_, _] = fail "-: type mismatch"
sub _ = fail "- called with wrong number of arguments"

equal :: Primitive
equal [IntVal x, IntVal y ]        = return $ if x == y then TrueVal else FalseVal
equal [StringVal x, StringVal y ]  = return $ if x == y then TrueVal else FalseVal
equal [ArrayVal x, ArrayVal y ]    = return $ if x == y then TrueVal else FalseVal
equal [TrueVal, TrueVal]           = return TrueVal
equal [TrueVal, FalseVal ]         = return FalseVal
equal [FalseVal, TrueVal ]         = return FalseVal
equal [FalseVal, FalseVal]         = return TrueVal
equal [UndefinedVal, UndefinedVal] = return TrueVal
equal [_, _]                       = fail "===: type mismatch"
equal _                            = fail "===: called with wrong number of arguments"

lt :: Primitive
lt [IntVal x, IntVal y] = return $ if x < y then TrueVal else FalseVal
lt [StringVal x, StringVal y] = return $ if x < y then TrueVal else FalseVal
lt [_, _] = fail "<: type mismatch"
lt _ = fail "< called with wrong number of arguments"

updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = SubsM (\c -> Right ((), Map.update (\_ -> Just val) name $ fst c))

insertEnv :: Ident -> Value -> SubsM ()
insertEnv name val = SubsM (\c -> Right ((), Map.insert name val $ fst c))

resetEnv :: Env -> SubsM  ()
resetEnv env = SubsM (\_ -> Right ((), env))

getEnv :: SubsM Env
getEnv = SubsM (\c -> Right (fst c, fst c))

getVar :: Ident -> SubsM Value
getVar name = SubsM (\c -> case Map.lookup name $ fst c of
                                Just val -> Right (val, fst c)
                                Nothing  -> Left $ Error "variable not found")

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\c -> case Map.lookup name $ snd c of
                                     Just val -> Right (val, fst c)
                                     Nothing  -> Left $ Error "function not found")

-- Evaluates list comprehension recursively, for lists of type ArrayVal.
--   [ for (x of xs) x']
-- Updates the environment with the current value of x in xs, such
-- that x' can be evaluated with evalExpr as we traverse along xs
evalComprList :: [Value] -> Ident -> Expr -> Env -> SubsM [Value]
evalComprList [] _ _ _             =
    return []
evalComprList (a:as) iden expr env = do
    stm (VarDecl iden $ Just $ valExprMap a) -- Update env with current x
    e2 <- evalExpr expr                      -- Evaluate x'
    es <- evalComprList as iden expr env
    return $ e2 : es

 -- Evaluates list comprehension recursively, for lists of type StringVal.
evalComprList' :: String -> Ident -> Expr -> Env -> SubsM [Value]
evalComprList' [] _ _ _             =
    return []
evalComprList' (a:as) iden expr env = do
    stm (VarDecl iden $ Just $ S.String [a])
    e2 <- evalExpr expr
    es <- evalComprList' as iden expr env
    return $ e2 : es

-- Collapse a list of StringVals into a single string
collapse :: [Value] -> String
collapse = foldl (\x (StringVal y) -> x ++ y ) ""

evalExpr :: Expr -> SubsM Value
evalExpr (Number int)    = return $ IntVal int
evalExpr (Array exprs)   = do
    es <- mapM evalExpr exprs
    return $ ArrayVal es
evalExpr (String str)    = return $ StringVal str
evalExpr (TrueConst)     = return TrueVal
evalExpr (FalseConst)    = return FalseVal
evalExpr (Undefined)     = return UndefinedVal
evalExpr (Var iden)      = do
    env <- getEnv
    case runSubsM (getVar iden) (env, snd initialContext) of
      Right (val, _) -> return val
      Left _         -> fail $ "Variable not declared: " ++ iden
evalExpr (Compr (iden, e1, _) e2) = do
    env <- getEnv
    ev1 <- evalExpr e1
    let tempEnv = env
    case ev1 of
      ArrayVal e  -> do resList <- evalComprList e iden e2 env
                        resetEnv tempEnv
                        return $ ArrayVal resList
      StringVal s -> do resList <- evalComprList' s iden e2 env
                        resetEnv tempEnv
                        return $ StringVal $ collapse resList
      _           -> fail "List comprehension: type not supported"
evalExpr (Call fn exprs) = do
    fun <- getFunction fn
    es <- mapM evalExpr exprs
    fun es
evalExpr (Assign iden expr) = do
    e <- evalExpr expr
    env <- getEnv
    case runSubsM (getVar iden) (env, snd initialContext) of
      Right _ -> updateEnv iden e
      Left _  -> fail $ "Variable not declared: " ++ iden
    return e
evalExpr (Comma expr1 expr2) = do
    evalExpr expr1
    evalExpr expr2

stm :: Stm -> SubsM ()
stm (VarDecl iden e) =
    case e of
      Just expr -> do ev <- evalExpr expr
                      insertEnv iden ev
      Nothing   -> insertEnv iden UndefinedVal
stm (ExprAsStm e)    = do
    evalExpr e
    return ()

runProg :: Program -> Either Error Env
runProg (Prog prog) =
  let (_, err) = foldl (\(context, env) stmt ->
                        case runSubsM (stm stmt) context of
                          Right (_, env') -> case env of --Check if error occured before
                                               Right _ -> ((env', snd context), Right env')
                                               Left s' -> (context, Left s')
                          Left s         -> (context, Left s))
                 (initialContext, Right (Map.empty :: Env)) prog
  in err
