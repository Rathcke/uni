module SubsInterpreter
       ( runProg
       , Error (..)
       , Value(..)
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import Control.Applicative as App
import qualified Data.Map as Map
import Data.Map(Map)

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

data Error = Error String
  deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", isEqual)
                       , ("<", lessThan)
                       , ("+", plus)
                       , ("*", multiply)
                       , ("-", minus)
                       , ("%", modulo)
                       , ("Array.new", arrayNew)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap = Control.Monad.liftM

instance App.Applicative SubsM where
  pure = return
  (<*>) sf s = sf >>= \f -> fmap f s

instance Monad SubsM where
  return a = SubsM (\c -> Right (a, fst c))
  (>>=) m f = SubsM (\ (env, pEnv) -> case runSubsM m (env, pEnv) of
                      Left str        -> Left str
                      Right (a, env') -> runSubsM (f a) (env', pEnv))
  fail s = SubsM (\_ -> Left (Error s))

-- Initial Context
arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(replicate n UndefinedVal)
arrayNew _ = fail "Array.new called with wrong number of arguments"

isEqual :: Primitive
isEqual [IntVal e1, IntVal e2]       = return (if e1 == e2 then TrueVal
                                     else FalseVal)
isEqual [StringVal e1, StringVal e2] = return (if e1 == e2 then TrueVal
                                     else FalseVal)
isEqual [TrueVal, FalseVal]          = return FalseVal
isEqual [FalseVal, TrueVal]          = return FalseVal
isEqual [TrueVal, TrueVal]           = return TrueVal
isEqual [FalseVal, FalseVal]         = return TrueVal
isEqual _                            = fail "Bad argument types in '==='"

lessThan :: Primitive
lessThan [IntVal e1, IntVal e2]       = return (if e1 < e2 then TrueVal
                                      else FalseVal)
lessThan [StringVal e1, StringVal e2] = return (if e1 < e2 then TrueVal
                                      else FalseVal)
lessThan _                            = fail "Bad argument types in '<'"

plus :: Primitive
plus [IntVal e1, IntVal e2]       = return $ IntVal (e1 + e2)
plus [StringVal e1, StringVal e2] = return $ StringVal (e1 ++ e2)
plus [StringVal e1, IntVal e2]    = return $ StringVal (e1 ++ show e2)
plus [IntVal e1, StringVal e2]    = return $ StringVal (show e1 ++ e2)
plus _                            = fail "Bad argument types in '+'"

multiply :: Primitive
multiply [IntVal e1, IntVal e2] = return $ IntVal (e1 * e2)
multiply _                      = fail "Both operators must be integers in '*'"

minus :: Primitive
minus [IntVal e1, IntVal e2] = return $ IntVal (e1 - e2)
minus _                      = fail "Both operators must be in '-'"

modulo :: Primitive
modulo [IntVal e1, IntVal e2] = return $ IntVal (e1 `mod` e2)
modulo _                      = fail "Both operator must be integers in '%'"

-- End initial Context

-- Utility Functions
getEnv :: SubsM Env
getEnv = SubsM (\(env, _) -> Right (env, env))

getPEnv :: SubsM PEnv
getPEnv = SubsM (\(env, penv) -> Right (penv, env))

putEnv :: Env -> SubsM ()
putEnv env = SubsM (\_ -> Right((), env))

{-
modify :: (Env -> Env) -> SubsM ()
modify f = do
  env <- getEnv
  putEnv (f env)
-}

updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = do
  env <- getEnv
  putEnv (Map.insert name val env)

getVar :: Ident -> SubsM Value
getVar name = do
  env <- getEnv
  case Map.lookup name env of
    Just a -> return a
    Nothing -> fail "Key does not exist in 'getVar'"

getFunction :: FunName -> SubsM Primitive
getFunction name = do
  penv <- getPEnv
  case Map.lookup name penv of
    Just a -> return a
    Nothing -> fail "Key does not exist in 'getFunction'"

-- End Utility Functions

-- Expr and Stms Evaluation
evalExpr :: Expr -> SubsM Value
evalExpr (Number n)           = return $ IntVal n
evalExpr (String s)           = return $ StringVal s
evalExpr (Array arr)          = do
  a <- arrEval arr
  return $ ArrayVal a
evalExpr (TrueConst)          = return TrueVal
evalExpr (FalseConst)         = return FalseVal
evalExpr (Undefined)          = return UndefinedVal
evalExpr (Var iden)           = getVar iden
evalExpr (Compr _ _)          = undefined
evalExpr (Call f arr)         = do
  fun <- getFunction f
  a <- arrEval arr
  fun a
evalExpr (Assign iden e1)     = do
  e <- evalExpr e1
  updateEnv iden e
  return e
evalExpr (Comma e1 e2)        = do
  _ <- evalExpr e1
  evalExpr e2

arrEval :: [Expr] -> SubsM [Value]
arrEval []        = return []
arrEval (a : arr) = do
  v  <- evalExpr a
  vs <- arrEval arr
  return (v : vs)

stm :: Stm -> SubsM ()
stm (VarDecl iden Nothing)   = updateEnv iden UndefinedVal
stm (VarDecl iden (Just e1)) = do
  e <- evalExpr e1
  updateEnv iden e
stm (ExprAsStm e1)           = do
  _ <- evalExpr e1
  return ()

program :: Program -> SubsM ()
program (Prog [])       = return ()
program (Prog (p:prog)) = do
  _  <- stm p
  program (Prog prog)

-- End Expr and Stms Evaluation

runProg :: Program -> Either Error Env
runProg prog =
  case runSubsM (program prog) initialContext of
    Left str        -> Left str
    Right (_, env)  -> Right env
