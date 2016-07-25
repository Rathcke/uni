module InterpreterTest where

import Test.HUnit
import SubsInterpreter as S
import SubsAst
import qualified Data.Map as Map
import Data.Map(Map)

type Env = Map Ident Value

---------------
---------------
listComprAST :: Program
listComprAST =
    Prog [
      VarDecl "xs" (
        Just (Array [
          Number 0,Number 1,Number 2,
          Number 3,Number 4,Number 5,
          Number 6,Number 7,Number 8,Number 9])),
      VarDecl "squares" (
        Just (Compr ("x",Var "xs",Nothing) 
          (Call "*" [Var "x",Var "x"]))),
      VarDecl "evens" (
        Just (Compr ("x",Var "xs",
          Just (ArrayIf (
            Call "===" [
              Call "%" [Var "x",Number 2],Number 0]) Nothing))
            (Var "x"))),
      VarDecl "many_a" (
        Just (Compr ("x",Var "xs",
          Just (ArrayForCompr ("y",Var "xs",Nothing)))
            (String "a"))),
      VarDecl "hundred" (
        Just (Compr ("i",Array [Number 0],
          Just (ArrayForCompr ("x",Var "xs",
            Just (ArrayForCompr ("y",Var "xs",Nothing)))))
              (Assign "i" (Call "+" [Var "i",Number 1]))))
    ]
evalListCompr :: Either Error Env
evalListCompr =
  Right( Map.fromList [("evens",ArrayVal [IntVal 0,IntVal 2,IntVal 4,IntVal 6,IntVal 8]),
                       ("hundred",ArrayVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9,IntVal 10,
                                            IntVal 11,IntVal 12,IntVal 13,IntVal 14,IntVal 15,IntVal 16,IntVal 17,IntVal 18,IntVal 19,IntVal 20,
                                            IntVal 21,IntVal 22,IntVal 23,IntVal 24,IntVal 25,IntVal 26,IntVal 27,IntVal 28,IntVal 29,IntVal 30,
                                            IntVal 31,IntVal 32,IntVal 33,IntVal 34,IntVal 35,IntVal 36,IntVal 37,IntVal 38,IntVal 39,IntVal 40,
                                            IntVal 41,IntVal 42,IntVal 43,IntVal 44,IntVal 45,IntVal 46,IntVal 47,IntVal 48,IntVal 49,IntVal 50,
                                            IntVal 51,IntVal 52,IntVal 53,IntVal 54,IntVal 55,IntVal 56,IntVal 57,IntVal 58,IntVal 59,IntVal 60,
                                            IntVal 61,IntVal 62,IntVal 63,IntVal 64,IntVal 65,IntVal 66,IntVal 67,IntVal 68,IntVal 69,IntVal 70,
                                            IntVal 71,IntVal 72,IntVal 73,IntVal 74,IntVal 75,IntVal 76,IntVal 77,IntVal 78,IntVal 79,IntVal 80,
                                            IntVal 81,IntVal 82,IntVal 83,IntVal 84,IntVal 85,IntVal 86,IntVal 87,IntVal 88,IntVal 89,IntVal 90,
                                            IntVal 91,IntVal 92,IntVal 93,IntVal 94,IntVal 95,IntVal 96,IntVal 97,IntVal 98,IntVal 99,IntVal 100]),
                       ("many_a",ArrayVal [StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",StringVal "a",
                                           StringVal "a",StringVal "a",StringVal "a",StringVal "a"]),
                       ("squares",ArrayVal [IntVal 0,IntVal 1,IntVal 4,IntVal 9,IntVal 16,IntVal 25,IntVal 36,IntVal 49,IntVal 64,IntVal 81]),
                       ("xs",ArrayVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9])])

---------------
---------------
basicListCompr :: Program
basicListCompr = 
    Prog [VarDecl "xs" (Just (Array [Number 1,Number 2,Number 3,Number 4,Number 5])),
          VarDecl "ys" (Just (String "teststring")),
          VarDecl "a" (Just (Compr ("x",Var "xs",Nothing) (Call "<" [Var "x",Number 2]))),
          VarDecl "b" (Just (Compr ("x",Var "xs",Nothing) (Call "===" [Var "x",Number 2]))),
          VarDecl "y" (Just (Compr ("x",Var "xs",Nothing) (Call "*" [Var "x",Var "x"]))),
          VarDecl "z1" (Just (Compr ("x",Var "xs",Nothing) (Call "+" [Var "x",Call "*" [Number 2,Number 2]]))),
          VarDecl "z2" (Just (Compr ("x",Var "ys",Nothing) (Call "+" [Var "x",Number 1])))]
evalBasicListCompr :: Either Error Env
evalBasicListCompr = 
    Right (Map.fromList [("a",ArrayVal [TrueVal,FalseVal,FalseVal,FalseVal,FalseVal]),
                         ("b",ArrayVal [FalseVal,TrueVal,FalseVal,FalseVal,FalseVal]),
                         ("xs",ArrayVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5]),
                         ("y",ArrayVal [IntVal 1,IntVal 4,IntVal 9,IntVal 16,IntVal 25]),
                         ("ys",StringVal "teststring"),
                         ("z1",ArrayVal [IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),
                         ("z2",StringVal "t1e1s1t1s1t1r1i1n1g1")])
---------------
---------------
simpleScopeAST :: Program
simpleScopeAST =
    Prog [VarDecl "x" (Just (Number 42)), 
          VarDecl "y" (Just (Compr ("x",String "abc",Nothing) (Var "x"))),
          VarDecl "z" (Just (Var "x"))]

evalSimpleScopeAST :: Either Error Env
evalSimpleScopeAST = Right (Map.fromList [("x",IntVal 42),
                                          ("y",StringVal "abc"),
                                          ("z",IntVal 42)])
---------------
---------------
opTest :: Program
opTest =
    Prog [VarDecl "a" (Just (Call "+" [Number 12,Number 13])),
          VarDecl "b" (Just (Call "-" [Number 4,Number 3])),
          VarDecl "d" (Just (Call "*" [Number 2,Number 3])),
          VarDecl "e" (Just (Call "%" [Number 10,Number 3])),
          VarDecl "f" (Just (Call "===" [TrueConst,FalseConst])),
          VarDecl "g" (Just (Call "===" [FalseConst,FalseConst])),
          VarDecl "h" (Just (Call "+" [String "hej",String "sa"])),
          VarDecl "i" (Just (Call "+" [Call "+" [String "eksamen tager kun ",Number 23],String " timer"])),
          VarDecl "j" (Just (Call "<" [Number 45,Number 40])),
          VarDecl "k" (Just (Call "<" [Number 100,Number 1000]))]
evalOpTest :: Either Error Env
evalOpTest =
    Right (Map.fromList [("a",IntVal 25),
                         ("b",IntVal 1),
                         ("d",IntVal 6),
                         ("e",IntVal 1),
                         ("f",FalseVal),
                         ("g",TrueVal),
                         ("h",StringVal "hejsa"),
                         ("i",StringVal "eksamen tager kun 23 timer"),
                         ("j",FalseVal),
                         ("k",TrueVal)])
---------------
---------------
funCall :: Program
funCall =
    Prog [ExprAsStm (Call "this.function.should.not.interpret" [Var "spaces",
                                                                Var "_a_",
                                                                Undefined,
                                                                String "THIS IS A STRING!!"])
         ]
---------------
---------------
varAssign :: Program
varAssign = 
    Prog [ExprAsStm (Assign "x" TrueConst)]
---------------
---------------
typeMismatchAdd :: Program
typeMismatchAdd = 
    Prog[ ExprAsStm (Call "+" [Number 12, TrueConst])]
typeMismatchSub :: Program
typeMismatchSub = 
    Prog[ ExprAsStm (Call "-" [Number 12, TrueConst])]
typeMismatchEq :: Program
typeMismatchEq = 
    Prog[ ExprAsStm (Call "===" [Number 12, TrueConst])]
typeMismatchLt:: Program
typeMismatchLt = 
    Prog[ ExprAsStm (Call "<" [Number 12, TrueConst])]
typeMismatchMod:: Program
typeMismatchMod = 
    Prog[ ExprAsStm (Call "%" [Number 12, TrueConst])]
typeMismatchMult:: Program
typeMismatchMult = 
    Prog[ ExprAsStm (Call "*" [Number 12, TrueConst])]

---------------
---------------
noFunction :: Either Error Env
noFunction =
    Left (Error "function not found")
noVariable :: Either Error Env
noVariable =
    Left (Error "Variable not declared: x")
typeErrorAdd :: Either Error Env
typeErrorAdd =
    Left (Error "+: type mismatch")
typeErrorSub :: Either Error Env
typeErrorSub =
    Left (Error "-: type mismatch")
typeErrorEq :: Either Error Env
typeErrorEq =
    Left (Error "===: type mismatch")
typeErrorLt :: Either Error Env
typeErrorLt =
    Left (Error "<: type mismatch")
typeErrorMult :: Either Error Env
typeErrorMult =
    Left (Error "*: type mismatch")
typeErrorMod :: Either Error Env
typeErrorMod =
    Left (Error "%: type mismatch")
---------------
---------------

testSucc1 :: Test
testSucc1 = TestCase (do assertEqual "simple scope program"
                                      evalSimpleScopeAST $ runProg simpleScopeAST)
testSucc2 :: Test
testSucc2 = TestCase (do assertEqual "evaluating expressions with the basic operations"
                                      evalOpTest $ runProg opTest)
testSucc3 :: Test
testSucc3 = TestCase (do assertEqual "basic list comprehension"
                                      evalBasicListCompr $ runProg basicListCompr)
testSucc4 :: Test
testSucc4 = TestCase (do assertEqual "Advanced list comprehension"
                                      evalListCompr $ runProg listComprAST)

testFail1 :: Test
testFail1 = TestCase (do assertEqual "Function is not in Env"
                                      noFunction $ runProg funCall)
testFail2 :: Test
testFail2 = TestCase (do assertEqual "Variable not declared"
                                      noVariable $ runProg varAssign)
testFail3 :: Test
testFail3 = TestCase (do assertEqual "Operator type mismatch, +"
                                      typeErrorAdd $ runProg typeMismatchAdd)
testFail4 :: Test
testFail4 = TestCase (do assertEqual "Operator type mismatch, -"
                                      typeErrorSub $ runProg typeMismatchSub)
testFail5 :: Test
testFail5 = TestCase (do assertEqual "Operator type mismatch, ==="
                                      typeErrorEq $ runProg typeMismatchEq)
testFail6 :: Test
testFail6 = TestCase (do assertEqual "Operator type mismatch, <"
                                      typeErrorLt $ runProg typeMismatchLt)
testFail7 :: Test
testFail7 = TestCase (do assertEqual "Operator type mismatch, %"
                                      typeErrorMod $ runProg typeMismatchMod)
testFail8 :: Test
testFail8 = TestCase (do assertEqual "Operator type mismatch, *"
                                      typeErrorMult $ runProg typeMismatchMult)

testsInterpreter:: Test
testsInterpreter = TestList [TestLabel "Simple scope program" testSucc1,
                             TestLabel "Evaluating various expressions with basic operators" testSucc2,
                             TestLabel "Basic list comprehension of various types" testSucc3,
                             TestLabel "Advanced list comprehension of various types" testSucc4,
                             TestLabel "Uknown function, not in Env" testFail1,
                             TestLabel "Variable not declared in Env" testFail2,
                             TestLabel "Operator type error" testFail3,
                             TestLabel "Operator type error" testFail4,
                             TestLabel "Operator type error" testFail5,
                             TestLabel "Operator type error" testFail6,
                             TestLabel "Operator type error" testFail7,
                             TestLabel "Operator type error" testFail8]
