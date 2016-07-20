module ParserTest where

import Test.HUnit
import SubsParser

introAST :: String
introAST = "Right (Prog [VarDecl \"xs\" (Just (Array [Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9])),VarDecl \"squares\" (Just (Compr (\"x\",Var \"xs\",Nothing) (Call \"*\" [Var \"x\",Var \"x\"]))),VarDecl \"evens\" (Just (Compr (\"x\",Var \"xs\",Just (ArrayIf (Call \"===\" [Call \"%\" [Var \"x\",Number 2],Number 0]) Nothing)) (Var \"x\"))),VarDecl \"many_a\" (Just (Compr (\"x\",Var \"xs\",Just (ArrayForCompr (\"y\",Var \"xs\",Nothing))) (String \"a\"))),VarDecl \"hundred\" (Just (Compr (\"i\",Array [Number 0],Just (ArrayForCompr (\"x\",Var \"xs\",Just (ArrayForCompr (\"y\",Var \"xs\",Nothing))))) (Assign \"i\" (Call \"+\" [Var \"i\",Number 1]))))])"

funNameAST :: String
funNameAST = "Right (Prog [VarDecl \"k\" (Just (Call \"one.two.three\" [Number 1,Number 2,Number 3]))])"

precedence1AST :: String
precedence1AST = "Right (Prog [ExprAsStm (Call \"+\" [Call \"-\" [Call \"+\" [Number 2,Call \"*\" [Number 3,Number 4]],Number 1],Call \"%\" [Number 2,Number 2]])])"


precedence2AST :: String
precedence2AST = "Right (Prog [ExprAsStm (Call \"===\" [Call \"+\" [Number 7,Number 42],Call \"<\" [Number 12,Number 99]])])"

associativityAST :: String
associativityAST = "Right (Prog [VarDecl \"k\" (Just (Assign \"l\" (Var \"m\")))])"

allowedIdentsAST :: String
allowedIdentsAST = "Right (Prog [VarDecl \"k\" (Just (Var \"_k02__\"))])"

illegalIdents :: String
illegalIdents = "Left \"Parsing error\" (line 1, column 10):\nunexpected \"f\"\nexpecting digit, \"*\", \"%\", \"+\", \"-\", \"<\", \"===\" or \";\""

reservedIdents :: String
reservedIdents = "Left \"Parsing error\" (line 1, column 8):\nunexpected illegal identifier" 

dotError :: String
dotError = "Left \"Parsing error\" (line 1, column 15):\nunexpected \" \""

-- Tests
introTest :: Test
introTest = TestCase (do parsedFile <- parseFile "tests/intro.js"
                         assertEqual "intro test file"
                                      introAST $ show parsedFile)
testSucc1 :: Test
testSucc1 = TestCase (do parsedFile <- parseFile "tests/funName"
                         assertEqual "funName and array test"
                                      funNameAST $ show parsedFile)

testSucc2 :: Test
testSucc2 = TestCase (do parsedFile <- parseFile "tests/precedence1"
                         assertEqual "+ / - / * / % precedence test"
                                      precedence1AST $ show parsedFile)

testSucc3 :: Test
testSucc3 = TestCase (do parsedFile <- parseFile "tests/precedence2"
                         assertEqual "=== / < precedence test"
                                      precedence2AST $ show parsedFile)

testSucc4 :: Test
testSucc4 = TestCase (do parsedFile <- parseFile "tests/associativity"
                         assertEqual "'=' associativity test"
                                      associativityAST $ show parsedFile)

testSucc5 :: Test
testSucc5 = TestCase (do parsedFile <- parseFile "tests/allowedIdents"
                         assertEqual "allowed idents (using '_') test"
                                      allowedIdentsAST $ show parsedFile)

testSucc6 :: Test
testSucc6 = TestCase (do parsedFile <- parseFile "tests/illegalIdents"
                         assertEqual "illegal ident starting with digit"
                                      illegalIdents $ show parsedFile)

testSucc7 :: Test
testSucc7 = TestCase (do parsedFile <- parseFile "tests/reservedIdents"
                         assertEqual "reserved ident"
                                      reservedIdents $ show parsedFile)

testSucc8 :: Test
testSucc8 = TestCase (do parsedFile <- parseFile "tests/dotError"
                         assertEqual "spaces after dot error"
                                       dotError $ show parsedFile)

tests:: Test
tests = TestList [TestLabel "intro test file" introTest,
                  TestLabel "funName test file" testSucc1,
                  TestLabel "*/%/+/- precedence test file" testSucc2,
                  TestLabel "=== / < precedence test file" testSucc3,
                  TestLabel "'=' associativity test file" testSucc4,
                  TestLabel "allowed Ident test file" testSucc5,
                  TestLabel "illegal Ident test file" testSucc6,
                  TestLabel "reserved Ident test file" testSucc7,
                  TestLabel "spaces after dot error" testSucc8]
