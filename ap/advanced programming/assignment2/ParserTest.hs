module ParserTest where

import Test.HUnit
import SalsaParser

simpleAST :: String
simpleAST =
  "Right [Rect \"box\" (Const 10) (Const 400) (Const 20) (Const 20) Green True," ++
         "Move \"box\" (Abs (Const 10) (Const 200))," ++
         "Move \"box\" (Rel (Const 100) (Const 0))," ++
         "Move \"box\" (Abs (Const 110) (Const 400))," ++
         "Move \"box\" (Rel (Minus (Const 0) (Const 100)) (Const 0))]"

multishapesAST :: String
multishapesAST =
  "Right [Par (Circ \"a38\" (Const 390) (Const 200) (Const 20) Plum True) " ++
             "(Rect \"a_box\" (Const 10) (Const 400) (Const 20) (Const 20) Orange True)," ++
         "Move \"a_box\" (Rel (Const 0) (Minus (Const 0) (Const 200)))," ++
         "Move \"a_box\" (Rel (Const 0) (Const 200))," ++
         "Move \"a38\" (Rel (Const 0) (Const 200))," ++
         "Move \"a38\" (Rel (Const 0) (Minus (Const 0) (Const 200)))," ++
         "Par (Move \"a38\" (Rel (Const 100) (Const 0))) " ++
             "(Move \"a_box\" (Rel (Const 100) (Const 0)))," ++
         "Par (Move \"a_box\" (Rel (Minus (Const 0) (Const 100)) (Const 0))) " ++
             "(Move \"a38\" (Rel (Minus (Const 0) (Const 100)) (Const 0)))," ++
         "Par (Toggle \"a38\") " ++
             "(Toggle \"a_box\")]"

multispaceAST :: String
multispaceAST = "Right [Par (Rect \"box\" (Const 10) (Const 400) (Const 20) (Const 20) Green True) " ++
                           "(Circ \"cirkel\" (Const 10) (Const 223) (Const 330) Blue True)]"

arithmeticPrecedenceAST :: String
arithmeticPrecedenceAST = "Right [Rect \"test\" (Const 1) (Const 2) (Plus (Const 2) (Div (Const 3) (Const 3))) (Const 4) Red True]"

noParse :: String
noParse = "Left NoParse"

--test = TestCase (assertEqual "description" "expected" actual)

testSucc1 :: Test
testSucc1 = TestCase (do parsedFile <- parseFile "tests/simple"
                         assertEqual "simple test file"
                                       simpleAST $ show parsedFile)
testSucc2 :: Test
testSucc2 = TestCase (do parsedFile <- parseFile "tests/multishapes"
                         assertEqual "test parallel operations, "
                                       multishapesAST $ show parsedFile)
testSucc3 :: Test
testSucc3 = TestCase (do parsedFile <- parseFile "tests/multipleSpaceSeparation"
                         assertEqual "test multiple space separation has no effect for result, "
                                       multispaceAST $ show parsedFile)

testSucc4 :: Test
testSucc4 = TestCase (do parsedFile <- parseFile "tests/precedence"
                         assertEqual "test arithmetic operations have the right precedence, "
                                       arithmeticPrecedenceAST $ show parsedFile)

testFail1 :: Test
testFail1 = TestCase (do parsedFile <- parseFile "tests/illFormedColor"
                         assertEqual "Ill formed color should fail, "
                                       noParse $ show parsedFile)
testFail2 :: Test
testFail2 = TestCase (do parsedFile <- parseFile "tests/keywordIdent"
                         assertEqual "ill formed ident should fail, "
                                       noParse $ show parsedFile)
testFail3 :: Test
testFail3 = TestCase (do parsedFile <- parseFile "tests/illFormedCommand"
                         assertEqual "ill formed command should fail, "
                                       noParse $ show parsedFile)
testFail4 :: Test
testFail4 = TestCase (do parsedFile <- parseFile "tests/illFormedExpression"
                         assertEqual "ill formed expression should fail, "
                                       noParse $ show parsedFile)
testFail5 :: Test
testFail5 = TestCase (do parsedFile <- parseFile "tests/illFormedPosition"
                         assertEqual "ill formed position should fail, "
                                       noParse $ show parsedFile)

tests:: Test
tests = TestList [TestLabel "simple test file" testSucc1,
                  TestLabel "multishape test file" testSucc2,
                  TestLabel "test multispace separation" testSucc3,
                  TestLabel "test arithmetic operators precedence" testSucc4,
                  TestLabel "test ill formed color" testFail1,
                  TestLabel "test ill formed ident" testFail2,
                  TestLabel "test ill formed command" testFail3,
                  TestLabel "test ill formed expression" testFail4,
                  TestLabel "Test ill formed position" testFail5]
