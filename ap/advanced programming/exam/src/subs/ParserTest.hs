module ParserTest where

import Test.HUnit
import SubsParser

listComprAST :: String
listComprAST =
  "Right (" ++
    "Prog [" ++
      "VarDecl \"xs\" (" ++
        "Just (Array [" ++
          "Number 0,Number 1,Number 2," ++
          "Number 3,Number 4,Number 5," ++
          "Number 6,Number 7,Number 8,Number 9]))," ++
      "VarDecl \"squares\" (" ++
        "Just (Compr (\"x\",Var \"xs\",Nothing) " ++
          "(Call \"*\" [Var \"x\",Var \"x\"])))," ++
      "VarDecl \"evens\" (" ++
        "Just (Compr (\"x\",Var \"xs\"," ++
          "Just (ArrayIf (" ++
            "Call \"===\" [" ++
              "Call \"%\" [Var \"x\",Number 2],Number 0]) Nothing)) " ++
            "(Var \"x\")))," ++
      "VarDecl \"many_a\" (" ++
        "Just (Compr (\"x\",Var \"xs\"," ++
          "Just (ArrayForCompr (\"y\",Var \"xs\",Nothing))) " ++
            "(String \"a\")))," ++
      "VarDecl \"hundred\" (" ++
        "Just (Compr (\"i\",Array [Number 0]," ++
          "Just (ArrayForCompr (\"x\",Var \"xs\"," ++
            "Just (ArrayForCompr (\"y\",Var \"xs\",Nothing))))) " ++
              "(Assign \"i\" (Call \"+\" [Var \"i\",Number 1]))))" ++
  "])"

simpleScopeAST :: String
simpleScopeAST =
  "Right (" ++
    "Prog [" ++
      "VarDecl \"x\" (Just (Number 42))," ++
      "VarDecl \"y\" (Just (Compr (\"x\",String \"abc\",Nothing) (Var \"x\")))," ++
      "VarDecl \"z\" (Just (Var \"x\"))" ++
  "])"

--parseString "1+2*4%5-2<100 === true;"
--Should be parsed as: (((1 + ((2 * 4) % 5)) - 2) < 100) === true
assocPrecOp1 :: String
assocPrecOp1 =
  "Right ("++
    "Prog [ExprAsStm (Call \"===\" [Call \"<\" [Call \"-\" [Call \"+\" [Number 1," ++
                                                                       "Call \"%\" [Call \"*\" [Number 2,Number 4],"++
                                                                                   "Number 5]],"++
                                                            "Number 2],"++
                                                "Number 100],"++
                                    "TrueConst]),"++
          "ExprAsStm (Call \"+\" [Call \"+\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"-\" [Call \"-\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"+\" [Call \"-\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"*\" [Call \"*\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"%\" [Call \"%\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"%\" [Call \"*\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"<\" [Call \"<\" [Number 1,Number 2],"++
                                 "Number 3]),"++
          "ExprAsStm (Call \"===\" [Call \"===\" [TrueConst,FalseConst],"++
                                 "Undefined])"++
  "])"

--parseString "a=b=c=1*2-3===2<2,hej=3<2;"
--Should be parsed as: a=(b=(c=((((1 * 2)-3) === (2 < 2))))), hej = (3 < 2)
assocPrecOp2 :: String
assocPrecOp2 =
  "Right ("++
    "Prog [ExprAsStm (Comma (Assign \"a\" (Assign \"b\" (Assign \"c\" (Call \"===\" [Call \"-\" [Call \"*\" [Number 1,Number 2],"++
                                                                                                "Number 3],"++
                                                                                    "Call \"<\" [Number 2,Number 2]])))) "++
                           "(Assign \"hej\" (Call \"<\" [Number 3,Number 2])))"++
  "])"

funCalls :: String
funCalls =
  "Right ("++
    "Prog [ExprAsStm (Call \"Array.new\" [Number 1,"++
                                         "Call \"+\" [Number 2,Number 2],"++
                                         "Assign \"variable\" (Number 2)]),"++
          "ExprAsStm (Call \"long.function.call.hello\" [Var \"spaces\","++
                                                        "Var \"_a_\","++
                                                        "Undefined,"++
                                                        "String \"THIS IS A STRING!!\"]),"++
          "ExprAsStm (Call \"emptycall\" [])"++
  "])"

noParse :: String
noParse = "Left (ParseError \"NoParse\")"

testSucc1 :: Test
testSucc1 = TestCase (do parsedFile <- parseFile "tests/scope.js"
                         assertEqual "description"
                                       simpleScopeAST $ show parsedFile)
testSucc2 :: Test
testSucc2 = TestCase (do parsedFile <- parseFile "tests/op_assoc_prec"
                         assertEqual "description"
                                       assocPrecOp1 $ show parsedFile)
testSucc3 :: Test
testSucc3 = TestCase (do parsedFile <- parseFile "tests/op_assoc_prec2"
                         assertEqual "description"
                                       assocPrecOp2 $ show parsedFile)
testSucc4 :: Test
testSucc4 = TestCase (do parsedFile <- parseFile "tests/intro.js"
                         assertEqual "description"
                                       listComprAST $ show parsedFile)
testSucc5 :: Test
testSucc5 = TestCase (do parsedFile <- parseFile "tests/funcalls"
                         assertEqual "description"
                                       funCalls $ show parsedFile)

testFail2 :: Test
testFail2 = TestCase (do parsedFile <- parseFile "tests/fail_reskey1"
                         assertEqual "Ident is reserved keyword 1"
                                       noParse $ show parsedFile)
testFail3 :: Test
testFail3 = TestCase (do parsedFile <- parseFile "tests/fail_reskey2"
                         assertEqual "Ident is reserved keyword 2"
                                       noParse $ show parsedFile)
testFail4 :: Test
testFail4 = TestCase (do parsedFile <- parseFile "tests/fail_reskey3"
                         assertEqual "Ident is reserved keyword 3"
                                       noParse $ show parsedFile)
testFail5 :: Test
testFail5 = TestCase (do parsedFile <- parseFile "tests/fail_reskey4"
                         assertEqual "Ident is reserved keyword 4"
                                       noParse $ show parsedFile)
testFail6 :: Test
testFail6 = TestCase (do parsedFile <- parseFile "tests/fail_reskey5"
                         assertEqual "Ident is reserved keyword 5"
                                       noParse $ show parsedFile)
testFail7 :: Test
testFail7 = TestCase (do parsedFile <- parseFile "tests/fail_reskey6"
                         assertEqual "Ident is reserved keyword 6"
                                       noParse $ show parsedFile)
testFail8 :: Test
testFail8 = TestCase (do parsedFile <- parseFile "tests/fail_illegalassign1"
                         assertEqual "Assign val to number"
                                       noParse $ show parsedFile)
testFail9 :: Test
testFail9 = TestCase (do parsedFile <- parseFile "tests/fail_illegalassign2"
                         assertEqual "Assign val to string"
                                       noParse $ show parsedFile)
testFail10 :: Test
testFail10 = TestCase (do parsedFile <- parseFile "tests/fail_illegalassign3"
                          assertEqual "Assign val to expression"
                                        noParse $ show parsedFile)
testFail11 :: Test
testFail11 = TestCase (do parsedFile <- parseFile "tests/fail_funcallspace"
                          assertEqual "Function call with space after '.'"
                                        noParse $ show parsedFile)
testFail12 :: Test
testFail12 = TestCase (do parsedFile <- parseFile "tests/fail_funcallspace2"
                          assertEqual "Function call with space before '.'"
                                        noParse $ show parsedFile)

testsParser:: Test
testsParser = TestList [TestLabel "Simple scope program" testSucc1,
                        TestLabel "Operater precedence and associativity, test 1" testSucc2,
                        TestLabel "Operater precedence and associativity, test 2" testSucc3,
                        TestLabel "Parsing list comprehension, with excessive (or no) spaces" testSucc4,
                        TestLabel "Parsing various function calls" testSucc5,
                        TestLabel "Reserved keyword as ident, 1" testFail2,
                        TestLabel "Reserved keyword as ident, 2" testFail3,
                        TestLabel "Reserved keyword as ident, 3" testFail4,
                        TestLabel "Reserved keyword as ident, 4" testFail5,
                        TestLabel "Reserved keyword as ident, 5" testFail6,
                        TestLabel "Reserved keyword as ident, 6" testFail7,
                        TestLabel "Assign val to number" testFail8,
                        TestLabel "Assign val to string" testFail9,
                        TestLabel "Assign val to expression" testFail10,
                        TestLabel "Space sep fun calls, 1" testFail11,
                        TestLabel "Space sep fun calls, 2" testFail12]
