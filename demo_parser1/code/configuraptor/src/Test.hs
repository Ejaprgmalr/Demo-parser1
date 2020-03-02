-- Rudimentary test suite. Feel free to replace anything.

import Absyn
import Parser
import Elaborator
import Solver

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests = testGroup "Minimal tests" [
--   testCase "parser" $
--     parseString dbt @?= Right dbi,
--   testCase "elaborator" $
--     elaborate dbi @?= Right dbf,
--   testCase "solver" $
--     solve dbf goal 3 @?= Right sol
--   ]
--   where
--     dbt = "resource r. component c: provides r."

--     dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
--     dbf = ([R "r"], [("c", [(R "r", (1,0))])])

--     goal = [(R "r", (0,1))]
--     sol = [("c", 1)]





tests :: TestTree
tests = testGroup "Tests" [unitests_Parser]
  


unitests_Parser :: TestTree
unitests_Parser = testGroup "---------Unit Tests for Parser----------"
  [testCase "Warmup Test 1: given in test framework" $
     parseString "resource r. component c : provides r." @?= Right (["r"], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "Warmup Test 2: givein in file goal" $
     parseString "resource PC. component DreamPC: provides PC;requires 12 GB-Ram;requires monitor;requires OS." @?= Right (["PC"],[IC "DreamPC" [(CKProvides,RSRes "PC"),(CKRequires,RSNum 12 (RSRes "GB-Ram")),(CKRequires,RSRes "monitor"),(CKRequires,RSRes "OS")]]),
   testCase "Priority Test 1 for RSpec: scalling operator tighter than ," $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   -- testCase "Priority Test 2 for RSpec: | and , are left associate while scalling operator is right",
   --   parseString "component PC: provides screen, 3 USB, 4 5 interface." @?= Right ([], [IC "PC" [(CKProvides, RS)]])
   testCase "whitespace Test 1: tab" $
     parseString "\t resource PC.component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "whitespace Test 2: newline" $
     parseString "\n resource PC.component \nPC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "whitespace Test 3 :spaces" $
     parseString "    resource PC.component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "Comment Test 1: nonnested comment:" $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS{this is comment}." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "Comment Test 2: nested comment with adjacent curly brace:" $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS{{this is comment}}." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "IDB Structure Test 1: single resource" $
     parseString "resource r." @?= Right (["r"], []),
   testCase "IDB Strucutre Test 2: single component" $
     parseString "component c : provides r." @?= Right ([], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "IDB Structure Test 3: 1 resource followed by 1 component" $
     parseString "resource r.component c : provides r." @?= Right (["r"], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "IDB Structure Test 4: 1 component followed by 1 resource" $
     parseString "component c : provides r.resource r." @?= Right (["r"], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "IDB Structure Test 5: multi resource" $
     parseString "resource r. resource e. resource s." @?= Right (["r", "e", "s"], []),
   testCase "IDB Structure Test 6: multi component" $
     parseString "component c: provides usb. component c2: uses USB." @?= Right ([], [IC "c" [(CKProvides, RSRes "usb")], IC "c2" [(CKUses, RSRes "USB")]])   
   -- testCase "IDB Structure Test 7: multiple resources and components appear alternately" $

  ] 






