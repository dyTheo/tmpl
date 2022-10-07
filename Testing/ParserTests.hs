module Testing.ParserTests (parser_tests) where

import Common.Testing
import Common.Types
import Common.Parser
import Turing.Parser

parser_tests :: IO ()
parser_tests = do
        test_actionp
        test_casep
        test_routinep
        test_programp

test_actionp :: IO ()
test_actionp = do
    putStrLn "\t[Testing actionp]"
    assert (parse actionp "L") (Just (L, "")) "testing left action"
    assert (parse actionp "R") (Just (R, "")) "testing right action"
    assert (parse actionp "H") (Just (H, "")) "testing hold action"
    assert (parse actionp "X") (Nothing) "testing invalid action"
    assert (parse actionp "LX") (Just (L, "X")) "testing action with extra input"
    assert (parse actionp "Pc") (Just (P 'c', "")) "testing print action"
    assert (parse actionp "P") (Nothing) "testing print action with no character"
    assert (parse actionp "PcX") (Just (P 'c', "X")) "testing print action with extra input"

test_casep :: IO ()
test_casep = do
    putStrLn "\t[Testing casep]"
    assert (parse casep "0 L q1")
           (Just (Case '0' [L] "q1", ""))
           "testing case with one action"
    assert (parse casep "* R q1")
           (Just (Case '*' [R] "q1", ""))
           "testing case with one action and wildcard"
    assert (parse casep "0 L,R q1")
           (Just (Case '0' [L, R] "q1", ""))
           "testing case with multiple actions"
    assert (parse casep "0 L,R q1 X")
            (Just (Case '0' [L, R] "q1", "X"))
            "testing case with multiple actions and extra input"
    assert (parse casep "0 L,P1,L,P0 q1")
            (Just (Case '0' [L, P '1', L, P '0'] "q1", ""))
            "testing case with multiple actions and print"
    assert (parse casep "0 q1")
            (Nothing)
            "testing case with no actions"
    assert (parse casep "X")
            (Nothing)
            "testing case with invalid input"

test_routinep :: IO ()
test_routinep = do
    putStrLn "\t[Testing routinep]"
    assert (parse routinep "q1 0 L q1")
           (Just (Routine "q1" [Case '0' [L] "q1"], ""))
           "testing routine with one case"
    assert (parse routinep "q1 0 L q1; 1 R q2")
           (Just (Routine "q1" [Case '0' [L] "q1", Case '1' [R] "q2"], ""))
           "testing routine with multiple cases"
    assert (parse routinep "q1 0 L q1 1 R q2")
           (Just (Routine "q1" [Case '0' [L] "q1", Case '1' [R] "q2"], ""))
           "testing syntax without separators"
    assert (parse routinep "q1 0 L,R q1")
           (Just (Routine "q1" [Case '0' [L, R] "q1"], ""))
           "testing routine with multiple actions"
    assert (parse routinep "q1 0 L q1 X")
           (Just (Routine "q1" [Case '0' [L] "q1"], "X"))
           "testing routine with extra input"
    assert (parse routinep "  q1  0  L   q1  ")
           (Just (Routine "q1" [Case '0' [L] "q1"], ""))
           "testing routine with random spaces"
    assert (parse routinep "q1 0 L q1 ;;;; 1 R q2 ;;;;")
           (Just (Routine "q1" [Case '0' [L] "q1", Case '1' [R] "q2"], ""))
           "testing syntax with extra separators"
    assert (parse routinep "q1 0 L")
           (Nothing)
           "testing routine with invalid input"

test_programp :: IO ()
test_programp = do
    putStrLn "\t[Testing programp]"
    assert (parse programp "q1 0 L q1")
           (Just (Program [Routine "q1" [Case '0' [L] "q1"]], ""))
           "testing program with one routine"
    assert (parse programp "q1 0 L q1 q2 1 R q2")
              (Just (Program [
                        Routine "q1" [Case '0' [L] "q1"], 
                        Routine "q2" [Case '1' [R] "q2"]
                    ], ""))
              "testing program with multiple routines"
    assert (parse programp "q1 0 L q1 ;;;; q2 1 R q2")
              (Just (Program [
                        Routine "q1" [Case '0' [L] "q1"], 
                        Routine "q2" [Case '1' [R] "q2"]
                    ], ""))
              "testing program with multiple separators"
    assert (parse programp "q1   0  L   q1    q2   1   R   q2  ")
              (Just (Program [
                        Routine "q1" [Case '0' [L] "q1"], 
                        Routine "q2" [Case '1' [R] "q2"]
                    ], ""))
              "testing program with random spaces"
    assert (parse programp "X")
              (Just (Program [], "X"))
              "testing program with no input"
    assert (parse programp "q1 0 L q1 1 R q2; q2 1 R q2")
              (Just (Program [
                        Routine "q1" [Case '0' [L] "q1", Case '1' [R] "q2"],
                        Routine "q2" [Case '1' [R] "q2"]
                    ], ""))
              "testing program with multiple cases and routines"
