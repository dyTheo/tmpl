module Turing.Interpreter where

import Common.Types 

next_state :: Program -> State -> State 
next_state (Program []) s = error $ "Error " ++ (state s) ++ " is not defined"
next_state (Program (x:xs)) s
    | (state s) == (name x) = run_state (cases x) (tape s)
    | otherwise             = next_state (Program xs) s

run_state :: [Case] -> Tape -> State
run_state [] t = error $ "Pattern " ++ [headc t] ++  " not found"
run_state (x:xs) t
    | (pattern x) == '*'       = run_case x t
    | (pattern x) == (headc t) = run_case x t
    | otherwise                = run_state xs t

run_case :: Case -> Tape -> State
run_case (Case _ [] s) t = State t s
run_case (Case n (x:xs) s) t = case x of
                                    L -> run_case (Case n xs s) (move_left t)
                                    R -> run_case (Case n xs s) (move_right t)
                                    H -> run_case (Case n xs s) t
                                    P c -> run_case (Case n xs s) (printc c t)

move_left :: Tape -> Tape
move_left (Tape [] h rs) = (Tape [] '#' (h:rs))
move_left (Tape (l:ls) h rs) = (Tape ls l (h:rs))

move_right :: Tape -> Tape
move_right (Tape ls h []) = (Tape (h:ls) '#' [])
move_right (Tape ls h (r:rs)) = (Tape (h:ls) r rs)

printc :: Char -> Tape -> Tape
printc c (Tape l h r) = (Tape l c r)

run_program :: State -> Program -> State 
run_program s p
    | (state s) == "Y" = s
    | (state s) == "N" = s
    | (state s) == "H" = s
    | otherwise = run_program (next_state p s) p
