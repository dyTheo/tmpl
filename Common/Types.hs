module Common.Types where

import Data.List (intercalate)

-- Possible actions
data Action = L | R | H | P Char deriving Eq

instance Show Action where
    show L = "L"
    show R = "R"
    show H = "H"
    show (P c) = "P" ++ [c]

-- Possible transitions from a state to another
data Case = Case {
    pattern :: Char,
    actions :: [Action],
    next :: String
} deriving Eq

instance Show Case where
    show (Case p ac n) = (p:" ") ++ (intercalate "," (map show ac)) ++ (' ':n)

-- State of the machine
data Routine = Routine {
    name :: String,
    cases :: [Case]
} deriving Eq

instance Show Routine where
    show (Routine n cs) = n ++ "\n" ++ (intercalate "\n" (map show cs))

-- A program is a list of states
newtype Program = Program {
    routines :: [Routine]
} deriving Eq

instance Show Program where
    show (Program rs) = "\n" ++ intercalate "\n" (map show rs)

-- 1d tape
data Tape = Tape {
    left :: [Char],
    headc :: Char,
    right :: [Char]
} deriving Eq

instance Show Tape where
    show (Tape l h r) = (reverse l) ++ ('<':h:'>':[]) ++ r

-- Running state of the machine (current state, current tape)
data State = State {
    tape :: Tape,
    state :: String
}
