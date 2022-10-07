module Turing.Parser where

import Common.Types 
import Common.Parser

actionp :: Parser Action
actionp = do
        c <- movep
        case c of
          'L' -> return L
          'R' -> return R
          'H' -> return H
          'P' -> do
            n <- movep
            return $ P n
          _ -> failp

casep :: Parser Case
casep = do
        ch <- spacefree movep
        ac <- splitby (spacefree $ charp ',') (spacefree actionp)
        next <- spacefree tokenp
        starp . spacefree $ charp ';'
        return (Case ch ac next)

routinep :: Parser Routine
routinep = do
        name <- spacefree tokenp
        cases <- plusp casep
        return (Routine name cases)

programp :: Parser Program
programp = do
        routines <- starp routinep
        starp $ charp ';'
        return (Program routines)
