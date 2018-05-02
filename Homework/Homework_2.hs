module Homework_2 where

--------------------------------------------------------------------
--                         Question 3
------------------------------------------------------------------

data Cmd = Pen Mode
        | MoveTo Int Int
        | Seq Cmd Cmd
        deriving Show

data Mode = Up | Down
        deriving Show

type State = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd -> State -> (State,Lines)
semS [] = (State,[(0,0,0,0)])

sem' :: Cmd -> Lines
sem' [] = [(0,0,0,0)]