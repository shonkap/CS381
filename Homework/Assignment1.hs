module Assignment1 where

-----names:
--- Claude Maimon
--- Tyler Jones
--- Pavel Shonka
--- Sean 
--------------------------------------------------------------------
--                         Question 1
------------------------------------------------------------------
-- a) Abstract syntax of the Mini Logo
data MiniLogo =  Pen Mode
              |  Moveto Position Position
              |  Def String Parameters MiniLogo
              |  Call String Values
              -- |  Seq MiniLogo MiniLogo
              |  Seq [MiniLogo] -- List of MiniLogo (i.e. MiniLogo, Op | MiniLogo)
              deriving (Show)

data Mode          = Up | Down
                   deriving (Show) -- Up | Down
data Position      = PosNum Int | PosName String
                   deriving (Show) -- List of Num | List of Name 
type Parameters    = [String] -- Name,pars | Name 
type Values        = [Int] -- Num,values | Num


-- b) Vector macro

vector = Def "vector" ["x1","y1","x2","y2"]
       --  (Seq (Pen Up)
       --       (Seq (Moveto (PosName "x1") (PosName "y1"))
       --            (Seq (Pen Down)
       --                  (Seq (Moveto (PosName "x2") (PosName "y2")) (Pen Up))))) -- It's Seq all the way down.
         (Seq [Pen Up, Moveto (PosName "x1") (PosName "y1"), Pen Down, Moveto (PosName "x2") (PosName "y2"), Pen Up])


-- c) Haskell function steps
-- With many thanks to Xinyao Wang for his help. This section of code would be much more cluttered with
-- nested paranthesies without your help.


steps :: Int -> [MiniLogo]
steps n = [Pen Up, Moveto (PosNum 0) (PosNum 0)]
          ++ parseSteps n ++ [Pen Up]

parseSteps :: Int -> [MiniLogo]
parseSteps 0 = []
parseSteps n = parseSteps (n-1) ++ [Moveto (PosNum (n-1)) (PosNum (n)), Moveto (PosNum (n)) (PosNum (n))]







--------------------------------------------------------------------
--                         Question 2
------------------------------------------------------------------
------ 2 a
------abstract syntax for the above language as a Haskell data type.


data Circuit = Full Gates Links  
            -- deriving Show
data Gates   = Name Int GateFn Gates 
             | GatesNull
             --deriving Show
data GateFn  = And | Or| Xor| Not --deriving Show
data Links   = From (Int,Int) (Int,Int) Links
             | LinksNull
             -- deriving Show

-------2 b
---------Represent the half adder circuit in abstract syntax
g1 :: Gates
g1 =  Name 1 Xor (Name 2 And GatesNull)

l1 :: Links
l1 = From (1,1) (2, 1) (From (1,2) (2 ,2) LinksNull) 

halfAdder :: Circuit
halfAdder = Full g1 l1


------ 2 c
----------Define a Haskell function that implements a pretty printer 
----------for the abstract syntax.



ppGateFn :: GateFn -> String
ppGateFn And = "and"
ppGateFn Or = "or" 
ppGateFn Xor = "xor"
ppGateFn Not = "not" 


ppLinks :: Links -> String
ppLinks (From (i1, i2) (i3, i4) l) = "from " ++show i1++ "." ++ show i2++ " to " ++show i3++ "." ++show i4 ++ ";\n" ++ ppLinks l 
ppLinks LinksNull = ""

ppGates :: Gates -> String
ppGates (Name i gn g) = show i++":"++ppGateFn gn++";\n"++ppGates g
ppGates GatesNull = ""

ppCircuit :: Circuit -> String
ppCircuit (Full g l) = ppGates g++ppLinks l


pp :: Circuit -> IO ()
--pp s = putStrLn (ppStmt s)  -- without indentation
pp s = putStrLn (ppCircuit s)  -- with indentation


--------------------------------------------------------------------
---                        Question 3
------------------------------------------------------------------
--- 3 a
--- Represent the expression -(3+4)*7 in the alternative abstract syntax
--  module Problem3 where

--- Original Abstract Syntax
data Expr = N Int
         | Plus Expr Expr
         | Times Expr Expr
         | Neg Expr
           deriving Show

--- Alternative Abstract Syntax
data Op = Add | Multiply | Negate deriving Show

data Exp = Num Int
         | Apply Op [Exp]
          deriving Show


--e1:: Exp
e1 = Apply Negate [Apply Multiply [Apply Add [Num 4,  Num 3] ], Num 7]

---3 b
---What are the advantages or disadvantages of either representation?

-- The abstract syntaxes represent the same language, so any advantages or disadvantages are all personal
-- preference. Personally, I find the second, alternative abstract syntax to be easier to use. I think
-- having two constructors, one for the expression and one for the operations is a logical breakdown. 
-- Moreover, the use of lists rather than many nested parentheses allows for a greater level of usability and
-- legibility.

-- What I consider to be the strength of the alternative syntax, some could consider to be its weakness.
-- The original syntax only contains one type constructor, and for some this may increase usability and legibility.



--- 3 c 
--- Define a function translate :: Expr -> Exp
translate :: Expr -> Exp
translate (N int1) = Num int1
translate (Plus x y) = Apply Add [translate x, translate y]
translate (Times expr1 expr2) = Apply Multiply [translate expr1, translate expr2]
translate (Neg expr1) = Apply Negate [translate expr1]