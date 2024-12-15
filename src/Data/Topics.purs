module Data.Topics
  where

import Control.Alt
import Data.Functor
import Prelude

import Control.Lazy (defer)
import Data.Array (cons, foldl, head, tail)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex.Flags (noFlags)
import Parsing (Parser)
import Parsing.Combinators (chainr, choice, sepBy, try)
import Parsing.Combinators.Array (many)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.String (char, eof, regex, satisfy, string)
import Parsing.String.Basic (alphaNum, letter, number, whiteSpace)

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Grammars
-------------------------------------------------------------------------------

<int> = <digit>+
<dec> = <int> '.' <int>

<num> = <dec> | <int>
<sym> = '_'? <alpha> <id-body>*

<id-body> = <alpha-num> | '_'

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<ukn> = <sym>
      | <fun>
      | <stm-0>
      | <num-0>
      | <set-0>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<stm-0> = <stm-1> '|' <stm-0>
        | <stm-1>

<stm-1> = "forall " <stm-2> ". " <stm-1>
        | "exists " <stm-2> ". " <stm-1>
        | <stm-2>

<stm-2> = <ukn> '=' <ukn>
        | <stm-3>

<stm-3> = <stm-3> '->' <stm-4>
        | <stm-4>

<stm-4> = <stm-5> ' /\ ' <stm-4>
        | <stm-5> ' \/ ' <stm-4>
        | <stm-5> 

<stm-5> = '-' <stm-5>
        | <stm-6>

<stm-6> = <num-stm>
        | <set-stm>
        | '(' <stm-0> ')'
        | "true"
        | "false"
        | <sym>
        | <fun>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<num-stm> = <num-0> '>'  <num-0> 
          | <num-0> '<'  <num-0> 
          | <num-0> '>=' <num-0> 
          | <num-0> '<=' <num-0>

<set-stm> = <ukn> 'in' <set-0>
          | <set-0> 'sub' <set-0>
          | <set-0> 'subeq' <set-0>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<num-0> = <num-0> '+' <num-1>
        | <num-0> '-' <num-1>
        | <num-1>

<num-1> = <num-1> '*' <num-2>
        | <num-1> '/' <num-2>
        | <num-2>

<num-2> = '-' <num-2> 
        | <num-3>

<num-3> = <num-4> '^' <num-3>
        | <num-4>

<num-4> = '(' <num-0> ')'
        | <num> | <sym>
        | <set-num>
        | <fun>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<set-0> = <set-0> 'union' <set-1>
        | <set-0> 'inter' <set-1>
        | <set-0> 'diffr' <set-1>
        | <set-1>
      
<set-1> = '{' <list-ukn> '}'
        | '{' <ukn> ':' <stm-0> '}'
        | <sym>
        | <fun>

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<list-ukn> = <ukn> (',' <ukn>)*

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<set-num> = '|' <set> '|'

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<fun> = <sym> '(' <list-ukn> ')'

-------------------------------------------------------------------------------

<lambda-expr> = <lambda-function> | <num-0> | <sym>

<lambda-function> = '(' '\' <sym> '.' <lambda-expr> ')' <lambda-expr>?

-------------------------------------------------------------------------------

<gmr> = <gmr-0> ('\n' <gmr-0>)*
      | <gmr-1>

<gmr-0> = <vars> '->' <gmr-1>

<gmr-1> = ...

------------------------------------------------------------------------------}

data Num = NInt Number
         | NDec Number Number

newtype Sym = Sym String

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data Exp = UknSym Sym
         | UknFun Fun
         | Stm0 Stm0
         | Num0 Num0
         | Set0 Set0

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

newtype Exps = Exps (Array Exp)

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data Stm0 = Where Stm1 Stm1
          | Stm1 Stm1

data Stm1 = Forall Stm2 Stm1
          | Exists Stm2 Stm1
          | Stm2 Stm2

data Stm2 = Eqq Exp Exp
          | Stm3 Stm3

data Stm3 = Implies Stm3 Stm4
          | Stm4 Stm4

data Stm4 = Con Stm5 Stm4
          | Dis Stm5 Stm4
          | Stm5 Stm5

data Stm5 = Not Stm5
          | Stm6 Stm6

data Stm6 = NumStm NumStm
          | SetStm SetStm
          | StmPar Stm0
          | StmT
          | StmF
          | StmSym Sym
          | StmFun Fun

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data NumStm = Gtt Num0 Num0
            | Ltt Num0 Num0
            | Geq Num0 Num0
            | Leq Num0 Num0
        
data SetStm = In Exp Set0
            | Subs Set0 Set0
            | Sube Set0 Set0

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data Num0 = Add Num0 Num1
          | Sub Num0 Num1
          | Num1 Num1

data Num1 = Mul Num1 Num2
          | Div Num1 Num2
          | Num2 Num2

data Num2 = Neg Num2
          | Num3 Num3

data Num3 = Pow Num4 Num3
          | Num4 Num4
        
data Num4 = NumPar Num0
          | Num Num
          | NumSym Sym
          | SetNum SetNum
          | NumFun Fun

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data Set0 = Union Set0 Set1
          | Inter Set0 Set1
          | Diffr Set0 Set1
          | Set1 Set1

data Set1 = Explicit Exps
          | Implicit Exp Stm0
          | SetSym Sym
          | SetFun Fun

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data SetNum = Card Set0

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

data Fun = Fun Sym Exps

{-----------------------------------------------------------------------------}

data LambdaEx = LambdaFun Sym LambdaEx (Maybe LambdaEx)
              | LambdaNum Num0
              | LambdaSym Sym

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Display
------------------------------------------------------------------------------}

instance showNum :: Show Num where
  show = case _ of
    NInt x -> show x
    NDec x y -> (show x) <> "." <> (show y)

instance showSym :: Show Sym where
  show (Sym x) = x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showExp :: Show Exp where
  show = case _ of
    UknSym x -> show x
    UknFun x -> show x
    Stm0 x -> show x
    Num0 x -> show x
    Set0 x -> show x
        
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showExps :: Show Exps where
  show (Exps xs) = case (head xs) of
    Nothing -> ""
    Just x -> case (tail xs) of
      Nothing -> show x
      Just xs -> foldl (\a x -> a <> ", " <> show x) (show x) xs
        
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showStm0 :: Show Stm0 where
  show = case _ of
    Where x y -> show x <> " | " <> show y
    Stm1 x -> show x

instance showStm1 :: Show Stm1 where
  show = case _ of
    Forall x y -> "forall " <> show x <> ". " <> show y
    Exists x y -> "exists " <> show x <> ". " <> show y
    Stm2 x -> show x 

instance showStm2 :: Show Stm2 where
  show = case _ of
    Eqq x y -> show x <> "=" <> show y
    Stm3 x -> show x

instance showStm3 :: Show Stm3 where
  show = case _ of
    Implies x y -> show x <> "->" <> show y
    Stm4 x -> show x

instance showStm4 :: Show Stm4 where
  show = case _ of
   Con x y -> show x <> " /\\ " <> show y
   Dis x y -> show x <> " \\/ " <> show y
   Stm5 x -> show x

instance showStm5 :: Show Stm5 where
  show = case _ of
    Not x -> "-" <> show x
    Stm6 x -> show x

instance showStm6 :: Show Stm6 where
  show = case _ of
    NumStm x -> show x
    SetStm x -> show x
    StmPar x -> "(" <> show x <> ")"
    StmT -> "true"
    StmF -> "false"
    StmSym x -> show x
    StmFun x -> show x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showNumStm :: Show NumStm where
  show = case _ of 
    Gtt x y -> show x <> " > " <> show y
    Ltt x y -> show x <> " < " <> show y
    Geq x y -> show x <> " >= " <> show y
    Leq x y -> show x <> " <= " <> show y 

instance showSetStm :: Show SetStm where
  show = case _ of
    In x y -> show x <> " in " <> show y
    Subs x y -> show x <> "sub" <> show y
    Sube x y -> show x <> "subeq" <> show y

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showNum0 :: Show Num0 where
  show = case _ of
    Add x y -> show x <> "+" <> show y
    Sub x y -> show x <> "-" <> show y
    Num1 x -> show x

instance showNum1 :: Show Num1 where
  show = case _ of
    Mul x y -> show x <> "*" <> show y
    Div x y -> show x <> "/" <> show y
    Num2 x -> show x

instance showNum2 :: Show Num2 where
  show = case _ of
    Neg x -> "-" <> show x
    Num3 x -> show x

instance showNum3 :: Show Num3 where
  show = case _ of
    Pow x y -> show x <> "^" <> show y
    Num4 x -> show x

instance showNum4 :: Show Num4 where
  show = case _ of
    NumPar x -> "(" <> show x <> ")"
    Num x -> show x
    NumSym x -> show x
    SetNum x -> show x
    NumFun x -> show x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


instance showSet0 :: Show Set0 where
  show = case _ of
    Union x y -> show x <> " union " <> show y
    Inter x y -> show x <> " inter " <> show y
    Diffr x y -> show x <> " diffr " <> show y
    Set1 x -> show x

instance showSet1 :: Show Set1 where
  show = case _ of
    Explicit x -> "{" <> show x <> "}"
    Implicit x y -> "{" <> show x <> " : " <> show y <> "}"
    SetSym x -> show x
    SetFun x -> show x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showSetNum :: Show SetNum where
  show = case _ of
    Card x -> "|" <> show x <> "|"


{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showFun :: Show Fun where
  show (Fun s xs) = show s <> "(" <> show xs <> ")"

{-----------------------------------------------------------------------------}

instance showLambda :: Show LambdaEx where
  show = case _ of
    LambdaFun x y z -> "(\\" <> show x <> ". " <> show y <> ")" <> show z
    LambdaNum x -> show x
    LambdaSym x -> show x

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Parser
-------------------------------------------------------------------------------

...

------------------------------------------------------------------------------}

fully :: forall b. Parser String b -> Parser String b
fully p = whiteSpace *> p <* eof

lexeme :: forall b. Parser String b -> Parser String b
lexeme p = p <* whiteSpace

token :: forall b. Parser String b -> Parser String b
token = lexeme <$> try

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

infix 4 cons as <:>

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

pNum :: Parser String Num
pNum = token $ (do a <- number
                   _ <- char '.'
                   b <- number
                   pure $ NDec a b)
                <|> (number <#> NInt)

pSym :: Parser String Sym
pSym = token $ (do x <- letter
                   xs <- many alphaNum
                   pure $ Sym $ fromCharArray (x <:> xs))

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

-- pExp :: Parser String Exp
-- pExp = pSym $> UknSym
-- --        <|> pFun $> UknFun
--        <|> pStm0 $> Stm0
-- --        <|> pNum0 $> Num0
-- --        <|> pSet0 $> Set0

-- {- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

-- pStm0 :: Parser String Stm0
-- pStm0 = chainr pStm1 (token $ char '|' $> Where)

-- pStm1 :: Parser String Stm1
-- pStm1 = (do _ <- token $ string "forall "
--             x <- pStm2
--             _ <- token $ string ". "
--             y <- pStm1
--             pure $ Forall x y)

-- pStm2 :: Parser String Stm2
-- pStm2 = defer \_ -> (do x <- pExp
--                         _ <- token $ char '='
--                         y <- pExp
--                         pure $ Eqq x y)

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Biased Generator
-------------------------------------------------------------------------------

...

------------------------------------------------------------------------------}

-- mkLambda :: Seed -> LambdaEx

{-----------------------------------------------------------------------------}

-- mkStm :: Seed -> Stm0 

{-----------------------------------------------------------------------------}

-- mkNum :: Seed -> Num0

{-----------------------------------------------------------------------------}

-- mkSet :: Seed -> Set0

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
CDCL
-------------------------------------------------------------------------------

Imperative pseudo-code from [Handbook of Satisfiability]:

    CDCL(phi, v)
        if (unit-propagation(phi, v) == conflict) then
            return UNSAT
        else
            dl <- 0
            while (not all-variables-assigned(phi,v))
                do (x,v) = pick-branching-variable(phi,v)
                    dl++
                    v <- v union {(x,v)}
                    if (unit-propagation(phi,v) == conflict) then
                        beta = conflict-analysis(phi,v)
                        if (beta < 0)
                            return UNSAT
                        else
                            backtrack(phi,v,beta)
                            dl <- beta
        return SAT

-------------------------------------------------------------------------------
CDCL(T)
-------------------------------------------------------------------------------

...

------------------------------------------------------------------------------}
--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Theory Engine
-------------------------------------------------------------------------------

...

------------------------------------------------------------------------------}
--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Quiz System
-------------------------------------------------------------------------------

...

------------------------------------------------------------------------------}
--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
References
-------------------------------------------------------------------------------

Handbook of Satisfiability
  Chapter 4
            Conflict-Driven Clause Learning
            SAT Solvers
  
  Armin Biere, Marijn Heule, Hans van Maaren and Toby Walsch
  https://www.cs.princeton.edu/~zkincaid/courses/fall18/readings/SATHandbook-CDCL.pdf

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

PureScript: Jordan's Reference
  https://jordanmartinez.github.io/purescript-jordans-reference-site/content/31-Design-Patterns/25-Parsing.html

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Design Patterns for Parser Combinators
  Jamie Willis, Nicolas Wu
  https://dl.acm.org/doi/pdf/10.1145/3471874.3472984

------------------------------------------------------------------------------}
--- /////////////////////////////////////////////////////////////////////// ---
