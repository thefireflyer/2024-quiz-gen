module Data.Topic where

import Prelude
import Random.PseudoRandom

import Data.Array (cons, foldl, head, index, range, tail)
import Data.Array as Data.Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as Data.String
import Data.Time (Time)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Topics
------------------------------------------------------------------------------}

data Topic
 = LC -- Lambda Calculus
 | ST -- Set Theory
 | PL -- Predicate Logic
 | Re -- Relations
 | Fn -- Functions
 | FA -- Finite Automata

-------------------------------------------------------------------------------

instance topicShow :: Show Topic where
  show :: Topic -> String
  show = case _ of
    LC -> "Lambda Calculus"
    ST -> "Set Theory"
    PL -> "Predicate Logic"
    Re -> "Relations"
    Fn -> "Functions"
    FA -> "Finite Automata"

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Quiz Type
------------------------------------------------------------------------------}

type Quiz
 = { maxTime  :: Int
   , maxScore :: Int
   , problems :: Array Problem
   , results  :: Maybe Results }

-------------------------------------------------------------------------------

type Problem
 = { topic    :: Topic
   , score    :: Int
   , question :: Question
   , solution :: Solution }

-------------------------------------------------------------------------------

type Results
 = { time0    :: Time
   , time1    :: Time
   , score    :: Int 
   , answers  :: Array Solution }


--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Question & Answer Types
------------------------------------------------------------------------------}

data PForm
 = YN        
 | Find      
 | Simplify  
 | NSimplify 
 | TruthTable
 | Props     

-------------------------------------------------------------------------------

instance pformShow :: Show PForm where
  show :: PForm -> String
  show = case _ of
    YN -> "Y/N"
    Find -> "Find"
    Simplify -> "Simplify"
    NSimplify -> "Negate and then simplify"
    TruthTable -> "Truth table"
    Props -> "Properties"

-------------------------------------------------------------------------------

data Question
 = QYN         (Array BExpr)
 | QFind       (Array OExpr) BExpr
 | QSimplify   Expr
 | QNSimplify  BExpr
 | QTruthTable (Array BExpr)
 | QProps      OExpr

-------------------------------------------------------------------------------

data Solution
 = SYN          (Array Boolean)
 | SFind        (Array OExpr)
 | SSimplify    Expr
 | SNSimplify   BExpr
 | STruthTable  (Array (Array Boolean))
 | SProps       -- [todo]

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Expression Types
------------------------------------------------------------------------------}

data Expr = BExpr | OExpr

-------------------------------------------------------------------------------

data LExpr
 = LFn Char (Maybe LExpr)
 | LUn NExpr

-------------------------------------------------------------------------------

data BExpr
 = Eqs OExpr OExpr -- equal
 | Ltt NExpr NExpr -- less than
 | Gtt NExpr NExpr -- greater than
 | Leq NExpr NExpr -- less than or equal
 | Geq NExpr NExpr -- greater than or equal
 | Dvs NExpr NExpr -- divisible
 | InS OExpr SExpr -- in
 | SbS SExpr SExpr -- subset
 | Sbb SExpr SExpr -- strict subset
 | And BExpr BExpr -- and
 | Or_ BExpr BExpr -- or
 | If_ BExpr BExpr -- if
 | Iff BExpr BExpr -- iff
 | Not BExpr       -- not
 | B__ Boolean     -- boolean value
 | BS_ String      -- string value

-------------------------------------------------------------------------------

data OExpr = NExpr | SExpr

-------------------------------------------------------------------------------

data NExpr
 = Add NExpr NExpr
 | Sub NExpr NExpr
 | Mul NExpr NExpr
 | Div NExpr NExpr
 | Mod NExpr NExpr
 | Pow NExpr NExpr
 | Log NExpr NExpr
 | Cos NExpr
 | Sin NExpr
 | Tan NExpr
 | Abs OExpr
 | N__ Int
 | NS_ String

-------------------------------------------------------------------------------

data SExpr
 = Explicit (Array OExpr)
 | Implicit OExpr BExpr
 | SS_      String
 | NN
 | ZZ
 | QQ
 | RR
 | CC

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Generator
-------------------------------------------------------------------------------

Quiz:
- Individual times add up to 10 minutes
- Individual scores add up to 15
- Start with low time, low score, questions; end with high time, high score questions.

------------------------------------------------------------------------------}

mkQuiz :: Array Topic -> Effect Quiz
mkQuiz ts = do 
  seed <- randomSeed

  log $ "[quiz] " <> (show seed)

  problems <- mkPs2 seed 1 15 ts []

  pure $ { maxTime:  15
         , maxScore: 15
         , problems: []
         , results:  Nothing }

-------------------------------------------------------------------------------

mkPs2 :: Seed -> Int -> Int -> Array Topic -> Array Problem -> Effect (Array Problem)
mkPs2 seed ind max ts ys
 = if (ind <= max) then do

    let label = "[" <> show ind <> "/" <> show max  <> "] "
        blank = foldl (\y _ -> y <> " ") "" (range 1 (Data.String.length label))

    -- log label

    let intr = randomR 0 (Data.Array.length ts - 1) seed
        seed' = intr.newSeed
    
    problem <- case index ts intr.newVal of
                  Just LC -> do
                    log $ label <> "Lambda Calculus"

                    -- x
                    -- (\x. x)
                    -- (\x. x) y
                    -- (\x. x)(\y. y) z
                    -- ((\x. (\y.x*y)) 2) 2
                    -- let inner = \s depth sym -> do
                    --       if (depth == 0) then
                    --         pure LUn (NS_ sym)
                    --       else do
                    --         let {newValue: sym', newSeed: s'} = random s
                    --             {newValue: evald, newSeed: s''} = random s'
                    --         if evald then
                    --           -- (\x. ...) ...
                    --         else
                    --           -- (\x. ...)


                    -- start with answer
                    -- mutate to get question


                    log $ blank <> "Q: Simplify ..."
                    
                    log $ blank <> "S: ..."

                  Just ST -> do
                    log $ label <> "Set Theory"

                    let intr' = randomR 1 2 seed'
                        seed'' = intr'.newSeed

                    if intr'.newVal == 1 then do
                      log $ blank <> "Q: Simplify the following expression: ..."

                      log $ blank <> "S: ..."
                    else do
                      log $ blank <> "Evaluating propositions"


                  Just PL -> do
                    log $ label <> "Predicate Logic"

                    let intr' = randomR 1 5 seed'
                        seed'' = intr'.newSeed

                    case intr'.newVal of
                      1 -> log $ blank <> "Q: Translate the following into symbolic form:"
                      2 -> log $ blank <> "Q: Simplify the following: "
                      3 -> log $ blank <> "Q: Negate and then simplify the following: "
                      4 -> log $ blank <> "Q: Write out the truth table for the following:"
                      5 -> log $ blank <> "Q: Which of the following are the same: "
                      _ -> throw "Invalid question type"
                      
                  Just Re -> log $ label <> "Relations"
                  Just Fn -> log $ label <> "Functions"
                  Just FA -> log $ label <> "Finite Automata"
                  Nothing -> throw "Invalid topic selection"
    
    log ""
    mkPs2 seed' (ind+1) max ts ys

  else pure ys

-------------------------------------------------------------------------------

mkPs :: Seed -> Int -> Int -> Array Problem -> Effect (Array Problem)
mkPs seed i m ys
 = if (i <= m) then do
    log $ "[generating quiz] [" <> show i <> "/" <> show m <> "]"

    let intr = randomR 0 1 seed
        seed' = intr.newSeed
        topic = case intr.newVal of
                  1 -> LC
                  2 -> ST
                  3 -> PL
                  4 -> Re
                  5 -> Fn
                  6 -> FA
                  _ -> LC

    
    log $ "                  topic: " <> show topic

    let intr' = randomR 0 1 seed'
        seed'' = intr'.newSeed
        format = case intr'.newVal of
                  1 -> YN     
                  2 -> Find      
                  3 -> Simplify  
                  4 -> NSimplify 
                  5 -> TruthTable
                  6 -> Props     
                  _ -> YN

      

    log $ "                  format: " <> show format

    problem <- case topic of
                LC -> case format of
                        YN -> pure { topic: LC
                                   , score: 1
                                   , question: QYN $ [B__ false]
                                   , solution: SYN $ [false]}
                        _ -> throw "TODO: unimplemented"
                _ -> throw "TODO: unimplemented"

    mkPs seed'' (i+1) m (cons problem ys)
  
  else pure ys
    

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Problem Data
------------------------------------------------------------------------------}

pScore :: Question -> Int
pScore = case _ of
  QYN _         -> 1
  QFind _ _     -> 4
  QSimplify _   -> 2
  QNSimplify _  -> 3
  QTruthTable _ -> 2
  QProps _      -> 2

--- /////////////////////////////////////////////////////////////////////// ---