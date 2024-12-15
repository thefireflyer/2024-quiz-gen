module Data.Topics2 where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array.NonEmpty (toArray)
import Data.Int (toNumber)
import Data.List (List(Nil), all, elem, filter, notElem, (:), foldl)
import Data.Maybe (Maybe(..))
import Data.Number (exp, pow)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Class.Console (log)
import Parsing (Parser)
import Parsing.Combinators (between, sepBy, try, (<?>))
import Parsing.Combinators.Array (many1)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.String (char, eof, string)
import Parsing.String.Basic (intDecimal, letter, whiteSpace)
import Random.PseudoRandom (Seed, randomR, randomSeed)

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Expression Types
------------------------------------------------------------------------------}

data Expr 
 = BExpr BExpr
 | NExpr NExpr
 | SExpr SExpr

derive instance eqExpr :: Eq Expr

-------------------------------------------------------------------------------

data LExpr
 = LFn String LExpr (Maybe LExpr)
 | LUn NExpr

-------------------------------------------------------------------------------

data BExpr
 = Eqs Expr Expr   -- equal
 | Ltt NExpr NExpr -- less than
 | Gtt NExpr NExpr -- greater than
 | Leq NExpr NExpr -- less than or equal
 | Geq NExpr NExpr -- greater than or equal
 | Dvs NExpr NExpr -- divisible
 | InS Expr SExpr  -- in
 | SbS SExpr SExpr -- subset
 | Sbb SExpr SExpr -- strict subset
 | And BExpr BExpr -- and
 | Or_ BExpr BExpr -- or
 | If_ BExpr BExpr -- if
 | Iff BExpr BExpr -- iff
 | Not BExpr       -- not
 | B__ Boolean     -- boolean value
 | BS_ String      -- string value

derive instance eqBExpr :: Eq BExpr

-------------------------------------------------------------------------------

data NExpr
 = Add NExpr NExpr
 | Sub NExpr NExpr
 | Mul NExpr NExpr
 | Div NExpr NExpr
 | Mod NExpr NExpr
 | Pow NExpr NExpr
 | N__ Number
 | NS_ String

derive instance eqNExpr :: Eq NExpr

-------------------------------------------------------------------------------

data SExpr
 = Explicit (List Expr)
 | Implicit Expr BExpr
 | Union SExpr SExpr
 | Inter SExpr SExpr
 | Diffr SExpr SExpr
 | SS_   String
 | NN
 | ZZ
 | QQ
 | RR
 | CC

derive instance eqSExpr :: Eq SExpr

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Display
------------------------------------------------------------------------------}

instance showE :: Show Expr where
  show = case _ of
    BExpr x -> show x
    NExpr x -> show x
    SExpr x -> show x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showL :: Show LExpr where
  show = case _ of
    LFn x y (Just z) -> "(\\" <> show x <> ". " <> show y <> ") " <> show z
    LFn x y Nothing -> "(\\" <> show x <> ". " <> show y <> ")"
    LUn x -> show x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showB :: Show BExpr where
  show = case _ of
    Not x -> "\\neg (" <> show x <> ")"
    And x y -> "(" <> show x <> ") /\\ (" <> show y <> ")" 
    Or_ x y -> "(" <> show x <> ") \\/ (" <> show y <> ")"
    If_ x y -> "(" <> show x <> ") -> (" <> show y <> ")"
    Iff x y -> "(" <> show x <> ") <-> (" <> show y <> ")"
    B__ x -> show x
    BS_ x -> x
    Eqs x y -> "(" <> show x <> ") = (" <> show y <> ")"
    Ltt x y -> "(" <> show x <> ") < (" <> show y <> ")"
    Gtt x y -> "(" <> show x <> ") > (" <> show y <> ")"
    Leq x y -> "(" <> show x <> ") <= (" <> show y <> ")"
    Geq x y -> "(" <> show x <> ") >= (" <> show y <> ")"
    Dvs x y -> "(" <> show x <> ") | (" <> show y <> ")"
    InS x y -> "(" <> show x <> ") in (" <> show y <> ")"
    SbS x y -> "(" <> show x <> ") subeq (" <> show y <> ")"
    Sbb x y -> "(" <> show x <> ") sub (" <> show y <> ")"

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showN :: Show NExpr where
  show = case _ of
    Add x y -> "(" <> show x <> ") + (" <> show y <> ")"
    Sub x y -> "(" <> show x <> ") - (" <> show y <> ")"
    Mul x y -> "(" <> show x <> ") * (" <> show y <> ")"
    Div x y -> "(" <> show x <> ") / (" <> show y <> ")"
    Mod x y -> "(" <> show x <> ") % (" <> show y <> ")"
    Pow x y -> "(" <> show x <> ") ^ (" <> show y <> ")"
    N__ x -> show x
    NS_ x -> x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

instance showS :: Show SExpr where
  show = case _ of
    Explicit Nil -> "{}"
    Explicit (x:xs) -> "{" <> foldl (\a x -> a <> ", " <> show x) (show x) xs <> "}"
    Implicit x y -> "{" <> show x <> " : " <> show y <> "}"
    Union x y -> "(" <> show x <> ") \\cup (" <> show y <> ")"
    Inter x y -> "(" <> show x <> ") \\cap (" <> show y <> ")"
    Diffr x y -> "(" <> show x <> ") - (" <> show y <> ")"
    SS_ x -> x
    NN -> "NN"
    ZZ -> "ZZ"
    QQ -> "QQ"
    RR -> "RR"
    CC -> "CC"

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Parser
------------------------------------------------------------------------------}

fully :: forall b. Parser String b -> Parser String b
fully p = whiteSpace *> p <* eof

lexeme :: forall b. Parser String b -> Parser String b
lexeme p = p <* whiteSpace

token :: forall b. Parser String b -> Parser String b
token = lexeme <$> try

pSym :: forall a. (String -> a) -> Parser String a
pSym f = token $ many1 letter <#> toArray >>> fromCharArray >>> f

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

pTop :: Parser String Expr
pTop =    try (fully (pBExpr <#> BExpr) <?> "complete boolean expression")
      <|> try (fully (pNExpr <#> NExpr) <?> "complete numeric expression")
      <|>     (fully (pSExpr <#> SExpr) <?> "complete set expression")

pExpr :: Parser String Expr
pExpr = defer \_ ->    (pBExpr <#> BExpr)
                   <|> (pNExpr <#> NExpr)
                   <|> (pSExpr <#> SExpr)

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

pNExpr :: Parser String NExpr
pNExpr = defer \_ -> buildExprParser 
                    [ [ Infix (token $ string "^" $> Pow) AssocLeft  ]
                    , [ Infix (token $ string "/" $> Div) AssocRight ]
                    , [ Infix (token $ string "*" $> Mul) AssocRight ]
                    , [ Infix (token $ string "-" $> Sub) AssocRight ]
                    , [ Infix (token $ string "+" $> Add) AssocRight ]
                    ]   ((token $ intDecimal <#> toNumber >>> N__ )
                    <|> (pSym NS_)
                    <|> ((between (token $ char '(') (token $ char ')')  pNExpr)
                        <?> "numeric expression"))

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

pBExpr :: Parser String BExpr
pBExpr = defer \_ -> buildExprParser 
                    [ [ Prefix (token $ string "!"     $> Not)]
                    , [ Infix  (token $ string "\\/"   $> Or_) AssocRight ]
                    , [ Infix  (token $ string "/\\"   $> And) AssocRight ]
                    , [ Infix  (token $ string "->"    $> If_) AssocRight ]
                    , [ Infix  (token $ string "<->"   $> Iff) AssocNone  ]
                    ]   ((token $ string "true"  $> B__ true )
                    <|> (token $ string "false" $> B__ false)
                    <|> (pSym BS_)
                    <|> ((between  (token $ char '(') (token $ char ')')  pBExpr)
                        <?> "boolean expression"))

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

pSExpr :: Parser String SExpr
pSExpr = defer \_ -> buildExprParser
                     [ [ Infix (token $ string "-"     $> Diffr) AssocRight ] 
                     , [ Infix (token $ string "\\cup" $> Union) AssocRight ]
                     , [ Infix (token $ string "\\cap" $> Inter) AssocRight ]
                     ]  ((string "CC" $> CC)
                    <|> (string "RR" $> RR)
                    <|> (string "NN" $> NN)
                    <|> (string "ZZ" $> ZZ)
                    <|> (pSym SS_)
                    <|> (between (token $ char '(') (token $ char ')') pSExpr)
                    <|> (between (token $ char '{') (token $ char '}')
                            (pExpr `sepBy` (token $ char ',')) <#> Explicit)
                    <|> ((between (token $ char '{') (token $ char '}')
                            (do x <- pExpr
                                _ <- token $ char ':'
                                y <- pBExpr
                                pure $ Implicit x y)))
                        <?> "set expression")

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Basic Simplify
------------------------------------------------------------------------------}

rr :: Expr -> Expr
rr = case _ of
  BExpr x -> BExpr $ rB x
  NExpr x -> NExpr $ rN x
  SExpr x -> SExpr $ rS x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

rB :: BExpr -> BExpr
rB = case _ of
  Eqs a b -> (case Eqs (rr a) (rr b) of
                Eqs (BExpr a') (BExpr b') -> Iff a' b'
                Eqs (NExpr _) (BExpr _) -> B__ false 
                Eqs (BExpr _) (NExpr _) -> B__ false 
                Eqs (NExpr _) (SExpr _) -> B__ false 
                Eqs (SExpr _) (NExpr _) -> B__ false 
                Eqs (SExpr _) (BExpr _) -> B__ false 
                Eqs (BExpr _) (SExpr _) -> B__ false
                Eqs a' b' | a' == b' -> B__ true
                Eqs (SExpr (Explicit as)) (SExpr (Explicit bs)) -> B__ (all (\a -> a `elem` bs) as && all (\b -> b `elem` as) bs)
                Eqs (SExpr (Implicit a ac)) (SExpr (Implicit b bc)) | a==b -> Iff ac bc
                x -> x )
  And a b -> (case And (rB a) (rB b) of
                  And (B__ a') (B__ b') -> B__ (a' && b')
                  x -> x)
  Or_ a b -> (case Or_ (rB a) (rB b) of
                  Or_ (B__ a') (B__ b') -> B__ (a' || b')
                  x -> x)
  If_ a b -> (case If_ (rB a) (rB b) of
                  If_ (B__ a') (B__ b') -> B__ (if a' then b' else true)
                  x -> x)
  Iff a b -> (case If_ (rB a) (rB b) of
                  If_ (B__ a') (B__ b') -> B__ (a' == b')
                  x -> x)
  Not a -> (case rB a of
              And x y -> Or_ (Not x) (Not y)
              Or_ x y -> And (Not x) (Not y)
              If_ x y -> And x (Not y)
              Iff x y -> Or_ (And x (Not y)) (And (Not x) y)
              Not x -> x
              B__ true -> B__ false
              B__ false -> B__ true
              Ltt x y -> Geq x y
              Gtt x y -> Leq x y
              Leq x y -> Gtt x y
              Geq x y -> Ltt x y 
              x -> Not x)
  Ltt a b -> (case Ltt (rN a) (rN b) of 
                Ltt (N__ a') (N__ b') -> B__ (a' < b')
                x -> x)
  Gtt a b -> (case Gtt (rN a) (rN b) of 
                Gtt (N__ a') (N__ b') -> B__ (a' > b')
                x -> x)
  Leq a b -> (case Leq (rN a) (rN b) of 
                Leq (N__ a') (N__ b') -> B__ (a' <= b')
                x -> x)
  Geq a b -> (case Geq (rN a) (rN b) of 
                Geq (N__ a') (N__ b') -> B__ (a' >= b')
                x -> x)
  Dvs a b -> (case Dvs (rN a) (rN b) of
                Dvs (N__ a') (N__ b') -> B__ (a' `mod` b' == (toNumber 0))
                x -> x)
  InS a b -> (case InS (rr a) (rS b) of 
                InS a' (Explicit xs) | a' `elem` xs ->  B__ true
                x -> x)
  SbS a b -> (case SbS (rS a) (rS b) of
                SbS (Explicit as) (Explicit bs) -> B__ (all (\a -> a `elem` bs) as)
                x -> x)
  x -> x


{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

rN :: NExpr -> NExpr
rN = case _ of
  Add a b -> (case Add (rN a) (rN b) of
                Add (N__ a) (N__ b) -> N__ (a+b)
                Add a b | a==b -> Mul (N__ (toNumber 2)) a
                x -> x)
  Sub a b -> (case Sub (rN a) (rN b) of
                Sub (N__ a) (N__ b) -> N__ (a-b)
                Sub a b | a==b -> N__ (toNumber 0)
                x->x)
  Mul a b -> (case Mul (rN a) (rN b) of
                Mul (N__ a) (N__ b) -> N__ (a*b)
                Mul a b | a==b -> Pow a (N__ (toNumber 2))
                x->x)
  Div a b -> (case Div (rN a) (rN b) of
                Div (N__ a) (N__ b) -> N__ (a/b)
                Div a b | a==b -> N__ (toNumber 1)
                x->x)
  Mod a b -> (case Mod (rN a) (rN b) of
                Mod (N__ a) (N__ b) -> N__ (a `mod` b)
                Mod a b | a==b -> N__ (toNumber 0)
                x->x)
  Pow a b -> (case Pow (rN a) (rN b) of
                Pow (N__ a) (N__ b) -> N__ (a `pow` b)
                Pow (Pow a b) c -> Pow a (Mul b c)
                x->x)
  x -> x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

rS :: SExpr -> SExpr
rS = case _ of
  Explicit xs -> Explicit (map rr xs)
  Implicit x xc -> Implicit (rr x) (rB xc)
  Union (Explicit as) (Explicit bs) -> Explicit ((filter (\a -> a `notElem` bs) as) <> bs)
  Inter (Explicit as) (Explicit bs) -> Explicit (filter (\a -> a `elem` bs) as)
  Diffr (Explicit as) (Explicit bs) -> Explicit (filter (\a -> a `notElem` bs) as)
  Union (Implicit a ac) (Implicit b bc) | a==b -> Implicit a (Or_ ac bc)
  Inter (Implicit a ac) (Implicit b bc) | a==b -> Implicit a (And ac bc)
  Diffr (Implicit a ac) (Implicit b bc) | a==b -> Implicit a (And ac (Not bc))
  x -> x

{-----------------------------------------------------------------------------}

free :: Expr -> Boolean
free = case _ of
  BExpr x -> freeB x
  NExpr x -> freeN x
  SExpr x -> freeS x

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

freeB :: BExpr -> Boolean
freeB = case _ of
  Eqs a b -> free a && free b
  Ltt a b -> freeN a && freeN b
  Gtt a b -> freeN a && freeN b
  Leq a b -> freeN a && freeN b
  Geq a b -> freeN a && freeN b
  Dvs a b -> freeN a && freeN b
  InS a b -> free a && freeS b
  SbS a b -> freeS a && freeS b
  Sbb a b -> freeS a && freeS b
  And a b -> freeB a && freeB b
  Or_ a b -> freeB a && freeB b
  If_ a b -> freeB a && freeB b
  Iff a b -> freeB a && freeB b
  Not a -> freeB a
  B__ _ -> false
  BS_ _ -> true

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

freeN :: NExpr -> Boolean
freeN = case _ of
  Add a b -> freeN a && freeN b
  Sub a b -> freeN a && freeN b
  Mul a b -> freeN a && freeN b
  Div a b -> freeN a && freeN b
  Mod a b -> freeN a && freeN b
  Pow a b -> freeN a && freeN b
  N__ _ -> false
  NS_ _ -> true

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

freeS :: SExpr -> Boolean
freeS _ = false -- todo!

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Biased Generator
------------------------------------------------------------------------------}

--- probability of termination
pOfT :: Number -> Number
pOfT x = l / ( (toNumber 1) + (exp (-k * (x - x0)) ))
  where 
    l  = toNumber 100 --- maximum probability of termination
    k  = toNumber 2   --- the logistic probability growth rate 
    x0 = toNumber 3   --- function mid point; 50% probability of termination at x_0


--- probability of type closure
pOfC :: Int
pOfC = 60

{-----------------------------------------------------------------------------}

mkX :: forall a. (Int -> Seed -> a) -> Effect a
mkX f = do
  seed <- randomSeed
  log $ "[seed]" <> (show seed)
  pure $ f 1 seed

{-----------------------------------------------------------------------------}

mkExpr :: Int -> Seed -> Expr
mkExpr d s = let r = randomR 0 100 s
            in if r.newVal < 40 then
              BExpr $ mkBExpr d r.newSeed
            else if r.newVal < 80 then
              NExpr $ mkNExpr d r.newSeed
            else
              SExpr $ mkSExpr d r.newSeed

{-----------------------------------------------------------------------------}

mkBExpr :: Int -> Seed -> BExpr 
mkBExpr d s = let r = randomR 0 100 s
                in if toNumber r.newVal < pOfT (toNumber d) then
                  B__ $ mkBool r.newSeed
                else
                  let rc = randomR 0 100 r.newSeed
                    in if rc.newVal < pOfC then
                      let r2 = randomR 0 100 rc.newSeed
                        in if r2.newVal < 25 then
                          let r3 = randomR 0 100 r2.newSeed
                            in And (mkBExpr (d+1) r2.newSeed) (mkBExpr (d+1) r3.newSeed)
                        else if r2.newVal < 50 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Or_ (mkBExpr (d+1) r2.newSeed) (mkBExpr (d+1) r3.newSeed)
                        else if r2.newVal < 75 then
                          Not $ mkBExpr (d+1) r.newSeed
                        else if r2.newVal < 80 then
                          let r3 = randomR 0 100 r2.newSeed
                            in If_ (mkBExpr (d+1) r2.newSeed) (mkBExpr (d+1) r3.newSeed)
                        else
                          let r3 = randomR 0 100 r2.newSeed
                            in Iff (mkBExpr (d+1) r2.newSeed) (mkBExpr (d+1) r3.newSeed)
                    else
                      let r2 = randomR 0 100 rc.newSeed
                        in if r2.newVal < 20 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Eqs (mkExpr (d+1) r2.newSeed) (mkExpr (d+1) r3.newSeed)
                        else if r2.newVal < 30 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Ltt (mkNExpr (d+1) r2.newSeed) (mkNExpr (d+1) r3.newSeed)
                        else if r2.newVal < 40 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Gtt (mkNExpr (d+1) r2.newSeed) (mkNExpr (d+1) r3.newSeed)
                        else if r2.newVal < 50 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Geq (mkNExpr (d+1) r2.newSeed) (mkNExpr (d+1) r3.newSeed)
                        else if r2.newVal < 60 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Leq (mkNExpr (d+1) r2.newSeed) (mkNExpr (d+1) r3.newSeed)
                        else if r2.newVal < 70 then
                          let r3 = randomR 0 100 r2.newSeed
                            in Dvs (mkNExpr (d+1) r2.newSeed) (mkNExpr (d+1) r3.newSeed)
                        else if r2.newVal < 80 then
                          let r3 = randomR 0 100 r2.newSeed
                            in InS (mkExpr (d+1) r2.newSeed) (mkSExpr (d+1) r3.newSeed)
                        else if r2.newVal < 90 then
                          let r3 = randomR 0 100 r2.newSeed
                            in SbS (mkSExpr (d+1) r2.newSeed) (mkSExpr (d+1) r3.newSeed)
                        else
                          let r3 = randomR 0 100 r2.newSeed
                            in SbS (mkSExpr (d+1) r2.newSeed) (mkSExpr (d+1) r3.newSeed)

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

mkBool :: Seed -> Boolean
mkBool s = let r = randomR 0 100 s
              in if r.newVal < 50 then true
              else false

{-----------------------------------------------------------------------------}

mkNExpr :: Int -> Seed -> NExpr
mkNExpr d s = 
  let pt = randomR 0 100 s
    in if toNumber pt.newVal < pOfT (toNumber d) then
      N__ $ toNumber $ mkNum pt.newSeed
    else
      let r = randomR 0 100 pt.newSeed
        in if r.newVal < 20 then
          let r2 = randomR 0 100 r.newSeed
            in Add (mkNExpr (d+1) r.newSeed) (mkNExpr (d+1) r2.newSeed)
          else if r.newVal < 40 then
            let r2 = randomR 0 100 r.newSeed
              in Sub (mkNExpr (d+1) r.newSeed) (mkNExpr (d+1) r2.newSeed)
          else if r.newVal < 60 then
            let r2 = randomR 0 100 r.newSeed
              in Mul (mkNExpr (d+1) r.newSeed) (mkNExpr (d+1) r2.newSeed)
          else if r.newVal < 80 then
            let r2 = randomR 0 100 r.newSeed
              in Div (mkNExpr (d+1) r.newSeed) (mkNExpr (d+1) r2.newSeed)
          else
            let r2 = randomR 0 100 r.newSeed
              in Pow (mkNExpr (d+1) r.newSeed) (mkNExpr (d+1) r2.newSeed)
            
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

mkNum :: Seed -> Int
mkNum s = (randomR 0 500 s).newVal

{-----------------------------------------------------------------------------}

mkSExpr :: Int -> Seed -> SExpr
mkSExpr d s = let r = randomR 0 100 s
                in if toNumber r.newVal < pOfT (toNumber d) then
                  let r2 = randomR 0 100 r.newSeed in
                  if r2.newVal < 30 then
                    let r3 = randomR 0 100 r2.newSeed 
                        r4 = randomR 0 100 r3.newSeed in
                    Explicit ((mkExpr (d+1) r2.newSeed) : (mkExpr (d+1) r3.newSeed) : (mkExpr (d+1) r4.newSeed) : Nil)
                  else if r2.newVal < 50 then
                    let r3 = randomR 0 100 r2.newSeed in
                    Implicit (mkExpr (d+1) r2.newSeed) (mkBExpr 2 r3.newSeed)
                  else if r2.newVal < 60 then NN
                  else if r2.newVal < 70 then ZZ
                  else if r2.newVal < 80 then QQ
                  else if r2.newVal < 90 then RR
                  else CC
                else
                  let r2 = randomR 0 100 r.newSeed in
                  if r2.newVal < 33 then
                    let r3 = randomR 0 100 r2.newSeed in
                    Union (mkSExpr (d+1) r2.newSeed) (mkSExpr (d+1) r3.newSeed)
                  else if r2.newVal < 66 then
                    let r3 = randomR 0 100 r2.newSeed in
                    Inter (mkSExpr (d+1) r2.newSeed) (mkSExpr (d+1) r3.newSeed)
                  else
                    let r3 = randomR 0 100 r2.newSeed in
                    Diffr (mkSExpr (d+1) r2.newSeed) (mkSExpr (d+1) r3.newSeed)

{-----------------------------------------------------------------------------}

mkXrX :: Effect Unit
mkXrX = do
  ex <- mkX mkExpr
  log $ show ex
  log $ "==>" <> show (rr ex)

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
