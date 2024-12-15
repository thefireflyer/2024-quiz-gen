module Page.Quiz where

import Prelude

import Data.Topic (Topic, Quiz)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen (AttrName(..), ClassName(..), Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Component Types
------------------------------------------------------------------------------}

type Input = Quiz

-------------------------------------------------------------------------------

data State = NoState

-------------------------------------------------------------------------------

data Action = NoOp

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Component Functions
------------------------------------------------------------------------------}

initialState :: forall input. input -> State
initialState _ = NoState

-------------------------------------------------------------------------------

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
 NoOp -> H.liftEffect $ log "no op"

-------------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render = case _ of
    NoState -> HH.div [HP.id "root"]
                      [HH.div [HP.class_ $ ClassName "problem"]
                              [HH.p_ [HH.text "Question 1: ..."]
                              ,HH.p_ [HH.text "$\\frac{5+5}{2}$"]
                              ,HH.p_ [HH.text "$$\\frac{5+5}{2}$$"]
                              ,HH.textarea [HP.placeholder "$$\\frac{5+5}{2}$$"]]
                              -- ,HH.input [HP.type_ HP.InputText, HP.attr (AttrName "contenteditable") "true"]]
                      ,HH.div [HP.class_ $ ClassName "topic"]
                              [HH.div_ [HH.text "9:53 remaining"]
                              ,HH.button [HE.onClick \_ -> NoOp] [HH.text "Submit"]]]

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Quiz Component
------------------------------------------------------------------------------}

quiz ∷ forall query input output m. MonadAff m => Component query input output m
quiz =
  H.mkComponent
    {
      initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

--- /////////////////////////////////////////////////////////////////////// ---