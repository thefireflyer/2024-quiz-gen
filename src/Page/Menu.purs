module Page.Menu where

import Prelude
import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Component Types
------------------------------------------------------------------------------}

type Input = Unit

-------------------------------------------------------------------------------

data State
 = Loading
 | Ready { scores :: Array (Array Int)}

-------------------------------------------------------------------------------

data Action = NoOp

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Component Functions
------------------------------------------------------------------------------}

initialState :: forall input. input -> State
initialState _ = Loading

-------------------------------------------------------------------------------

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
 NoOp -> H.modify_ \s -> s

-------------------------------------------------------------------------------

render :: forall m. State -> H.ComponentHTML Action () m
render = case _ of
    Loading -> HH.text "Loading..."
    Ready scores -> HH.text $ show $ scores

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Menu Component
------------------------------------------------------------------------------}

menu ∷ forall query input output m. Component query input output m
menu =
  H.mkComponent
    {
      initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

--- /////////////////////////////////////////////////////////////////////// ---