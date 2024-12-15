module Main where

import Prelude

import Data.Common (OpaqueSlot)
import Data.Topic (Quiz, Topic(..), mkQuiz)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign.MathJax (refresh)
import Halogen (ClassName(..), Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Page.Quiz (quiz)
import Type.Proxy (Proxy(..))

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
Main Function
------------------------------------------------------------------------------}

main :: Effect Unit
main = 
  void $ mkQuiz [LC, ST, PL]
  -- HA.runHalogenAff do
    
  --   body <- HA.awaitBody  
  --   runUI app unit body


--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
App Data
-------------------------------------------------------------------------------}

data State = Menu
           | Loading Int
           | Test Quiz

-------------------------------------------------------------------------------

type ChildSlots =
  ( menu :: OpaqueSlot Unit
  , quiz :: OpaqueSlot Unit
  )

-------------------------------------------------------------------------------

data Action 
  = Start (Array Topic)
  | Final
  | Exit

--- /////////////////////////////////////////////////////////////////////// ---
{------------------------------------------------------------------------------
App Component
-------------------------------------------------------------------------------}

app ∷ forall query input output m. MonadAff m => Component query input output m
app =
  H.mkComponent
    {
      initialState: \_ -> Menu
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where 
    ---------------------------------------------------------------------------

    render = case _ of
      Menu -> menu
      Loading pr -> loading pr
      Test d
           -> HH.slot_ (Proxy :: _ "quiz") unit quiz d

    ---------------------------------------------------------------------------

    handleAction = case _ of
      Start ts -> do 
        H.put $ Loading 0
        
        H.liftEffect $ log "[app] Starting quiz"
        H.liftEffect $ log $ "[app] Topics: " <> show ts
        quiz <- H.liftEffect $ mkQuiz ts
        -- pure unit
        H.put $ Test quiz

        H.liftEffect $ refresh unit

      Final -> H.liftEffect $ throw "Not implemented!"
      Exit  -> H.modify_ \_ -> Menu

    ---------------------------------------------------------------------------

    menu = HH.div [HP.id "root"]
                  [HH.h1_ [HH.text "CSCI 301 Practice"]
                  ,startQuiz
                  ,HH.div_ menuItems]
  
    ---------------------------------------------------------------------------

    startQuiz = HH.div [HP.class_ $ ClassName "topic"]
                          [HH.text "Practice Quiz"
                          ,HH.button [HE.onClick \_ -> Start [LC, ST, PL, Re, Fn, FA]]
                                    [HH.text "Start"]]


    ---------------------------------------------------------------------------

    menuItems = map menuItem [LC, ST, PL, Re, Fn, FA]
    menuItem topic = HH.div [HP.class_ $ ClassName "topic"]
                          [HH.text $ show topic
                          ,HH.button [HE.onClick \_ -> Start [topic]]
                                    [HH.text "Start"]]

    ---------------------------------------------------------------------------

    loading pr = HH.div [HP.id "loading"]
                     [HH.div [HP.id "comm"]
                             [HH.div_ [HH.text "Loading..."]
                             ,HH.div_ [HH.text $ show pr <> "%"]]
                     ,HH.div [HP.id "progress-bar"]
                             [HH.div [HP.id "progress"
                                     ,HP.style $ "width: " <> show pr <> "%"] []]]

    ---------------------------------------------------------------------------

--- /////////////////////////////////////////////////////////////////////// ---