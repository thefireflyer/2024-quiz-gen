module Component.Router where

-- import Data.Common
-- import Prelude

-- import Data.Topic (Topic(..))
-- import Effect.Exception (Error)
-- import Halogen (Component)
-- import Halogen as H
-- import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties (id)
-- import Page.Menu as MenuPage
-- import Page.Quiz as QuizPage
-- import Type.Proxy (Proxy(..))

-- --- /////////////////////////////////////////////////////////////////////// ---
-- {------------------------------------------------------------------------------
-- Component Types
-- -------------------------------------------------------------------------------}

-- type Input = Unit

-- -------------------------------------------------------------------------------

-- data State = Menu
--            | Quiz  Int (Array Topic)
--            | Final Int
--            | Error Error

-- -------------------------------------------------------------------------------

-- data Action = NoOp

-- -------------------------------------------------------------------------------

-- data Query = Navigate

-- -------------------------------------------------------------------------------

-- type ChildSlots =
--   ( menu :: OpaqueSlot Unit
--   , quiz :: OpaqueSlot Unit
--   )

-- --- /////////////////////////////////////////////////////////////////////// ---
-- {------------------------------------------------------------------------------
-- Component Functions
-- -------------------------------------------------------------------------------}

-- initialState :: forall input. input -> State
-- initialState _ = Menu

-- -------------------------------------------------------------------------------

-- handleAction :: forall output m. 
--   Action -> H.HalogenM State Action ChildSlots output m Unit

-- handleAction = case _ of
--  NoOp -> H.modify_ \s -> s

-- -------------------------------------------------------------------------------

-- handleQuery :: forall output m. 
--   Query -> H.HalogenM State Action ChildSlots output m Unit
  
-- handleQuery = case _ of
--   Navigate -> H.modify_ \s -> s

-- -------------------------------------------------------------------------------

-- render :: forall m. State -> H.ComponentHTML Action ChildSlots m
-- render = case _ of
--     Menu      -> HH.slot_ (Proxy :: _ "menu") unit MenuPage.component unit
--     Quiz  _ _ -> HH.slot_ (Proxy :: _ "quiz") unit QuizPage.component unit
--     Final _   -> HH.div_ [HH.text "Final"]
--     Error e   -> HH.div_ [HH.text (show e)]

-- --- /////////////////////////////////////////////////////////////////////// ---
-- {------------------------------------------------------------------------------
-- Router Component
-- -------------------------------------------------------------------------------}

-- component ∷ forall query input output m. Component query input output m
-- component =
--   H.mkComponent
--     {
--       initialState
--     , render
--     , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
--                                      , handleQuery  = handleQuery }
--     }

-- --- /////////////////////////////////////////////////////////////////////// ---
-- {------------------------------------------------------------------------------
-- Main Menu
-- -------------------------------------------------------------------------------}

-- -- menu ∷ ∀ w. HH.HTML w Action
-- -- menu = HH.div 
-- --         [id "page"]
-- --         [ HH.h1_ [HH.text "CSCI 301 Practice"]
-- --         , HH.div [id "page-inner"] menuItems]

-- -- menuItems ∷ ∀ w. Array (HH.HTML w Action)
-- -- menuItems = [ HH.p_ [HH.a [HE.onClick \_ -> Navigate (Quiz 0 [LambdaCalculus])] 
-- --                           [HH.text "Week 1 : Lambda Calculus"]]
-- --             -- , HH.p_ [HH.a_ [HH.text "Week 2 : Lambda Calculus"]]
-- --             -- , HH.p_ [HH.a_ [HH.text "Week 3 : Lambda Calculus"]]
-- --             -- , HH.p_ [HH.a_ [HH.text "Week 4 : Lambda Calculus"]]
-- --             -- , HH.p_ [HH.a_ [HH.text "Week 5 : Lambda Calculus"]]
-- --             , HH.p_ [HH.a_ [HH.text "Week _ : Final Practice"]]]


-- --- /////////////////////////////////////////////////////////////////////// ---
-- {------------------------------------------------------------------------------
-- Quiz Page
-- -------------------------------------------------------------------------------}



-- --- /////////////////////////////////////////////////////////////////////// ---