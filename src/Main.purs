module Main where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Control.Cycle (CYCLE, run2)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.XStream (fromCallback, STREAM, addListener, Stream)
import DOM (DOM)
import Data.Array (fromFoldable)
import Data.Generic (gShow, class Generic)
import Data.Maybe (Maybe(Just))
import Data.Monoid (mempty)
import Data.Set (insert, Set)
import Data.Tuple.Nested (tuple2)
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (awaitBody)
import Halogen.VirtualDOM.Driver (runUI)

data Direction
  = Left
  | Right
  | Up
  | Down

data Coords = Coords Int Int
derive instance genericCoords :: Generic Coords
instance showCoords :: Show Coords where
  show = gShow
derive instance eqCoords :: Eq Coords
derive instance ordCoords :: Ord Coords

data Query a
  = MoveCursor Direction a
  | ClearBoard a

type State =
  { cursor :: Coords
  , points :: Set Coords
  , width :: Int
  , height :: Int
  , increment :: Int
  }

initialState :: State
initialState =
  { cursor: Coords 0 0
  , points: mempty
  , width: 800
  , height: 600
  , increment: 10
  }

isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint state (Coords x y) =
  x < 0 || (state.increment * x) > (state.width - state.increment) ||
  y < 0 || (state.increment * y) > (state.height - state.increment)

ui :: forall m. H.Component HH.HTML Query Void m
ui = H.component {render, eval, initialState}
  where
    style = HH.prop (HH.PropName "style") (Just $ HH.AttrName "style")

    point inc color (Coords x y) = do
      let x' = show $ inc * x
      let y' = show $ inc * y
      let inc' = show inc
      HH.div
        [ style $
            "position: absolute; left: " <>
            x' <>
            "px; top: " <>
            y' <>
            "px; width: " <>
            inc' <>
            "px; height: " <>
            inc' <>
            "px; background-color: " <>
            color <>
            ";"
        ]
        []

    render :: State -> H.ComponentHTML Query
    render s = do
      let point' = point s.increment
      HH.div_
        [ HH.h1_
          [ HH.text "Hello!!!" ]
        , HH.div
            [ style $
                "position: relative; width: " <>
                show s.width <>
                "px; height: " <>
                show s.height <>
                "px; border: 1px solid black;"
            ] $
            (point' "black" <$> fromFoldable s.points) <>
            [point' "grey" s.cursor]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (MoveCursor direction next) = do
      H.modify moveCursor
      pure next
      where
        shiftCursor (Coords x y) = case direction of
          Up -> Coords x (y - 1)
          Down -> Coords x (y + 1)
          Left -> Coords (x - 1) y
          Right -> Coords (x + 1) y
        moveCursor :: State -> State
        moveCursor s = do
          let cursor' = shiftCursor s.cursor
          if isInvalidPoint s cursor'
            then s
            else
              s
              { cursor = cursor'
              , points = insert s.cursor s.points
              }
    eval (ClearBoard next) = do
      H.modify $ \s -> s {points = mempty :: Set Coords}
      pure next

yolo :: forall e a. Aff e a -> Eff e Unit
yolo = void <$> runAff (const $ pure unit) (const $ pure unit)

sendDirections :: forall e o.
  (H.HalogenIO Query o (Aff e)) ->
  Direction ->
  Eff e Unit
sendDirections app x =
  yolo $ app.query $ H.action $ MoveCursor x

dom :: forall e o.
  (H.HalogenIO Query o (Aff (H.HalogenEffects (stream :: STREAM | e)))) ->
  Stream Direction ->
  Eff
    (H.HalogenEffects
      ( stream :: STREAM
      | e
      )
    )
    (Stream Unit)
dom app s = do
  addListener
    { next: sendDirections app
    , error: const $ pure unit
    , complete: const $ pure unit
    }
    s
  pure mempty

foreign import data KEYBOARD :: !
foreign import onKeyboardDown :: forall e.
  ( Int ->
    Eff
      ( kb :: KEYBOARD
      , stream :: STREAM
      | e
      )
      Unit
  ) ->
  Eff
    ( kb :: KEYBOARD
    , stream :: STREAM
    | e
    )
    Unit

kb :: forall a e.
  Stream a ->
  Eff
    ( kb :: KEYBOARD
    , stream :: STREAM
    | e
    )
    (Stream Direction)
kb _ = do
  keycodes <- fromCallback onKeyboardDown
  pure $ keyCodeToQuery =<< keycodes
  where
    keyCodeToQuery = case _ of
      38 -> pure Up
      40 -> pure Down
      37 -> pure Left
      39 -> pure Right
      _ -> mempty

main :: forall e.
  Eff
    ( avar :: AVAR
    , ref :: REF
    , err :: EXCEPTION
    , dom :: DOM
    , stream :: STREAM
    , cycle :: CYCLE
    , kb :: KEYBOARD
    | e
    )
    Unit
main = runHalogenAff do
  body <- awaitBody
  app <- runUI ui body
  liftEff $ run2 main' (tuple2 (dom app) kb)
  pure unit
  where
    main' _ kb' = tuple2 kb' mempty
