module Main where

import CSS (absolute, backgroundColor, black, border, grey, height, left, position, px, relative, solid, top, width)
import Control.Cycle (runRecord)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.XStream (STREAM, Stream, addListener, fromCallback)
import DOM (DOM)
import Data.Array (fromFoldable)
import Data.Generic (gShow, class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Set (insert, Set)
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (awaitBody)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.VDom.Driver (runUI)
import Prelude hiding (top)

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

ui :: forall e. H.Component HH.HTML Query Unit Void (Aff e)
ui = H.component {render, eval, initialState: const initialState, receiver: const Nothing}
  where
    px' = px <<< toNumber
    point inc color (Coords x y) = do
      let x' = inc * x
      let y' = inc * y
      HH.div
        [ style do
            position absolute
            left (px' x')
            top (px' y')
            width (px' inc)
            height (px' inc)
            backgroundColor color
        ]
        []

    render :: State -> H.ComponentHTML Query
    render s = do
      let point' = point s.increment
      HH.div_
        [ HH.h1_
          [ HH.text "Hello!!!" ]
        , HH.div
            [ style do
                position relative
                width (px' s.width)
                height (px' s.height)
                border solid (px' 1) black
            ] $
            (point' black <$> fromFoldable s.points) <>
            [point' grey s.cursor]
        ]

    eval :: Query ~> H.ComponentDSL State Query Void (Aff e)
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
  (H.HalogenIO Query o (Aff (stream :: STREAM | e))) ->
  Stream Direction ->
  Eff
    ( stream :: STREAM
    | e
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

foreign import data KEYBOARD :: Effect
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

kb :: forall e.
  Stream Unit ->
  Eff
    ( kb :: KEYBOARD
    , stream :: STREAM
    | e
    )
    (Stream Direction)
kb _ = do
  keys <- fromCallback onKeyboardDown
  pure $ keyCodeToQuery =<< keys
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
    , exception :: EXCEPTION
    , dom :: DOM
    , stream :: STREAM
    | e
    )
    Unit
main = runHalogenAff do
  body <- awaitBody
  app <- runUI ui unit body
  let drivers = {dom: dom app, kb}
  _ <- liftEff $ runRecord main' drivers
  pure unit
  where
    main' :: { dom :: Stream Unit, kb :: Stream Direction } -> { dom :: Stream Direction, kb :: Stream Unit }
    main' sources = {dom: sources.kb, kb: mempty}
