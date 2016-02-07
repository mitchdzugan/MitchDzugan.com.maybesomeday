{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module Main where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Network.HTTP.Client as C
import Data.Proxy
import System.IO.Unsafe (unsafePerformIO)
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))

import Reflex
import Reflex.Dom

import Shared.Api

import Servant.API
import Servant.Client

manager = unsafePerformIO $ C.newManager C.defaultManagerSettings

api :: Proxy API
api = Proxy

getUsers :<|> getAdd :<|> getSub :<|> getMult :<|> getDiv :<|> _ = client api (BaseUrl Http "localhost" 8080 "") manager

fmapApi apif e = performEvent $ fmap (liftIO . runExceptT . apif) e

main = mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown "+" (constDyn ops) def
  ny <- numberInput
  values <- combineDyn (,) nx ny
  events <- combineDyn stringToOp (_dropdown_value d) values
  res <- fmapApi id $ updated events
  resString <- holdDyn "0" $ fmap show res
  text " = "
  dynText resString

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Integer))
numberInput = do
  let errorState = Map.singleton "style" "border-color: red"
      validState = Map.singleton "style" "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                       & textInputConfig_initialValue .~ "0"
                       & textInputConfig_attributes .~ attrs
      result <- mapDyn readMay $ _textInput_value n
      attrs <- mapDyn (\r -> case r of
                                  Just _ -> validState
                                  Nothing -> errorState) result
  return result

stringToOp _ (Nothing, _) = return 0
stringToOp _ (_, Nothing) = return 0
stringToOp "-" ((Just x), (Just y)) = getSub x y
stringToOp "*" ((Just x), (Just y)) = getMult x y
stringToOp "/" ((Just x), (Just y)) = getDiv x y
stringToOp _   ((Just x), (Just y)) = getAdd x y

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]
