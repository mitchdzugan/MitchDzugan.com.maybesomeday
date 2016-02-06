{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module Main where

import Prelude hiding (mapM, mapM_, all, sequence)

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

getUsers :<|> _ = client api (BaseUrl Http "localhost" 8080 "") manager

main = mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown "*" (constDyn ops) def
  ny <- numberInput
  values <- combineDyn (,) nx ny
  result <- combineDyn (\o (x,y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
  resultString <- mapDyn show result
  text " = "
  dynText resultString

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
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

stringToOp s = case s of
                      "-" -> (-)
                      "*" -> (*)
                      "/" -> (/)
                      _ -> (+)

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]
