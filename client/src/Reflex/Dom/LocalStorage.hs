{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances
           , RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies
           , ConstraintKinds, TemplateHaskell, ForeignFunctionInterface
           , JavaScriptFFI, PatternSynonyms #-}

module Reflex.Dom.LocalStorage where


import Prelude hiding (mapM, mapM_, all, sequence)

import           GHCJS.Types
import qualified Data.JSString as JSS
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad.IO.Class
import Reflex
import Reflex.Dom
import qualified Data.Aeson as A

getLocalStorage :: String -> IO String
getLocalStorage dest = do 
  res <- js_getLocalStorage $ JSS.pack dest
  return $ JSS.unpack res

setLocalStorage :: String -> String -> IO ()
setLocalStorage dest val = js_setLocalStorage (JSS.pack dest) (JSS.pack val)

foreign import javascript unsafe
  "localStorage.getItem($1) || ''"
  js_getLocalStorage :: JSString -> IO JSString

foreign import javascript unsafe
  "localStorage.setItem($1, $2)"
  js_setLocalStorage :: JSString -> JSString -> IO ()


fromLocalStorage :: (MonadWidget t m, A.ToJSON a, A.FromJSON a) => String -> Event t (Maybe a -> Maybe a) -> m (Dynamic t (Maybe a))
fromLocalStorage storageName actions = do
  initialString <- liftIO $ getLocalStorage storageName
  values <- foldDyn ($) (A.decode (LC.pack initialString)) actions
  performEvent_ $ fmap (liftIO . (setLocalStorage storageName) . LC.unpack . A.encode ) $ updated values
  return values
