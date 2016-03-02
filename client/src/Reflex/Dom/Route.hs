{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances
           , RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies
           , ConstraintKinds, TemplateHaskell, ForeignFunctionInterface
           , JavaScriptFFI, PatternSynonyms #-}

module Reflex.Dom.Route where


import Prelude hiding (mapM, mapM_, all, sequence)

import           GHCJS.Types
import qualified Data.JSString as JSS
import Control.Monad
import Data.ByteString.Char8
import Control.Monad.IO.Class
import qualified Data.Text as T
import Web.Routes.Boomerang (boomerangSite,Router)
import Text.Boomerang.HStack
import Reflex
import Reflex.Dom
import Web.Routes

getWindowLocation :: IO String
getWindowLocation = do
    liftM JSS.unpack $ js_windowLocationPathname

pushState :: String -> IO ()
pushState s = js_windowHistoryPushState $ JSS.pack s

foreign import javascript unsafe
  "window.location.pathname"
  js_windowLocationPathname :: IO JSString

foreign import javascript unsafe
  "window.history.pushState({}, '', $1)"
  js_windowHistoryPushState :: JSString -> IO ()


class HasRouteEvent a t url where
  routeEvent :: a -> Event t url

route :: (MonadWidget t m, HasRouteEvent widgRet t url) => (url -> String) 
                                                        -> (String -> Either String url) 
                                                        -> (url -> m widgRet) 
                                                        -> m widgRet 
                                                        -> m (Dynamic t widgRet)
route enc dec trf e404 = do
  initalLocation <- liftIO getWindowLocation
  let initialRoute = pick (dec initalLocation)
  rec routeEvents <- widgetHold initialRoute $ fmap trf $ switchPromptlyDyn revs
      revs <- mapDyn routeEvent routeEvents
  performEvent_ . fmap pushUrlState $ switchPromptlyDyn revs
  return routeEvents
  where pick (Right r) = trf r
        pick (Left _) = e404
        pushUrlState = liftIO . pushState . enc

routeBoomerang :: (MonadWidget t m, HasRouteEvent widgRet t url) => Router () (url :- ()) 
                                                                 -> (url -> m widgRet) 
                                                                 -> m widgRet 
                                                                 -> m (Dynamic t widgRet)
routeBoomerang smap trf e404 = route toString fromString trf e404
  where bs = boomerangSite (\_ url -> url) smap
        fixEmpty "" = "/"
        fixEmpty s = s
        fromString = (parsePathSegments bs) . decodePathInfo . pack
        toString = fixEmpty . T.unpack . (\(ps, params) -> encodePathInfo ps params) . (formatPathSegments bs)
