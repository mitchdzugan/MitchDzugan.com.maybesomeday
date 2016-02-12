{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, ForeignFunctionInterface, JavaScriptFFI, PatternSynonyms #-}
module Main where

import Prelude hiding (mapM, mapM_, all, sequence)

import           GHCJS.Types
import qualified Data.JSString as JSS
import Control.Monad
import Data.ByteString.Char8
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Network.HTTP.Client as C
import Data.Proxy
import System.IO.Unsafe (unsafePerformIO)
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Text as T

import Reflex
import Reflex.Dom

import Shared.Api

import Servant.API
import Servant.Client

import Sitemap

import Web.Routes


import Text.Boomerang.String (parseString)

manager = unsafePerformIO $ C.newManager C.defaultManagerSettings

api :: Proxy API
api = Proxy

getUsers :<|> getAdd :<|> getSub :<|> getMult :<|> getDiv :<|> _ :<|> _ = client api (BaseUrl Http "localhost" 8080 "") manager

mapEventIO io = performEvent . fmap (liftIO . io)

fmapApi apif = mapEventIO (runExceptT . apif)

thing :: (MonadWidget t m) => String -> m (Event t (Either String Sitemap))
thing s = do
  (buttonHome, _) <- elAttr' "button" ("class" =: "") $ text "Home"
  (buttonBlog, _) <- elAttr' "button" ("class" =: "") $ text "Blog"
  (buttonBlogNew, _) <- elAttr' "button" ("class" =: "") $ text "Blog New"
  text s
  return $ leftmost [ fmap (\_ -> Right HomeR) (domEvent Click buttonHome)
                    , fmap (\_ -> Right BlogR) (domEvent Click buttonBlog)
                    , fmap (\_ -> Right NewBlogPostR) (domEvent Click buttonBlogNew)
                    ]

rf :: (MonadWidget t m) => Either String Sitemap -> m (Event t (Either String Sitemap))
rf (Left s) = thing $ "ohhh nooo " ++ s
rf (Right sm) = thing $ show sm

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

getRouteR (Left _) = HomeR
getRouteR (Right x) = x

fixEmpty "" = "/"
fixEmpty s = s

route :: (MonadWidget t m) => (Either String Sitemap -> m (Event t (Either String Sitemap))) -> m ()
route trf = do
  initalLocation <- liftIO getWindowLocation
  rec routeEvents <- widgetHold (trf (parsePathSegments bs (decodePathInfo (pack initalLocation)))) $ fmap trf $ switchPromptlyDyn routeEvents
  performEvent . fmap (liftIO . pushState . fixEmpty . T.unpack . (\(ps, params) -> encodePathInfo ps params) . (formatPathSegments bs) . getRouteR) $ switchPromptlyDyn routeEvents
  return ()

main = mainWidget $ el "div" $ do
  route rf
