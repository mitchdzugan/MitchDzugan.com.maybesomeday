{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances
           , RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies
           , ConstraintKinds, TemplateHaskell, ForeignFunctionInterface
           , JavaScriptFFI, PatternSynonyms #-}
module Main where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Network.HTTP.Client as C
import Data.Proxy
import qualified Data.Text.Internal as TI
import System.IO.Unsafe (unsafePerformIO)
import Safe (readMay)
import Reflex
import Reflex.Dom
import Reflex.Dom.Route
import Reflex.Dom.LocalStorage
import Shared.Api
import Servant.API
import Servant.Client
import Sitemap

manager :: C.Manager
manager = unsafePerformIO $ C.newManager C.defaultManagerSettings

api :: Proxy API
api = Proxy

getUsers :: ExceptT ServantError IO [User]
getAdd :: Integer -> Integer -> ExceptT ServantError IO Integer
getSub :: Integer -> Integer -> ExceptT ServantError IO Integer
getMult :: Integer -> Integer -> ExceptT ServantError IO Integer
getDiv :: Integer -> Integer -> ExceptT ServantError IO Integer
rep :: Maybe TI.Text -> ExceptT ServantError IO [Char]
getUsers :<|> getAdd :<|> getSub :<|> getMult :<|> getDiv :<|> rep :<|> _ :<|> _ = client api (BaseUrl Http "localhost" 8080 "") manager

{-}
mapEventIO io = performEvent . fmap (liftIO . io)
fmapApi apif = mapEventIO (runExceptT . apif)
-}

e404_f :: (MonadWidget t m) => m (SharedState t Sitemap)
e404_f = do
  text "404 doesnt exist"
  return SharedState {routing =  never}

thing :: (MonadWidget t m) => String -> m (SharedState t Sitemap)
thing s = do
  (buttonHome, _) <- elAttr' "button" ("class" =: "") $ text "Home"
  (buttonBlog, _) <- elAttr' "button" ("class" =: "") $ text "Blog"
  (buttonBlogNew, _) <- elAttr' "button" ("class" =: "") $ text "Blog New"
  text s
  return SharedState {routing = leftmost [ fmap (\_ -> HomeR) (domEvent Click buttonHome)
                                         , fmap (\_ -> BlogR) (domEvent Click buttonBlog)
                                         , fmap (\_ -> NewBlogPostR) (domEvent Click buttonBlogNew)
                                         ]}

rf :: (MonadWidget t m) => Sitemap -> m (SharedState t Sitemap)
rf sm = thing $ show sm

data SharedState t url = SharedState { routing :: Event t url 
                                     }

instance HasRouteEvent (SharedState t Sitemap) t Sitemap where
  routeEvent = routing

addMaybs :: Maybe Int -> Maybe Int -> Maybe Int
addMaybs Nothing r = r
addMaybs (Just l) Nothing = Just l
addMaybs (Just l) (Just r) = Just (l + r)

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Int))
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

main :: IO ()
main = mainWidget $ el "div" $ do
  maybs <- numberInput
  numS <- (fromLocalStorage "myNums" $ fmap (addMaybs) $ updated maybs) >>= (mapDyn show)
  _ <- routeBoomerang sitemap rf e404_f
  text " === "
  dynText numS
