{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances
           , RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies
           , ConstraintKinds, TemplateHaskell, ForeignFunctionInterface
           , JavaScriptFFI, PatternSynonyms #-}
module Main where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad.IO.Class
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
postUser :: String -> String -> ExceptT ServantError IO User
getAdd :: Integer -> Integer -> ExceptT ServantError IO Integer
getSub :: Integer -> Integer -> ExceptT ServantError IO Integer
getMult :: Integer -> Integer -> ExceptT ServantError IO Integer
getDiv :: Integer -> Integer -> ExceptT ServantError IO Integer
rep :: Maybe TI.Text -> ExceptT ServantError IO [Char]
getLol :: ExceptT ServantError IO Integer
getNoo :: ExceptT ServantError IO Integer
getTest :: ExceptT ServantError IO Integer :<|> ExceptT ServantError IO Integer
getUsers :<|> postUser :<|> getAdd :<|> getSub :<|> getMult :<|> getDiv :<|> getTest :<|> rep :<|> _ :<|> _ = client api (BaseUrl Http "localhost" 8080 "") manager
getLol :<|> getNoo = getTest

mapEventIO io = performEvent . fmap (liftIO . io)
fmapApi apif = mapEventIO (runExceptT . (const apif))

e404_f :: (MonadWidget t m) => m (SharedState t Sitemap)
e404_f = do
  text "404 doesnt exist"
  return SharedState {routing =  never}

thing :: (MonadWidget t m) => String -> m (SharedState t Sitemap)
thing s = do
  buttonHome <- el' "button" (text "Home") >>= return . (domEvent Click) . fst
  buttonBlog <- el' "button" (text "Blog") >>= return . (domEvent Click) . fst
  buttonBlogNew <- el' "button" (text "Blog New") >>= return . (domEvent Click) . fst
  fmapApi (postUser "ayy" "lmao") buttonBlogNew
  text s
  return SharedState {routing = leftmost [ fmap (const HomeR) buttonHome
                                         , fmap (const BlogR) buttonBlog
                                         , fmap (const NewBlogPostR) buttonBlogNew
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
