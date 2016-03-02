{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts
           , TypeFamilies, ConstraintKinds, TemplateHaskell
           , OverloadedStrings, TypeOperators #-}

module Sitemap where

import Prelude hiding ((.))
import Control.Category 
import Text.Boomerang.TH
import Web.Routes.Boomerang ((<>))
import Text.Boomerang.Prim
import Text.Boomerang.Texts
import Data.Text.Internal
import Text.Boomerang.HStack

data Sitemap 
  = HomeR
  | LoginR
  | LogoutR
  | RegisterR
  | BlogR
  | NewBlogPostR
  | ViewBlogPostR Integer 
  | EditBlogPostR Integer
  deriving (Eq,Show)
makeBoomerangs ''Sitemap

sitemap :: forall r. Boomerang TextsError [Text] r (Sitemap :- r)
sitemap =  rHomeR
        <> "login" . rLoginR
        <> "logout" . rLogoutR
        <> "register" . rRegisterR
        <> "blog" . (  rBlogR
                    <> rNewBlogPostR </> "new"
                    <> rViewBlogPostR </> integer 
                    <> rEditBlogPostR </> integer </> "edit"
                    )
