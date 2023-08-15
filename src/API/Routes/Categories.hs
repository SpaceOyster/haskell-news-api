{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Routes.Categories where

import API.Modifiers.Protected (Protected)
import App.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import DB
import Data.Aeson as A
import Data.CaseInsensitive as CI (CI (original), mk)
import Data.Text
import Data.Text.Extended as T
import Database.Beam
import Effects.Database as DB (MonadDatabase (..))
import Effects.Log as Log (MonadLog, logInfo, logWarning)
import Entities.Category
import Entities.User
import Servant
import Servant.Docs as Docs (ToSample (toSamples), singleSample)

type CategoriesAPI = Get '[JSON] Text :<|> Protected :> ReqBody '[JSON] CategoryJSON :> PostCreated '[JSON] CategoryJSON

newtype CategoryJSON = CategoryJSON Category

instance A.ToJSON CategoryJSON where
  toJSON (CategoryJSON Category {..}) =
    A.object $ ["name" A..= CI.original _categoryName] <> maybeParentField
    where
      catIdM = case _categoryParentCategory of
        CategoryId idMaybe -> idMaybe
      maybeParentField = maybe [] (\i -> ["parent" A..= CI.original i]) catIdM

instance A.FromJSON CategoryJSON where
  parseJSON = A.withObject "CategoryJSON" $ \o -> do
    _categoryName <- CI.mk <$> o A..: "name"
    _categoryParentCategory <- CategoryId . fmap CI.mk <$> o A..:? "parent"
    return $ CategoryJSON $ Category {..}

instance Docs.ToSample CategoryJSON where
  toSamples _ =
    [ ("Category may have no parent", CategoryJSON cat1),
      ("Category can have a parent", CategoryJSON cat2)
    ]
    where
      cat2 =
        Category
          { _categoryName = "Haskell",
            _categoryParentCategory = CategoryId $ Just "Functional Programing"
          }
      cat1 =
        Category
          { _categoryName = "Outer Space",
            _categoryParentCategory = CategoryId Nothing
          }

categories :: ServerT CategoriesAPI App
categories = listCategories :<|> postCategories

listCategories = return "GET categories endpoint"

postCategories ::
  ( DB.MonadDatabase m,
    Log.MonadLog m,
    MonadIO m,
    MonadError ServerError m
  ) =>
  User ->
  CategoryJSON ->
  m CategoryJSON
postCategories usr (CategoryJSON cat) =
  if _userIsAdmin usr then doCreateCategory else doOnUnauthorised
  where
    table = _newsCategories newsDB
    creatorLogin = CI.original (_userLogin usr)
    doCreateCategory = do
      insertNewCategory table cat
      doCheckIfSuccessfull
    doCheckIfSuccessfull = do
      newCatMaybe <- lookupCategory table $ _categoryName cat
      case newCatMaybe of
        Nothing -> doLogDBError >> throwError err503
        Just c -> doLogSuccess >> return (CategoryJSON c)
    doOnUnauthorised = doLogUnauthorised >> throwError err401
    doLogSuccess =
      Log.logInfo $
        "User " <> creatorLogin <> " created new category :" <> T.tshow cat
    doLogUnauthorised =
      Log.logWarning $
        "User " <> creatorLogin <> " is not authorised to create a new category"
    doLogDBError =
      Log.logWarning $
        "Category " <> T.tshow cat <> " was not added to Database"
