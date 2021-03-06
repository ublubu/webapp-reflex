{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Meathead.ModuleWidget where

import Reflex
import Reflex.Dom

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import API.Module

import Apply
import Combinators
import CommonWidgets
import Utils

import Meathead.App
import Meathead.ModuleCache
import Meathead.Pages

-- TODO: cloning doesn't seem to work

-- TODO: newtype for userId
moduleWidget :: forall t m. (MonadWidget t m)
             => Maybe Text -- userId
             -> Dynamic t (PageState -> Text)
             -> Dynamic t ModuleCache
             -> Dynamic t ModulePageState
             -> m (BubbleApp' t)
moduleWidget mUserId makeHref cache pageStateD =
  ec $ f @/ pageStateD
  where f ModulePageState{..} =
          if _mpEditing then moduleEditWidget' _mpGuid cache
          else moduleViewWidget mUserId makeHref _mpGuid cache

-- TODO: ask the user to sign in
moduleCreationWidget :: forall t m. (MonadWidget t m)
                     => Dynamic t ModuleCache
                     -> Dynamic t (Maybe Text) -- parentGuid from ModuleCreatePage
                     -> m (BubbleApp' t)
moduleCreationWidget cache pageStateD =
  ec $ maybe (moduleCreateWidget cache) (flip moduleCloneWidget cache) @/ pageStateD

-- bottom-level form that represents the content of a Module
moduleEditForm :: forall t m. (MonadWidget t m)
               => ModuleEdit
               -> m (Dynamic t ModuleEdit)
moduleEditForm ModuleEdit{..} = do
  title <- el "p" $ do
    text "title: "
    simpleTextInput _meTitle
  description <- el "p" $ do
    text "description: "
    nullToNothing $/ simpleTextInput (fromMaybe "" _meDescription)
  code <- el "p" $ do
    text "code: "
    simpleTextInput _meCode
  ModuleEdit @/ title /#/ description /#/ code
  where nullToNothing x = if T.null x then Nothing else Just x

-- wrapper widget for widgets that bubble a "create module" request
-- routes to new module's page on completion
newModuleWidget :: forall t m. (MonadWidget t m)
                => Dynamic t ModuleCache
                -> (RequestGuid -> m (BubbleApp t (Event t ())))
                -> m (BubbleApp' t)
newModuleWidget cache makeWidget = do
  reqGuid <- getReqGuid
  Bubbling bub submit <- makeWidget reqGuid
  guidE <- eWhenFirst submit $ moduleCreated reqGuid cache
  return . bubbling $ bub & _1 %~ ecCat (wrapCat $ pageViewModule <$> guidE)

-- top-level widget to create a widget from scratch
moduleCreateWidget :: forall t m. (MonadWidget t m)
                   => Dynamic t ModuleCache
                   -> m (BubbleApp' t)
moduleCreateWidget cache = do
  newModuleWidget cache $ flip moduleCreateWidget_ (Nothing, emptyModuleEdit)

-- top-level widget to create a cloned widget
moduleCloneWidget :: forall t m. (MonadWidget t m)
                  => Text
                  -> Dynamic t ModuleCache
                  -> m (BubbleApp' t)
moduleCloneWidget parentGuid cache = do
  newModuleWidget cache $ \reqGuid -> moduleCloneWidget_ reqGuid parentGuid cache

-- NOTE: does this need a cancel button? (seems that back button should be sufficient)
moduleCreateWidget_ :: forall t m. (MonadWidget t m)
                    => RequestGuid
                    -> ModuleCreate
                    -> m (BubbleApp t (Event t ()))
moduleCreateWidget_ reqGuid (mParentGuid, moduleEdit) = do
  paraT "new module"
  moduleEditD <- moduleEditForm moduleEdit
  submit <- el "p" $ onceButton "create"
  moduleD <- (CreateModule reqGuid . (mParentGuid,)) @/ moduleEditD
  return $ Bubbling (never, wrapCat $ tagDyn moduleD submit) submit

moduleCloneWidget_ :: forall t m. (MonadWidget t m)
                   => RequestGuid
                   -> Text
                   -> Dynamic t ModuleCache
                   -> m (BubbleApp t (Event t ()))
moduleCloneWidget_ reqGuid guid cache = do
  withFirstCachedModuleView guid cache $
    \moduleD -> ec $ moduleCreateWidget_ reqGuid $/ mapDyn f moduleD
  where f (_, _, moduleEdit) = (Just guid, moduleEdit)

-- top-level widget for editing a module
-- routes to "view module" page on exit
-- TODO: back button route on cancel?
moduleEditWidget' :: forall t m. (MonadWidget t m)
                  => Text
                  -> Dynamic t ModuleCache
                  -> m (BubbleApp' t)
moduleEditWidget' guid cache =
  moduleEditWidget guid (pageViewModule guid) cache

-- NOTE: after the form is created, it does not update itself from the cache
--       i.e. user's edits cannot be clobbered by incoming changes from DB, etc
moduleEditWidget :: forall t m. (MonadWidget t m)
                 => Text
                 -> PageState
                 -> Dynamic t ModuleCache
                 -> m (BubbleApp' t)
moduleEditWidget guid exitPage cache = do
  withFirstCachedModuleView guid cache $
    \moduleD -> ec $ flip moduleEditWidget_ exitPage @/ moduleD

moduleEditWidget_ :: forall t m. (MonadWidget t m)
                  => ModuleView
                  -> PageState
                  -> m (BubbleApp' t)
moduleEditWidget_ (ModuleMeta{..}, _, moduleEdit) exitPage = mdo
  moduleEditD <- moduleEditForm moduleEdit
  moduleEditD' <- (UpdateModule _mmGuid) @/ moduleEditD

  save <- el "p" $ buttonWhile uncompleted "save"
  cancel <- el "p" $ buttonWhile uncompleted "cancel"
  submit <- el "p" $ buttonWhile uncompleted "save and exit"
  uncompleted <- falseAfter $ leftmost [cancel, submit]

  let saveEvent = tagDyn moduleEditD'
      exitEvent = eventValue exitPage
      saveClick :: BubbleApp' t
      saveClick = bubbleWrapWith _2 $ saveEvent save
      cancelClick :: BubbleApp' t
      cancelClick = bubbleWrapWith _1 $ exitEvent cancel
      submitClick :: BubbleApp' t
      submitClick = bubble (wrapCat $ exitEvent submit, wrapCat $ saveEvent submit)

  return $ ecConcat [saveClick, cancelClick, submitClick]

-- top-level widget for viewing a module
moduleViewWidget :: forall t m. (MonadWidget t m)
                 => Maybe Text -- userId
                 -> Dynamic t (PageState -> Text)
                 -> Text
                 -> Dynamic t ModuleCache
                 -> m (BubbleApp' t)
moduleViewWidget mUserId makeHref guid cache =
  withCachedModuleView guid cache $
  \moduleD -> ec $ moduleViewWidget_ mUserId @/ moduleD /# makeHref

-- NOTE: "edit" link routes to "edit module" page
-- TODO: "delete" button?
-- TODO: monad transformer (?) for collecting all bubbled events from a layout?
moduleViewWidget_ :: forall t m. (MonadWidget t m)
                  => Maybe Text -- userId
                  -> ModuleView
                  -> Dynamic t (PageState -> Text)
                  -> m (BubbleApp' t)
moduleViewWidget_ mUserId (ModuleMeta{..}, mParentGuid, ModuleEdit{..}) makeHref = do
  editClicks <- if mUserId /= Just _mmAuthor then return ecNever
                else appLink makeHref (pageEditModule _mmGuid) "edit"
  cloneClicks <- appLink makeHref (ModuleCreatePage $ Just _mmGuid) "clone"
  el "p" $ do
    text "title: "
    textT _meTitle
  el "p" $ do
    text "guid: "
    textT _mmGuid
  let f pg = el "p" $ do
        text "parent: "
        moduleLink makeHref pg pg
  parentClicks <- maybe (return ecNever) f mParentGuid
  el "p" $ do
    text "author: "
    textT _mmAuthor
  let g desc = el "p" $ do
        text "description: "
        textT desc
  maybe noContents g _meDescription
  el "p" $ do
    text "code: "
    textT _meCode
  return $ ecConcat [editClicks, cloneClicks, parentClicks]

withFirstCachedModuleView :: forall t m a. (MonadWidget t m, EventContainer t m a)
                     => Text
                     -> Dynamic t ModuleCache
                     -> (Dynamic t ModuleView -> m (BubbleApp t a))
                     -> m (BubbleApp t a)
withFirstCachedModuleView = withCachedModuleView_ dFirst

withCachedModuleView :: forall t m a. (MonadWidget t m, EventContainer t m a)
                     => Text
                     -> Dynamic t ModuleCache
                     -> (Dynamic t ModuleView -> m (BubbleApp t a))
                     -> m (BubbleApp t a)
withCachedModuleView = withCachedModuleView_ return

withCachedModuleView_ :: forall t m a x. (MonadWidget t m, EventContainer t m a)
                      => (Dynamic t (Maybe ModuleView) -> m (Dynamic t (Maybe ModuleView)))
                      -> Text
                      -> Dynamic t ModuleCache
                      -> (Dynamic t ModuleView -> m (BubbleApp t a))
                      -> m (BubbleApp t a)
withCachedModuleView_ filterModuleD guid cache makeWidget = do
  -- fetch the module
  moduleD <- filterModuleD =<< moduleLookup guid @/ cache
  module_ <- sample . current $ moduleD
  readModule <- maybe (fire $ ReadModule guid) (const $ return ecNever) module_
  childEvents <- dWhenJust moduleD makeWidget
  return $ childEvents & bBubble . _2 %~ ecCons readModule

moduleLink :: (MonadWidget t m)
           => Dynamic t (PageState -> Text)
           -> Text
           -> Text
           -> m (BubbleApp' t)
moduleLink makeHref guid label =
  appLink makeHref link label
  where link = pageViewModule guid

-- TODO: lens? move to API.Module
viewToCreate :: ModuleView -> ModuleCreate
viewToCreate (_, mParentGuid, edit) = (mParentGuid, edit)

emptyModuleEdit :: ModuleEdit
emptyModuleEdit =
  ModuleEdit "" Nothing ""
