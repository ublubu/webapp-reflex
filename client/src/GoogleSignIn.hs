module GoogleSignIn where

import GHCJS.Types
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Marshal

import Reflex
import Reflex.Dom

import Data.Monoid
import Data.Text (Text)

import Callback
import Utils

googleClientId :: String
googleClientId = "808800165858-1ma83vrqlk94apianbbe2magdp8vado0.apps.googleusercontent.com"

gSignInHeadEl :: (MonadWidget t m) => m ()
gSignInHeadEl = do
  elAttr "meta" ("name" =: "google-signin-client_id"
                <> "content" =: googleClientId) noContents
  elAttr "script" ("src" =: "https://apis.google.com/js/platform.js"
                  <> "async" =: "async"
                  <> "defer" =: "defer") noContents

data GoogleToken = GoogleToken { _googleIdToken :: Text } deriving (Eq, Show)

instance FromJSVal GoogleToken where
  fromJSVal = (fmap . fmap) GoogleToken . fromJSVal

gSignInEvent :: (MonadWidget t m) => m (Event t GoogleToken)
gSignInEvent = callbackEvent setGoogleSignInCallback

foreign import javascript unsafe "onGoogleSignIn_ = function(x) { return $1(x.getAuthResponse().id_token); }"
  setGoogleSignInCallback :: Callback (JSVal -> IO ()) -> IO ()

gSignInButton :: (MonadWidget t m) => m ()
gSignInButton =
  elAttr "div" ("class" =: "g-signin2"
                <> "data-onsuccess" =: "onGoogleSignIn_"
                <> "data-theme" =: "dark") noContents
