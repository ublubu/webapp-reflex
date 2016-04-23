module SignIn where

import Reflex
import Reflex.Dom

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Either.Combinators

import Servant.API.ResponseHeaders (getResponse)

import qualified APIClient as API
import GoogleSignIn
import API.SignIn (CookieData(..))

-- TODO: why is this separate from GoogleSignIn?

signInEvent :: (MonadWidget t m) => m (Event t CookieData)
signInEvent = do
  googleSignIns <- gSignInEvent
  signInAttempts <- performEvent (fmap attemptSignIn googleSignIns)
  return . fmap getResponse . fmapMaybe rightToMaybe $ signInAttempts
  where attemptSignIn = liftIO . runEitherT . API.tokensignin . Just . _googleIdToken
