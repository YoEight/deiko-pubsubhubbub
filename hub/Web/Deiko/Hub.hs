module Web.Deiko.Hub where

import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State  (evalStateT, execStateT, runStateT)
import           Control.Monad.Trans  (lift)

import qualified Data.Text.Lazy       as T

import           Network.HTTP.Types   (Status)

import           Web.Deiko.Hub.Types
import qualified Web.Scotty           as Scotty

param :: (Scotty.Parsable a) => T.Text -> Hub Scotty.ActionM a
param = lift . Scotty.param

params :: Hub Scotty.ActionM [(T.Text, T.Text)]
params = lift Scotty.params

status :: Status -> Hub Scotty.ActionM ()
status = lift . Scotty.status

text :: T.Text -> Hub Scotty.ActionM ()
text = lift . Scotty.text

raise :: T.Text -> Hub Scotty.ActionM a
raise = lift . Scotty.raise

evalHub :: Monad m => Hub m a -> HubOpts -> m a
evalHub (Hub action) opts =
  evalStateT (runReaderT action opts) emptyEvents
