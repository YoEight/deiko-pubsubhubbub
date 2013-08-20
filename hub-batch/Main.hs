import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (allocate, liftResourceT)
import Data.Conduit
import Data.Time.Clock
import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import PubState

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

main = print "Hello Web !"

retrieve :: IO ()
retrieve =
  runSqlite dbname $ do
  runMigration migrateAll
  toBeDelivered $$ subscribers
  where
    toBeDelivered = selectSource [PubStateValue ==. Fetched] []

    subscribers =
      allocate (newManager def) closeManager >>= \(_, manager) ->
        awaitForever $ \(Entity key (Pub topic _)) ->
          (lift $ selectFirst [PubContentIdent ==. key] []) >>= \res ->
            let getBytes = pubContentContent . entityVal
                bytes    = maybe (error "absurd") getBytes res
                filters  = [SubTopic ==. topic, SubActivated ==. True]
                src      = selectSource filters []
                src2     = mapOutput (bytes,) src in
            lift (src2 $$ send key manager)

    send pubId manager =
      awaitForever $ \(content, (Entity key sub)) ->
        do req  <- liftIO $ parseUrl $ show (subCallback sub)
           resp <- liftResourceT $ http (req { method = "POST" }) manager
           liftResourceT (responseBody resp $$+- return ())
           case responseStatus resp of
             s | status200 >= s && s < status300 -> return ()
               | otherwise ->
                 liftIO getCurrentTime >>= \time ->
                   let updv      = [PubFailureCount +=. 1
                                   ,PubFailureDate =. time]
                       selv      = [PubFailureSubId ==. key
                                   ,PubFailurePubId ==. pubId]
                       onExist k = update (entityKey k) updv
                       notExist  = insert_ (PubFailure key pubId 0 time)
                       select    = selectFirst selv []
                       action    = maybe notExist onExist =<< select in
                   lift action

dbname :: Text
dbname = "deiko-pubsubhubbub.sqlite3"
