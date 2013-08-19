import Data.Time.Clock
import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.Quasi
import Data.Typeable (Typeable)

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

main = print "Hello Web !"
