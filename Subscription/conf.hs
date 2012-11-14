module Subscription.Conf where

import Database.MongoDB
import Data.Text

data DB = DB { dbHost :: Host, dbCollection :: Text } 

data Conf = Conf { confDb :: DB } 