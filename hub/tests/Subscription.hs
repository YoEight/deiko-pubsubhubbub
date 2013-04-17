import Control.Monad.State

import Test.QuickCheck

data SubParam = Callback T.Text
              | Mode T.Text
              | Topic T.Text
              | Verify T.Text
              | Other T.Text deriving (Eq, Show)

newtype Params = Params { getParams :: [SubParam] } deriving Eq

data TestSubState = TST { tstCallback :: Bool
                        , tstMode :: Bool
                        , tstTopic :: Bool
                        , tstVerify :: Bool }

instance Show Params where
    show (Params xs) = show xs

instance Arbitrary SubParam where
    arbitrary = do
      n <- choose (1, 5) :: Gen Int
      return $ go n 
          where 
            go 1 = Callback "http://www.google.com"
            go 2 = Mode "subscribe"
            go 3 = Topic "http://www.google.com"
            go 4 = Verify "sync"
            go 5 = Other "other"

prop_identity :: SubParam -> Bool
prop_identity x = x == x

completeSubRequest :: [SubParam] -> Bool
completeSubRequest xs = toBool $ execState (traverse_ go xs) init_state
    where
      init_state = TST False False False False

      toBool (TST callback mode topic verify) = callback && mode && topic && verify

      go (Callback _) = modify $ \s -> s{tstCallback=True}
      go (Mode _)     = modify $ \s -> s{tstMode=True}
      go (Topic _)    = modify $ \s -> s{tstTopic=True}
      go (Verify _)   = modify $ \s -> s{tstVerify=True}
      go _            = return ()

toParam :: SubParam -> Param
toParam (Callback value) = ("hub.callback", value)
toParam (Mode value)     = ("hub.mode", value)
toParam (Topic value)    = ("hub.topic", value)
toParam (Verify value)   = ("hub.verify", value)
toParam (Other value)    = ("hub.other", value)

runEither :: Either a b -> Bool
runEither = either (const False) (const True)

prop_list :: Property
prop_list = forAll (choose (1, 25)) $
            \i -> let go = replicateM i arbitrary
                  in do
                    xs <- go
                    let params = map toParam xs
                        result = runEither $ parseHubRequest params
                    if completeSubRequest xs then label "complete subrequest" $ property result 
                                             else label "uncomplete subrequest" $  property $ not result
