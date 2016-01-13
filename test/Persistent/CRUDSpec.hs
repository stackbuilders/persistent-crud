module Persistent.CRUDSpec where

import           Database.Persist
import           Database.Persist.Sql
import           Persistent.CRUD
import           Test.Hspec

spec :: Spec
spec =
  describe "createUser" $
    it "returns the created user id" $ do
      let email     = "foo@bar.com"
          firstName = "Foo"
          lastName  = "Bar"

      numberOfUsers <- withDB $ do
        createUser email firstName lastName
        countUsers

      numberOfUsers `shouldBe` (1 :: Int)

withTestDB :: SqlPersistM a -> IO a
withTestDB q =
  withDB $ do
    runMigration migrateAll
    transactionSave
    r <- q
    transactionUndo
    return r
