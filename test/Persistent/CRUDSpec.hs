module Persistent.CRUDSpec where

import           Persistent.CRUD
import           Test.Hspec
import Database.Persist
import Database.Persist.Sql

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
