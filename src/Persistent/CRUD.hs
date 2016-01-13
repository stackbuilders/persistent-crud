{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Persistent.CRUD where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  email String
  firstName String
  lastName String

  -- UniqueEmail email
  -- UniqueFirstNameAndLastName firstName lastName
  deriving Show

Post
  content String
  userId UserId
  created UTCTime

  deriving Show

Comment
  content String
  postId PostId
  userId UserId
  created UTCTime

  deriving Show
|]

runMigrations :: IO ()
runMigrations = withDB (runMigration migrateAll)

withDB :: SqlPersistM a -> IO a
withDB q = runStdoutLoggingT $ withSqliteConn "db" (liftIO . runSqlPersistM q)

createUser :: String -> String -> String -> SqlPersistM (Maybe UserId)
createUser email firstName lastName =
  insertUnique $ User email firstName lastName

findUserById :: UserId -> SqlPersistM (Maybe (Entity User))
findUserById userId = selectFirst [UserId ==. userId] []

findAllUsers :: SqlPersistM [Entity User]
findAllUsers = selectList [] []

countUsers :: SqlPersistM Int
countUsers = count ([] :: [Filter User])

updateUser :: UserId -> String -> String -> String -> SqlPersistM User
updateUser userId email firstName lastName =
  updateGet
    userId
    [ UserEmail     =. email
    , UserFirstName =. firstName
    , UserLastName  =. lastName
    ]

deleteUserById :: UserId -> SqlPersistM ()
deleteUserById userId = delete userId

createPost :: String -> UserId -> SqlPersistM (Entity Post)
createPost = undefined

createComment :: String -> PostId -> UserId -> SqlPersistM (Entity Comment)
createComment = undefined
