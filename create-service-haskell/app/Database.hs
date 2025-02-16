{-# LANGUAGE OverloadedStrings #-}

module Database (connectDB) where

import Database.MySQL.Simple

connectDB :: IO Connection
connectDB = connect defaultConnectInfo
    { connectHost = "db"
    , connectDatabase = "todo_db"
    , connectUser = "todo_user"
    , connectPassword = "todo_password"
    }