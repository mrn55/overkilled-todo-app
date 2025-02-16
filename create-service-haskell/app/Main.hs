{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Database.MySQL.Simple
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import qualified Data.Text.Lazy as T
import System.Environment (getEnv)

-- Define the database connection function
connectDB :: IO Connection
connectDB = do
    host     <- getEnv "DATABASE_HOST"
    user     <- getEnv "DATABASE_USER"
    password <- getEnv "DATABASE_PASSWORD"
    database <- getEnv "DATABASE_NAME"
    connect defaultConnectInfo
        { connectHost     = host
        , connectUser     = user
        , connectPassword = password
        , connectDatabase = database
        }

-- Todo data type with ID
data Todo = Todo 
    { id    :: Maybe Int
    , title :: Text
    } deriving (Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

main :: IO ()
main = do
    putStrLn "Starting server on port 5001..."
    scotty 5001 $ do
        post "/todo" $ do
            todo  <- jsonData :: ActionM Todo
            conn <- liftIO connectDB
            liftIO $ execute conn "INSERT INTO todos (title) VALUES (?)" [title todo]
            [Only newId] <- liftIO $ query_ conn "SELECT LAST_INSERT_ID()"
            liftIO $ close conn
            json $ Todo (Just newId) (title todo)
