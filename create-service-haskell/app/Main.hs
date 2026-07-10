{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Database.MySQL.Simple
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import System.Environment (getEnv)
import Network.HTTP.Types.Status (status503)

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
        get "/healthz" $ do
            text "ok\n"

        get "/readyz" $ do
            result <- liftIO ((try $ do
                conn <- connectDB
                [Only value] <- query_ conn "SELECT 1" :: IO [Only Int]
                close conn
                return value) :: IO (Either SomeException Int))
            case result of
                Right 1 -> json $ object ["status" .= ("ready" :: Text)]
                _ -> do
                    status status503
                    json $ object ["status" .= ("unready" :: Text)]

        post "/todo" $ do
            todo  <- jsonData :: ActionM Todo
            conn <- liftIO connectDB
            _ <- liftIO $ execute conn "INSERT INTO todos (title) VALUES (?)" [title todo]
            [Only newId] <- liftIO $ query_ conn "SELECT LAST_INSERT_ID()"
            liftIO $ close conn
            json $ Todo (Just newId) (title todo)
