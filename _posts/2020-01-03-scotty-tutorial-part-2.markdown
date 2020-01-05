---
layout: post
title: Scotty Tutorial Part 2
date: 2020-01-03
description: Adding persistent state to our application with PostgreSQL.
img: Haskell.png # Add image post (optional)
fig-caption: # Add figcaption (optional)
tags: [Scotty, Haskell]
---

Your *package.yaml* file should now have the following dependencies:

```yaml
dependencies:
- base >= 4.7 && < 5
- scotty
- text
- aeson
- email-validate
- bytestring
- containers
- wai-cors
- wai-extra
- http-types
- postgresql-simple
- postgresql-simple-migration
- resource-pool
- load-env
- time
```


Let's add a separate file to our *src* Directory and call it *DB.hs* this is the file where we will handle all of our database interactions.

In our *src/Lib.hs* file lets add the following import.
*src/Lib.hs*
```haskell
import qualified DB
```

This will allow our file to be compiled with the rest of our application.

Now the first thing we need to do is figure out what our schema is going to look like for our user in the database.

We know that our users will have the following fields

```haskell
data UserSignup = UserSignup
  { name :: Name
  , email :: Email
  , password :: Password
  } deriving (Eq, Show)
```

What we will want to do is ensure that the email of all of our users is unique and that their passwords are not stored as plain text. In ordeer to do this we could always manually convert all of our inputs to match these requirements by calling `toLower` on the email and by adding the `bcrypt` package, but why would we do this when our storage engine has the capability to by just adding a few extensions and will enforce these requirements for us. The extensions we will use are `pgcrypto` and `citext`.

Now what else might we want to add to our user model in our database. How about an image for when we allow profile image uploads via AWS S3, a created_at timestamp, and an about_me where users can tell us a bit about themselves. If we put all that together we get an sql file like this.

```sql
create extension citext;
create extension pgcrypto;

CREATE TABLE users (
  id bigserial primary key not null,
  email citext not null unique,
  name json NOT NULL,
  pass text not null,
  image text,
  about_me text,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
 
```

Let's first start off by creating a database for us to connect to (or make it however you would like to do it).

```shell
$ sudo -i -u postgres psql
$ postgres=# CREATE USER ethan WITH PASSWORD 'scotty';
$ postgres=# CREATE DATABASE scotty-tutorial WITH owner 'ethan';
$ postgres=# \q
```

And now lets get a connection to our database that we can use in our application.

let's add the following to our file.

*src/DB.hs*
```
module DB where

import           Database.PostgreSQL.Simple

getConn :: IO Connection
getConn = do
  let host = "127.0.0.1"
  let port = 5432
  let user = "ethan"
  let pass = "scotty"
  let db = "scotty-tutorial"
  let cInfo = ConnectInfo host port user pass db
  conn <- connect cInfo
  return $ conn
```

this uses the `connect :: ConnectInfo -> IO Connection` function from `postgresql-simple` and the :
```haskell
data ConnectInfo = ConnectInfo {
      connectHost :: String
    , connectPort :: Word16
    , connectUser :: String
    , connectPassword :: String
    , connectDatabase :: String
    } deriving (Generic,Eq,Read,Show,Typeable)
```
data type now it's not good practice to hardcode your credentials into a file so this is the perfect opportunity to introduce the `load-env` Library. This allows us to import the environment variables from a .env file. Often times this is used with the create `configurator` library so that you can store multiple configurations in your file and use an environment variable to select the proper configuration, but for us we'll stick with the `load-env` library for now and maybe use `configurator` in the future if we start to get into a more complex setup.

lets make a *.env* file at the root of our directory and add the following to it (this is not in the src file it is the file above it that is the root.)

*/.env*
```shell
PG_CONNECT_USER=ethan
PG_CONNECT_HOST=127.0.0.1
PG_CONNECT_PASSWORD=scotty
PG_CONNECT_DATABASE=scotty-tutorial
PG_CONNECT_PORT=5432

```

Ok now to load the file we will call the following function: `loadEnvFrom :: FilePath -> IO ()` from `load-env` in our *Lib.hs* file prior to doing anything else. Let's add the proper imports and function call.

*src/Lib.hs*
```haskell
import qualified LoadEnv as LE

main :: IO ()
main = do
  LE.loadEnvFrom "./.env"
  app
```
Great now we should have access to all our Environemnt variables that we just declared in our */.env* file. So lets go back to our database file and make a function that collects the values associated with the environement variables and spits out a `ConnectInfo`. To do this we will need the `System.Environment` package that comes with the `prelude`. We have two options for the functions that we can use `getEnv :: String -> IO String` or we can use `lookupEnv :: String -> IO (Maybe String)` In my opinion the best option for use is `getEnv` because we would like our application to fail if we dont have the proper variables.

Let's import this package in our file and then get those variables.

*src/DB.hs*
```haskell
module DB where

import           Database.PostgreSQL.Simple
import qualified System.Environment as SE
import           Data.Word


getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- SE.getEnv "PG_CONNECT_HOST" 
  port <- SE.getEnv "PG_CONNECT_PORT"
  user <- SE.getEnv "PG_CONNECT_USER"
  db <- SE.getEnv "PG_CONNECT_DATABASE"
  pass <- SE.getEnv "PG_CONNECT_PASSWORD"
  return $ ConnectInfo host (read port :: Word16) user pass db

getConn :: IO Connection
getConn = do
  cInfo <- getConnectInfo
  conn <- connect cInfo
  return $ conn
```

The port comes as type `String` and we need to covert it to `Word16` so we use the `read` function and convert the string to a `Word16` value which we also import from `Data.Word`.

Ok so now that we have a connection let's use it to migrate/initialize our database. The `postgresql-simple-migration` libary is our tool for this and we will use the following functions: 

```haskell
withTransaction :: Connection -> IO a -> IO a

runMigrations :: Bool -> Connection -> [MigrationCommand] -> IO (MigrationResult String)
```

We want to wrap our migrations in a transaction so we will supply the connection via withTransaction and we also want to take advatage of the following commands so that we can write our SQL files in a directory: `MigrationInitialization, MigrationDirectory FilePath` this will allow us to initialize our migrations and also to run the scripts in a file that we have somewhere on our computer the most convenient place is the root of our directory so lets make the folder *./migrations* and lets put a file in there *./migrations/001_users.sql*. In that file we will put our sql that we came up with earlier to represnt our users table:

*./migrations/001_users.sql*
```sql
create extension citext;
create extension pgcrypto;

CREATE TABLE users (
  id bigserial primary key not null,
  email citext not null unique,
  name json NOT NULL,
  pass text not null,
  image text,
  about_me text,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
```
and we'll add the following to our file:

*src/DB.hs*
```haskell
module DB where

import           Database.PostgreSQL.Simple
import qualified System.Environment as SE
import           Data.Word
import           Database.PostgreSQL.Simple.Migration

...

migrateDB :: Connection -> IO ()
migrateDB conn = do
  res <- withTransaction conn (runMigrations False conn [MigrationInitialization, MigrationDirectory "./migrations"])
  print res
```

The reason we add *001_* to our file is because we want to make sure that the order of our file's being executed is correct. The next file will be *002_* the `postgresql-simple-migration` library only allows for linear migrations which if you're coming from a more popular migration library in another language is probably annoying but for our applicaiton it gets the job done.

Now let's add a function that ties in all this logic together and returns a connection for our applicaiton to use after it is done migrating. Our Function should look like this

*src/DB.hs*
```haskell
module DB (initDB) where

import           Database.PostgreSQL.Simple
import qualified System.Environment as SE
import           Data.Word
import           Database.PostgreSQL.Simple.Migration

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- SE.getEnv "PG_CONNECT_HOST" 
  port <- SE.getEnv "PG_CONNECT_PORT"
  user <- SE.getEnv "PG_CONNECT_USER"
  db <- SE.getEnv "PG_CONNECT_DATABASE"
  pass <- SE.getEnv "PG_CONNECT_PASSWORD"
  return $ ConnectInfo host (read port :: Word16) user pass db

getConn :: IO Connection
getConn = do
  cInfo <- getConnectInfo
  conn <- connect cInfo
  return $ conn

migrateDB :: Connection -> IO ()
migrateDB conn = do
  res <- withTransaction conn (runMigrations False conn [MigrationInitialization, MigrationDirectory "./migrations"])
  print res

initDB :: IO Connection
initDB = do
  conn <- getConn 
  migrateDB conn
  return $ conn
```

and now we add the following to our *Lib.hs*

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import           Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV
import           Web.Scotty
import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import qualified Data.Text.Lazy as TL
import qualified DB as DB
import qualified LoadEnv as LE

main :: IO ()
main = do
  LE.loadEnvFrom "./.env"
  conn <- DB.initDB
  app

...
```
Now let's run the following:

```shell
$ stack build
$ stack exec scotty-tutorial-exe
```

and we should see this printed to the console:

``` 
MigrationSuccess
```

Now lets kill our server and run it again. you'll see the following line:

```
NOTICE:  relation "schema_migrations" already exists, skipping
```

If you want you can take out the `MigrationInitialization` Command but it's fine to be in there.

Unfortunately right now all we have is one connection that's probably not a very good decision for us in terms of design. Most db libraries I've used in other languages handle the conneciton pooling for you but `postgresql-simple` doesn't so we will implement this using the `resource-pool` package. The chief functions of interest to us are:

```haskell
createPool
    :: IO a
    -- ^ Action that creates a new resource.
    -> (a -> IO ())
    -- ^ Action that destroys an existing resource.
    -> Int
    -- ^ The number of stripes (distinct sub-pools) to maintain.
    -- The smallest acceptable value is 1.
    -> NominalDiffTime
    -- ^ Amount of time for which an unused resource is kept open.
    -- The smallest acceptable value is 0.5 seconds.
    --
    -- The elapsed time before destroying a resource may be a little
    -- longer than requested, as the reaper thread wakes at 1-second
    -- intervals.
    -> Int
    -- ^ Maximum number of resources to keep open per stripe.  The
    -- smallest acceptable value is 1.
    --
    -- Requests for resources will block if this limit is reached on a
    -- single stripe, even if other stripes have idle resources
    -- available.
     -> IO (Pool a)

withResource :: (MonadBaseControl IO m) => Pool a -> (a -> m b) -> m b
```

`withResource` allows us to supply a connection to a function and then put that connection back into the pool when we are done with it. This follows the common `bracket` pattern found in `base`

so lets reimplement our `getConn` function to be `getPool`.

*src/DB.hs*
```haskell
...
import           Data.Pool

...

getPool :: IO (Pool Connection)
getPool = do
  cInfo <- getConnectInfo
  createPool (connect cInfo) close 2 5 5

showConnectCount ∷ IO ()
showConnectCount = do
	cInfo <- getConnectInfo
    conn <- connect cInfo
    x <- query_ conn "SELECT COUNT (*) FROM pg_stat_activity"
    let c = fromOnly ∘ head $ (x ∷ [Only Integer])
    close conn
    print c
```

This will open 2 stripes to the database with 5 connections per stripe and it will destroy a resource after 5 seconds of being idle. The `showConnectCount` is just a utiltiy function that you can use if you would like to see how many connections you have open at any given time.

Now lets make a helper function `withPool` that we will use to supply connections to our functions that require interacting with the Database, and that will round out the bulk of the work that needs to be done for DB interfacing. Our file should now look like this

*src/DB.hs*
```haskell
module DB 
  ( initDB
  , showConnectCount
  , withPool
  ) where

import           Database.PostgreSQL.Simple
import qualified System.Environment as SE
import           Data.Word
import           Database.PostgreSQL.Simple.Migration
import           Data.Pool

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- SE.getEnv "PG_CONNECT_HOST" 
  port <- SE.getEnv "PG_CONNECT_PORT"
  user <- SE.getEnv "PG_CONNECT_USER"
  db <- SE.getEnv "PG_CONNECT_DATABASE"
  pass <- SE.getEnv "PG_CONNECT_PASSWORD"
  return $ ConnectInfo host (read port :: Word16) user pass db

getConn :: IO Connection
getConn = do
  cInfo <- getConnectInfo
  conn <- connect cInfo
  return $ conn

getPool :: IO (Pool Connection)
getPool = do
  cInfo <- getConnectInfo
  createPool (connect cInfo) close 2 5 5

migrateDB :: Connection -> IO ()
migrateDB conn = do
  res <- withTransaction conn (runMigrations False conn [MigrationDirectory "./migrations"])
  print res

withPool :: (Pool Connection) -> (Connection -> IO a) -> IO a
withPool pool action = withResource pool action

initDB :: IO (Pool Connection)
initDB = do
  pool <- getPool
  withPool pool migrateDB
  return $ pool

showConnectCount :: IO ()
showConnectCount = do
  conn <- getConn
  x <- query_ conn "SELECT COUNT (*) FROM pg_stat_activity"
  let c = fromOnly . head $ (x :: [Only Integer])
  close conn
  print c
```

Now It's time to actually save a user into the database. notice that despite the verbosity of this post we were able to create a fairly concise DB interface almost from scratch with very few libraries (2) and with the addtion of one more library we were able to add migrations to this small set of functions. and we've yet to do anything that is very technical in terms of `haskell` no need for Tansformers, Free Monads, Tagless Final. 

*NOTE: scotty is actually implemented as a monad transformer but it hides those details away from us when using just the **scotty** function which is helpful*


In order to save our user into the database we will need to create a `ToRow` instance of our user (or we can manually extract the paramters but there's really no need to do that)

Let's go into our *Lib.hs* file import the necessary library and write our instance

*src/Lib.hs*
```haskell
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField

...

instance ToJSON Name where
  toJSON name = object 
   [ "firstname" .= firstname name
   , "lastname" .= lastname name
   ]

instance ToField Name where
  toField a = toJSONField a

instance ToRow UserSignup where
  toRow user = [toField (email user), toField (password user), toField (name user)]

```

So we write our `ToRow` instance for `UserSignup` and we also need to write a `ToField` instance for `Name` so that we can save that into our database. The `toRow` function expects an array of actions `[Aciton]` and the `toField` function expects a value with a `ToField` typeclass constraint. Luckily we don't need to provide these for each field as `postgresql-simple` takes care of many common datatypes for us however it doesn't have an instance for json values. instead it gives us the elper function toJSONField which takes a value with a ToJSON instance and converts it into a value that can be handled by postgresql-simple. 


Perfect we're almost there. Lets write the function to add the user to the database now.


```haskell
import           Data.Int
import           Data.Pool
...

addUserToDB :: (Pool Connection) -> UserSignup -> IO (Int64)
addUserToDB pool usr = do
  val <- DB.withPool pool (\c -> execute c statement usr)
  return $ val
  where statement = "INSERT INTO users (email, password, name) VALUES (?, crypt(?, gen_salt('bf', 8)), ?)"

...
```

so the `addUserToDB` function takes a `Connection Pool` and a `UserSignup` and it outputs the ID of the user that was just created. Which is just what we want. the `execute` function looks like this: `execute :: (ToRow a) => Connection -> Query -> a -> IO Int64`. Int64 isn't included in the prelude but it is in base so we need to include it by importing `Data.Int`.


Well let's give it a shot let's wire up our "/user" route so that it creates a user in our database.

```haskell
app :: (Pool Connection) -> IO ()
app pool = do
  scotty 3000 $ do
   post "/user" $ do
    b <- body
    let j = (eitherDecode b):: Either String UserSignup
    case j of
      Left e -> do
        status status400
        json $ JSONError e
      Right s -> do
        uId <- liftAndCatchIO $ addUserToDB pool s 
        text $ TL.pack (show uId)
```

We'll be using the `liftAndCatchIO` function for now to handle this as it comes with the `scotty` library and we'd like to keep this simple. The `addUserToDB` function has a result `IO (Int64)` but the `post` function has type `post :: RoutePattern -> ActionM () -> ScottyM ()` when we are using `IO` in this function we must lift that IO up into the ActionM () monad. Typically this is done with the function `liftIO` but we will use the utility that comes with the `scotty` library for now. 

Ok let's fire up the server and see if this works!

I'm going to send the following JSON data
```json
{
  "name": {
    "firstname": "Ethan",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com",
  "password": "Holamundo@1"
}
```

And now we get back:

```
1
```

Ok things are working as expected now let's see what happens when we send the same data to test for unique constraint violations.

```
<h1>500 Internal Server Error</h1>SqlError {sqlState = "23505", sqlExecStatus = FatalError, sqlErrorMsg = "duplicate key value violates unique constraint \"users_email_key\"", sqlErrorDetail = "Key (email)=(ethangardner@ethan.com) already exists.", sqlErrorHint = ""}
```

Well that's sort of good. The database didn't save a duplicate record but we just sent our user a terrible looking response. Let's wrap our query in an Either statement to try and clean this up.


In order to catch this exception we will use the `try` function in the `Control.Exception` library. So we'll change our with pool function to this: 

*src/DB.hs*
```haskell
withPool :: (Pool Connection) -> (Connection -> IO a) -> IO (Either SqlError a)
withPool pool action = try (withResource pool action)
```

That should be sufficient to catch the error we're looking for. Now SqlError has the following structure:

```haskell
data SqlError = SqlError {
     sqlState       :: ByteString
   , sqlExecStatus  :: ExecStatus
   , sqlErrorMsg    :: ByteString
   , sqlErrorDetail :: ByteString
   , sqlErrorHint   :: ByteString
   } deriving (Eq, Show, Typeable)
```

What we're looking for from PostgreSQL is a unique constraint violation. We can find this here: https://www.postgresql.org/docs/10/errcodes-appendix.html

Persuing through the table we see that unique constraint violation has code: 23505. If you look back at the response we got from our server you'll see that sqlState was set to "23505" of let's write the error handling piece of our code and use our new `withPool` function in the route handler.

*src/Lib.hs*
```haskell
...

app :: (Pool Connection) -> IO ()
app pool = do
  scotty 3000 $ do
   post "/user" $ do
    b <- body
    let j = (eitherDecode b):: Either String UserSignup
    case j of
      Left e -> do
        status status400
        json $ JSONError e
      Right s -> do
        uId <- liftAndCatchIO $ addUserToDB pool s
        case (uId) of 
         Left e -> do
          status status400
          json $ JSONError e
         Right uid -> text $ TL.pack (show uid)

...

addUserToDB :: (Pool Connection) -> UserSignup -> IO (Either String Int64)
addUserToDB pool usr = do
  val <- DB.withPool pool (\c -> execute c statement usr)
  case val of
   Left e -> do
    case (sqlState e) of
      ("23505") -> return $ Left "That email is already taken please try signing up with a different email."
      _         -> return $ Left $ "Unexpected Server Error. Please try again later."
   Right s -> return $ Right s 
  where statement = "INSERT INTO users (email, password, name) VALUES (?, crypt(?, gen_salt('bf', 8)), ?)"

...
```

Now lets fire up our server and try sending that json over again and we get the following:

```
{
    "message": "That email is already taken please try signing up with a different email."
}
```

Perfect that's just what we wanted to show our new user. Now we didn't really properly handle that server error because our unexpected error should have thrown a 500. We could use `error` to throw that but there's not much of a purpose for that right now instead we'll take care of that when we start refactoring our app and going over Logging.

Ok we're in a pretty good spot right now but our files are a little messy and I think it's safe to refactor our application now. Refactor haskell apps is such an incredible experience if you've come from most other languages. You won't get to experience much of it on this refactor as we're really just changing the folder structure, but you should be able to see it later.

For this project we are going to separate our Domain by Feature and We'll write Adapters for our external services.

Our files currently look like this:

*src/Lib.hs*
```haskell
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV
import           Web.Scotty
import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import qualified Data.Text.Lazy as TL
import           Network.HTTP.Types.Status
import qualified LoadEnv as LE
import qualified DB as DB
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Data.Int
import           Database.PostgreSQL.Simple.ToField
import           Data.Pool

main :: IO ()
main = do
  LE.loadEnvFrom "./.env"
  pool <- DB.initDB
  app pool

app :: (Pool Connection) -> IO ()
app pool = do
  scotty 3000 $ do
   post "/user" $ do
    b <- body
    let j = (eitherDecode b):: Either String UserSignup
    case j of
      Left e -> do
        status status400
        json $ JSONError e
      Right s -> do
        uId <- liftAndCatchIO $ addUserToDB pool s
        case (uId) of 
         Left e -> do
          status status400
          json $ JSONError e
         Right uid -> text $ TL.pack (show uid)

   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

mkName :: Text -> Text -> Either Text Name
mkName fName lName = Name <$> fstName <*> lstName 
 where fstName = nameFieldTester fName
       lstName = nameFieldTester lName

mkUserSignup :: Name -> Email -> Password -> Either Text UserSignup
mkUserSignup na em pass = UserSignup <$> mkName (firstname na) (lastname na) <*> (validateEmail em) <*> (passwordValidation pass)  

nameFieldTester :: Text -> Either Text NameField
nameFieldTester field
 | T.length field > 50 = Left nameError
 | T.length field == 0 = Left nameError
 | otherwise           = Right field
 where nameError = "Name Fields must be less than 50 characters and Greater than 0 characters"

passwordValidation :: Text -> Either Text Password
passwordValidation a
 | T.length a < 8  = Left passwordErrorMessage
 | T.length a > 20 = Left passwordErrorMessage
 | (passwordStrength a 0 0 0) = Right a
 | otherwise = Left passwordErrorMessage
 where passwordErrorMessage = "Passwords Must be greater than 8 Characters and less than 20 and have at least 1 uppercase letter and 1 special or numeric character"

passwordStrength :: Text -> Int -> Int -> Int -> Bool
passwordStrength a lowercases uppercases specialcases
 | T.length a                   == 0 = if ((lowercases > 1) && (uppercases >= 1) && (specialcases >= 1)) then (True) else (False)
 | elem (T.head a) lowerCases   == True = passwordStrength (T.drop 1 a) (lowercases + 1) (uppercases) (specialcases) 
 | elem (T.head a) upperCases   == True = passwordStrength (T.drop 1 a) (lowercases) (uppercases + 1) (specialcases)
 | elem (T.head a) specialCases == True = passwordStrength (T.drop 1 a) (lowercases) (uppercases) (specialcases + 1)
 | otherwise = True
 where lowerCases = ['a'..'z']
       upperCases = ['A'..'Z']
       specialCases = ['1','2','3', '4', '5', '6', '7', '8', '9', '!', '@', '#', '$', '%', '^', '&', '*', '?']
 
validateEmail :: Text -> Either Text Email
validateEmail candidate = 
 case isEmail of
  Left e -> Left (pack e)
  Right s -> Right (TE.decodeUtf8 $ EV.toByteString s)
 where isEmail = EV.validate (TE.encodeUtf8 candidate)

addUserToDB :: (Pool Connection) -> UserSignup -> IO (Either String Int64)
addUserToDB pool usr = do
  val <- DB.withPool pool (\c -> execute c statement usr)
  case val of
   Left e -> do
    case (sqlState e) of
      ("23505") -> return $ Left "That email is already taken please try signing up with a different email."
      _         -> return $ Left $ "Unexpected Server Error. Please try again later."
   Right s -> return $ Right s 
  where statement = "INSERT INTO users (email, password, name) VALUES (?, crypt(?, gen_salt('bf', 8)), ?)"

type Email     = Text
type Password  = Text
type NameField = Text

data UserSignup = UserSignup
  { name :: Name
  , email :: Email
  , password :: Password
  } deriving (Eq, Show)

data Name = Name
  { firstname :: NameField
  , lastname :: NameField
  } deriving (Eq, Show)

data JSONError = JSONError { message :: String }

instance ToJSON JSONError where
  toJSON je = object ["message" .= message je]


instance FromJSON UserSignup where
  parseJSON (Object v) = do
   n <- v .: "name"
   e <- v .: "email"
   p <- v .: "password"
   case (mkUserSignup n e p) of
     Left er -> fail $ (unpack er)
     Right r -> return r 
  parseJSON _ = fail $ "Expected the UserSignup to be an object"

instance FromJSON Name where
  parseJSON (Object v) =
   Name <$>
   v .: "firstname"  <*>
   v .: "lastname"
  parseJSON _ = fail $ "Expected the UserSignup to have a name of type object"

instance ToJSON Name where
  toJSON name = object 
   [ "firstname" .= firstname name
   , "lastname" .= lastname name
   ]

instance ToField Name where
  toField a = toJSONField a

instance ToRow UserSignup where
  toRow user = [toField (email user), toField (password user), toField (name user)]
```

*src/DB.hs*
```haskell
module DB 
  ( initDB
  , showConnectCount
  , withPool
  ) where

import           Database.PostgreSQL.Simple
import qualified System.Environment as SE
import           Data.Word
import           Database.PostgreSQL.Simple.Migration
import           Data.Pool
import           Control.Exception

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- SE.getEnv "PG_CONNECT_HOST" 
  port <- SE.getEnv "PG_CONNECT_PORT"
  user <- SE.getEnv "PG_CONNECT_USER"
  db <- SE.getEnv "PG_CONNECT_DATABASE"
  pass <- SE.getEnv "PG_CONNECT_PASSWORD"
  return $ ConnectInfo host (read port :: Word16) user pass db

getConn :: IO Connection
getConn = do
  cInfo <- getConnectInfo
  conn <- connect cInfo
  return $ conn

getPool :: IO (Pool Connection)
getPool = do
  cInfo <- getConnectInfo
  createPool (connect cInfo) close 2 5 5

migrateDB :: Connection -> IO ()
migrateDB conn = do
  res <- withTransaction conn (runMigrations False conn [MigrationInitialization, MigrationDirectory "./migrations"])
  print res

withPool :: (Pool Connection) -> (Connection -> IO a) -> IO (Either SqlError a)
withPool pool action = try (withResource pool action)

initDB :: IO (Pool Connection)
initDB = do
  pool <- getPool
  withPool pool migrateDB
  return $ pool

showConnectCount :: IO ()
showConnectCount = do
  conn <- getConn
  x <- query_ conn "SELECT COUNT (*) FROM pg_stat_activity"
  let c = fromOnly . head $ (x :: [Only Integer])
  close conn
  print c

```

We're going to change our directory structure to look like this:

```
├── Adapter
│   ├── PG
│   │   └── Main.hs
│   └── Scotty
│       └── Main.hs
├── Domain
│   ├── Types
│   │   ├── CommonJSON.hs
│   │   ├── Name.hs
│   │   └── UserSignup.hs
│   └── User
│       ├── PG.hs
│       └── Service.hs
└── Lib.hs
```

There will be very little change to our code I'll go through any major changes and explain it but you've already seen everything in here.

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import qualified LoadEnv as LE
import qualified Adapter.Scotty.Main as Server
import qualified Adapter.PG.Main as DB

main :: IO ()
main = do
  LE.loadEnvFrom "./.env"
  pool <- DB.initDB
  Server.main pool
```

*src/Adapter/PG/Main.hs*
```haskell
module Adapter.PG.Main
  ( initDB
  , showConnectCount
  , withPool
  ) where

import           Database.PostgreSQL.Simple
import qualified System.Environment as SE
import           Data.Word
import           Database.PostgreSQL.Simple.Migration
import           Data.Pool
import           Control.Exception

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
  host <- SE.getEnv "PG_CONNECT_HOST" 
  port <- SE.getEnv "PG_CONNECT_PORT"
  user <- SE.getEnv "PG_CONNECT_USER"
  db <- SE.getEnv "PG_CONNECT_DATABASE"
  pass <- SE.getEnv "PG_CONNECT_PASSWORD"
  return $ ConnectInfo host (read port :: Word16) user pass db

getConn :: IO Connection
getConn = do
  cInfo <- getConnectInfo
  conn <- connect cInfo
  return $ conn

getPool :: IO (Pool Connection)
getPool = do
  cInfo <- getConnectInfo
  createPool (connect cInfo) close 2 5 5

migrateDB :: Connection -> IO ()
migrateDB conn = do
  res <- withTransaction conn (runMigrations False conn [MigrationInitialization, MigrationDirectory "./migrations"])
  print res

withPool :: (Pool Connection) -> (Connection -> IO a) -> IO (Either SqlError a)
withPool pool action = try (withResource pool action)

initDB :: IO (Pool Connection)
initDB = do
  pool <- getPool
  withPool pool migrateDB
  return $ pool

showConnectCount :: IO ()
showConnectCount = do
  conn <- getConn
  x <- query_ conn "SELECT COUNT (*) FROM pg_stat_activity"
  let c = fromOnly . head $ (x :: [Only Integer])
  close conn
  print c

```

*src/Adapter/Scotty/Main.hs*
```haskell
module Adapter.Scotty.Main
    ( main
    ) where

import           Web.Scotty
import           Database.PostgreSQL.Simple
import           Data.Pool

import qualified Domain.User.Service as UserService

main :: (Pool Connection) -> IO ()
main pool = do
  server pool

server :: (Pool Connection) -> IO ()
server pool = do
  scotty 3000 $ do
   post "/user" $ UserService.signupUser pool
   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

*src/Domain/Types/CommonJSON.hs*
```haskell
module Domain.Types.CommonJSON where

import           Data.Aeson

data JSONError = JSONError { message :: String }

instance ToJSON JSONError where
  toJSON je = object ["message" .= message je]
```

*src/Domain/Types/Name.hs*
```haskell
module Domain.Types.Name 
 ( mkName
 , Name(..)
 ) where

import           Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV
import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import qualified Data.Text.Lazy as TL
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField


mkName :: Text -> Text -> Either Text Name
mkName fName lName = Name <$> fstName <*> lstName 
 where fstName = nameFieldTester fName
       lstName = nameFieldTester lName

nameFieldTester :: Text -> Either Text NameField
nameFieldTester field
 | T.length field > 50 = Left nameError
 | T.length field == 0 = Left nameError
 | otherwise           = Right field
 where nameError = "Name Fields must be less than 50 characters and Greater than 0 characters"

type Email     = Text
type Password  = Text
type NameField = Text

data Name = Name
  { firstname :: NameField
  , lastname :: NameField
  } deriving (Eq, Show)

instance FromJSON Name where
  parseJSON (Object v) =
   Name <$>
   v .: "firstname"  <*>
   v .: "lastname"
  parseJSON _ = fail $ "Expected the UserSignup to have a name of type object"

instance ToJSON Name where
  toJSON name = object 
   [ "firstname" .= firstname name
   , "lastname" .= lastname name
   ]

instance ToField Name where
  toField a = toJSONField a
```

*src/Domain/Types/UserSignup.hs*
```haskell
module Domain.Types.UserSignup 
 ( mkUserSignup
 , UserSignup
 , Email
 , Password
 )where

import           Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV
import           Data.Aeson hiding (json)
import           Data.Aeson.Types
import qualified Data.Text.Lazy as TL
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Domain.Types.Name

mkUserSignup :: Name -> Email -> Password -> Either Text UserSignup
mkUserSignup na em pass = UserSignup <$> mkName (firstname na) (lastname na) <*> (validateEmail em) <*> (passwordValidation pass)  

passwordValidation :: Text -> Either Text Password
passwordValidation a
 | T.length a < 8  = Left passwordErrorMessage
 | T.length a > 20 = Left passwordErrorMessage
 | (passwordStrength a 0 0 0) = Right a
 | otherwise = Left passwordErrorMessage
 where passwordErrorMessage = "Passwords Must be greater than 8 Characters and less than 20 and have at least 1 uppercase letter and 1 special or numeric character"

passwordStrength :: Text -> Int -> Int -> Int -> Bool
passwordStrength a lowercases uppercases specialcases
 | T.length a                   == 0 = if ((lowercases > 1) && (uppercases >= 1) && (specialcases >= 1)) then (True) else (False)
 | elem (T.head a) lowerCases   == True = passwordStrength (T.drop 1 a) (lowercases + 1) (uppercases) (specialcases) 
 | elem (T.head a) upperCases   == True = passwordStrength (T.drop 1 a) (lowercases) (uppercases + 1) (specialcases)
 | elem (T.head a) specialCases == True = passwordStrength (T.drop 1 a) (lowercases) (uppercases) (specialcases + 1)
 | otherwise = True
 where lowerCases = ['a'..'z']
       upperCases = ['A'..'Z']
       specialCases = ['1','2','3', '4', '5', '6', '7', '8', '9', '!', '@', '#', '$', '%', '^', '&', '*', '?']
 
validateEmail :: Text -> Either Text Email
validateEmail candidate = 
 case isEmail of
  Left e -> Left (pack e)
  Right s -> Right (TE.decodeUtf8 $ EV.toByteString s)
 where isEmail = EV.validate (TE.encodeUtf8 candidate)

type Email     = Text
type Password  = Text


data UserSignup = UserSignup
  { name :: Name
  , email :: Email
  , password :: Password
  } deriving (Eq, Show)

instance FromJSON UserSignup where
  parseJSON (Object v) = do
   n <- v .: "name"
   e <- v .: "email"
   p <- v .: "password"
   case (mkUserSignup n e p) of
     Left er -> fail $ (unpack er)
     Right r -> return r 
  parseJSON _ = fail $ "Expected the UserSignup to be an object"

instance ToRow UserSignup where
  toRow user = [toField (email user), toField (password user), toField (name user)]

```

*src/Domain/User/PG.hs*
```haskell
module Domain.User.PG 
 ( addUserToDB
 ) where

import qualified Adapter.PG.Main as PG
import           Database.PostgreSQL.Simple
import           Data.Pool
import           Data.Int
import           Domain.Types.UserSignup

addUserToDB :: (Pool Connection) -> UserSignup -> IO (Either String Int64)
addUserToDB pool usr = do
  val <- PG.withPool pool (\c -> execute c statement usr)
  case val of
   Left e -> do
    case (sqlState e) of
      ("23505") -> return $ Left "That email is already taken please try signing up with a different email."
      _         -> return $ Left $ "Unexpected Server Error. Please try again later."
   Right s -> return $ Right s 
  where statement = "INSERT INTO users (email, password, name) VALUES (?, crypt(?, gen_salt('bf', 8)), ?)"
```

*src/Domain/User/Service.hs*
```haskell
module Domain.User.Service 
 ( signupUser
 ) where

import           Domain.User.PG as PG
import           Domain.Types.UserSignup
import           Domain.Types.CommonJSON

import           Web.Scotty
import           Data.Aeson hiding (json)
import           Network.HTTP.Types.Status
import           Database.PostgreSQL.Simple
import           Data.Pool
import qualified Data.Text.Lazy as TL

signupUser :: (Pool Connection) -> ActionM ()
signupUser pool = do
  b <- body
  let j = (eitherDecode b):: Either String UserSignup
  case j of
   Left e -> do
    status status400
    json $ JSONError e
   Right s -> do
    uId <- liftAndCatchIO $ PG.addUserToDB pool s
    case (uId) of 
     Left e -> do
      status status400
      json $ JSONError e
     Right uid -> text $ TL.pack (show uid)
```

This file contains the major change we can see that there is the introduction of the ActionM () monad. Where is this monad used? Let's look at the `scotty` docs. We can see that the routing function in `scotty` have the following type signature: `get :: RoutePattern -> ActionM () -> ScottyM ()` and if we look at the response handlers like `json` we see they have the following type signature: `json :: ToJSON a => a -> ActionM ()`. That's the only major change we can see here. Later on we will refactor this so that the service doesn't have any scotty functions in it so that our domain is less tightly coupled to our adapter. This is a good design decision as the creation of a user should have no relevance to routing which is what `scotty` should take care of.

Now let's make sure that instead of sending back a text response on success we send back a json response. Let's return back the user's information. Let's make a type that contains the information that we want to send back to our user. The 


*src/Domain/Types/UserSignup.hs*
```haskell
module Domain.Types.UserSignup 
 ( mkUserSignup
 , UserSignup
 , Email
 , Password
 , SuccessfulSignup(..)
 )where

...

import           Data.Int

...

newtype SuccessfulSignup = SuccessfulSignup { uId :: Int } deriving (Eq, Show) 

instance ToJSON SuccessfulSignup where
  toJSON ss = object 
   [ "user_id" .= uId ss
   ]
```

*src/Domain/User/Service.hs*
```haskell
signupUser :: (Pool Connection) -> ActionM ()
signupUser pool = do
  b <- body
  let j = (eitherDecode b):: Either String UserSignup
  case j of
   Left e -> do
    status status400
    json $ JSONError e
   Right s -> do
    uId <- liftAndCatchIO $ PG.addUserToDB pool s
    case (uId) of 
     Left e -> do
      status status400
      json $ JSONError e
     Right uid -> json $ SuccessfulSignup uid
```

and lets change our Database Access function to the following:

*src/Domain/User/PG.hs*
```haskell
addUserToDB :: (Pool Connection) -> UserSignup -> IO (Either String Int64)
addUserToDB pool usr = do
  val <- PG.withPool pool (\c -> query c statement usr) :: IO (Either SqlError [Only Int64])
  case val of
   Left e -> do
    case (sqlState e) of
      ("23505") -> return $ Left "That email is already taken please try signing up with a different email."
      _         -> return $ Left $ "Unexpected Server Error. Please try again later."
   Right s -> do
    case (length s) of
     1 -> return $ Right $ fromOnly $ head s 
     _ -> return $ Left $ "Unexpected Server Error. Please try again later."
  where statement = "INSERT INTO users (email, password, name) \
                    \VALUES (?, crypt(?, gen_salt('bf', 8)), ?) \ 
                    \RETURNING user_id"
```

Now we can fire up our server and try that out with a new email and we have just what we wanted.

Ok now that we have the ability to add user's let's give ourselves the ability to see our user's data. Let's first write the datatype:

*src/Domain/Types/User.hs*
```haskell
module Domain.Types.User
 ( User(..)
 , UserWrapper(..)
 ) where

import           Data.Text as T
import           Data.Aeson
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Time
import           Domain.Types.Name
import           Data.Int
import           Data.Time

newtype UserWrapper = UserWrapper { user :: User } deriving (Eq, Show)

instance ToJSON UserWrapper where
  toJSON u = object 
   [ "user" .= u
   ]

data User = User 
  { user_id :: Int64
  , email :: Text
  , name :: Name
  , image :: Maybe Text
  , about_me :: Maybe Text
  , created_at :: UTCTime
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON u = object 
   [ "user_id" .= user_id u
   , "email" .= email u
   , "name" .= name u
   , "image" .= image u
   , "about_me" .= about_me u
   , "created_at" .= created_at u
   ]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
```
*Using show as the means of converting UTCTimestamp probably isnt a great idea but let's just go ahead and use it*

Compiling that code should give us the following error:

```haskell
    • No instance for (Database.PostgreSQL.Simple.FromField.FromField
                         Name)
        arising from a use of ‘field’
    • In the second argument of ‘(<*>)’, namely ‘field’
      In the first argument of ‘(<*>)’, namely
        ‘User <$> field <*> field <*> field’
      In the first argument of ‘(<*>)’, namely
        ‘User <$> field <*> field <*> field <*> field’
   |
35 |   fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field
```

And GHC tells us exactly what's wrong there's no FromField instance for Name since it can have some arbitrary JSON structure. Let's add the following to our file:

*src/Domain/Types/Name.hs*
```haskell
...
import           Database.PostgreSQL.Simple.FromField

...
instance FromField Name where
  fromField a = fromJSONField a
```

Now let's write a database function that will give us access to our data.

*src/Domain/User/PG.hs*
```haskell
module Domain.User.PG 
 ( addUserToDB
 , getUserByID
 ) where

...
import           Domain.Types.User
...

getUserByID :: (Pool Connection) -> Integer -> IO (Either String User)
getUserByID pool uId = do
  val <- PG.withPool pool (\c -> query c statement (Only uId)) :: IO (Either SqlError [User])
  case val of
   Left e -> return $ Left $ "Unexpected Server Error. Please try again later."
   Right s -> do
    case (length s) of
     1 -> return $ Right $ head s 
     _ -> return $ Left $ "That user does not exist."
  where statement = "SELECT user_id, CAST (email as TEXT), name, image, about_me, created_at FROM users WHERE user_id = ?"
```

Now let's add a service to glue the routing and database access together:

*src/Domain/User/Service.hs*
```haskell
module Domain.User.Service 
 ( signupUser
 , getUser
 ) where

...
import           Domain.Types.User
...

getUser :: (Pool Connection) -> ActionM ()
getUser pool = do
  uId <- (Just <$> (param "user_id":: ActionM Integer)) `rescue` (\msg -> return $ Nothing)
  case uId of
    Nothing -> do
     status status400
     json $ JSONError "No Integer supplied in user_id paramter of url"
    Just i -> do
     usr <- liftAndCatchIO $ PG.getUserByID pool i 
     case usr of
      Left e -> do
       status status400
       json $ JSONError e
      Right s -> do
       json $ UserWrapper s
```

And finally we wire up our route:

*src/Adapter/Scotty/Main.hs*
```haskell
server :: (Pool Connection) -> IO ()
server pool = do
  scotty 3000 $ do
   post "/user" $ UserService.signupUser pool
   get "/user/:user_id" $ UserService.getUser pool
   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

Ok let's give our new function a try and see what we get I made a new user and it's ID was 17

```json
{
    "user": {
        "email": "ethangardner9@ethan.com",
        "image": null,
        "name": {
            "lastname": "Gardner",
            "firstname": "Ethan"
        },
        "about_me": null,
        "created_at": "2019-12-23T04:52:35.003301Z",
        "user_id": 17
    }
}
```

Perfect. How about a user that doesn't exist (hit a route like localhost:3000/user/2000):

```json
{
    "message": "That user does not exist."
}
```

And lastly what if we send something that isn't an int:

```html
<h1>404: File Not Found!</h1>
```

This is because if the `read` function fails scotty automatiaclly calls `next` which allows the server to try and find another handler that matches in our case we don't have one so we rightfully return a `404`.

Ok now we can see our users data. But the problem is so can everyone! In the next post we'll add authentication via JWT's to our app.