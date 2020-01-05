---
layout: post
title: Scotty Tutorial Part 1
date: 2020-01-02
description: Basic Server Setup and JSON validation.
img: Haskell.png # Add image post (optional)
fig-caption: # Add figcaption (optional)
tags: [Scotty, Haskell]
---

Ok lets get started we will start by building a larger haskell application from the ground up and we will first build simply using just explicit paramter passing and as few language extensions as possible for simplicity. In order to get started we will first focus on building out our user signup validation and setting up a basic server. Lets add some packages to our project so that we can really get started you will need to add the following to your package.yaml file.

*package.yaml*
```yaml
name:                scotty-tutorial

default-extensions:
- OverloadedStrings

dependencies:
- base >= 4.7 && < 5
- scotty
- text
- aeson
- email-validate
- bytestring
- containers



library:
  source-dirs: src

executables:
  scotty-tutorial-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - scotty-tutorial

tests:
  scotty-tutorial-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - scotty-tutorial

```

Adding the ```OverloadedStrings``` pragma to the default extensions will make it so that we don't need to put ```{-# LANGUAGE OverloadedStrings #-}``` at the top of each of our source files. The pragmas in the default extensions will be used on all of our source files.

The ```OverloadedStrings``` extesnsion allows us to have a more polymorphic version of String literals in our application so that we don't need to explicitly declare `Text` as our datatype when writing our strings.

Then in order to install these modules we will run the following comamnd:

```shell
$ stack build
```

One of the most unfortunate parts of haskell is waiting for all your code to compile if you come from python, node.js, or any other interpreted language you won't be used to the frequent breaks needed to recompile all of these packages. It would be nice if the binaries could be directly downloaded and there is some work being done on this currently via Nix but that is far outside the scope of this tutorial.

Once we've got everything installed we can start building the core logic to our user signup.

We are going to want the following data from our users when they signup:

* Name: JSON
* Email: Text
* Password: Text

So to model this in haskell we will end up using the following datatypes:

```haskell
data UserSignup = UserSignup
  { name :: Name
  , email :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

data Name = Name
  { firstname :: Text
  , lastname :: Text
  } deriving (Eq, Show, Generic)
```

For now we'll keep all this in the file src/Lib.hs and worry about the folder structure later as we build out the application.

 So your files should look like this.

 *app/Main.hs*
```haskell
module Main where

import qualified Lib

main :: IO ()
main = Lib.main
``` 

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

main :: IO ()
main = putStrLn "someFunc"

data UserSignup = UserSignup
  { name :: Name
  , email :: Text
  , password :: Text
  } deriving (Eq, Show)

data Name = Name
  { firstname :: Text
  , lastname :: Text
  } deriving (Eq, Show)
```

 Now in true haskell spirit lets compile our code get yelled at by GHC and frantically scramble to fix all those errors.

 In order to develop more effectively what we will do is use auto reloading of our sorce code files. So instead of using ```stack build``` we will use the following flags 

```shell
$ stack build --fast --file-watch
```

**Note:** *Note you can also use ghcid but this requires more setup.*

Now with that settled lets look at the errors we get. One of the errors you should see is:
```Not in scope: type constructor or class `Text'```
So how do we fix that... well if you rember the ```text``` package we added to our *package.yaml* file contains the necessary type ```Text``` that we are looking for so lets add this line to our file:

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import Data.Text as T

main :: IO ()
main = putStrLn "someFunc"
...
```

Once we add that to the file and hit save we should see our errors disappear.

Now let's make sure that when we are making these data types that they comply with what we expect.

Lets first start with creating the smart constructor for ```Name``` Lets arbitrarily assume that all `firstname` and `lastname` should be less than 50 characters and more than 0. We can use that to come up with the following functions.

*src/Lib.hs*
```haskell
...

mkName :: Text -> Text -> Either Text Name
mkName fName lName = Name <$> fstName <*> lstName 
 where fstName = nameFieldTester fName
       lstName = nameFieldTester lName

nameFieldTester :: Text -> Either Text Text
nameFieldTester field
 | T.length field > 50 = Left "Name Fields must be less than 50 characters and Greater than 0 Characters"
 | T.length field == 0 = Left "Name Fields must be less than 50 characters and Greater than 0 characters"
 | otherwise                   = Right field

...
```

Now for password we want to ensure that the password follows some strength criteria. Lets say that we need at least 8 characters and we need to make sure that we have at least one upper case and 1 special or numeric character. We could write a Regex but lets have some fun with haskell and skip introducing a new package. Here is a super quick rudimentary solution I came up with.

*src/Lib.hs*
```haskell 
...
passwordValidation :: Text -> Either Text Text
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
...
```

Now lastly we want to confirm that our potential user has a valid email. In order to do this we will use the very robust ```email-validate``` package that we included in our *package.yaml*. In this section we will get our first taste of the TypeFoo needed in order to deal with haskells ByteString, Text, and String Types.  

In the `email-validate` package we see the follwing function.

*src/Lib.hs*
```haskell
validate :: ByteString -> Either String EmailAddress
```

Perfect!

Well not really we need to convert our potential email address that has type `Text` to `ByteString` and we want to return `Text` in our function not `String` and we don't want to return an `EmailAddress` we want to return Text. Sooo what do we do. Lets write a wrapper around the function that handles all this logic.

first lets make sure we have the correct packages

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV

...

```

Then we can write our function:

*src/Lib.hs*
```haskell
...
validateEmail :: Text -> Either Text Text
validateEmail candidate = 
 case isEmail of
  Left e -> Left (pack e)
  Right s -> Right (TE.decodeUtf8 $ EV.toByteString s)
 where isEmail = EV.validate (TE.encodeUtf8 candidate)
...
```

Lets explain this a little. We pass our potential password in but it's of type text so we convert that to a ByteString with `encodeUtf8`. Then we get a `Either String EmailAddress` from the isEmail value. If we get a `Left String` we use `pack :: String -> Text` to return a Text value and if we get an `EmailAddress` we convert it to text by using `decodeUtf8 :: ByteString -> Text` and `toByteString :: EmailAddress -> ByteString`. 

and now we can construct our UserSignup smart constructor.

**Note:** *This doesn't necessarily comply with best practices for smart constructors. Normally you would put these functions in another module and export only `mkUserSignup` and `UserSignup` with our it's constructors that way you must pass the validation to create the desired value.*

*src/Lib.hs*
```haskell
mkUserSignup :: Name -> Text -> Text -> Either Text UserSignup
mkUserSignup na em pass = UserSignup <$> mkName (firstname na) (lastname na) <*> (validateEmail em) <*> (passwordValidation pass)
```

Ok so we did all that work for validation and we now have a file that looks like this:

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV

main :: IO ()
main = putStrLn "someFunc"

mkName :: Text -> Text -> Either Text Name
mkName fName lName = Name <$> fstName <*> lstName 
 where fstName = nameFieldTester fName
       lstName = nameFieldTester lName

mkUserSignup :: Either Text Name -> Text -> Text -> Either Text UserSignup
mkUserSignup na em pass = UserSignup <$> na <*> (validateEmail em) <*> (passwordValidation pass)  

nameFieldTester :: Text -> Either Text Text
nameFieldTester field
 | T.length field > 50 = Left nameError
 | T.length field == 0 = Left nameError
 | otherwise           = Right field
 where nameError = "Name Fields must be less than 50 characters and Greater than 0 characters"

passwordValidation :: Text -> Either Text Text
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
 
validateEmail :: Text -> Either Text Text
validateEmail candidate = 
 case isEmail of
  Left e -> Left (pack e)
  Right s -> Right (TE.decodeUtf8 $ EV.toByteString s)
 where isEmail = EV.validate (TE.encodeUtf8 candidate)

data UserSignup = UserSignup
  { name :: Name
  , email :: Text
  , password :: Text
  } deriving (Eq, Show)

data Name = Name
  { firstname :: Text
  , lastname :: Text
  } deriving (Eq, Show)

```

All these `Text` Fields are going to be prone to errors we could easily switch up passwords and emails at a later point when we refactor. So lets leverage some of haskells type system to remedy this.

lets create the following type aliases and change our data types to match

*src/Lib.hs*
```haskell
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
```

and now our functions look like this:

*src/Lib.hs*
```haskell
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
```

It doesn't introduce any extra type safety but it helps us to better describe our domain for when we look at our code later.

Ok great now time for the fun part... Web Development. Yep that's right now we're going to start actually processing some data. 

For our server we will be using the `scotty` library that we included in our `package.yaml` it is part of the `WAI` (Web Application Interface) ecosystem. That was originally developed by Michael Snoyman. It is the backbone of all the major haskell web server libraries like `servant`, `yesod`, and `snap`. For this tutorial I chose the `scotty` library because in my opinion it is the easiest library to get started with. It is intended to be like `sinatra` from ruby, `flask` from python, or `express` from node.js. Well enough talking lets get to it and start slappng some code down.

For the sake of simplicity we are going to keep going with our *Lib.hs* file with the intention to clean up our folder architecture later.  


lets change *src/Lib.hs* so that it looks like this now:

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import           Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV
import           Web.Scotty

main :: IO ()
main = app

app :: IO ()
app = do
  scotty 3000 $ do
   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

...    
```

now we'll run:

```shell
$ stack build
```

and then we'll run

```shell
$ stack exec scotty-tutorial-exe
```

In order to actually run the server. You should see

```shell
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

In your terminal. You can now go visit `localhost:3000/whatever` and youll see the text thaty `scotty` renders. Why is this happending?

Lets take a look at the method:

*src/Lib.hs*
```haskell
  scotty 3000 $ do
   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

this tells scotty that any HTTP request that is a `GET` with only one route paramter should have it's route paramter extracted by the function ```param :: Parsable a => Text -> ActionM a``` and since it is produced by the ActionM monad we will get whatver the a value is due to `<-` binding.

Notice if we go to somewhere with two parameters though we will get an error try visting `localhost:3000/ethan/gardner` and you will get a `404` from the server. Or try using a different request verb like `POST` or `PUT` and you will also get an error. We will handle these errors later on.


So lets get taking care of business with this whole user signup ordeal. Lets replace our app function with this.

*src/Lib.hs*
```haskell
app :: IO ()
app = do
  scotty 3000 $ do
   post "/user" $ do
    user <- jsonData :: ActionM UserSignup
    text $ "GREAT USER CREATED"

   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

So we will get the opportunity to deserialize a user and serialize a user back into JSON *Discliamer never send passwords back to a user this is just to show how to write the instances at first and we will change it*

If we start up GHC again:

```shell
$ stack build --fast --file-watch
```

we should see the following error:

```
No instance for (aeson-1.4.6.0:Data.Aeson.Types.FromJSON.FromJSON
                         UserSignup)                        
```

What do they mean? Well they're telling us that we need to write an instance of FromJSON for our UserSignup type, this is because jsonData's type signature is: `jsonData :: FromJSON a => ActionM a`

So lets import `aeson` which we added to our *package.yaml*

*src/Lib.hs*
```haskell
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as EV
import           Web.Scotty
import           Data.Aeson
```

and we can start writing our instance:

*src/Lib.hs*
```haskell
instance FromJSON UserSignup where
  parseJSON (Object v) = 
   UserSignup <$>
   v .: "name"  <*>
   v .: "email" <*>
   v .: "password" 
  parseJSON _ = fail $ "Expected the UserSignup to be an object"
```

GHC yells at us though and says:

```shell
No instance for (FromJSON Name) arising from a use of `.:'
```

Well duh we need to write that one as well.

*src/Lib.hs*
```haskell
instance FromJSON Name where
  parseJSON (Object v) = 
   Name <$>
   v .: "firstname"  <*>
   v .: "lastname"
  parseJSON _ = fail $ "Expected the UserSignup to have a name of type object"
```

If we add the following we can get GHC to shut up and we can go right on our way. For anyone that's experienced in haskell though they probably know that all these instance could've been derived generically with the:

```haskell
{-# LANGUAGE DeriveGeneric #-}
```

and then shortend to

```haskell
instance FromJSON UserSignup where
instance FromJSON Name where
```

But we want to stick to the basics here so lets just write them by hand for now and pull out the tricks later.


Ok so lets build our app and run it

```haskell
$ stack exec scotty-tutorial-exe
```
and then we'll send over some json.

```json
{
  "name": {
    "firstname": "Ethan",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com",
  "password": "Holamundo1"
}
```

and we get back:

```GREAT USER CREATED```

now lets send it some bad json:

```json
{
  "name": {
    "firstname": "Ethan",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com"
}
```

ooo we don't get what we want there:

```
<h1>500 Internal Server Error</h1>jsonData - no parse: Error in $: key "password" not found. Data was:{
  "name": {
    "firstname": "Ethan",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com"
}
```

Yeah that's not so pretty. Let's fix that so that we can actually get something useful back from this for our potential users.

We can do this two different ways. We can use the function `rescue` provided by scotty or we can use `eitherDecode` provided by `aeson` and pass the request body to it directly. Let's go with the second option so that we are a little bit less tied to the `scotty` framework.

lets change our app function to look like this:

*src/Lib.hs*
```haskell
app :: IO ()
app = do
  scotty 3000 $ do
   post "/user" $ do
    b <- body
    let j = (eitherDecode b):: Either String UserSignup
    case j of
      Left e -> text (TL.pack e)
      Right s -> text ("User Created")

   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

If we pass the same bad json we should see a better error message:

```
Error in $: key "password" not found
```

Ehh that's a little better but we're still getting a 200 from this request let's make it so that we receive a 400 when we get invalid data in order to accomplish that we will use a function provided by scotty `status :: Status -> ActionM ()` but Status is not provided by scotty we will need the `http-types` package we installed earlier for that.

so lets add the following imoprt to our file and change our `app` function to reflect the desired change.

*src/Lib.hs*
```haskell
...
import           Network.HTTP.Types.Status

main :: IO ()
main = app

app :: IO ()
app = do
  scotty 3000 $ do
   post "/user" $ do
    b <- body
    let j = (eitherDecode b):: Either String UserSignup
    case j of
      Left e -> do
        status status400
        text (TL.pack e)
      Right s -> text ("User Created")

   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
...
```

Now your probably thinking that we could just send any data that fits the user schema and we would accept it... and your're right so how are we going to fix that? We'll we will use our `mkUserSignup` function in the `FromJSON` instance of `UserSignup`. So lets modify out instance to look like the following:

 *src/Lib.hs*
```haskell
...
instance FromJSON UserSignup where
  parseJSON (Object v) = do
   n <- v .: "name"
   e <- v .: "email"
   p <- v .: "password"
   case (mkUserSignup n e p) of
     Left er -> fail $ (unpack er)
     Right r -> return r 
  parseJSON _ = fail $ "Expected the UserSignup to be an object"
...
```

Perfect now you can send in all the data you want via JSON and it will have to pass our UserSignup constructor!

Let's test it out lets send the following json:

```json
{
  "name": {
    "firstname": "Ethan",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com",
  "password": "holamundo@1"
}
```

Response:

```html
Error in $: Passwords Must be greater than 8 Characters and less than 20 and have at least 1 uppercase letter and 1 special or numeric character
```

```json
{
  "name": {
    "firstname": "Ethannnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com",
  "password": "Holamundo@1"
}
```

Response:

```html
Error in $: Name Fields must be less than 50 characters and Greater than 0 characters
```

And you can try it with some more inputs if you like but for now I think this is Ok. Of Course it probably would be nice to collect all of those errors and send them back to the user, and we will later but you'll need a little bit more advanced knowledge of how to carry around the failures of each of these and we'll also strong type Email, Password, and Name by making them new types. If you'd like to take a peak at the approach we will take you can look at this blog post (https://bitemyapp.com/blog/strong-types-and-testing/) by Chris Allen who is the Author of *Haskell Programming From First Principles* which i heavily recommend to anyone that is trying to learn haskell.

So now that we have our error messages all tied up let's wrap them in some JSON to send back to our user. 

 *src/Lib.hs*
```haskell
data JSONError = JSONError { message :: String }

instance ToJSON JSONError where
  toJSON je = object ["message" .= message je]
```

And then lets change our `app` function to properly handle the errors by changing it to the following:
```haskell
data JSONError = JSONError { message :: String }

instance ToJSON JSONError where
  toJSON je = object ["message" .= message je]
```

and when we send bad json let's check out the response:
```json
{
  "name": {
    "firstname": "Ethannnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn",
    "lastname": "Gardner"
  },
  "email": "ethangardner@ethan.com",
  "password": "Holamundo@1"
}
```

Response:

```html
{
    "message": "Error in $: Name Fields must be less than 50 characters and Greater than 0 characters"
}
```

Great well I think we're at a decent stopping point here the next thing we'll do is figure out how we can save our users into a Postgreql database so we can start getting on with adding some cooler features to this like Email Verification, AWS S3 image upload and more as well as Refactor our application into a more complex monad transformer stack that will allow us to add features with less verbosity.

 The final `Lib.hs` file looks like this.

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
import           Network.HTTP.Types.Status

main :: IO ()
main = app

app :: IO ()
app = do
  scotty 3000 $ do
   post "/user" $ do
    b <- body
    let j = (eitherDecode b):: Either String UserSignup
    case j of
      Left e -> do
        status status400
        json $ JSONError e
      Right s -> text ("User Created")

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
```