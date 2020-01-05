---
layout: post
title: Scotty Tutorial Part 3
date: 2020-01-04
description: Adding Authentication and Authroizaton via JWT's to our application.
img: Haskell.png # Add image post (optional)
fig-caption: # Add figcaption (optional)
tags: [Scotty, Haskell]
---

```
- jose
- cryptonite
- HsOpenSSL
- mtl
- transformers
- unordered-containers
```

If you havent already you should go through the first two parts of this tutorial if you want to go straight to this part you can find it here: 

In this part of the tutorial we will go through adding authentiation to our app. There are two approaches that we can go with here. We can use session based authentication or JWT based authentication. JWT authentication has become popular but most applications dont actually use it for it's intended purpose. Most tutorials show JWT's being used exactly like a session token. JWT's should be used when you need to allow a user access to multiple services. Sessions are simpler than JWT's and allow for quicker invalidation of user permissions. For our application it is probably better to use sessions at this point, but i think most people reading this would rather see JWT authentication so I will show that in this part of the tutorial. If i find the time I may also go over the session based approach.

*I'd like to preface this with the fact that I am not a web security expert by any means so if i say something wrong please point it out*

The `jose` library appears to be the most up to date library for our purpose. This is the approach you might take if you were planning on having an Auth server that would genreate all of the tokens and then have microservices that decode the token using a public key to verify that the token generated was from out authentication server. The public key will be available as part of a JWK and will be available via an endpoint on our server. The other potential servers in our app's ecosystem can then access this endpoint to obtain the JWK's for the decoding.

What most would refer to as a JWT is not actually a JWT. They are referring to a JWS (JSON Web Signature) or a JWE (JSON Web Encryption). We will be using a JWS that is signed with an RSA Private Key. Let's first generate our keys using a common application `ssh-keygen` and `openssl`.

*I am developing on a linux machine but Windows users should be able to use ssh-keygen if they have git installed as for OpenSSL i am not sure what the windows alternative would be.*

Let's make a config directory in the root of our folder and run the following command: 

*There's no need to password protect these keys*

```shell
$ ssh-keygen -t rsa -b 4096 -m PEM -f key
$ openssl rsa -in ./key -pubout -outform PEM -out key.pub
```

This generates a 4096 bit RSA key pair. This gives us a folder structure like so

```shell
.
├── key
└── key.pub
```

Ok great now we have the starting material to create our public and private JWK's I wasn't able to find a way to directly make the RSA Key's into JWK's with the `jose` library although I am sure this can be done *(the jose library seems very comprehensive and the documentation seems great, but with it's heavy usage of `lens` it can definetely be hard for someone to navigate around. Another library you could use that seems to be more beginner friendly is `jose-jwt` but it hadn't been updated in almost two years at the time of this writing and it depends on the `jose` library which is frequently updated.)* The main functions that we want to implement are shown as examples in the `jose` documentation and those functions are:

http://hackage.haskell.org/package/jose-0.8.2.0/docs/Crypto-JWT.html

```haskell
doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doJwtSign jwk claims = runExceptT $ do
  alg <- bestJWSAlg jwk
  signClaims jwk (newJWSHeader ((), alg)) claims

doJwtVerify :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
doJwtVerify jwk jwt = runExceptT $ do
  let config = defaultJWTValidationSettings (== "bob")
  verifyClaims config jwk jwt
```

Ok so let's start by making our JWK's becuase that is what we will need to encrypt our tokens. Let's make a folder in our Adapters folder called Auth. We will call this an Adapter since it is working with outside files. Our end file will look like this. Dont worry we'll walk through each step.

*src/Adapter/Auth/Main.hs*
```haskell
module Adapter.Auth.Main (mkAuthEnv) where

import           Crypto.JOSE.JWK
import           Crypto.PubKey.RSA.Types as RSAT
import           OpenSSL.PEM
import           OpenSSL.EVP.PKey as EVP
import           OpenSSL.RSA as ORSA
import           Domain.Types.AuthEnv 

mkAuthEnv :: IO AuthEnv 
mkAuthEnv = do
  privKey <- makePrivateKey
  let pubKey = RSAT.toPublicKey (KeyPair privKey)
  let pubJWK = fromKeyMaterial (RSAKeyMaterial (toRSAPublicKeyParameters pubKey))
  let privJWK = fromRSA privKey
  return $ AuthEnv privJWK pubJWK (60 * 60)

makePrivateKey :: IO RSAT.PrivateKey
makePrivateKey = do
  privKey <- readFile "./config/key"
  priK <- readPrivateKey privKey PwNone :: IO SomeKeyPair
  let k = toKeyPair $ priK :: Maybe (RSAKeyPair)
  let kp = convertRSA k
  case kp of
   Just pka -> do
   	return pka
   Nothing -> error "No Private Key Could Be Made"

convertRSA :: Maybe RSAKeyPair -> Maybe RSAT.PrivateKey
convertRSA Nothing = error "No RSA Key Pair Supplied"
convertRSA (Just pk) = do
  let rsad = ORSA.rsaD pk
  let rsap = ORSA.rsaP pk
  let rsaq = ORSA.rsaQ pk
  let rsadmp1 = ORSA.rsaDMP1 pk
  let rsadmq1 = ORSA.rsaDMQ1 pk
  let rsaiqmp1 = ORSA.rsaIQMP pk
  let pubkey = makePubKey pk
  RSAT.PrivateKey <$> Just pubkey <*> Just rsad <*> Just rsap <*> Just rsaq <*> rsadmp1 <*> rsadmq1 <*> rsaiqmp1

makePubKey :: RSAKeyPair -> RSAT.PublicKey
makePubKey pubk = do
  let rsa_size = ORSA.rsaSize pubk
  let rsa_n = ORSA.rsaN pubk
  let rsa_e = ORSA.rsaE pubk
  RSAT.PublicKey rsa_size rsa_n rsa_e
```

The only other file we need to make is the file that contains our `AuthEnv` datatype. And that file will look like this:

*src/Domain/Types/AuthEnv.hs*
```haskell
module Domain.Types.AuthEnv (AuthEnv(..)) where

import           Crypto.JOSE.JWK

data AuthEnv = AuthEnv 
  { privateJWK :: JWK
  , publicJWK :: JWK
  , expirationTimeDifference :: Int
  } deriving (Eq, Show)
```

There's not much going on with this datatype it's pretty self explanitory. This datatype will be passed to all our functions that require being signed and verified.

Ok so back to our functions in our Auth Adapter. We can find the conversion functions in `Crypto.JOSE.JWK` that we want to utilize:

```haskell
fromKeyMaterial :: KeyMaterial -> JWK

fromRSA :: PrivateKey -> JWK
```
http://hackage.haskell.org/package/jose-0.8.2.0/docs/Crypto-JOSE-JWK.html#g:4

So what we need to do it marshall the strings in our files into `KeyMaterial` and `PrivateKey` those two come from another library called `cryptonite` but in order to get made into the `PrivateKey` we need to use `HsOpenSSL` (again probably another way):

I don't mind throwing an error here since this should take place prior to the app starting and if there are any issues with generating the key i would want the app to shutdown.

*src/Adapter/Auth/Main.hs*
```haskell
makePrivateKey :: IO RSAT.PrivateKey
makePrivateKey = do
  privKey <- readFile "./config/key"
  priK <- readPrivateKey privKey PwNone :: IO SomeKeyPair
  let k = toKeyPair $ priK :: Maybe (RSAKeyPair)
  let kp = convertRSA k
  case kp of
   Just pka -> do
   	return pka
   Nothing -> error "No Private Key Could Be Made"
```

we use the `readPrivateKey` function from `OpenSSL.PEM` (our key is currently stored in PEM format). If you didnt supply a password use `PwNone`. 

Now we have `SomeKeyPair` from `readPrivateKey :: String -> PemPasswordSupply -> IO SomeKeyPair` with this we can use `toKeyPair :: SomeKeyPair -> Maybe RSAKeyPair` to get our RSAKeyPair and then we use our functions that we created:

*src/Adapter/Auth/Main.hs*
```haskell
convertRSA :: Maybe RSAKeyPair -> Maybe RSAT.PrivateKey
convertRSA Nothing = error "No RSA Key Pair Supplied"
convertRSA (Just pk) = do
  let rsad = ORSA.rsaD pk
  let rsap = ORSA.rsaP pk
  let rsaq = ORSA.rsaQ pk
  let rsadmp1 = ORSA.rsaDMP1 pk
  let rsadmq1 = ORSA.rsaDMQ1 pk
  let rsaiqmp1 = ORSA.rsaIQMP pk
  let pubkey = makePubKey pk
  RSAT.PrivateKey <$> Just pubkey <*> Just rsad <*> Just rsap <*> Just rsaq <*> rsadmp1 <*> rsadmq1 <*> rsaiqmp1

makePubKey :: RSAKeyPair -> RSAT.PublicKey
makePubKey pubk = do
  let rsa_size = ORSA.rsaSize pubk
  let rsa_n = ORSA.rsaN pubk
  let rsa_e = ORSA.rsaE pubk
  RSAT.PublicKey rsa_size rsa_n rsa_e
```

This takes us from the datatype in the `HsOpenSSL` to the `cryptonite` library which is ultimately what the `jose` library uses.


Once we have our `PrivateKey` we can easily get our `PublicKey` using the `toPublicKey :: KeyPair -> PublicKey` which we can supply with a `KeyPair` since the type has the following signature `KeyPair PrivateKey`. And then we have eveything we need for our original target functions to create a `JWK`:

```haskell
fromKeyMaterial :: KeyMaterial -> JWK

fromRSA :: PrivateKey -> JWK
```

Perfect now we have the key's needed to properly secure our tokens and we will supply our tokens inside of our `AuthEnv` datatype. 

Now we need to create our functions that will utilize these key's in order to actually use our datatype.

Here we can see why it is often a good idea to supply a more abstract datatype to our Application environment than an explicit type. As of right now we supply our application with `Pool Connection` this is a very context specific type. Instead lets create a type that we will have the ability to supply our application with the proper information (To more advanced haskellers this obviously would be a good time to include a reader datatype but we will do that in a later part of the tutorial as the only advantage it gives us now is to get rid of explicit parameter passing.).


Lets create our type:

*src/Domain/Types/AppEnv.hs*
```haskell
module Domain.Types.AppEnv where

import           Data.Pool 
import           Database.PostgreSQL.Simple
import           Domain.Types.AuthEnv

data AppEnv = AppEnv
  { pgEnv :: (Pool Connection)
  , authEnv :: AuthEnv
  }
```

I've seen this type be called a lot of different things Config, Env, Context, etc. This is generally just personal preference it accomplishes the same goal and allows us to easily add new fields to the datatype as our App changes.

now let's go back to our `Lib.hs` file and add our new datatype.

*src/Lib.hs*
```haskell
module Lib
    ( main
    ) where

import qualified LoadEnv as LE

import qualified Adapter.Scotty.Main as Server
import qualified Adapter.Auth.Main as AuthAdapter
import qualified Adapter.PG.Main as DB
import           Domain.Types.AppEnv


main :: IO ()
main = do
  LE.loadEnvFrom "./.env"
  authEnv <- AuthAdapter.mkAuthEnv
  pool <- DB.initDB
  Server.main (AppEnv pool authEnv)
```

And since we changed our datatype now we need to change our `Server.main` function to reflect this. So let's go to our file and make those changes.


*src/Adapter/Scotty/Main.hs*
```haskell
module Adapter.Scotty.Main
    ( main
    ) where

import           Web.Scotty
import qualified Domain.User.Service as UserService
import           Domain.Types.AppEnv

main :: AppEnv -> IO ()
main env = do
  server env

server :: AppEnv -> IO ()
server env = do
  scotty 3000 $ do
   post "/user" $ UserService.signupUser env
   get "/user/:user_id" $ UserService.getUser env
   get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
```

That was simple enough if you're following along with the `stack build --file-watch --fast` command you should see `GHC` guiding us through the process of refactoring which is really an amazing developer experience.

lets refactor our service handlers now so that they can take our `AppEnv` datatype

*src/Domain/User/Service.hs*
```haskell
module Domain.User.Service 
 ( signupUser
 , getUser
 ) where

import           Domain.User.PG as PG
import           Domain.Types.UserSignup
import           Domain.Types.User
import           Domain.Types.CommonJSON
import           Domain.Types.AppEnv

import           Web.Scotty
import           Data.Aeson hiding (json)
import           Network.HTTP.Types.Status
import           Database.PostgreSQL.Simple
import           Data.Pool
import qualified Data.Text.Lazy as TL

signupUser :: AppEnv -> ActionM ()
signupUser env = do
  b <- body
  let j = (eitherDecode b):: Either String UserSignup
  case j of
   Left e -> do
    status status400
    json $ JSONError e
   Right s -> do
    usr <- liftAndCatchIO $ PG.addUserToDB (pgEnv env) s
    case (usr) of 
     Left e -> do
      status status400
      json $ JSONError e
     Right u -> json $ SuccessfulSignup u

getUser :: AppEnv -> ActionM ()
getUser env = do
  uId <- (Just <$> (param "user_id":: ActionM Integer)) `rescue` (\msg -> return $ Nothing)
  case uId of
    Nothing -> do
     status status400
     json $ JSONError "No Integer supplied in user_id paramter of url"
    Just i -> do
     usr <- liftAndCatchIO $ PG.getUserByID (pgEnv env) i 
     case usr of
      Left e -> do
       status status400
       json $ JSONError e
      Right s -> do
       json $ UserWrapper s
```

And now that we can give our routes access to our AuthEnv let's make a login route that issues a token for our users. Let's first start by figuring out what claims we want to put into our ClaimSet. Lets put the user's id `user_id` in there. Looking at the RFC spec for the JWT this goes under the field `sub` and we'll put the users email in as well.


the other reserved fields in the RFC Spec are:

- iss: The issuer of the token.
- sub: The subject of the JWT.
- aud: Who the JWT is intended for.
- exp: When the JWT expires.
- nbf: The time at which the token should not be evaluated before.
- iat: When the JWT was issued.
- jti: The unique identifier for the token this can be used for blacklisting.

None of these fields are required by the specification. Currently we will use the following:

- iss
- sub
- aud
- exp
- nbf
- iat

We don't really have a need for the jti command as of right now but maybe we will later at which time we will add it.

We will also add in an email field as a public claim. Great so let's create a datatype that has these claims and let's call it `AuthUser`


*src/Domain/Types/AuthUser.hs*
```haskell
module Domain.Types.AuthUser where

import           Data.Int
import           Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T

data AuthUser = AuthUser
  { auth_user_id :: Int64
  , auth_user_email :: T.Text
  } deriving (Eq, Show)

instance FromRow AuthUser where
  fromRow = AuthUser <$> field <*> field
```

Ok that's a good enough container for now. Let's now create our function for issuing our token. Authentication will be a core part of our application so we will throw that in the Domain folder.

*src/Domain/Auth/Auth.hs*
```haskell
module Domain.Auth.Auth where

import           Crypto.JOSE
import           Crypto.JOSE.Error as E
import           Crypto.JOSE.JWK
import           Crypto.JOSE.JWS as CJWS
import           Crypto.JOSE.Header
import           Crypto.JOSE.Compact
import           Control.Monad.Except
import           Crypto.JWT
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import           Domain.Types.AuthEnv
import           Data.Aeson hiding (json)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Domain.Types.AuthUser

mkStandardClaims :: IO (HM.HashMap T.Text Value)
mkStandardClaims = do
  t <- getCurrentTime 
  let rounded = fromIntegral $ (floor $ utcTimeToPOSIXSeconds t)
  let standardClaims = [("iss", String "http://www.ethan.com/"), ("aud", String "http://www.ethan.com/"), ("iat", Number rounded), ("exp", Number $ (rounded + (60 * 60)))] 
  return $ HM.fromList standardClaims

issueToken :: AuthEnv -> AuthUser -> IO (Either String L.ByteString)
issueToken env authUser = do
  standardClaims <- mkStandardClaims
  -- let claimsWithEmail = HM.insert "email" (String $ userEmail $ auth_user_email authUser) standardClaims
  let claimsWithEmail = HM.insert "email" (String $ auth_user_email authUser) standardClaims
  -- let claimsWithSub = HM.insert "sub" (Number $ fromIntegral $ userID $ auth_user_id authUser) claimsWithEmail
  let claimsWithSub = HM.insert "sub" (Number $ fromIntegral $ auth_user_id authUser) claimsWithEmail
  -- let sc = fromMaybe emptyClaimsSet (decode $ encode claimsWithSub) 
  let sc = (eitherDecode $ encode claimsWithSub) :: Either String ClaimsSet
  case sc of
   Left e -> return $ Left e  
   Right s -> do
    signedJWT <- doJwtSign (privateJWK env) s
    case signedJWT of
     Left e -> do
      return $ Left "Unable to sign JWT"
     Right js -> do
      return $ Right (encodeCompact js)

doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doJwtSign jwk claims = runExceptT $ do
  alg <- bestJWSAlg jwk
  signClaims jwk (newJWSHeader ((), alg)) claims
```
Ok so you might be looking at this and think wow that doesn't seem to make much sense why would we `decode` something immediately after we `encode` it. The reason we are doing this is because we are trying to avoid using the `lens` library for right now. The `jose` library provides a very good api for ensuring correct JWT claimssets are made and we could use it without utilizing `lens` with the function `addClaim :: Text -> Value -> ClaimsSet -> ClaimsSet` but for now let's just stick with this.

You might have also noticed the `runExceptT` this uses the `ExceptT` monad transformer. We could rewrite this function to not use this but this is the example provided in the docs so let's just go with it. The specific details of how this works are largely unimportant right now.

And lastly you might have noticed we use the `encodeCompact` function. We use this to put our JWS into the compact serialization form that is often used for passing around JWT's

Now let's create our route that will utilize this function.

*src/Domain/User/Service.hs*
```haskell
module Domain.User.Service 
 ( signupUser
 , getUser
 , loginUser
 ) where

import           Domain.User.PG as PG
import           Domain.Types.UserSignup
import           Domain.Types.User
import           Domain.Types.CommonJSON
import           Domain.Types.UserSignin
import           Domain.Types.AppEnv
import           Domain.Types.IssueToken
import           Web.Scotty
import           Data.Aeson hiding (json)
import           Network.HTTP.Types.Status
import           Database.PostgreSQL.Simple
import           Data.Pool
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Domain.Auth.Auth as Auth
import qualified Data.ByteString.Lazy as L

...

loginUser :: AppEnv -> ActionM ()
loginUser env = do
  b <- body
  let j = (eitherDecode b):: Either String UserSignin
  case j of
   Left e -> do
    status status400
    json $ JSONError e
   Right s -> do
    usr <- liftAndCatchIO $ PG.userLogin (pgEnv env) s
    case (usr) of 
     Left e -> do
      status status400
      json $ JSONError e
     Right u -> do
      tok <- liftAndCatchIO $ Auth.issueToken (authEnv env) u
      case tok of
        Left err -> json $ JSONError err
        Right t -> json $ IssueToken t
```

and let's make the `UserSignin` datatype and the appropriate database function to return our AuthUser.

*src/Domain/Types/UserSignin.hs*
```haskell
module Domain.Types.UserSignin where

import qualified Data.Text as T
import           Data.Aeson

data UserSignin = UserSignin
 { email :: T.Text
 , password :: T.Text
 } deriving (Eq, Show)

instance FromJSON UserSignin where
  parseJSON (Object v) = do
   e <- v .: "email"
   p <- v .: "password"
   return $ UserSignin e p
  parseJSON _ = fail $ "Expected the UserSignup to be an object"
```

Nothing new here.


*src/Domain/User/PG.hs*
```haskell
module Domain.User.PG 
 ( addUserToDB
 , getUserByID
 , userLogin
 ) where

...

import           Domain.Types.UserSignin as US
import           Domain.Types.AuthUser

...

userLogin :: (Pool Connection) -> UserSignin -> IO (Either String AuthUser)
userLogin pool usr = do
  val <- PG.withPool pool (\c -> query c statement (US.email usr, US.password usr)) :: IO (Either SqlError [AuthUser])
  case val of
   Left e -> return $ Left "Unexpected Server Error. Please try again later."
   Right s -> do
    print $ s
    case (length s) of
     1 -> return $ Right $ head s 
     _ -> return $ Left "That user does not exist."
  where statement = "SELECT user_id, cast (email as text) FROM users WHERE email = ? AND password = crypt(?, password) LIMIT 1"
```

And last but not least let's make our type to return our token in.


*src/Domain/Types/IssueToken.hs*
```haskell
module Domain.Types.IssueToken (IssueToken(..)) where

import qualified Data.ByteString.Lazy as L
import           Data.Aeson
import qualified Data.Text.Lazy.Encoding as TLE

newtype IssueToken = IssueToken { token :: L.ByteString }

instance ToJSON IssueToken where
  toJSON u = object 
   [ "token" .= (TLE.decodeUtf8 (token u))
   ]
```

and now let's test out our application with some of the data that we already created i am going to send the following data:

```json
{
  "email": "ethangardner11@ethan.com",
  "password": "Holamundo@1"
}
```

and in response i get:

```json
{
    "token": "eyJhbGciOiJQUzUxMiJ9.e30.NqLk6dmuRPuXrLMj83ImiyvSTj2QxmGhBijyuG1xefZzzLp1WFWBhKQl8QOru7DaFdzKwRhC78mSqpuRETRCy3d9t0VZtSYrRH200UMh3YAkLFc3UQMMSINTvsOlksWYfcZgnep2IOqG2KWGh4aNyzu-FnrEBJpfdUQJMKQFDrOM-qOuLJO78S-pBg_BHVBILR0cbQdQA7fftXaXa8HwAnKbLPzgwhzfS1HjllBOD-0K_hbSUaAzfO4rfddV9Fa48Jm6dYChisQZ5d0I3UTKvI2BUomWUsFTBfbGquHGv49fDGiewuf8zmJzJWzRqQLOXLloNL66_3JVNWfV1x1MYBQvQ96e0UoG6sMeaFfX4MXQFHLLabj_VW5pb4Vd0xEHhD64IZjHFBa-bQvFiDvsOMBBVvSrE3fKvdu7JBgr4Re9LIhCGb07CJOXQeh58VAJKWtu7bhH7PIipHWBbeF78ehflCLa_TFx2kMayyP4B2oQR9mPIq2H1kxR7VicvC7a8duvUX3I3Be4AO0HX1fwF2cr3rQ5Pa98J_n3VRygLNoL1yrlmdLEKbt-H5F77DGwfxEEbKn-47nbHoX3T2JBV89TWrxfpOOIrREUSffWWMSYIlA5R5Nzh8az_GvFcl1-XjTsi5eCbcK-f3HANboGLqDPXln0RHayCX5fAfCxmvA"
}
```

Great looks like everything is working fine. Now let's write the function that will verify the claims in our token and tell us if the user should be allowed to hit the rest endpoints that they are attempting to access.