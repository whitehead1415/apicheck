{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Prelude hiding (read)
import Test.QuickCheck
import Network.Wreq
import Control.Monad
import Control.Applicative
import Control.Lens hiding (elements)
import Data.Aeson
import GHC.Generics
import Data.Either

--Models

newtype Name = Name String deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name

newtype Email = Email String deriving (Show, Generic)

instance FromJSON Email
instance ToJSON Email

newtype Password = Password String deriving (Show, Generic)

instance FromJSON Password
instance ToJSON Password

data Resp a = Resp a
            | JsonError {error :: String} 
            | DecodingError
              deriving (Show)

instance (FromJSON a) => FromJSON (Resp a) where
  parseJSON obj@(Object v) =
    Resp <$> parseJSON obj
    <|> JsonError <$> v .: "error"
    <|> pure DecodingError 
              
data User = User { userName :: Name
                 , userEmail :: Email
                 , userPass :: Password
                 } deriving (Show, Generic)

data JsonMsg = JsonMsg { msg :: String } deriving (Show, Generic)

instance FromJSON JsonMsg
instance ToJSON JsonMsg

instance FromJSON User
instance ToJSON User

data Valid a = Valid a deriving (Show)
data Invalid a = Invalid a deriving (Show)

newtype Tester a = Tester a

-- Generators

validName = fmap Name $ elements ["name1", "name2", "name3"]
invalidName = fmap Name $ elements ["invalidName1", "invalidName2", "invalidName3"]

validEmail = fmap Email $ elements ["email1", "email2", "email3"]
invalidEmail = fmap Email $ elements ["invalidEmail1", "invalidEmail2", "invalidEmail3"]

validPass = fmap Password $ elements ["invalidPass1", "invalidPass2", "invalidPass3"]
invalidPass = fmap Password $ elements ["pass1", "pass2", "pass3"]

-- Generating Instances

class Variant a where
  valid :: Gen a
  invalid :: Gen a

instance Variant Name where
  valid = validName
  invalid = invalidName

instance Variant Email where
  valid = validEmail
  invalid = invalidEmail

instance Variant Password where
  valid = validPass
  invalid = invalidPass

instance Variant User where
  valid = liftM3 User valid valid valid
  invalid = liftM3 User invalid invalid invalid

instance (Variant a, CRUD a) => Arbitrary (Tester a) where
  arbitrary = liftM Tester $ frequency [(1, valid)]

--instance (Variant a) => Arbitrary (Valid a) where
  --arbitrary = liftM Valid valid

--instance (Variant a) => Arbitrary (Invalid a) where
  --arbitrary = liftM Invalid invalid

--instance (Variant a) => Arbitrary a where
  --arbitrary = frequency [(8, valid), (2, invalid)]

--instance Arbitrary User where
  --arbitrary = frequency [(8, valid . create), (2, invalid)]

class CRUD a where
  create :: a -> IO (Response (Resp a))
  --read :: Gen -> String -> IO (Response (Resp a))
  --list :: Gen -> IO (Response (Resp [a]))
  --update :: Gen a -> String -> IO (Response (Resp a))
  --del :: Gen -> a -> String -> IO (Response (Resp JsonMsg))

apiEndPoint = "http://localhost:3000/api/users"

instance CRUD User where
  create user = asJSON =<< post apiEndPoint (toJSON user)
  --read id = asJSON =<< get (apiEndPoint ++ id)
  --list = asJSON =<< get apiEndPoint
  --update user id = asJSON =<< put (apiEndPoint ++ id) (toJSON user)
  --del user id = asJSON =<< delete (apiEndPoint ++ id)

-- Properties

main = putStrLn "FOO"
