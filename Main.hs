{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Prelude hiding (read)
import Test.QuickCheck
import Test.QuickCheck.Monadic  (assert, monadicIO, pick, pre, run)
import Network.Wreq
import Control.Monad
import Control.Applicative
import Control.Lens hiding (elements)
import Data.Aeson
import GHC.Generics

apiEndPoint = "http://localhost:3000/api/users"

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
  parseJSON _ = mzero
              
data User = User { userName :: Name
                 , userEmail :: Email
                 , userPass :: Password
                 } deriving (Show, Generic)

data JsonMsg = JsonMsg { msg :: String } deriving (Show, Generic)

instance FromJSON JsonMsg
instance ToJSON JsonMsg

instance FromJSON User
instance ToJSON User

-- Generators

validName = fmap Name $ elements ["name1", "name2", "name3"]
invalidName = fmap Name $ elements ["invalidName1", "invalidName2", "invalidName3"]

validEmail = fmap Email $ elements ["email1", "email2", "email3"]
invalidEmail = fmap Email $ elements ["invalidEmail1", "invalidEmail2", "invalidEmail3"]

validPass = fmap Password $ elements ["pass1", "pass2", "pass3"]
invalidPass = fmap Password $ elements ["invalidPass1", "invalidPass2", "invalidPass3"]

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

data CrudOperation a = Create (a -> IO (Response (Resp a)))
                     | Read (String -> IO (Response (Resp a)))
                     | List (IO (Response [Resp a]))
                     | Update (a -> String -> IO (Response (Resp a)))
                     | Delete (a -> String -> IO (Response (Resp JsonMsg)))

instance Show (CrudOperation a) where
  show (Create _) = "a -> IO (Response (Resp a))"
  show (Read _) = "String -> IO (Response (Resp a))"
  show (List _) = "IO (Response [Resp a])"
  show (Update _) = "a -> String -> IO (Response (Resp a))"
  show (Delete _) = "a -> String -> IO (Response (Resp JsonMsg))"

instance (Resource a) => Arbitrary (CrudOperation a) where
  arbitrary = frequency [(1, elements [Create create])
                        ,(1, elements [Read read])
                        ,(1, elements [List list])
                        ,(1, elements [Update update])
                        ,(1, elements [Delete del])
                        ]

data Varied a = Valid a
              | Invalid a deriving Show

data ResourceTester a = ResourceTester { testModel :: Varied a
                                       , operation :: CrudOperation a
                                       } deriving Show

instance (Variant a, Resource a) => Arbitrary (ResourceTester a) where
  arbitrary = do
    model <- liftM Valid $ frequency [(1, valid), (1, invalid)]
    op <- arbitrary
    return $ ResourceTester model op

class Resource a where
  create :: a -> IO (Response (Resp a))
  read :: String -> IO (Response (Resp a))
  list :: IO (Response [Resp a])
  update :: a -> String -> IO (Response (Resp a))
  del :: a -> String -> IO (Response (Resp JsonMsg))

instance Resource User where
  create user = asJSON =<< post apiEndPoint (toJSON user)
  read id = asJSON =<< get (apiEndPoint ++ "/" ++ id)
  list = asJSON =<< get apiEndPoint
  update user id = asJSON =<< put (apiEndPoint ++ "/" ++ id) (toJSON user)
  del user id = asJSON =<< delete (apiEndPoint ++ "/" ++ id)

getInner :: Varied a -> a
getInner (Valid a) = a 
getInner (Invalid a) = a

prop_tester_user :: ResourceTester User -> Property
prop_tester_user (ResourceTester model (Create op)) =
  monadicIO $ do
    res <- run $ op $ getInner model
    assert True
prop_tester_user (ResourceTester model (Read op)) =
  monadicIO $ do
    res <- run $ op "foo"
    assert True
prop_tester_user (ResourceTester model (List op)) =
  monadicIO $ do
    res <- run op 
    assert True
prop_tester_user (ResourceTester model (Update op)) =
  monadicIO $ do
    res <- run $ op (getInner model) "foo"
    assert True
prop_tester_user (ResourceTester model (Delete op)) =
  monadicIO $ do
    res <- run $ op (getInner model) "foo"
    assert True

  -- Properties
main :: IO()
--main = quickCheck prop_tester_user
--main = sample (arbitrary :: Gen (ResourceTester User))
main = putStrLn "FOO"
