{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}


import Prelude hiding (read)
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Monadic  (assert, monadicIO, pick, pre, run)
import qualified Network.Wreq as W
import Control.Monad
import Control.Applicative
import Control.Lens hiding (elements)
import Data.Aeson hiding (Result)
import GHC.Generics

import Control.Monad.State

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

data JsonResponse a = JsonResponse a
                    | JsonError {error :: String} 
                    | DecodingError
                    deriving (Show)

type WreqResponse a = W.Response (JsonResponse a)

wreqCreate requestData = W.asJSON =<< W.post apiEndPoint (toJSON requestData)
wreqRead id = W.asJSON =<< W.get (apiEndPoint ++ "/" ++ id)
--wreqList = asJSON =<< get apiEndPoint
--wreqUpdate resource id = asJSON =<< put (apiEndPoint ++ "/" ++ id) (toJSON resource)
--wreqDel resource id = asJSON =<< delete (apiEndPoint ++ "/" ++ id)

instance (FromJSON a) => FromJSON (JsonResponse a) where
  parseJSON obj@(Object v) =
    JsonResponse <$> parseJSON obj
    <|> JsonError <$> v .: "error"
    <|> pure DecodingError 
  parseJSON _ = mzero

data Varied a = Valid a
              | Invalid a deriving Show

getVaried :: Varied a -> a
getVaried (Valid a) = a
getVaried (Invalid a) = a

class AppInterface a where
  type Result :: *
  operate :: Varied a -> IO Result
  validate :: Varied a -> Result -> Bool

instance AppInterface CreateUser where
  type Result = WreqResponse User
  operate userRequest = wreqCreate $ getVaried userRequest
  validate _ _ = True

instance AppInterface GetUser where
  type Result = WreqResponse User
  operate id = wreqRead $ getUserId $ getVaried id
  validate _ _ = True

data CreateUser = CreateUser { createUserName :: Name
                             , createUserEmail :: Email
                             , createUserPass :: Password
                             } deriving (Show, Generic)

data GetUser = GetUser { getUserId :: String } deriving (Show)

data Operation a = forall t. (Show t, AppInterface t) => Operation (Varied t)

data AppTest = forall t. AppTest (Operation (Varied t))

instance Show AppTest where
  show (AppTest (Operation a)) = show $ getVaried a

instance Arbitrary AppTest where
  arbitrary = fmap AppTest $ frequency [ (1, fmap Operation $ frequency [ (1, fmap Valid (valid::(Gen CreateUser)))
                                                                        , (1, fmap Invalid (invalid::(Gen CreateUser))) ])
                                       , (1, fmap Operation $ frequency [ (1, fmap Valid (valid::(Gen GetUser)))
                                                                        , (1, fmap Invalid (invalid::(Gen GetUser))) ])
                                       ]

instance FromJSON CreateUser
instance ToJSON CreateUser

data User = User { userId :: String
                 , userName :: Name
                 , userEmail :: Email
                 } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

--           | UpdateUser { userId :: String
--                        , userName :: Name
--                        , userEmail :: Email
--                        , oldPass :: Password
--                        , newPass :: Password
--                        }
--           | DeleteUser { userName :: Name }
--           | GetUser { userName :: Name } 
--           deriving (Show, Generic)

data JsonMsg = JsonMsg { msg :: String } deriving (Show, Generic)

instance FromJSON JsonMsg
instance ToJSON JsonMsg

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

instance Variant CreateUser where
  valid = liftM3 CreateUser valid valid valid
  invalid = liftM3 CreateUser invalid invalid invalid

instance Variant GetUser where
  valid = fmap GetUser $ elements ["foo"]
  invalid = fmap GetUser $ elements ["bar"]


-- data CrudOperation a = Create (a -> IO (Response (Resp a)))
--                      | Read (String -> IO (Response (Resp a)))
--                      | List (IO (Response [Resp a]))
--                      | Update (a -> String -> IO (Response (Resp a)))
--                      | Delete (a -> String -> IO (Response (Resp JsonMsg)))

-- instance Show (CrudOperation a) where
--   show (Create _) = "a -> IO (Response (Resp a))"
--   show (Read _) = "String -> IO (Response (Resp a))"
--   show (List _) = "IO (Response [Resp a])"
--   show (Update _) = "a -> String -> IO (Response (Resp a))"
--   show (Delete _) = "a -> String -> IO (Response (Resp JsonMsg))"

-- instance (CRUD a) => Arbitrary (CrudOperation a) where
--   arbitrary = frequency [(1, elements [Create create])
--                         ,(1, elements [Read read])
--                         ,(1, elements [List list])
--                         ,(1, elements [Update update])
--                         ,(1, elements [Delete del])
--                         ]

-- instance (Variant a, CRUD a) => Arbitrary (ResourceTester a) where
--   arbitrary = do
--     model <- liftM Valid $ frequency [(1, valid), (1, invalid)]
--     op <- arbitrary
--     return $ ResourceTester model op

-- class (FromJSON a, ToJSON a) => CRUD a where
--   create :: a -> IO (Response (Resp a))
--   read :: a -> IO (Response (Resp a))
--   list :: IO (Response [Resp a])
--   update :: a -> String -> IO (Response (Resp a))
--   del :: a -> String -> IO (Response (Resp JsonMsg))

-- instance (FromJSON a, ToJSON a) => CRUD a where
--   create resource = asJSON =<< post apiEndPoint (toJSON resource)
--   read id = asJSON =<< get (apiEndPoint ++ "/" ++ id)
--   list = asJSON =<< get apiEndPoint
--   update resource id = asJSON =<< put (apiEndPoint ++ "/" ++ id) (toJSON resource)
--   del resource id = asJSON =<< delete (apiEndPoint ++ "/" ++ id)


prop_test_app :: AppTest -> Property
prop_test_app (AppTest (Operation a)) = 
  monadicIO $ do
    res <- run $ operate a
    assert $ validate a res

users :: State [String] ()
users = put ["foo", "bar", "baz"]

type UsedState = [String]

saveState :: String -> State UsedState ()
saveState x = do
  a <- get
  put (x:a)

genFromState :: State UsedState (Gen String)
genFromState = do
  xs <- get
  return $ elements xs

-- Properties
--main :: IO()
main = do
  --quickCheck prop_test_app
  --sample (arbitrary :: Gen AppTest)
  --print $ runState users ("foo", ["bar"])
  --let a = fst $ runState genFromState []
  --sample a
  putStrLn "FOOBAR"
