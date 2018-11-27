{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Helper.Fixtures where

import           Helper.Helper (getAdmins)
import           Import

newtype UserFixtures = UserFixtures
  { allUsersF :: [Entity User]
  } deriving (Eq, Show)

newtype AdminFixtures = AdminFixtures
  { allAdminsF :: [Entity Admin]
  } deriving (Eq, Show)

newtype CompanyFixtures = CompanyFixtures
  { allCompaniesF :: [Entity Company]
  } deriving (Eq, Show)

data Fixtures = Fixtures
  { userF  :: UserFixtures
  , adminF :: AdminFixtures
  -- , companyF :: CompanyFixtures
  } deriving (Eq, Show)

makeAccount :: Text -> Text -> DB (Entity User)
makeAccount = createUser

makeAccounts :: DB [Entity User]
makeAccounts = do
  admins <- getAdmins
  case admins of
    Nothing -> return []
    Just a -> traverse (\x -> makeAccount (aemail x) (apassword x)) a

makeAdmin :: Key User -> DB (Maybe (Entity Admin))
makeAdmin k = do
  a <- createAdmin k
  case a of
    Left admin -> pure $ Just admin
    Right _ -> pure Nothing

makeAdmins :: [Key User] -> DB [Entity Admin]
makeAdmins k = do
  admins <- traverse makeAdmin k
  pure $ catMaybes admins

{-# INLINABLE unsafeIdx #-}
unsafeIdx :: (MonoFoldable c) => c -> Integer -> Element c
unsafeIdx xs n
  | n < 0     = error "negative index"
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (error ("index too large: " ++ show n)) xs n

makeCompany :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> DB (Entity Company)
makeCompany = createCompany

makeCompanies :: DB [Entity Company]
makeCompanies =
  sequenceA
    [
        makeCompany "Agilent Technologies" "http://agilent.com" "" "" "Agilent Technologies is an American public research, development and manufacturing company established in 1999 as a spin-off from Hewlett-Packard. The resulting IPO of Agilent stock was the largest in the history of Silicon Valley at the time." "https://upload.wikimedia.org/wikipedia/en/thumb/1/14/Agilent.svg/440px-Agilent.svg.png" "A",
        makeCompany "Alcoa Corporation" "http://alcoa.com" "" "" "Alcoa Corporation is an American industrial corporation. It is the world's fifth largest producer of aluminum, with corporate headquarters in Pittsburgh, Pennsylvania. Alcoa conducts operations in 10 countries." "https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Alcoa_logo_2016.png/300px-Alcoa_logo_2016.png" "AA",
        makeCompany "Aac Holdings" "www.americanaddictioncenters.com" "" "" "AAC Holdings, Inc. provides inpatient substance abuse treatment services for individuals with drug and alcohol addiction in the United States. Its therapy services include motivational interviewing, cognitive behavioral therapy, rational emotive behavior therapy, dialectical behavioral therapy, solution-focused therapy, eye movement desensitization and reprocessing, and systematic family intervention services. As of December 31, 2016, the company operated 12 residential substance abuse treatment facilities, 18 standalone outpatient centers, and 202 sober living beds. It also offers Internet marketing services to families and individuals, who are struggling with addiction and seeking treatment options through online directories of treatment providers, treatment provider reviews, forums, and professional communities; and online marketing solutions to other treatment providers, such as facility profiles, audience targeting, lead generation, and tools for digital reputation management. In addition, it performs drug testing and diagnostic laboratory services; and provides physician services to its clients. AAC Holdings, Inc. was incorporated in 2014 and is headquartered in Brentwood, Tennessee." "https://americanaddictioncenters.org/wp-content/themes/AAC/pub/images/300xNxaac_horiz_white.png.pagespeed.ic.8yl6KxliXV.png" "AAC"
    ]

runInsertAdminsAction :: IO ()
runInsertAdminsAction = do
    _ <- runDBA insertFixtures
    return ()

insertFixtures :: DB Fixtures
insertFixtures = do
  allUsersF <- makeAccounts
  -- allCompaniesF <- makeCompanies
  let sasa = unsafeIdx allUsersF 0
      vidas = unsafeIdx allUsersF 1
  allAdminsF <- makeAdmins [entityKey sasa, entityKey vidas]
  let userF = UserFixtures {..}
      adminF = AdminFixtures {..}
      -- companyF = CompanyFixtures {..}
  return Fixtures {..}
