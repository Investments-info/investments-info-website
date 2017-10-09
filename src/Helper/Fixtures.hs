{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Helper.Fixtures where

import Import

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
  { userF :: UserFixtures
  , adminF :: AdminFixtures
  , companyF :: CompanyFixtures
  } deriving (Eq, Show)

sasaEmail, sasaPassword :: Text
sasaEmail = "brutallesale@gmail.com"

sasaPassword = "sasa"

vidasEmail, vidasPassword :: Text
vidasEmail = "vpleta@gmx.ch"

vidasPassword = "vidas"

makeAccount :: Text -> Text -> DB (Entity User)
makeAccount email pass = do
  userEnt <- createUser email pass
  return userEnt

makeAccounts :: DB [Entity User]
makeAccounts =
  sequenceA
    [makeAccount sasaEmail sasaPassword, makeAccount vidasEmail vidasPassword]

makeAdmin :: Key User -> DB (Entity Admin)
makeAdmin = createAdmin

makeAdmins :: [Key User] -> DB [Entity Admin]
makeAdmins = traverse makeAdmin

{-# INLINABLE unsafeIdx #-}
unsafeIdx :: (MonoFoldable c) => c -> Integer -> Element c
unsafeIdx xs n
  | n < 0     = error "negative index"
  | otherwise = foldr (\x r k -> case k of
                                   0 -> x
                                   _ -> r (k-1)) (error ("index too large: " ++ show n)) xs n


-- [
-- ("A","Agilent Technologies"),
-- ("AA","Alcoa Corporation"),
-- ("AAC","Aac Holdings"),
-- ("AAN","Aaron's Inc"),
-- ("AAP","Advanced Auto Parts Inc")
-- ]

makeCompany :: Text -> Text -> Text -> Text -> Text -> DB (Entity Company)
makeCompany title website description image ticker = do
  compEnt <- createCompany title website description image ticker
  return compEnt

makeCompanies :: DB [Entity Company]
makeCompanies =
  sequenceA
    [
        makeCompany "Agilent Technologies" "http://agilent.com" "Agilent Technologies is an American public research, development and manufacturing company established in 1999 as a spin-off from Hewlett-Packard. The resulting IPO of Agilent stock was the largest in the history of Silicon Valley at the time." "https://upload.wikimedia.org/wikipedia/en/thumb/1/14/Agilent.svg/440px-Agilent.svg.png" "A",
        makeCompany "Alcoa Corporation" "http://alcoa.com" "Alcoa Corporation is an American industrial corporation. It is the world's fifth largest producer of aluminum, with corporate headquarters in Pittsburgh, Pennsylvania. Alcoa conducts operations in 10 countries." "https://upload.wikimedia.org/wikipedia/en/thumb/5/56/Alcoa_logo_2016.png/300px-Alcoa_logo_2016.png" "AA"
    ]

insertFixtures :: DB Fixtures
insertFixtures = do
  allUsersF <- makeAccounts
  allCompaniesF <- makeCompanies
  let sasa = unsafeIdx allUsersF 0
      vidas = unsafeIdx allUsersF 1
  allAdminsF <- makeAdmins [entityKey sasa, entityKey vidas]
  let userF = UserFixtures {..}
      adminF = AdminFixtures {..}
      companyF = CompanyFixtures {..}
  return Fixtures {..}
