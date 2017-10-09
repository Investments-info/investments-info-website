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

data Fixtures = Fixtures
  { userF :: UserFixtures
  , adminF :: AdminFixtures
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

insertFixtures :: DB Fixtures
insertFixtures = do
  allUsersF <- makeAccounts
  let sasa = unsafeIdx allUsersF 0
      vidas = unsafeIdx allUsersF 1
  allAdminsF <- makeAdmins [entityKey sasa, entityKey vidas]
  let userF = UserFixtures {..}
      adminF = AdminFixtures {..}
  return Fixtures {..}
