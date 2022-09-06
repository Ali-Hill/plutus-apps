{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

--  This module contains a contract model for positive testing of the
--  simplified escrow contract in Plutus.Contracts.Tutorial.Escrow,
--  with generated escrow targets. See the "Parameterising Models and
--  Dynamic Contract Instances" section of the tutorial.

module Spec.Tutorial.Escrow3(prop_Escrow, prop_FinishEscrow, prop_NoLockedFunds, prop_FixedTargets, EscrowModel) where

import Control.Lens hiding (both, elements)
import Control.Monad (void, when)
import Data.Data
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (Datum, minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Value
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel

import Plutus.Contracts.Tutorial.Escrow hiding (Action (..))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck

data EscrowModel = EscrowModel { _contributions :: Map Wallet Value
                               , _targets       :: Map Wallet Value
                               , _phase         :: Phase
                               } deriving (Eq, Show, Data)

data Phase = Initial | Running deriving (Eq, Show, Data)

makeLenses ''EscrowModel

deriving instance Eq (ContractInstanceKey EscrowModel w s e params)
deriving instance Show (ContractInstanceKey EscrowModel w s e params)

instance ContractModel EscrowModel where
  data Action EscrowModel = Init [(Wallet, Integer)]
                          | Redeem Wallet
                          | Pay Wallet Integer
                          | Refund Wallet
    deriving (Eq, Show, Data)

  data ContractInstanceKey EscrowModel w s e params where
    WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError (EscrowParams Datum)

  initialState = EscrowModel { _contributions = Map.empty
                             , _targets       = Map.empty
                             , _phase         = Initial
                             }

  initialInstances = []

  startInstances _ (Init wns) =
    [StartContract (WalletKey w) (escrowParams wns) | w <- testWallets]
  startInstances _ _ = []

  instanceWallet (WalletKey w) = w

  instanceContract _ WalletKey{} params = testContract params

  nextState a = case a of
    Init wns -> do
      phase   .= Running
      targets .= Map.fromList [(w, Ada.adaValueOf (fromInteger n)) | (w,n) <- wns]
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      let leftoverValue = fold contribs <> inv (fold targets)
      deposit w leftoverValue
      contributions .= Map.empty
      wait 1
    Refund w -> do
      v <- viewContractState $ contributions . at w . to fold
      contributions %= Map.delete w
      deposit w v
      wait 1

  precondition s a = case a of
    Init tgts   -> currentPhase == Initial
                && and [Ada.adaValueOf (fromInteger n) `geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
    Redeem _    -> currentPhase == Running
                && fold (s ^. contractState . contributions) `geq` fold (s ^. contractState . targets)
    Pay _ v     -> currentPhase == Running
                && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut
    Refund w    -> currentPhase == Running
                && w `Map.member` (s ^. contractState . contributions)
    where currentPhase = s ^. contractState . phase 

{- 

below gives 0 redeem rejected and 4% redeem actions which is better

still fails. this is because to make the targets it needs to give a value less than the minimum ada. Either this fails by precondition or if you remove it then balancing fails. 

Prelude Test.QuickCheck Spec.Tutorial.Escrow3 Main> quickCheck prop_FinishEscrow
*** Failed! Falsified (after 11 tests and 1 shrink):                            
BadPrecondition
  [Do $ Init [(Wallet 2,17)], 
   Do $ Pay (Wallet 1) 9, 
   Do $ Pay (Wallet 1) 2, 
   Do $ Pay (Wallet 4) 5]
  [Action (Pay (Wallet 1) 2)]
  (EscrowModel {_contributions = fromList [(Wallet 4,Value 
              (Map [(,Map [("",5000000)])])),
              (Wallet 1,Value (Map [(,Map [("",11000000)])]))], 
              _targets = fromList [(Wallet 2,Value (Map [(,Map [("",17000000)])]))], 
              _phase = Running})

BadPrecondition
[Do $ Var 0 := Init [(Wallet 2,17)],Do $ Var 1 := Pay (Wallet 1) 9,Do $ Var 3 := Pay (Wallet 1) 2,Do $ Var 4 := Pay (Wallet 4) 5]
Some (Pay (Wallet 1) 2)
Some (Pay (Wallet 1) 2)

  precondition s a = case a of
    Init tgts   -> currentPhase == Initial
                && and [Ada.adaValueOf (fromInteger n) `geq` Ada.toValue minAdaTxOut | (_,n) <- tgts]
    Redeem _    -> currentPhase == Running
                && fold (s ^. contractState . contributions) == fold (s ^. contractState . targets)
    Pay _ v     -> currentPhase == Running
                && Ada.adaValueOf (fromInteger v) `geq` Ada.toValue minAdaTxOut
                && (fold (s ^. contractState . contributions) <> Ada.adaValueOf (fromInteger v)) `leq` fold (s ^. contractState . targets)
    Refund w    -> currentPhase == Running
                && w `Map.member` (s ^. contractState . contributions)
    where currentPhase = s ^. contractState . phase
-}

  perform h _ _ a = case a of
    Init _         -> do
      return ()
    Pay w v        -> do
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.adaValueOf $ fromInteger v)
      delay 1
    Redeem w       -> do
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      delay 1
    Refund w       -> do
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      delay 1

  arbitraryAction s
    | s ^.contractState . phase == Initial
      = Init <$> arbitraryTargets
    | otherwise
      = frequency $ [ (3, Pay <$> elements testWallets <*> choose (1, 30)) ] ++
                    [ (1, Redeem <$> elements testWallets)
                    | (s ^. contractState . contributions . to fold) `geq` (s ^. contractState . targets . to fold)
                    ]  ++
                    [ (1, Refund <$> elements testWallets) ]


  shrinkAction _ (Init tgts) = map Init (shrinkList (\(w,n)->(w,)<$>shrink n) tgts)
  shrinkAction _ (Pay w n)   = [Pay w n' | n' <- shrink n]
  shrinkAction _ _           = []

arbitraryTargets :: Gen [(Wallet,Integer)]
arbitraryTargets = do
  ws <- sublistOf testWallets
  vs <- infiniteListOf $ choose (1,30)
  return $ zip ws vs

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

testContract :: EscrowParams Datum -> Contract () EscrowSchema EscrowError ()
testContract params = selectList [ void $ payEp params
                                 , void $ redeemEp params
                                 , void $ refundEp params
                                 ] >> testContract params


prop_Escrow :: Actions EscrowModel -> Property
prop_Escrow = propRunActions_


escrowParams :: [(Wallet, Integer)] -> EscrowParams d
escrowParams tgts =
  EscrowParams
    { escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w) (Ada.adaValueOf (fromInteger n))
        | (w,n) <- tgts
        ]
    }

-- This is the first--bad--approach to recovering locked funds.
{-finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy w1
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: Wallet -> DL EscrowModel ()
finishingStrategy w = do
    currentPhase <- viewContractState phase
    when (currentPhase /= Initial) $ do
      currentTargets  <- viewContractState targets
      currentContribs <- viewContractState contributions
      let deficit = fold currentTargets <> inv (fold currentContribs)
      when (deficit `gt` Ada.adaValueOf 0) $
        action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
      action $ Redeem w

-- This unilateral strategy fails.
noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy w1
  , nlfpWalletStrategy = finishingStrategy    }  -}

-- this is the better strategy based on refunds
finishEscrow :: DL EscrowModel ()
finishEscrow = do
    anyActions_
    finishingStrategy
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: DL EscrowModel ()
finishingStrategy = do
    contribs <- viewContractState contributions
    monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs]



{-
finishingStrategy :: Wallet -> DL EscrowModel ()
finishingStrategy w = do
    currentPhase <- viewContractState phase
    when (currentPhase /= Initial) $ do
      currentTargets  <- viewContractState targets
      currentContribs <- viewContractState contributions
      let deficit = fold currentTargets <> inv (fold currentContribs)
      when (deficit `gt` Ada.adaValueOf 0) $
        action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
      action $ Redeem w
-}



--(targs Map.! w) `gt` deficit
--- 

{-
DLScript
  [Do $ Init [(Wallet 4,5)], 
   Do $ Pay (Wallet 4) 2, 
   Do $ Pay (Wallet 5) 2]

Unilateral strategy for Wallet 4 should have gotten it at least
  SymValue {symValMap = fromList [], actualValPart = Value (Map [])}
but it got
  SymValue {symValMap = fromList [], actualValPart = Value (Map [(,Map [("",-4000000)])])}
Prelude Test.QuickCheck Spec.Tutorial.Escrow3 Main> 


walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ do
      when (currentPhase /= Initial) $ do
        let deficit = fold targs <> inv (fold contribs)
        when (w `Map.member` targs) $ do
          if (deficit `gt` Ada.adaValueOf 0)
            then 
              if ((targs Map.! w) `gt` ((contribs Map.! w) <> deficit <> Ada.adaValueOf 1))
                then action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
                else action $ Refund w
          else 
            if ((targs Map.! w) `geq` (contribs Map.! w))
              then action $ Redeem w
              else action $ Refund w
    when (Prelude.not (w `Map.member` targs)) $ action $ Refund w


-}

{-
*** Failed! Falsified (after 48 tests and 23 shrinks):     
DLScript
  [Do $ Init [(Wallet 3,4),(Wallet 4,29)], 
   Do $ Pay (Wallet 3) 2, 
   Do $ Pay (Wallet 4) 4, 
   Do $ Pay (Wallet 4) 26]

Unilateral strategy for Wallet 3 should have gotten it at least
  SymValue {symValMap = fromList [], actualValPart = Value (Map [])}
but it got
  SymValue {symValMap = fromList [], actualValPart = Value (Map [(,Map [("",-4000000)])])}
Prelude Test.QuickCheck Spec.Tutorial.Escrow3 Main> 

-- its not the individual contributions that we need
-}

-- with the stategy i didnt understan the error some reddem some refund xcr

{-
Actions (17559 in total):
66.143% Pay
10.957% WaitUntil
10.428% Refund
 9.351% Redeem
 1.845% Init
 1.276% Unilateral

Refunded wallets (100 in total):
24% 3
23% 2
21% 1
15% 4
12% 0
 5% 5

Wait interval (1924 in total):
40.59% <10
33.37% 10-19
21.62% 20-29
 4.42% 30-39

Wait until (1924 in total):
25.57% 100-199
10.03% 20-29
 9.88% 200-299
 9.56% 10-19
 7.12% 30-39
 6.81% <10
 6.60% 60-69
 5.82% 40-49
 5.09% 80-89
 4.31% 70-79
 4.21% 50-59
 3.74% 90-99
 1.25% 300-399
-}
-- this works
-- monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])


walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ do
      when (currentPhase /= Initial) $ do
        let deficit = fold targs <> inv (fold contribs)
        when (w `Map.member` targs) $ do
          if (deficit `gt` Ada.adaValueOf 0)
            then 
              if ((targs Map.! w) `gt` ((contribs Map.! w) <> deficit))
                then 
                  do 
                    action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit 
                    action $ Redeem w
                else action $ Refund w
          else 
            if ((targs Map.! w) `gt` (contribs Map.! w))
              then action $ Redeem w
              else action $ Refund w
    when (Prelude.not (w `Map.member` targs)) $ action $ Refund w

{-
walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ do
      when (currentPhase /= Initial) $ do
        let deficit = fold targs <> inv (fold contribs)
        when (w `Map.member` targs) $ do
          if (deficit `gt` Ada.adaValueOf 0)
            then 
              if ((targs Map.! w) `gt` ((contribs Map.! w) <> deficit <> Ada.adaValueOf 1))
                then 
                  do 
                    action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit 
                    action $ Redeem w
                else action $ Refund w
          else 
            if ((targs Map.! w) `gt` (contribs Map.! w))
              then action $ Redeem w
              else action $ Refund w
    when (Prelude.not (w `Map.member` targs)) $ action $ Refund w
-}

{-
-- this still fails when I pay in a number greater than I can redeem. 
-- If 4 is paid in but then I only get to redeem 2 then I am still down 2. 
walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ do
      when (currentPhase /= Initial) $ do
        let deficit = fold targs <> inv (fold contribs)
        when (w `Map.member` targs) $ do
          if (deficit `gt` Ada.adaValueOf 0)
            then 
              if ((targs Map.! w) `gt` ((contribs Map.! w) <> deficit))
                then action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
                else action $ Refund w
            else action $ Redeem w 
    when (Prelude.not (w `Map.member` targs)) $ action $ Refund w-}

{-
walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ do
      when (currentPhase /= Initial) $ do
        let deficit = fold targs <> inv (fold contribs)
        when (w `Map.member` targs) $ do
          if (deficit `gt` Ada.adaValueOf 0)
            then 
              if ((targs Map.! w) `gt` deficit)
                then action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
                else action $ Refund w
            else action $ Redeem w
    when (Prelude.not (w `Map.member` targs)) $ action $ Refund w
-}

{-
walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ do
      when (currentPhase /= Initial) $ do
        let deficit = fold targs <> inv (fold contribs)
        when (w `Map.member` targs) $ do
          when (deficit `gt` Ada.adaValueOf 0) $
            action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
          action $ Redeem w
    when (Prelude.not (w `Map.member` targs)) $ action $ Refund w
-}

{-
walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    currentPhase <- viewContractState phase
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (currentPhase /= Initial) $ do
      let deficit = fold targs <> inv (fold contribs)
      when (w `Map.member` targs) $ do
        when (deficit `gt` Ada.adaValueOf 0) $
          action $ Pay w $ round $ Ada.getAda $ max minAdaTxOut $ Ada.fromValue deficit
        action $ Redeem w
    when (w `Map.member` contribs && Prelude.not (w `Map.member` targs)) $ action $ Refund w
-}


    --when (w `Map.member` contribs) $ action $ Refund w


--action $ Refund w


{-walletStrategy :: Wallet -> DL EscrowModel ()
walletStrategy w = do
    contribs <- viewContractState contributions
    targs <- viewContractState targets
    when (w `Map.member` contribs) $ action $ Refund w-}


noLockProof :: NoLockedFundsProof EscrowModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy
  , nlfpWalletStrategy = walletStrategy    }

prop_FinishEscrow :: Property
prop_FinishEscrow = forAllDL finishEscrow prop_Escrow

prop_NoLockedFunds :: Property
prop_NoLockedFunds = checkNoLockedFundsProof noLockProof

fixedTargets :: DL EscrowModel ()
fixedTargets = do
  action $ Init [(w1,10),(w2,20)]
  anyActions_

prop_FixedTargets :: Property
prop_FixedTargets = forAllDL fixedTargets prop_Escrow
