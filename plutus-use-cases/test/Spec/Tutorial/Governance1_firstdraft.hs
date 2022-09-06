{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
----------------------------

{-# LANGUAGE FlexibleInstances  #-} -- addition 
{-# LANGUAGE GADTs              #-} -- addition
{-# LANGUAGE StandaloneDeriving #-} -- addition
{-# LANGUAGE DeriveDataTypeable #-} -- addition
{-# LANGUAGE TypeFamilies       #-} -- addition
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Spec.Tutorial.Governance1(tests, doVoting, tf, testTree, prop_Gov) where

--import Control.Lens (view)
import Control.Lens hiding (both, elements)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)

import Ledger qualified
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Wallet.Emulator qualified as EM

import Plutus.Contract.Test
import Plutus.Contracts.Governance qualified as Gov
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinByteString, fromBuiltin)

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit qualified as HUnit

----------------------------------------------------------------
import           Data.Default               (Default (..))
import Data.Data
import Plutus.Contract
import Plutus.Contract.Test.ContractModel
import PlutusTx.AssocMap qualified as AssocMap
import Ledger.Ada qualified as Ada
import Test.QuickCheck qualified as QC

{-
Is it possible to do multiple votes with same token by transferring it between wallets. 
-}

data GovernanceModel = GovernanceModel { _state :: (BuiltinByteString, Bool)
                                       , _targets :: AssocMap.Map Ledger.TokenName Bool 
                                      } deriving (Eq, Show, Data)
makeLenses ''GovernanceModel

deriving instance Eq (ContractInstanceKey GovernanceModel w s e params)
deriving instance Show (ContractInstanceKey GovernanceModel w s e params)

instance ContractModel GovernanceModel where
  data Action GovernanceModel = NewLaw Wallet BuiltinByteString
                          | AddVote Wallet Ledger.TokenName Bool
                          | StartProposal Wallet BuiltinByteString TokenName Slot
    deriving (Eq, Show, Data)

  data ContractInstanceKey GovernanceModel w s e params where
      GovH  :: Wallet -> ContractInstanceKey GovernanceModel () Gov.Schema Gov.GovError ()
      ProposalH :: Wallet -> ContractInstanceKey GovernanceModel () Gov.Schema Gov.GovError Proposal

  --start a contract instance in each of the test wallets (with contract parameter ()),
  initialInstances = [StartContract (GovH w) () | w <- testWallets]

  --tells the framework which wallet each contract key should run in,
  instanceWallet (ProposalH w) = w
  instanceWallet (GovH w) = w

  -- tells the framework which contract to run for each key
  instanceContract _ GovH{} _ = Gov.contract @Gov.GovError params
  instanceContract _ ProposalH{} p = Gov.proposal @Gov.GovError params p

  --startInstances to start contract
  -- first thing is init action

  --startInstances _ StartProposal w bs t slot  =   
  --startInstances _ _    = []

  -- use init action to start to model set up phase 
  
  perform h _ _ a = case a of
    NewLaw w l        -> do
      Trace.callEndpoint @"new-law" (h $ GovH w) (fromBuiltin l)
      delay 1
    AddVote w t b     -> do
      Trace.callEndpoint @"add-vote" (h $ GovH w) (t , b)
      delay 1

  nextState a = case a of
    NewLaw w l -> do
        -- will all wallets not get 2 ada and a token 
        withdraw w $ Ada.toValue Ledger.minAdaTxOut --(Gov.mkTokenName baseName w)
        state .= (l , True)
        wait 1
    AddVote w t v -> do 
        oldMap <- viewContractState targets
        withdraw w $ Ada.toValue Ledger.minAdaTxOut -- <> theToken
        targets .= AssocMap.insert t v oldMap
        wait 1

  initialState = GovernanceModel { _state = ("" , False)
                             , _targets       = AssocMap.empty
                             }    

  arbitraryAction s = QC.frequency $ [ 
                                    (1, NewLaw <$> QC.elements testWallets <*> QC.elements laws)
                                  ] ++
                                  [ 
                                    (1, AddVote <$> QC.elements testWallets <*> QC.elements tokenExample <*> QC.choose (True, False)) 
                                  ] 


  shrinkAction _ _ = []



  precondition s a = case a of
    NewLaw w l -> snd (s ^. contractState . state) == False 
    AddVote w t v  -> True 

laws :: [ BuiltinByteString ]
laws = ["lawv1", "lawv2", "lawv3"]

tokenExample :: [ Ledger.TokenName ]
tokenExample = ["token1", "token2", "token3"]

prop_Gov :: Actions GovernanceModel -> QC.Property
prop_Gov = propRunActions_



{-
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      contribs <- viewContractState contributions
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      -- omit next two lines to disable disbursement of the surplus
      let leftoverValue = fold contribs <> inv (fold targets)
      deposit w leftoverValue
      contributions .= Map.empty
      wait 1 -}


{-
  nextState a = case a of
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      contributions .= Map.empty
      wait 1
-}

{-      
  nextState a = case a of
    Pay w v -> do
      withdraw w (Ada.adaValueOf $ fromInteger v)
      contributions %= Map.insertWith (<>) w (Ada.adaValueOf $ fromInteger v)
      wait 1
    Redeem w -> do
      targets <- viewContractState targets
      sequence_ [ deposit w v | (w, v) <- Map.toList targets ]
      contributions .= Map.empty
      wait 1  
-}



--  data ContractInstanceKey EscrowModel w s e params where
  --  WalletKey :: Wallet -> ContractInstanceKey EscrowModel () EscrowSchema EscrowError (


--traverse_ (\(nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (nm, True)  >> Trace.waitNSlots 1)
  --                    (take ayes namesAndHandles)


testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

{-
exProposal :: Gov.Proposal 
exProposal = Gov.Proposal { Gov.newLaw = lawv1
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                                 , Gov.tokenName = token2
                                 }
-}


------------------------------------------------------------

tests :: TestTree  
tests =
    testGroup "governance tests"
    [ checkPredicate "vote all in favor, 2 rounds - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.validatorAddress $ Gov.typedValidator params) (maybe False ((== lawv3) . Gov.law) . listToMaybe))
        (doVoting 10 0 2)

    , checkPredicate "vote 60/40, accepted - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.validatorAddress $ Gov.typedValidator params) (maybe False ((== lawv2) . Gov.law) . listToMaybe))
        (doVoting 6 4 1)

    , checkPredicate "vote 50/50, rejected - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.validatorAddress $ Gov.typedValidator params) (maybe False ((== lawv1) . Gov.law) . listToMaybe ))
        (doVoting 5 5 1)

    , goldenPir "test/Spec/governance.pir" $$(PlutusTx.compile [|| Gov.mkValidator ||])
    , HUnit.testCase "script size is re-asonable"
                     ( reasonable (Scripts.validatorScript $ Gov.typedValidator params)
                                  23000
                     )
    ]

numberOfHolders :: Integer
numberOfHolders = 10

baseName :: Ledger.TokenName
baseName = "TestLawToken"

-- | A governance contract that requires 6 votes out of 10
params :: Gov.Params
params = Gov.Params
    { Gov.initialHolders = EM.mockWalletPaymentPubKeyHash . knownWallet <$> [1..numberOfHolders]
    , Gov.requiredVotes = 6
    , Gov.baseTokenName = baseName
    }

lawv1, lawv2, lawv3 :: BuiltinByteString
lawv1 = "Law v1"
lawv2 = "Law v2"
lawv3 = "Law v3"

doVoting :: Int -> Int -> Integer -> EmulatorTrace ()
doVoting ayes nays rounds = do
    let activate w = (Gov.mkTokenName baseName w,)
                 <$> Trace.activateContractWallet (knownWallet w)
                                                  (Gov.contract @Gov.GovError params)
    namesAndHandles <- traverse activate [1..numberOfHolders]
    let handle1 = snd (head namesAndHandles)
    let token2 = fst (namesAndHandles !! 1)
    void $ Trace.callEndpoint @"new-law" handle1 (fromBuiltin lawv1)
    void $ Trace.waitNSlots 10
    slotCfg <- Trace.getSlotConfig
    let votingRound (_, law) = do
            now <- view Trace.currentSlot <$> Trace.chainState
            void $ Trace.activateContractWallet w2
                (Gov.proposalContract @Gov.GovError params
                    Gov.Proposal { Gov.newLaw = law
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                                 , Gov.tokenName = token2
                                 })
            void $ Trace.waitNSlots 1
            traverse_ (\(nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (nm, True)  >> Trace.waitNSlots 1)
                      (take ayes namesAndHandles)
            traverse_ (\(nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (nm, False) >> Trace.waitNSlots 1)
                      (take nays $ drop ayes namesAndHandles)
            Trace.waitNSlots 15

    traverse_ votingRound (zip [1..rounds] (cycle [lawv2, lawv3]))

-- below is new

tf :: IO () 
tf = Trace.runEmulatorTraceIO' def def (doVoting 10 0 1)

testTree :: IO ()
testTree = defaultMain tests

--testWallets :: [Wallet]
--testWallets = [w1, w2, w3, w4, w5] -- removed five to increase collisions (, w6, w7, w8, w9, w10])

{-
data EscrowModel = EscrowModel { _contributions :: Map Wallet Value.Value
                               , _targets       :: Map Wallet Value.Value
                               } deriving (Eq, Show, Data)

makeLenses ''EscrowModel
-}









