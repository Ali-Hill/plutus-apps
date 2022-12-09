{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
----------------------------

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# LANGUAGE ParallelListComp    #-}
module Spec.Governance(tests, doVoting,
                                tf,
                                testTree,
                                prop_Gov,
                                   prop_finishGovernance, prop_NoLockedFunds
                                   ,check_propGovernanceWithCoverage
                                   ,prop_Fail
                                   ,prop_Fail2
                                   ,prop_FinishFast
                                   ,prop_NoLockedFundsFast
                                   ) where

--import Control.Lens (view)
import Control.Lens hiding (both, elements)
-- import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)

import Ledger hiding (increaseTransactionLimits)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Wallet.Emulator qualified as EM

import Plutus.Contract.Test
import Plutus.Contracts.Governance qualified as Gov
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
-- import PlutusTx qualified
import PlutusTx.Prelude (BuiltinByteString)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit qualified as HUnit

----------------------------------------------------------------
import Control.Monad
import Data.Data
import Data.Default (Default (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Ada qualified as Ada
import Plutus.Contract (EmptySchema)
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.Coverage
import Test.QuickCheck qualified as QC

import Data.Semigroup (Sum (..))


options :: CheckOptions
options = defaultCheckOptionsContractModel & (increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits)


data GovernanceModel = GovernanceModel { _state        :: (BuiltinByteString, Bool)
                                       , _targets      :: Map Ledger.TokenName Bool
                                       , _walletTokens :: Map Wallet TokenName
                                       , _endSlot      :: Slot
                                       , _phase        :: Phase
                                       , _proposedLaw  :: BuiltinByteString
                                      } deriving (Eq, Show, Data)

data Phase = Initial | Establishing | Proposing | Voting | Tallying | Finish deriving (Eq, Show, Data)

makeLenses ''GovernanceModel


deriving instance Eq (ContractInstanceKey GovernanceModel w s e params)
deriving instance Show (ContractInstanceKey GovernanceModel w s e params)

instance ContractModel GovernanceModel where
  data Action GovernanceModel = Init Wallet --might not need wallet here
                          | NewLaw Wallet BuiltinByteString
                          | AddVote Wallet Ledger.TokenName Bool
                          | StartProposal Wallet BuiltinByteString TokenName Slot
                          | CheckLaw Wallet
                          | Tally Wallet
    deriving (Eq, Show, Data)

  data ContractInstanceKey GovernanceModel w s e params where
      GovH  :: Wallet -> ContractInstanceKey GovernanceModel () Gov.Schema Gov.GovError ()
      ProposalH :: Wallet -> ContractInstanceKey GovernanceModel () EmptySchema Gov.GovError Gov.Proposal

  --start a contract instance in each of the test wallets (with contract parameter ()),
  --initialInstances = [StartContract (GovH w) () | w <- testWallets]
  initialInstances = [] -- old

  --tells the framework which wallet each contr
  instanceWallet (GovH w)      = w
  instanceWallet (ProposalH w) = w


  -- tells the framework which contract to run for each key
  instanceContract _ GovH{} _      = Gov.contract @Gov.GovError params
  instanceContract _ ProposalH{} p = Gov.proposalContract @Gov.GovError params p

  startInstances _ (Init _) =
    [StartContract (GovH w) () | w <- testWallets]
  startInstances _ (StartProposal w l t slot) =
    [StartContract (ProposalH w)
              Gov.Proposal { Gov.newLaw = Gov.Law l
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime def $ slot -- POSIXTime (getSlot slot)
                                 , Gov.tokenName = t
                                 }]
  startInstances _ _ = []

  perform h _ s a = case a of
    Init _ -> do
      return ()
    NewLaw w l        -> do
      Trace.callEndpoint @"new-law" (h $ GovH w) (Gov.Law l)
      delay 2
    AddVote w t b     -> do
      Trace.callEndpoint @"add-vote" (h $ GovH w) (t , b)
      delay 1
    StartProposal _ _ _ _ -> do
      return ()
      delay 1
    CheckLaw w    -> do
      Trace.callEndpoint @"check-law" (h $ GovH w) (fst (s ^. contractState . state))
      delay 1
    Tally _ -> do
      delay 1

  nextState a = case a of
    Init _ -> do
      phase .= Establishing
    NewLaw w l -> do
        -- will all wallets not get 2 ada and a token
        let mph = Scripts.forwardingMintingPolicyHash (Gov.typedValidator params)

        walletTokens .= Map.fromList [(w' , t) | w' <- testWallets | t <- tokens]

        sequence_ [deposit w' $ Ada.toValue Ledger.minAdaTxOut | w' <- testWallets]
        sequence_ [deposit w' $ Gov.votingValue mph t | w' <- testWallets | t <- tokens]

        -- the current wallet loses the minimum ada * (no of wallets + 1) since we deposit ada to all wallets
        withdraw w $ Ada.toValue (Ledger.minAdaTxOut * (fromInteger $ numberOfHolders + 1))

        state .= (l , True)
        phase .= Proposing
        wait 2
    AddVote _ t v -> do
        -- adds vote but there is no change in wallet.
        oldMap <- viewContractState targets
        targets .= Map.insert t v oldMap
        wait 1
    StartProposal _ l _ slot  -> do
      proposedLaw .= l
      endSlot .= slot
      targets .= Map.empty
      curSlot <- viewModelState currentSlot
      when (curSlot <= slot) $ phase .= Voting
      wait 1
    CheckLaw _ -> do
      phase .= Proposing
      wait 1
    Tally _ -> do
      phase .= Finish
      wait 1

  nextReactiveState slot = do
    deadline <- viewContractState endSlot
    s <- viewContractState phase
    votes <- (viewContractState targets)
    pLaw <- (viewContractState proposedLaw)
    when ((slot >= deadline) && (s == Voting)) $ do
      let Sum ayes = foldMap (\b -> Sum $ if b then 1 else (0 :: Int)) votes
      when (ayes >= 5) $ state .= (pLaw, True)
      phase .= Tallying


  initialState = GovernanceModel { _state = ("" , False)
                             , _targets       = Map.empty
                             , _walletTokens = Map.empty
                             , _endSlot = 0
                             , _phase = Initial
                             , _proposedLaw = ""
                             }

  arbitraryAction s
    | s ^.contractState . phase == Initial
      = Init <$> QC.elements testWallets
    | s ^.contractState . phase == Establishing
      = NewLaw <$> QC.elements testWallets <*> QC.elements laws
    | s ^.contractState . phase == Proposing
      = StartProposal <$> QC.elements testWallets <*> QC.elements laws <*> QC.elements tokens <*> (Slot . QC.getPositive <$> QC.scale (*10) QC.arbitrary)
    | s ^.contractState . phase == Finish
      = CheckLaw <$> QC.elements testWallets
    | s ^.contractState . phase == Tallying
      = Tally <$> QC.elements testWallets
    | otherwise
      =   AddVote <$> QC.elements testWallets <*> QC.elements tokens <*> QC.choose (True, False)

  shrinkAction _ _ = []


  precondition s a = case a of
    Init _ -> currentPhase == Initial
    NewLaw _ _ -> currentPhase /= Initial
                  && snd (s ^. contractState . state) == False
    AddVote w t _  -> currentPhase == Voting
                      && ownsVotingToken' w t (s ^. contractState . walletTokens)
    StartProposal w _ t _ -> currentPhase == Proposing
                                && ownsVotingToken' w t (s ^. contractState . walletTokens)
                                -- && viewModelState currentSlot < slot Note: I thought I would be able to do this
    Tally _ -> currentPhase == Tallying
    CheckLaw _ -> currentPhase == Finish
                                -- Gov.ownsVotingToken (Scripts.forwardingMintingPolicyHash (Gov.typedValidator params)) t
                                -- && snd (s ^. contractState . state) == False
    where currentPhase = s ^. contractState . phase



ownsVotingToken' :: Wallet -> TokenName -> Map Wallet TokenName -> Bool
ownsVotingToken' w t m = case Map.lookup w m of
                            Nothing -> False
                            Just tn -> t == tn

laws :: [ BuiltinByteString ]
laws = ["lawv1", "lawv2", "lawv3"]

-- this sets the number of wallets
numberOfHolders :: Integer
numberOfHolders = 10

tokens :: [TokenName]
tokens = zipWith (const (Gov.mkTokenName (Gov.baseTokenName params))) (Gov.initialHolders params) [1..]


baseName :: Ledger.TokenName
baseName = "TestLawToken"

-- | A governance contract that requires 5 votes out of 10
params :: Gov.Params
params = Gov.Params
    { Gov.initialHolders = EM.mockWalletPaymentPubKeyHash . knownWallet <$> [1..numberOfHolders]
    , Gov.requiredVotes = 5
    , Gov.baseTokenName = baseName
    }

prop_Gov :: Actions GovernanceModel -> QC.Property
prop_Gov = propRunActionsWithOptions options defaultCoverageOptions (\ _ -> pure True)

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]

finishGovernance :: DL GovernanceModel ()
finishGovernance = do
    anyActions_
    finishingStrategy
    assertModel "Locked funds are not zero" (symIsZero . lockedValue)

finishingStrategy :: DL GovernanceModel ()
finishingStrategy = do
    -- contribs <- viewContractState contributions
    -- monitor (tabulate "Refunded wallets" [show . Map.size $ contribs])
    slot     <- viewModelState currentSlot
    currentPhase <- viewContractState phase
    monitor $ QC.tabulate "Phase" [show currentPhase]
    when (currentPhase == Proposing) $ do
      action $ StartProposal w1 "lawv1" "TestLawToken1" (slot + 10)
    when (currentPhase /= Initial && currentPhase /= Establishing) $ do
      waitUntilDeadline
    --sequence_ [action $ Refund w | w <- testWallets, w `Map.member` contribs]

walletStrategy :: Wallet -> DL GovernanceModel ()
walletStrategy _ = do
    waitUntilDeadline

waitUntilDeadline :: DL GovernanceModel ()
waitUntilDeadline = do
    deadline <- viewContractState endSlot
    slot     <- viewModelState currentSlot
    when (slot < (deadline + 5)) $ waitUntilDL (deadline + 5)

noLockProof :: NoLockedFundsProof GovernanceModel
noLockProof = defaultNLFP
  { nlfpMainStrategy   = finishingStrategy
  , nlfpWalletStrategy = walletStrategy    }

prop_finishGovernance :: QC.Property
prop_finishGovernance = forAllDL finishGovernance prop_Gov

prop_FinishFast :: QC.Property
prop_FinishFast = forAllDL finishGovernance $ const True

prop_NoLockedFunds :: QC.Property
prop_NoLockedFunds = checkNoLockedFundsProof noLockProof

prop_NoLockedFundsFast :: QC.Property
prop_NoLockedFundsFast = checkNoLockedFundsProofFast noLockProof

check_propGovernanceWithCoverage :: IO ()
check_propGovernanceWithCoverage = do
  cr <- quickCheckWithCoverage QC.stdArgs (set coverageIndex Gov.covIdx' $ defaultCoverageOptions) $ \covopts ->
    QC.withMaxSuccess 1000 $ propRunActionsWithOptions @GovernanceModel options covopts (const (pure True))
  writeCoverageReport "Governance" cr

failDL :: DL GovernanceModel ()
failDL = do
          action $ Init w10
          action $ NewLaw w9 "lawv3"
          action $ StartProposal w9 "lawv2" "TestLawToken9" (Slot {getSlot = 1054})
          action $ AddVote w7 "TestLawToken7" True
          action $ AddVote w5 "TestLawToken5" True
          action $ AddVote w10 "TestLawToken10" True
          action $ AddVote w4 "TestLawToken4" True
          waitUntilDL 1053
          action $ AddVote w6 "TestLawToken6" True
          action $ Tally w8
          action $ CheckLaw w8

-- fixed by changing next reactive state to (slot >= deadline) from (slot > deadline)
-- then adding a tally action to allow the proposal contract to change to the right state

failDL2 :: DL GovernanceModel ()
failDL2 = do
          action $ Init w6
          action $ NewLaw w3 "lawv1"
          waitUntilDL 127
          action $ StartProposal w7 "lawv1" "TestLawToken7" (Slot {getSlot = 127})
          action $ StartProposal w9 "lawv2" "TestLawToken9" (Slot {getSlot = 968})
          action $ AddVote w2 "TestLawToken2" True
          action $ AddVote w5 "TestLawToken5" True
          action $ AddVote w7 "TestLawToken7" True
          action $ AddVote w6 "TestLawToken6" True
          action $ AddVote w4 "TestLawToken4" True
          waitUntilDL 968
          action $ Tally w3
          action $ CheckLaw w3

--fixed by changing
-- when (curSlot < slot) $ phase .= Voting
-- to
-- when (curSlot <= slot) $ phase .= Voting

prop_Fail :: QC.Property
prop_Fail = QC.withMaxSuccess 1 $ forAllDL failDL prop_Gov

prop_Fail2 :: QC.Property
prop_Fail2 = QC.withMaxSuccess 1 $ forAllDL failDL2 prop_Gov

{-
 [Init (Wallet 8),
  NewLaw (Wallet 10) "lawv1",
  StartProposal (Wallet 10) "lawv3" "TestLawToken10" (Slot {getSlot = 133}),
  AddVote (Wallet 3) "TestLawToken3" True,
  AddVote (Wallet 7) "TestLawToken7" True,
  AddVote (Wallet 1) "TestLawToken1" True,
  AddVote (Wallet 6) "TestLawToken6" True,
  AddVote (Wallet 8) "TestLawToken8" True,
  WaitUntil (Slot {getSlot = 134}),
  CheckLaw (Wallet 2)]
-}


{-
 [Init (Wallet 6),
  NewLaw (Wallet 3) "lawv1",
  WaitUntil (Slot {getSlot = 127}),
  StartProposal (Wallet 7) "lawv1" "TestLawToken7" (Slot {getSlot = 127}),
  StartProposal (Wallet 9) "lawv2" "TestLawToken9" (Slot {getSlot = 968}),
  AddVote (Wallet 2) "TestLawToken2" True,
  AddVote (Wallet 5) "TestLawToken5" True,
  AddVote (Wallet 7) "TestLawToken7" True,
  AddVote (Wallet 6) "TestLawToken6" True,
  AddVote (Wallet 4) "TestLawToken4" True,
  WaitUntil (Slot {getSlot = 968}),
  Tally (Wallet 3),
  CheckLaw (Wallet 3)]
-}



{-
 [Init (Wallet 8),
  NewLaw (Wallet 6) "lawv2",
  StartProposal (Wallet 3) "lawv1" "TestLawToken3" (Slot {getSlot = 285}),
  WaitUntil (Slot {getSlot = 285}),
  CheckLaw (Wallet 7)]
-}



{-
fail :: DL EscrowModel ()
fail = do
          action $ Init w10
          action $ NewLaw w9 "lawv3"
          action $ Pay w2 val
          action $ Pay w3 val
          action $ Redeem w4
 [Init (Wallet 10),
  NewLaw (Wallet 9) "lawv3",
  StartProposal (Wallet 9) "lawv2" "TestLawToken9" (Slot {getSlot = 1054}),
  AddVote (Wallet 7) "TestLawToken7" True,
  AddVote (Wallet 5) "TestLawToken5" True,
  AddVote (Wallet 10) "TestLawToken10" True,
  AddVote (Wallet 4) "TestLawToken4" True,
  WaitUntil (Slot {getSlot = 1054}),
  AddVote (Wallet 6) "TestLawToken6" True,
  CheckLaw (Wallet 8)]
-}

------------------------------------------------------------


tests :: TestTree
tests =
    testGroup "governance tests"
    [ checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits) "vote all in favor, 2 rounds - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.validatorAddress $ Gov.typedValidator params) (maybe False ((== Gov.Law lawv3) . Gov.law) . listToMaybe))
        (doVoting 10 0 2)

    , checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits) "vote 60/40, accepted - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.validatorAddress $ Gov.typedValidator params) (maybe False ((== Gov.Law lawv2) . Gov.law) . listToMaybe))
        (doVoting 6 4 1)

    , checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits) "vote 50/50, rejected - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress (Scripts.validatorAddress $ Gov.typedValidator params) (maybe False ((== Gov.Law lawv1) . Gov.law) . listToMaybe ))
        (doVoting 5 5 1)

    -- TODO: turn this on again when reproducibility issue in core is fixed
    -- , goldenPir "test/Spec/governance.pir" $$(PlutusTx.compile [|| Gov.mkValidator ||])
    , HUnit.testCase "script size is re-asonable"
                     ( reasonable (Scripts.validatorScript $ Gov.typedValidator params)
                                  23000
                     )
    ]



lawv1, lawv2, lawv3 :: BuiltinByteString
lawv1 = "Law v1"
lawv2 = "Law v2"
lawv3 = "Law v3"

-- | Proposal
-- slot should always be now
{-
proposal :: Slot -> TokenName -> BuiltinByteString -> Gov.Proposal
proposal s t l = Gov.Proposal { Gov.newLaw = Gov.Law l
                        , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime
                                                    def
                                                     $ s + 20 --double check what this does
                        , Gov.tokenName = t
                        }
-}

{-
data Proposal = Proposal
    { newLaw         :: BuiltinByteString
    -- ^ The new contents of the law
    , tokenName      :: TokenName
    -- ^ The name of the voting tokens. Only voting token owners are allowed to propose changes.
    , votingDeadline :: POSIXTime
    -- ^ The time when voting ends and the votes are tallied.
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
-}


doVoting :: Int -> Int -> Integer -> EmulatorTrace ()
doVoting ayes nays rounds = do
    let activate w = (Gov.mkTokenName baseName w,)
                 <$> Trace.activateContractWallet (knownWallet w)
                                                  (Gov.contract @Gov.GovError params)
    namesAndHandles <- traverse activate [1..numberOfHolders]
    let handle1 = snd (head namesAndHandles)
    let token2 = fst (namesAndHandles !! 1)
    void $ Trace.callEndpoint @"new-law" handle1 (Gov.Law lawv1)
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

    traverse_ votingRound (zip [1..rounds] (cycle [Gov.Law lawv2, Gov.Law lawv3]))

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
