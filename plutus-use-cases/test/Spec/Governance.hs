{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Spec.Governance(tests, doVoting, prop_Gov, prop_Fail, prop_Fail2) where

import Control.Lens hiding (both, elements)
import Control.Monad
import Data.Data
import Data.Default (Default (..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Semigroup (Sum (..))

import Cardano.Node.Emulator.Params qualified as Params
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Ledger qualified
import Ledger.Slot
import Ledger.Typed.Scripts qualified as Scripts
import Wallet.Emulator qualified as EM

import Plutus.Contract (EmptySchema)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel
import Plutus.Contracts.Governance qualified as Gov
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value (TokenName)
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Prelude (BuiltinByteString)


import Test.QuickCheck qualified as QC

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit





------------------------------------------------

options :: CheckOptions
options = defaultCheckOptionsContractModel & (increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits . increaseTransactionLimits)


data GovernanceModel = GovernanceModel { _state        :: (BuiltinByteString, Bool)
                                       , _targets      :: Map (Wallet, TokenName) Bool -- TokenName Bool
                                       , _walletTokens :: Map Wallet TokenName
                                       , _endSlot      :: Slot
                                       , _phase        :: Phase
                                       , _proposedLaw  :: BuiltinByteString
                                      } deriving (Eq, Show, Data, Generic)

data Phase = Initial | Establishing | Proposing | Voting | Tallying | Finish deriving (Eq, Show, Data, Generic)

makeLenses ''GovernanceModel


deriving instance Eq (ContractInstanceKey GovernanceModel w s e params)
deriving instance Show (ContractInstanceKey GovernanceModel w s e params)

instance ContractModel GovernanceModel where
  data Action GovernanceModel = Init Wallet --might not need wallet here
                          | NewLaw Wallet BuiltinByteString
                          | AddVote Wallet TokenName Bool
                          | StartProposal Wallet BuiltinByteString TokenName Slot
                          | CheckLaw Wallet
                          | Tally Wallet
    deriving (Eq, Show, Data, Generic)

  data ContractInstanceKey GovernanceModel w s e params where
      GovH  :: Wallet -> ContractInstanceKey GovernanceModel () Gov.Schema Gov.GovError ()
      ProposalH :: Wallet -> ContractInstanceKey GovernanceModel () EmptySchema Gov.GovError (Ledger.Address, Gov.Proposal)

  --start a contract instance in each of the test wallets (with contract parameter ()),
  --initialInstances = [StartContract (GovH w) () | w <- testWallets]
  initialInstances = [] -- old

  --tells the framework which wallet each contr
  instanceWallet (GovH w)      = w
  instanceWallet (ProposalH w) = w


  -- tells the framework which contract to run for each key
  instanceContract _ GovH{} _            = Gov.contract @Gov.GovError params
  instanceContract _ ProposalH{} (a , p) = Gov.proposalContract @Gov.GovError params a p

  startInstances _ (Init _) =
    [StartContract (GovH w) () | w <- testWallets]
  startInstances _ (StartProposal w l t slot) =
    [StartContract (ProposalH w)
              (Ledger.toPlutusAddress $ mockWalletAddress w,
              Gov.Proposal { Gov.newLaw = Gov.Law l
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime def $ slot -- POSIXTime (getSlot slot)
                                 , Gov.tokenName = t
                                 })]
  startInstances _ _ = []

  perform h _ s a = case a of
    Init _ -> do
      return ()
    NewLaw w l        -> do
      Trace.callEndpoint @"new-law" (h $ GovH w) (Gov.Law l)
      delay 2
    AddVote w t b     -> do
      Trace.callEndpoint @"add-vote" (h $ GovH w) (Ledger.toPlutusAddress $ mockWalletAddress w, t , b)
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

        sequence_ [deposit w' $ Ada.toValue Ledger.minAdaTxOutEstimated | w' <- testWallets]
        sequence_ [deposit w' $ Gov.votingValue mph t | w' <- testWallets | t <- tokens]

        -- the current wallet loses the minimum ada * (no of wallets + 1) since we deposit ada to all wallets
        withdraw w $ Ada.toValue (Ledger.minAdaTxOutEstimated * (fromInteger $ numberOfHolders + 1))

        state .= (l , True)
        phase .= Proposing
        wait 2
    AddVote w t v -> do
        -- adds vote but there is no change in wallet.
        oldMap <- viewContractState targets
        targets .= Map.insert (w , t) v oldMap
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
      -- fix so the below is a parameter
      when (ayes >= 6) $ state .= (pLaw, True)
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

tokens :: [TokenName]
tokens = zipWith (const (Gov.mkTokenName (Gov.baseTokenName params))) (Gov.initialHolders params) [1..]

{-
-- | A governance contract that requires 5 votes out of 10
params :: Gov.Params
params = Gov.Params
    { Gov.initialHolders = EM.mockWalletPaymentPubKeyHash . knownWallet <$> [1..numberOfHolders]
    , Gov.requiredVotes = 5
    , Gov.baseTokenName = baseName
    }
-}

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

{- fix later
check_propGovernanceWithCoverage :: IO ()
check_propGovernanceWithCoverage = do
  cr <- quickCheckWithCoverage QC.stdArgs (set coverageIndex Gov.covIdx' $ defaultCoverageOptions) $ \covopts ->
    QC.withMaxSuccess 50 $ propRunActionsWithOptions @GovernanceModel options covopts (const (pure True))
  writeCoverageReport "Governance" cr
-}


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

prop_Fail :: QC.Property
prop_Fail = QC.withMaxSuccess 1 $ forAllDL failDL prop_Gov

prop_Fail2 :: QC.Property
prop_Fail2 = QC.withMaxSuccess 1 $ forAllDL failDL2 prop_Gov

-----------------------------------------------


validatorAddress :: Ledger.CardanoAddress
validatorAddress
  = Scripts.validatorCardanoAddress Params.testnet
  $ Gov.typedValidator params

tests :: TestTree
tests =
    testGroup "governance tests"
    [ checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits) "vote all in favor, 2 rounds - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress validatorAddress (maybe False ((== lawv3) . Gov.law) . listToMaybe))
        (doVoting 10 0 2)

    , checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits) "vote 60/40, accepted - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress validatorAddress (maybe False ((== lawv2) . Gov.law) . listToMaybe))
        (doVoting 6 4 1)

    , checkPredicateOptions (defaultCheckOptions & increaseTransactionLimits) "vote 50/50, rejected - SUCCESS"
        (assertNoFailedTransactions
        .&&. dataAtAddress validatorAddress (maybe False ((== lawv1) . Gov.law) . listToMaybe ))
        (doVoting 5 5 1)

    -- TODO: turn this on again when reproducibility issue in core is fixed
    -- , goldenPir "test/Spec/governance.pir" $$(PlutusTx.compile [|| Gov.mkValidator ||])

    , HUnit.testCase "script size is reasonable"
                     ( reasonable (Scripts.validatorScript $ Gov.typedValidator params)
                                  23000
                     )
    ]

numberOfHolders :: Integer
numberOfHolders = 10

baseName :: TokenName
baseName = "TestLawToken"

-- | A governance contract that requires 6 votes out of 10
params :: Gov.Params
params = Gov.Params
    { Gov.initialHolders = Ledger.toPlutusAddress . EM.mockWalletAddress . knownWallet <$> [1..numberOfHolders]
    , Gov.requiredVotes = 6
    , Gov.baseTokenName = baseName
    }

lawv1, lawv2, lawv3 :: Gov.Law
lawv1 = Gov.Law "Law v1"
lawv2 = Gov.Law "Law v2"
lawv3 = Gov.Law "Law v3"

doVoting :: Int -> Int -> Integer -> EmulatorTrace ()
doVoting ayes nays rounds = do
    let activate wId = (Ledger.toPlutusAddress $ mockWalletAddress w, Gov.mkTokenName baseName wId,)
                 <$> Trace.activateContractWallet w (Gov.contract @Gov.GovError params)
           where
               w = knownWallet wId
    namesAndHandles <- traverse activate [1..numberOfHolders]
    let handle1 = (\(_,_,h) -> h) (head namesAndHandles)
    let token2 = (\(_,t,_) -> t) (namesAndHandles !! 1)
    let owner = Ledger.toPlutusAddress $ mockWalletAddress w2
    void $ Trace.callEndpoint @"new-law" handle1 lawv1
    void $ Trace.waitNSlots 10
    slotCfg <- Trace.getSlotConfig
    let votingRound (_, law) = do
            now <- view Trace.chainCurrentSlot <$> Trace.chainState
            void $ Trace.activateContractWallet w2
                (Gov.proposalContract @Gov.GovError params owner
                    Gov.Proposal { Gov.newLaw = law
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                                 , Gov.tokenName = token2
                                 })
            void $ Trace.waitNSlots 1
            traverse_ (\(ow, nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (ow, nm, True)  >> Trace.waitNSlots 1)
                      (take ayes namesAndHandles)
            traverse_ (\(ow, nm, hdl) -> Trace.callEndpoint @"add-vote" hdl (ow, nm, False) >> Trace.waitNSlots 1)
                      (take nays $ drop ayes namesAndHandles)
            Trace.waitNSlots 15

    traverse_ votingRound (zip [1..rounds] (cycle [lawv2, lawv3]))
