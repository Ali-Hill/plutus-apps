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
{-# LANGUAGE ParallelListComp #-} -- to allow parallel list comprehensions
module Spec.Tutorial.Governance1(tests, doVoting, tf, testTree, prop_Gov) where

--import Control.Lens (view)
import Control.Lens hiding (both, elements)
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)

import Ledger 
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
import Plutus.Contract.Test.ContractModel.Symbolics --for tokens
import Control.Monad -- for forM
--import Data.Set qualified as Set --might not need


--Notes 
-- we need to ensure that all voting etc is done with the correct token. 

--import Prelude qualified as Haskell --new


{-
Is it possible to do multiple votes with same token by transferring it between wallets. 
-}

data GovernanceModel = GovernanceModel { _state :: (BuiltinByteString, Bool)
                                       , _targets :: AssocMap.Map Ledger.TokenName Bool
                                       , _phase :: Phase
                                      } deriving (Eq, Show, Data)

data Phase = Initial | Establishing | Proposing | Voting | Finish deriving (Eq, Show, Data)

makeLenses ''GovernanceModel


deriving instance Eq (ContractInstanceKey GovernanceModel w s e params)
deriving instance Show (ContractInstanceKey GovernanceModel w s e params)

instance ContractModel GovernanceModel where
  data Action GovernanceModel = Init Wallet --might not need wallet here
                          | NewLaw Wallet BuiltinByteString
                          | AddVote Wallet Ledger.TokenName Bool
                          | StartProposal Wallet BuiltinByteString TokenName Slot
    deriving (Eq, Show, Data)

  data ContractInstanceKey GovernanceModel w s e params where
      GovH  :: Wallet -> ContractInstanceKey GovernanceModel () Gov.Schema Gov.GovError ()
      ProposalH :: Wallet -> ContractInstanceKey GovernanceModel () EmptySchema Gov.GovError Gov.Proposal

  --start a contract instance in each of the test wallets (with contract parameter ()),
  --initialInstances = [StartContract (GovH w) () | w <- testWallets]
  initialInstances = [] -- old 

  --tells the framework which wallet each contr  
  instanceWallet (GovH w) = w
  instanceWallet (ProposalH w) = w


  {-
contract ::
    AsGovError e
    => Params
    -> Contract () Schema e ()

-- | The contract for proposing changes to a law.
proposalContract ::
    AsGovError e
    => Params
    -> Proposal
    -> Contract () EmptySchema e ()


  (Gov.proposalContract @Gov.GovError params
           Gov.Proposal { Gov.newLaw = law
               , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                   , Gov.tokenName = token2
                      })
  -}


  {-
testContract :: EscrowParams Datum -> Contract () EscrowSchema EscrowError ()
testContract params = selectList [ void $ payEp params
                                 , void $ redeemEp params
                                 , void $ refundEp params
                                 ] >> testContract params
  -}


  {-
contract ::
    AsGovError e
    => Params
    -> Contract () Schema e ()

proposalContract ::
    AsGovError e
    => Params
    -> Proposal
    -> Contract () EmptySchema e ()

  -}

  -- tells the framework which contract to run for each key
  instanceContract _ GovH{} _ = Gov.contract @Gov.GovError params
  instanceContract _ ProposalH{} p = Gov.proposalContract @Gov.GovError params p


  -- @Gov.GovError params p

  --startInstances to start contract
  -- first thing is init action

  --startInstances _ StartProposal w bs t slot  =   
  --startInstances _ _    = []
  startInstances _ (Init w) = 
    [StartContract (GovH w) () | w <- testWallets]
  startInstances _ (StartProposal w l t slot) = 
    --do 
    --slotCfg <- Trace.getSlotConfig
    --now <- view Trace.currentSlot <$> Trace.chainState
    [StartContract (ProposalH w)
              Gov.Proposal { Gov.newLaw = l
                                 , Gov.votingDeadline = POSIXTime (getSlot slot)
                                 , Gov.tokenName = t
                                 } | w <- testWallets]
  startInstances _ _ = []

  -- use init action to start to model set up phase 
  -- copy 207 and 209 to generate hash of tokens
{-
        let mph = Scripts.forwardingMintingPolicyHash (typedValidator params)
        void $ SM.runInitialise theClient (GovState (toBuiltin bsLaw) mph Nothing) (Ada.lovelaceValueOf 1)
        let tokens = Haskell.zipWith (const (mkTokenName (baseTokenName params))) (initialHolders params) [1..]
        void $ SM.runStep theClient $ MintTokens tokens
-}
  --(h $ ProposalH w)

  -- it is currently failing because It chooses an arbitrary token. 


  --ProposalH :: Wallet -> ContractInstanceKey GovernanceModel () EmptySchema Gov.GovError Gov.Proposal

  perform h _ _ a = case a of
    Init _ -> do
      return ()
    NewLaw w l        -> do
      Trace.callEndpoint @"new-law" (h $ GovH w) (fromBuiltin l)
      delay 1
    AddVote w t b     -> do
      Trace.callEndpoint @"add-vote" (h $ GovH w) (t , b)
      delay 1
    StartProposal w l t slot -> do 
      return ()
--    StartProposal  -> do 
       {-Trace.observableState (Trace.activateContract (h $ ProposalH w)
              (Gov.Proposal { Gov.newLaw = l
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                                 , Gov.tokenName = t
                                 }) ())
      delay 1-}

{-
    let votingRound (_, law) = do
            now <- view Trace.currentSlot <$> Trace.chainState
            void $ Trace.activateContractWallet w2
                (Gov.proposalContract @Gov.GovError params
                    Gov.Proposal { Gov.newLaw = law
                                 , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                                 , Gov.tokenName = token2
                                 })
            void $ Trace.waitNSlots 1
-}

{-
      withdraw w1 $ Ada.toValue ((fromInteger . toInteger . length $ wallets) * Ledger.minAdaTxOut)
      -- Create the tokens
      ts <- forM tokenNames $ \t -> do
        tok <- createToken t
        mint (symAssetClassValue tok (toInteger $ length wallets * 1000000))
        return tok
      -- Give the tokens to the wallets
      forM_ wallets $ \ w -> do
        deposit w $ Ada.toValue Ledger.minAdaTxOut
        deposit w $ mconcat [ symAssetClassValue t 1000000 | t <- ts ]
      exchangeableTokens %= (Set.fromList ts <>)
      wait 21
  -}

-- https://marlowe-playground-staging.plutus.aws.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-Test-ContractModel.html
{-
guessTokenVal :: Value
guessTokenVal =
    let sym = Scripts.forwardingMintingPolicyHash $ G.typedValidator gameParam
    in G.token sym "guess"
-}
  nextState a = case a of
    Init w -> do 
      phase .= Establishing
      --sequence_ [withdraw w $ Ada.toValue Ledger.minAdaTxOut | w <- testWallets] 
    NewLaw w l -> do
        -- will all wallets not get 2 ada and a token 
        let mph = Scripts.forwardingMintingPolicyHash (Gov.typedValidator params)
        let tokens = zipWith (const (Gov.mkTokenName (Gov.baseTokenName params))) (Gov.initialHolders params) [1..]

        sequence_ [deposit w $ Ada.toValue Ledger.minAdaTxOut | w <- testWallets] 
        sequence_ [deposit w $ (Gov.votingValue mph t) | w <- testWallets | t <- tokens]

        -- the current wallet loses the minimum ada * (no of wallets + 1) since we deposit ada to all wallets
        withdraw w $ Ada.toValue (Ledger.minAdaTxOut * (fromInteger $ numberOfHolders + 1))

        state .= (l , True)
        phase .= Proposing
        wait 1
    AddVote w t v -> do 
        oldMap <- viewContractState targets
        withdraw w $ Ada.toValue Ledger.minAdaTxOut -- <> theToken
        targets .= AssocMap.insert t v oldMap
        wait 1
    StartProposal w l t slot  -> do
      phase .= Voting

  initialState = GovernanceModel { _state = ("" , False)
                             , _targets       = AssocMap.empty
                             , _phase = Initial
                             }    
                           
  arbitraryAction s 
    | s ^.contractState . phase == Initial
      = Init <$> QC.elements testWallets
    | s ^.contractState . phase == Proposing
      = StartProposal <$> QC.elements testWallets <*> QC.elements laws <*> QC.elements tokenExample <*> (Slot . QC.getPositive <$> QC.scale (*10) QC.arbitrary)
    | otherwise
      =   QC.frequency $ [ 
                                    (1, NewLaw <$> QC.elements testWallets <*> QC.elements laws) 
                                  ] ++
                                  [ 
                                    (1, AddVote <$> QC.elements testWallets <*> QC.elements tokenExample <*> QC.choose (True, False)) 
                                  ] 
                                  {-[
                                    (1, StartProposal)
                                  ]-}


  shrinkAction _ _ = []


  precondition s a = case a of
    Init _ -> currentPhase == Initial
    NewLaw w l -> currentPhase /= Initial 
                  && snd (s ^. contractState . state) == False 
    AddVote w t v  -> currentPhase == Voting  --True 
    StartProposal w l t slot -> currentPhase == Proposing 
                                -- && snd (s ^. contractState . state) == False
    where currentPhase = s ^. contractState . phase

laws :: [ BuiltinByteString ]
laws = ["lawv1", "lawv2", "lawv3"]

tokenExample :: [ Ledger.TokenName ]
tokenExample = ["token1", "token2", "token3"]

prop_Gov :: Actions GovernanceModel -> QC.Property
prop_Gov = propRunActions_



{-
exProposal :: Gov.Proposal 
exProposal = Gov.Proposal 
                  { Gov.newLaw = lawv1
                    , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime slotCfg $ now + 20
                    , Gov.tokenName = token2
                  }
-}






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


-- we need to figure out this wallet issue 
-- Why are there 10 wallets with the below code

--testWallets :: [Wallet]
--testWallets = [w1, w2, w3, w4, w5]


testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5, w6, w7, w8, w9, w10]

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

-- this sets the number of wallets
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

-- | Proposal
-- slot should always be now
proposal :: Slot -> TokenName -> BuiltinByteString -> Gov.Proposal
proposal s t l = Gov.Proposal { Gov.newLaw = l
                        , Gov.votingDeadline = TimeSlot.slotToEndPOSIXTime 
                                                    def
                                                     $ s + 20 --double check what this does
                        , Gov.tokenName = t
                        }


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









