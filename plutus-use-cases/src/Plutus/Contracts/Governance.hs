{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
-- {-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-} --added for coverage
{-# LANGUAGE ViewPatterns       #-}
-- | A basic governance contract in Plutus.
module Plutus.Contracts.Governance (
    -- $governance
      contract
    , proposalContract
    , Params(..)
    , Proposal(..)
    , Schema
    , mkTokenName
    , typedValidator
    , mkValidator
    , GovState(..)
    , Law(..)
    , Voting(..)
    , GovError
    , votingValue
    , ownsVotingToken
    -- * Coverage
    -- , covIdx
    ) where

import Control.Lens (makeClassyPrisms, review)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup (Sum (..))
import Data.String (fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (POSIXTime, PaymentPubKeyHash, TokenName)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Interval qualified as Interval
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract
import Plutus.Contract.StateMachine (AsSMContractError, State (..), StateMachine (..), Void)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Code
import PlutusTx.Coverage
import PlutusTx.Prelude
import Prelude qualified as Haskell

-- $governance
-- * When the contract starts it produces a number of tokens that represent voting rights.
-- * Holders of those tokens can propose changes to the state of the contract and vote on them.
-- * After a certain period of time the voting ends and the proposal is rejected or accepted.
newtype Law = Law { unLaw :: BuiltinByteString }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The parameters for the proposal contract.
data Proposal = Proposal
    { newLaw         :: Law
    -- ^ The new contents of the law
    , tokenName      :: TokenName
    -- ^ The name of the voting tokens. Only voting token owners are allowed to propose changes.
    , votingDeadline :: POSIXTime
    -- ^ The time when voting ends and the votes are tallied.
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Voting = Voting
    { proposal :: Proposal
    , votes    :: AssocMap.Map TokenName Bool
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data GovState = GovState
    { law    :: Law
    , mph    :: MintingPolicyHash
    , voting :: Maybe Voting
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data GovInput
    = MintTokens [TokenName]
    | ProposeChange Proposal
    | AddVote TokenName Bool
    | FinishVoting
    | Check
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The endpoints of governance contracts are
--
-- * @new-law@ to create a new law and distribute voting tokens
-- * @add-vote@ to vote on a proposal with the name of the voting token and a boolean to vote in favor or against.
type Schema =
    Endpoint "new-law" Law
        .\/ Endpoint "add-vote" (TokenName, Bool)
        .\/ Endpoint "check-law" BuiltinByteString -- ByteString

-- | The governace contract parameters.
data Params = Params
    { baseTokenName  :: TokenName
    -- ^ The token names that allow voting are generated by adding an increasing number to the base token name. See `mkTokenName`.
    , initialHolders :: [PaymentPubKeyHash]
    -- ^ The public key hashes of the initial holders of the voting tokens.
    , requiredVotes  :: Integer
    -- ^ The number of votes in favor required for a proposal to be accepted.
    }

data GovError =
    GovContractError ContractError
    | GovStateMachineError SM.SMContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''GovError

instance AsContractError GovError where
    _ContractError = _GovContractError

instance AsSMContractError GovError where
    _SMContractError = _GovStateMachineError

type GovernanceMachine = StateMachine GovState GovInput

{-# INLINABLE machine #-}
machine :: Params -> GovernanceMachine
machine params = SM.mkStateMachine Nothing (transition params) isFinal where
    {-# INLINABLE isFinal #-}
    isFinal _ = False

{-# INLINABLE mkValidator #-}
mkValidator :: Params -> Scripts.ValidatorType GovernanceMachine
mkValidator params = SM.mkValidator $ machine params

typedValidator :: Params -> Scripts.TypedValidator GovernanceMachine
typedValidator = Scripts.mkTypedValidatorParam @GovernanceMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

client :: Params -> SM.StateMachineClient GovState GovInput
client params = SM.mkStateMachineClient $ SM.StateMachineInstance (machine params) (typedValidator params)

-- | Generate a voting token name by tagging on a number after the base token name.
mkTokenName :: TokenName -> Integer -> TokenName
mkTokenName base ix = fromString (Value.toString base ++ Haskell.show ix)

{-# INLINABLE votingValue #-}
votingValue :: MintingPolicyHash -> TokenName -> Value.Value
votingValue mph tokenName =
    Value.singleton (Value.mpsSymbol mph) tokenName 1

{-# INLINABLE ownsVotingToken #-}
ownsVotingToken :: MintingPolicyHash -> TokenName -> TxConstraints Void Void
ownsVotingToken mph tokenName = Constraints.mustSpendAtLeast (votingValue mph tokenName)

{-# INLINABLE transition #-}
transition :: Params -> State GovState -> GovInput -> Maybe (TxConstraints Void Void, State GovState)
transition Params{..} State{ stateData = s, stateValue} i = case (s, i) of

    (GovState{mph}, MintTokens tokenNames) ->
        let (total, constraints) = foldMap
                (\(pk, nm) -> let v = votingValue mph nm in (v, Constraints.mustPayToPubKey pk v))
                (zip initialHolders tokenNames)
        in Just (constraints <> Constraints.mustMintValue total, State s stateValue)

    (GovState law mph Nothing, ProposeChange proposal@Proposal{tokenName}) ->
        let constraints = ownsVotingToken mph tokenName
        in Just (constraints, State (GovState law mph (Just (Voting proposal AssocMap.empty))) stateValue)

    (GovState law mph (Just (Voting p oldMap)), AddVote tokenName vote) ->
        let newMap = AssocMap.insert tokenName vote oldMap
            -- Correct validity interval should be:
            -- @
            --   Interval (LowerBound NegInf True) (Interval.strictUpperBound $ votingDeadline p)
            -- @
            -- See Note [Validity Interval's upper bound]
            validityTimeRange = Interval.to (votingDeadline p - 2)
            constraints = ownsVotingToken mph tokenName
                        <> Constraints.mustValidateIn validityTimeRange
        in Just (constraints, State (GovState law mph (Just (Voting p newMap))) stateValue)

    (GovState oldLaw mph (Just (Voting p votes)), FinishVoting) ->
        let Sum ayes = foldMap (\b -> Sum $ if b then 1 else 0) votes
        in Just (mempty, State (GovState (if ayes >= requiredVotes then newLaw p else oldLaw) mph Nothing) stateValue)

    _ -> Nothing

getLaw :: GovState -> BuiltinByteString
getLaw (GovState (Law l) _ _) = l

-- | The main contract for creating a new law and for voting on proposals.
contract ::
    AsGovError e
    => Params
    -> Contract () Schema e ()
contract params = forever $ mapError (review _GovError) endpoints where
    theClient = client params
    endpoints = selectList [initLaw, addVote, checkLaw]

    addVote = endpoint @"add-vote" $ \(tokenName, vote) ->
        void $ SM.runStep theClient (AddVote tokenName vote)

    initLaw = endpoint @"new-law" $ \law -> do
        let mph = Scripts.forwardingMintingPolicyHash (typedValidator params)
        void $ SM.runInitialise theClient (GovState law mph Nothing) (Ada.lovelaceValueOf 1)
        let tokens = Haskell.zipWith (const (mkTokenName (baseTokenName params))) (initialHolders params) [1..]
        void $ SM.runStep theClient $ MintTokens tokens

    checkLaw = endpoint @"check-law" $ \l -> do
                maybeState <- SM.getOnChainState theClient
                case maybeState of
                        -- Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=(GovState law mph Nothing)}}, _)
                        Nothing
                            -> error ()
                        Just (SM.getStateData -> (GovState law mph Nothing), _)
                            -> do if l == getLaw (GovState law mph Nothing) then void $ SM.runStep theClient $ Check else error ()
                        Just (SM.getStateData -> (GovState law mph (Just (Voting p oldMap))), _)
                            -> do if l == getLaw (GovState law mph Nothing) then void $ SM.runStep theClient $ Check else error () --
                                    -- void $ SM.runStep theClient $ Check -- error ()
                        _ -> do return ()
                                -- changed runStep void $ SM.runStep theClient $ Check to return ()

               {-
                case maybeState of
                        -- Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=(GovState law mph Nothing)}}, _)
                        Nothing
                            -> error ()
                        Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=(GovState law mph Nothing)}}, _)
                            -> if l == getLaw (GovState law mph Nothing) then void $ SM.runStep theClient $ Check else error ()
                        Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=(GovState law mph (Just (Voting p oldMap)))}}, _)
                            -> if l == getLaw (GovState law mph Nothing) then void $ SM.runStep theClient $ Check else error () --
                                    -- void $ SM.runStep theClient $ Check -- error ()
                        _ -> return ()
                                -- changed runStep void $ SM.runStep theClient $ Check to return ()
                    -}
{-
    checkLaw = endpoint @"check-law" $ \l -> do
                SM.getOnChainState theClient >>= \s
                    -> case s of
                        -- Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=(GovState law mph Nothing)}}, _)
                        Nothing
                            -> error ()
                        Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=(GovState law mph Nothing)}}, _)
                            -> void $ SM.runStep theClient $ Check
                        _ -> void $ SM.runStep theClient $ Check
                        -- void $ SM.runStep theClient $ Check
                        --void $ SM.runStep theClient $ Check
-}
    --getOnChainState

-- | The contract for proposing changes to a law.
proposalContract ::
    AsGovError e
    => Params
    -> Proposal
    -> Contract () EmptySchema e ()
proposalContract params proposal = mapError (review _GovError) propose where
    theClient = client params
    propose = do
        void $ SM.runStep theClient (ProposeChange proposal)

        logInfo @Text "Voting started. Waiting for the voting deadline to count the votes."
        void $ awaitTime $ votingDeadline proposal

        logInfo @Text "Voting finished. Counting the votes."
        void $ SM.runStep theClient FinishVoting

PlutusTx.makeLift ''Params
PlutusTx.unstableMakeIsData ''Law
PlutusTx.makeLift ''Law
PlutusTx.unstableMakeIsData ''Proposal
PlutusTx.makeLift ''Proposal
PlutusTx.unstableMakeIsData ''Voting
PlutusTx.makeLift ''Voting
PlutusTx.unstableMakeIsData ''GovState
PlutusTx.makeLift ''GovState
PlutusTx.unstableMakeIsData ''GovInput
PlutusTx.makeLift ''GovInput

-- covIdx :: CoverageIndex
-- covIdx = getCovIdx $$(PlutusTx.compile [|| mkValidator ||])
