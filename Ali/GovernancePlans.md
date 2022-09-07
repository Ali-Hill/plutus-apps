Governance
----------

* When the contract starts it produces a number of tokens that represent voting rights.
* Holders of those tokens can propose changes to the state of the contract and vote on them.
* After a certain period of time the voting ends and the proposal is rejected or accepte

# Schema 

`
type Schema =
    Endpoint "new-law" ByteString
        .\/ Endpoint "add-vote" (TokenName, Bool)
`

There are two actions that can be taken. New-law and add-vote

1. New Law

New law is used to create a new law and distibute voting tokens. 

2. Add Vote 

Add vote is used to vote on a proposal with the name of the voting token and a boolean to vote in favour or against
 
# Parameters 
n
`
-- | The governace contract parameters.
data Params = Params
    { baseTokenName  :: TokenName
    -- ^ The token names that allow voting are gennerated by adding an increasing number to the base token name. See `mkTokenName`.
    , initialHolders :: [PaymentPubKeyHash]
    -- ^ The public key hashes of the initial holders of the voting tokens.
    , requiredVotes  :: Integer
    -- ^ The number of votes in favor required for a proposal to be accepted.
    }
`
# Validator 

A transaction is valid if 

` machine :: Params -> GovernanceMachine
machine params = SM.mkStateMachine Nothing (transition params) isFinal where
    {-# INLINABLE isFinal #-}
    isFinal _ = False
`

