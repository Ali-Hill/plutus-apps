State Machine Notes.
----------------------


In the blockchain, 

- the state machine will be represented by a UTxO sitting at the state machine address. 
- The state of the machine will be the datum of that UTxO. 
- A transition will be a transaction that consumes the current state, using a redeemer that characterizes the transition, and then produces a new UTxO at the same address, where the datum now reflects the new state.

A StateMachine has two type parameters, s and i, which stand for state and input. These correspond to datum and redeemer, respectively.

Transition is: 
- GovState is the Datum / State
- GovInput is the input

It is a record type with four fields. Probably the most important one is smTransition, which defines which transitions can move which states which other states.

` The State s type is basically the datum. It consists of the state itself and a value. `

The first component of the tuple specifies additional constraints that the transaction that does this must satisfy. Until now, we have only seen constraints in off-chain code.

Given the state type s, and a transaction that tries to consume this UTxO with a redeemer i, we can indicate that this transition is not allowed by returning Nothing. If it is allowed, we return a tuple.

The second component of the tuple is the new state (the new datum and value), which is the new UTxO sitting at the same address, with the first UTxO having been consumed.

