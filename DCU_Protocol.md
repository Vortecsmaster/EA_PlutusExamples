NOTE: //'d lines are alternative design routed discussed during the lecture and kept in the file as reminders of those paths we didn't take

ASSUMPTION: Datums are checked exactly. Data Types are what they claim to be. There are no additional fields. All data is bounded by reasonable use. 

# Example Lending Platform

## Actions

- Create Union (Closed Union)
- Ask for Loan
- Approve Loan
- Sieze Collateral
- Repay Loan
- Close Union

## Script Outline

( Thread Token -> Participation Token )
( Treasury Script -> Participation Token )

( Thread Token -> Treasury Script )
( Debt Position -> Treasury Script )
( Proxy/Inbox Script -> Treasury Script )

( Thread Token -> Borrow Request )
( Treasury Script -> Borrow Request )

( Proxy/Inbox Script -> Debt Position )
( Tread Token -> Debt Position )

## Transaction Outline

### (Tx 1) Creation of a Union 

Inputs:

User 1 (X ADA)
User 2 (Y ADA)

Outputs:

Treasury [ participationPolicy = Parts, threadID = ID ] ( X+Y ADA, 1 Thread Token [ID] )
User 1 ( X Prats [ID] )
User 2 ( Y Parts [ID] )

## Logic Outline

### Thread Token

- Looks at first input utxo and uses its UtxoRef as TokenName

### Participation Token

- Looks at first input utxo and uses its UtxoRef as TokenName
- Can be only created when instantiating a treasury
- // Check that amount of ADA deposited to first output = amount minted
- Check that amount of ADA deposited to Treasury Script = amount minted
- threadID of Treasury = ID of Participation Token 
- participationPolicy = ownPolicy

### Treasury Script

Datum:
participationPolicy :: ValidatorHash
threadID :: TokenName

Redeemer:
ApproveLoan ( borrowID :: )
CloseUnion 
ClawBack
GetMessage 

- will look at its continuing output by threadID
- on close, will burn Thread Token 
- disallow transactions with Debt Position, Proxy/Inbox Script
- on ClawBack, (do logic for collateral claim, incl. allow Debt Position + vote or something) 
- on GetMessage, ( pull money out of proxy/inbox )

### Borrow Request

Datum:
owner: Address
ask: Value
treasuryID: TokenName

Redeemer:
Cancel
Approve

- Wants owner to recieve ask (// from treasuryID)

### Debt Position

Datum:
owner: PubkeyHash
ask: Value
interest: Value
treasuryID: TokenName

Redeemer:

ClawBack 
- treasuryID token holder can 'open' the box (token present on input) 
// - recieve message via proxy/inbox from ireasuryID

PayBack
- pay to proxy/inbox script (uniq = UtxoRef of self)
- owner signs the tx
// - owner pays ask to treasuryID (utxo on putput with ID - on input with ID = ask+interest)

### Proxy/Inbox Script

Datum:
id: TokenName
source: TokenName ( how to verify that source is what it claims to be so that we can have a message passing pattern fully sound -- homework )
message: DATA
uniq: ByteString

- id holder can 'open' the box (token present on input)

## Potential Vulnerabilities

- Double Satisfacion on Loan Grant 
- Double Satisfaction on Treasury Repay (+) 
- Debt Position Flash Loan (+)
- Proxy/Inbox Flash Loan (+)
- Datums Bounded (+)
- Dust Attack
- Concurrency / DoS
- Staking Loss
- Ghost Voting / Actions
- Reference Loss 