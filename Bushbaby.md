## Bushbaby Protocol Description

### Purpose

Bushbaby is a conceptually simple peer-to-peer gossip protocol to replicate Bamboo data structures.  It is still under heavy development and subject to breaking changes.

### Encoding requirements

- [Bamboo](https://github.com/AljoschaMeyer/bamboo) - append-only log
	- Represents the underlying data which is replicated in this protocol 
- [Base62](https://ieeexplore.ieee.org/document/6020065) - numeric encoding
	- Makes ED25519 identity keys more human-friendly
- [BLAKE2](https://www.blake2.net) - fast secure hashing
	- Encodes comparable values which are not to be sent in the clear
- [CBOR](https://www.rfc-editor.org/rfc/rfc8943) - Compact Binary Object Representation
	- Serializes object data (primarily arrays) for transfer
- [NaCl](https://nacl.cr.yp.to) - Networking and Cryptography Library
	- Secures the peer communications
- [STLV](https://github.com/AljoschaMeyer/stlv) - Simple Type-Length-Value
	- Wraps protocol packets for simplified parsing

### Key Concepts

Nodes are true peers. There is no concept of client/server based on "who called whom" or "who requested what."  As such, there is no request/response cycle.  Once the handshake is complete, each peer may choose to "make known" whatever data it chooses in the "replication state".

Peers participate in a "clump" which identifies the provenance of the logs to be replicated.  Clump identifiers are raw binaries of indeterminate length.  Ergonomically, they are generally expected to be human-readable strings but this is not a requirement.

Each protocol packet sent is wrapped in STLV for simplicity of parsing.  The type numbers are described below.

Peers are encouraged to simply drop connections which send malformed protocol messages.  There is little-to-no value in communicating error states outside of information leakage.

This document may describe behavior exhibited by the reference implementation.  These are, indeed, descriptive and not normative statements.

### Nonce boxes

The majority of communications (everything except "HELLO") are sent in so-called "nonce boxes".
These are simply a concatenation of a 24-byte nonce and NaCl secret box which uses that nonce. These are wrapped via STLV with the appropriate integer message type.

The process is fairly straightforward, once "send" and "receive" keys are established in the "AUTH" state.

Constructing an outgoing message consists of:
 
	- Generating the message
	- Selecting a random nonce
	- Creating a secret box containing the message, secured with the selected nonce and "send key"
	- Wrapping the concatenated nonce and secretbox in the appropriate STLV message type

Unpacking an incoming message consists of:

	- Decoding the STLV protocol packet
	- Extracting the nonce from the first 24 bytes
	- Opening the secret box containing the message, secured with the provided nonce and "receive key"
	- Handling the contained message

Nodes should take care neither to send nor accept repeated nonces in a given connection.  This can indicate (and make one vulnerable to) a replay attack.

### Handshake

Each Bushbaby peer is presumed to have a long-term ED25519 identity by which it wishes to be known.  It may or may not publish logs under this identity.  Even if not, having a stable long-term identity will promote better connectivity.  Peers may choose not to connect with unknowns or take other message-dropping measures which hurt propagation.

An ephemeral Curve25519 key pair is created per-connection to secure the communications in that connection.  This handshake section describes that key exchange.

#### Type 1 message -- "HELLO"

The "HELLO" message is the introduction between peers. It consists of an STLV-encoded message of type `1` containing the concatenation of:

	- Long term identity public key
	- Selected ephemeral public key for this connection
	- HMAC of the clump identitifier in which this peer is participating with a key of this node's ephemeral public key

A received "HELLO" packet allows the peer to confirm a shared clump identifier and construct the necessary values for authentication.

##### Packet construction

- Generate an ephemeral Curve25519 key pair for this session: `our_epk`, `our_esk`
- Compute an HMAC (`hmac`) with the `clump_id` as message and `our_epk` as the key
- Concatenate the long-term public Ed25519 key (`our_pk`), `our_epk`, `hmac` into the `hello_message`
- Wrap the `hello_message` into an STLV type `1` message

##### Packet interpretation

- Unwrap the type `1` STLV message
- Extract the first 32 bytes as the peer's longterm Ed25519 key (`their_pk`)
- Extract the next 32 bytes at the peer's ephemeral Curve25519 key (`their_epk`)
- Extract the final 32 bytes as the handshake HMAC `hmac`
- Verify the `hmac` with the `clump_id` in which we are participating as the message and `their_epk` as the key

#### Type 2 message -- "AUTH"

The "AUTH" message is used to establish a trusted channel between peers which have exchanged "HELLO" messages.

Two keys are derived from the available information.

The "send key" will be used to create secret boxes readable by the peer. It is the 256-bit Blake2b hash of the Curve25519 secret derived from:

	- Node's own ephemeral secret key
	- Curve25519 equivalent of the peer's long-term ED25519 key

 The "receive key" will be used to unbox such received boxes. It is the 256-bit Blake2b hash of the Curve25519 secret derived from:

	- Curve25519 equivalent of the node's long-term ED25519 key
	- Peer's ephemeral public key

The clump identifier is concatenated with the receive key and signed with the node's longterm key.  This is sent in a "nonce box" STLV-encoded message of type `2`.

A received "AUTH" packet allows the peer to confirm agreement on identities and keys to be used throughout the replication connection.

##### Packet construction

- Compute the `send_key` via
	- scalarmult of `our_esk` with the Curve25519 equivalent of `their_pk`
	- Hash to 32 bytes with Blake2b
- Compute the `recv_key` via
    - scalarmult of the Curve25519 equivalent of `our_pk` with `their_epk`
	- Hash to 32 bytes with Blake2b
- Prepare `auth_msg` as the concatenation of the `clump_id` and `recv_key`
- Compute a detached signature for `auth_msg` with  long-term Ed25519 secret key  (`our_sk`)
- Build an STLV type `2` nonce box as above.

##### Packet interpretation

- Unwrap the type `2` STLV message
- Unpack the nonce box as above
- Verify the signature
	- Packet data is the signature	
	- Message to verify is the concatenation of `clump_id` and `send_key`
	- Signing key is `their_pk`	

### Replication 

Once the handshake is complete, peers enter the replication state. The replication messages are used to exchange metadata about logs and the logs themselves.  Each message is an independent "nonce box" with a CBOR-encoded array of data.  While implementations may use internal state for decision-making with regard to traffic, there is no requirement to maintain any state with respect to replication messages.

#### Type 5 -- "HAVE"

This message type indicates the logs which the node has stored locally and is willing to share.  Its data portion should be an array of length 3 arrays containing:

	- Base62-encoded ED25519 source identity
	- integer log id
	- integer maximum available sequence number

###### Packet creation

- Prepare a list of stored data as an array of arrays `have_list`:
	- [[base62-encoded source identity, integer `log_id`, integer maximum `seq`],...]
- CBOR-encode `have_list`
- Build an STLV type `5` nonce box as above

##### Packet interpretation

- Unwrap the type 5 STLV message
- Decode the CBOR contents, ensuring there is no error or extraneous data

#### Type 6 -- "WANT"

This message type indicates the logs which the node is interested in acquiring.  Its data portion should be an array of arrays.  The arrays should consist of a base62-encoded source identity and integers for the other values.

Each sub-array should be of length 1-4, indicating:

	- [source identity] - Full logs for the source identity
	- [source identity, log id] - Full log for source identity's log id
	- [source identity, log id, n] - The certificate pool from sequence number `1` to `n`
	- [source identity, log id, from, to] - All entries spanning `from` - `to`

##### Packet creation

- Determine an array of log specification arrays to request `want_list`:
	- [[base62-encoded source identity], [base62-encoded source identity, `log_id`],  [base62-encoded source identity, `log_id`, seq],  [base62-encoded source identity, `log_id`, `start_seq`, `end_seq`],...]
- CBOR-encode `want_list`
- Build an STLV type `6` nonce box as above

##### Packet interpretation

- Unwrap the type `6` nonce box
- Decode the CBOR contents, ensuring there is no error or extraneous data

#### Type 8 -- "BAMBOO"

This message type contains an array of Bamboo objects.  Each array element should be entry data concatenated with the payload.  There is no specific requirement on how to order these arrays or group them into protocol packets.  It is, however, worth noting that each element will require peer verification.  Providing them in individual "chain groups" ordered from lowest to highest sequence number facilitates the easiest management.  Therefore this makes it the most likely that the peer will accept and propagate the log.

##### Packet creation

- Obtain an array of the binary form of log data `bamboo_data`:
	- Be sure it is ordered from low-to-high `seq` to ensure verifiability
	- Binary form is a straight concatenation of the log entry header and body content
- CBOR-encode the `bamboo_data` array
- Build an STLV type `8` nonce box as above

##### Packet interpretation

- Unwrap the type `8` nonce box
- Decode the CBOR contents, ensuring there is no error or extraneous data 

### Peering Notes

It is worth noting, once again, that there is no request/response cycle in the protocol. Each message stands on its own. This might lead to deadlocks between ill-behaved peers.  Idle timeouts should be implemented to close connections which do not seem to be advancing propagation.

It is also likely advantageous for a node to maintain information about the behavior of its peers. Such information can help determine appropriate handling of future connections and messages. 

- A node may send HELLO either immediately upon connect or wait for peer identification
- A node may send AUTH either immediately upon HELLO or wait for its peer to do so first
- A node may choose not to share its HAVE list and merely "push" logs it wishes to "promote."
- A node may choose to ignore its peer's WANT list and aggressively "hoard" logs it sees announced

Some of this may be seen as anti-social behavior, but that is for the node operator's discretion, not a protocol-level concern.

Since Bamboo packets may contain any sort of data in any kind of order, peers may well wish to curate which items they choose to persist and propagate.  No node is under any obligation to store-and-forward any particular data.

Bushbaby is content-agnostic.  There are no references to using the content of the logs themselves within this protocol.  Applications may wish to define interactions with implementations based on log contents.  Such concerns are outside the scope of this protocol.

The preferred response to all bad-actor behavior/ill-formed protocol messages from a peer is disconnection.  There is little to be gained (and potentially much to be lost) from further communication or "error reporting."
