---
title: Why Haskellers should be interested in Smart Contracts
date: September 16, 2016
---

### Why Haskellers should be interested in 'Smart Contracts'

For most Haskellers the phrase 'smart contract' might bring up some vague
inclinations of contracts in languages like Racket, but the term is increasingly
coming to mean an interesting way of writing and executing stored procedure
programs on an immutable distributed database. The technology is very early but
is piquing the interesting of people like myself who see an amazing potential of
the ideas, but also the peril and human cost of building the technology on top
of a set of foundations that lacks rigor and discipline.

**Foundations**

There are a few definitions of smart contracts, that most applicable here is:

> Smart contracts are executable programs run on top of an immutable distributed
> database whose inputs and outputs are maintained globally consistent by a
> distributed consensus protocol.

In particular I'm not constraining the definition to contracts that run on a
*blockchain* which is a specific minimal implementation of a distributed
database which has certain properties that are amenable to creation of so-called
cryptocurrencies. Probably the biggest turnoff from looking at this technology
is that the community around the technology is populated by particularly vocal
cryptoanarchists with fringe views. And while those people do exist, there are
also a lot of people like myself who are interested in the technology
independent of the currency component, for it's pure applications in database,
programming language theory, and distributed systems. 

On the industrial side of the space there are several emerging platforms on
which to deploy smart contracts:

1. Ethereum
1. Ripple Codius
1. Mastercoin
1. Intel Sawtooth
1. Hyperledger Fabric
1. R3 Corda
1. Raft with Stored Procedures

With the exception of Ethereum most of the platforms are not in a usable state
and some of which are quite likely vaporware.

**Technical Details**

The current state of the art, [Ethereum is a public
blockchain](https://solidity.readthedocs.io/en/develop/introduction-to-smart-contracts.html#overview)
that embed a Turing-complete virtual machine that can be scripted in a language
known as Solidity.

In PL parlance an Etheruem-flavored smart contract is basically a Smalltalk
object that allows message passing through transactions on a blockchain. The
code is deployed by it's owner and then anyone on the network can interact it
with it by message passing which may result in data or state changes on the
global network. The execution occurs on the 'miners' on the network who execute
the contract code with the inputs and outputs specified in the transaction and
compute a solution to a RAM-hard SHA inversion problem called Ethash which adds
the new block to the chain and updates the global state of contract network.
Whichever miner solves the problem first gets to append the newly hashed block
and the process continues ad-infinitum with the global network converging on
consensus.

From a programming language perspective this introduces the non-trivial
constraint that programs must necessarily terminate. The current implementation
accomplishes this by attaching a cost semantics to each opcode in the virtual
machine that expends a finite resource called 'gas' that is a function of the
current block. In programming language space smart contracts are necessarily
[**total programs**](https://en.wikipedia.org/wiki/Total_functional_programming)
meaning they must probably terminating.

Solidity, while being an interesting proof of concept, is dangerously
under-contained and very difficult to analyze statically.  As a case in point, I
gave a talk on this subject to room full of veteran programmers (database and
operating system architects) and even after walking through the basic structure
of the code none of the them could figure out where the bug in this basic code
was.

```javascript
contract Coin {
    mapping (address -> uint) balances;
    
    function Coin() {}

    function() { 
        balances[msg.sender] += msg.amount;
    }

    function sendAll(address recipient){
        if (balances[msg.sender] > 0) {
            balances[recipient] = balances[msg.sender];
            balances[msg.sender] = 0;
        }
    }

    function withdraw() {
        uint toSend = balances[msg.sender];
        bool success = msg.sender.call.value(toSend)();
        if (success) 
            balances[msg.sender] = 0; // Vulnerable to call-stack attack
    }
}
```

The basic structure of this contract is the construction of a token which is
pegged to a specific amount of the ambient currency on Ethereum called 'ether'.
The contract allows people to exchange the values, send their balance to another
recipient or withdraw their balance back. the ``function()`` behaves like
Smalltalk's ``message-not-understood`` and handles calls that don't invoke a
method a contract.

The particular problem with this contract is that the ``send`` in the withdraw
function is particularly dangerous to an exploit when invoked from a malicious
contract which repeatedly call into the contract and then implement a default
function which calls withdraw repeatedly until the maximum call-stack (1023) of the
contract is reached and the balance is never zeroed-out.

```javascript
contract Malicious {
    Coin toAttack = Coin(coin_address);
    bool shouldAttack = true;

    function Malicious() { 
      toAttack.call.value(msg.amount)(); 
    }


    function() { 
        if (shouldAttack && msg.sender == coin_address) {
            shouldAttack = false;
            toAttack.withdraw();
        }
    }

    function attack(){ 
      toAttack.withdraw(); 
    }
}
```

This is a subtle bug and really indicates how difficult it is to reason about
these kind of contracts are to analyze using current schools of thought. 

**DAO**

I first heard about the DAO contract from the [New York
Times](http://www.nytimes.com/2016/05/22/business/dealbook/crypto-ether-bitcoin-currency.html?_r=0)
from a friend of mine who works in venture who was very excited about the model
of *distributed autonomous organizations* and automated venture pools .  The
article outlined all of the systemic and social structural problems and casually
made the somewhat prescient remark "Young, complex machines tend to have flaws
and vulnerabilities that you can't anticipate". Two weeks later the same class
of bug as the above code was exploited and the contract was compromised in one
of the more spectacular failures around this new technology. Of note is that
underlying protocol behaved exactly as specified, and the bug was simply a fact
of the contract language not *making invalid states unrepresentable* and making
it too hard to reason about.

At the heart of this statement is getting at the deeper problem of "How we do we
know what the code will do before we run it". The answer to this question is
obvious to most of us who have read the literature of programming language
semantics and it's associated theories of verification, but this still remains a
fairly niche domain in CS education and we're seeing the manifestation of that
in projects like Solidity which are making the same mistakes of the past instead
of standing on the shoulders of work that is already done.

If anything, the precedent after the DAO-hack is that software verification is
no longer purely in the realm of academics and hobbyists and the latest work in
dependent type theory, model checkers, and types has suddenly found immediate
relevance that needs no further explanation other than to prevent these kind of
catastrophic failures from happening again.  More importantly in the Haskell
ecosystem we have an abundance of riches with regards to tools for software
verification from tools like QuickCheck, SBV, and best in class support for
compile design and domain language implementation. 

**Future**

It's an exciting new emerging field and more interesting for us, the ideas and
technology is precisely at the intersection of Haskell's strengths and would
drastically benefit from the enthusiasm and expertise of people who are willing
to dabble in the more formal side of programming. The maturation of technology
is likely to occur in 2017 but the foundations are being laid this year. The
technology is a bit early, but hopefully some of the light are going off in your
head when you consider the exciting new applications of programmable distributed
ledgers endowed with the strengths of modern tools like Haskell.
