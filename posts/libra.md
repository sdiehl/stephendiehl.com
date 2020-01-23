---
title: Facebook Libra is Architecturally Unsound
date: November 2, 2019
---

### Facebook Libra is Architecturally Unsound

Coming out of blogging retirement with a post that diverges from my usual nerdy
pursuits about Haskelling and maths. I've spent the last few years working on
financial technology in the EU and I felt it was appropriate to write on what I
see as a undercovered topic in tech journalism. 

In the last few months Facebook has released what they claim is a new financial
services platform called Libra. Libra aims to be digital settlement system based
on a basket of international currencies that are managed on a "blockchain" and
held in a cash pool governed in Switzerland. The stated goals of the project are
lofty and have massive geopolitical consequences.

There are many sensible articles written in the [Financial
Times](http://ftalphaville.ft.com/2019/06/19/1560920406000/Facebook-s-Libra-will-not-help-the-unbanked/) and [New York
Times](https://www.nytimes.com/2019/11/04/opinion/facebook-libra-cryptocurrency.html)
about the unsound monetary and economic assumptions that underlie the proposed
financial structure, but there aren't enough technologists who have given their
analysis from a technical perspective. Not many people who work on financial
infrastructure speak publicly about their work and so this project has gotten
quite a bit of a pass in tech journalism. Financial journalism
really needs to do their due diligence on this project as the internals are
exposed for the world to see. For reference I am referring to the code is open
sourced at this [Github repository](https://github.com/libra/libra) and under
the [Calibra Organisation](https://github.com/calibra).

What is laid bare for the world to see is an architecturally schizophrenic code
artifact claiming to be a new reliable platform for global payment
infrastructure.  Yet the actual implementation diverges from this goal in
bizarre ways when one actually dives into the codebase.  I'm sure there is an
interesting story about the internal corporate politics of this project and as
such I thought it apt to do some diligence on what I see as a truly strange set
of architectural choices that break the entire system and put consumers at risk.

There is another technical review of this architecture from
[mcclure111](https://twitter.com/mcclure111/status/1142485680515366913) which
comes to similar conclusion about the apparent faux-innovation and mental
gymnastics this project does to pretend to be a decentralised blockchain when
it's actually just a slow replicated database for shadow banking.

I won't pretend to have an objective opinion about Facebook as a company. Few
people in tech view the company in a positive light anymore. Reading through the
publications released, it is clear there is a fundamental deception in the
stated goal and implementation of the project. Put concisely, this project will
not empower anyone. It is a pivot from a company whose advertising business is
so embroiled in scandal and corruption that it has no choice but to try to
diversify into payments and credit scoring to survive. The clear long term goal
is to act as a data broker and mediate consumers access to credit based on their
private social media data. This is such an utterly terrifying and dystopian
story that should cause more alarm than it does.

The only saving grace of this story is the artifact they open sourced is so
hilariously unsuited for the task they set out to do it can only be regarded as
an act of hubris. There are several core architectural errors in this project:

#### Libra's byzantine tolerance on a permissioned network is an incoherent design.

Byzantine fault tolerance is a fairly niche area of distributed systems research
that concerns the ability of a networked system to endure arbitrary failures of
its components while taking corrective actions critical to the system’s
operation. Networks that are byzantine tolerant must resist several types of
attacks including restarts, crashes, malicious payloads, and malicious voting in
leader elections. This design decision is central to Libra and it makes zero
sense.

The time complexity overhead from this additional structure varies based on
algorithms. There is a wide amount of literature where Paxos and Raft derivative
algorithms that have been enriched with Byzantine tolerant features but all of
these structures come with an additional communication overhead on top of the
$O(n^2)$ communication cost to maintain the quorum.  The algorithm chosen by
Libra still has a worst-case $O(n^2)$ communication cost in the case of
leadership failure. And occurs additional overhead from potential leadership
reelections on several types of network failure events. 

For a system that is designed to be run in a consortia of highly regulated
multinational corporates, all running Facebook signed code and access controlled
by Facebook it simply makes no sense to deal with malicious actors at the
consensus level. Why is this system designed to be byzantine tolerant at all
rather than just maintaining a consistent audit log for compliance checks.  The
possibility that a Libra node run by Mastercard or Andressen Horrowitz would
suddenly start running malicious code is such a bizarre scenario to plan for and
is better solved by simply enforcing protocol integrity and through
non-technical (i.e. legal) means.

In congressional testimony the product was stated as a challenger to emerging
international payment protocols such as WeChat, Alipay and M-Pesa. Yet none of
these systems are designed to run on byzantine tolerant pools of validators.
They are simply designed in the traditional high-throughput bus that orders
ledger transactions according to a fixed set of rules. This is the natural
approach to designing a payment system.  Preventing double-spends and forks
is simply not an issue that a properly designed payment rails should ever have
to deal with *by design*.

The overhead from the consensus algorithm serves no purpose and will only limit
throughput of the whole system, and appears to be there here no reason other
than apparently cargo culting public blockchain technology which is not designed
for this use case.

#### Libra has no transaction privacy.

By the admission of the whitepaper the system is designed to be *pseudononymous*
meaning the addresses used at the protocol are derived from elliptic curve
public keys and contain no metadata about the accounts.  Yet nowhere in the
governance structure description for the organisation or the protocol itself
does it indicate how the economic data involved in transactions would be
obscured from the validators. The system is designed to be a very large way of
replicating transactions to a number of external parties who under existing
European and US bank secrecy laws should not be privy to the economic details.

Data policies are difficult to coordinate across borders, especially with
disparate laws and regulations across jurisdictions having differing cultural
views on data protection and privacy. The protocol itself is *completely open to
consortia members* by default which is a clear technical design deficiency
unsuited to meet the requirements it was designed for.

#### Libra HotStuff BFT is not capable of achieving the throughput necessary for a payment rail.

In the United Kingdom clearing systems like BACs are capable of clearing
something on the order of 580,000,000 transactions a month. While highly tuned
systems like Visa are capable of achieving 150,000,000 transactions a day. The
performance of these systems is a function of the size of transactions, network
routing, system load and [AML (anti-money
laundering)](https://en.wikipedia.org/wiki/Money_laundering#Combating) checks.

For domestic transfers, the efficiency problems that Libra tries to solve,
aren't really problems in nation states which have modernised their clearing
infrastructure in the last decade.  For retail consumers in the European Union,
moving money is simply a non-issue.  It can be done simply with a standard
smartphone in seconds with traditional infrastructure. For large corporate
treasury department there are different mechanisms and regulations involved for
moving large quantities of money.

There is no technical reason that cross border payments could also not settle
instantly, except for the differences in rules and requirements across the
jurisdictions involved. If the required preventive measures (customer due
diligence, sanctions screening, etc) are completed multiple times at different
steps in the transaction chain this can delay the transaction. Nevertheless,
this delay is purely a function of the governing law and compliance rather than
the technology. 

For consumers there is no reason why a transaction in the United Kingdom won't
clear in seconds.  For retail transactions in the EU, they are really only
rate-limited by [KYC (know your
customer)](https://en.wikipedia.org/wiki/Know_your_customer) and AML constraints
imposed by governments and regulators which would equally apply to Libra
payments. Even if Facebook were to overcome the hurdles on international money
and private data movement, the model as proposed is hundreds of person-years
away from being able to handle global transaction throughput and would likely
have to be completely redesigned from first principles.

#### Libra's Move language is not sound.

The whitepaper makes a bold set of claims about a new untested language called
Move which are quite dubious from a programming language theory (PLT) perspective.
 
> “Move” is a new programming language for implementing custom transaction logic and “smart contracts”
on the Libra Blockchain. Because of Libra’s goal to one day serve billions of people, Move is designed with
safety and security as the highest priorities.

> The key feature of Move is the ability to define custom resource types with semantics inspired by linear
logic

In the public blockchains, smart contracts refer to logic deployed on public
networks which allows escrowing, laundering money, and the issuance of
extralegal securities and gambling products. These are typically done in a
shockingly badly designed language called Solidity, which from an academic PL
perspective, makes PHP look like a work of genius.  Oddly the new language
designed by Facebook seems to have no use case in common with these technologies
as it is effectively a scripting language designed for unclear corporate use cases.

In private distributed ledgers, smart contracts are one of those terms that are
thrown around by consultants without much regard for clear definitions or
purpose. Enterprise software consultants generally thrive on ambiguity and smart
contracts are the apotheosis of enterprise obscurantism because they can be
defined to mean literally anything.

On the nature of it's alleged safety we have to look at the language's
semantics. Soudness in PLT generally consists of two different proofs called
"progress" and "preservation" which determine the consistency of the whole space
of evaluation rules for the language. More concretely, in type theory a function
is "linear" if it consumes its argument exactly once and is "affine" if it
consumes it at most once. A linear type system gives a static guarantee that a
claimed linear function really is linear by prescribing types to all of
functions subexpressions and tracking call sites. This is a subtle property to
prove and requires quite a bit of machinery to do for whole program analysis.
Linear typing is still a very academic research area that has inspired
uniqueness typing in Clean and ownership typing in Rust. There are some
tentative proposals for adding linear types to Glasgow Haskell Compiler.

The claim of the Move language to use of linear types appears to be
unsubstantiated by a dive into the compiler as it [reveals no such typechecker
logic](https://github.com/libra/libra/tree/master/language/compiler). As far as
one can tell the whitepaper cites the canonical literature from Girard and
Pierce and does nothing of the sort in the actual implementation.

On top of this, the formal semantics of the supposedly safe language are nowhere
to be found in either the implementation or the paper. The language is small
enough that a full correctness proof of the semantics in Coq or Isabelle is
tractable.  Indeed, an end to end compiler which did a full proof-carrying
transformation down to bytecode is quite viable using modern toolchains invented
in the last decade. We've known how to do this since the work of [George Necula
and Peter Lee](http://www.eecs.berkeley.edu/~necula/Papers/tr96-165.ps.gz) all
the way back in 1996.

From a programming language theory perspective the claim that Move is sound and
secure is impossible to answer as the claims seem to reduce to nothing more than
handwaving and marketing rather than actual proof. This is an alarming position
for a language engineering project which expects the public to trust it to
handle billions of dollars.

#### Libra's cryptography engineering is unsound.

Building sound cryptosystems is a very difficult engineering problem and a
healthy dose of paranoia around cryptography is always the healthy attitude when
dealing with dangerous code.  There are major leaps forward in this space like
the Microsoft Everest project building a verifiably secure [TLS
stack](https://www.microsoft.com/en-us/research/project/project-everest-verified-secure-implementations-https-ecosystem/).
The tools to build verifiable primitives exist today and while expensive to do,
are certainly not outside the economic capacity of Facebook to build. Yet the
team has chosen not to on a project stated to be reliable for global finance. 

The libra project
[depends](https://github.com/libra/libra/blob/d74c33eeff695bd1fd3e51f4e536d4eb288f1070/crypto/crypto/Cargo.toml#L12)
on several fairly new "wild west" libraries for building experimental
cryptosystems that have only emerged in the last few years. It is impossible to
say whether the dependencies on the following tools are secure or not as none of
these libraries have had security audits nor do they have standard practice
disclosure policies.  Several core libraries in particular are indeterminate
about their vulnerabilities to side channel and timing attacks.

The library gets even more experimental and ventures quite outside the
[Cryptography Standard
Model](https://en.wikipedia.org/wiki/Standard_model_(cryptography)) by folding
in very novel techniques like verifiable random functions, bilinear pairings and
threshold signatures. These techniques and libraries might be sound, but the
amalgamation of all of them into one system should raise some serious concerns
about attack surface area. The combination of all of these new tools and
techniques makes the burden of proof much higher.

It should be assumed this entire crypto stack is vulnerable to a variety of
attacks until proven otherwise. The "move fast and break things" model should
not apply to cryptographic tools handling consumer financial data.

#### Libra has no capacity for consumer protection mechanisms.

A defining feature of a payment rail is the ability to reverse transaction in
case payments need to be undone by legal action or if they result in accidental
or system malfunction. The Libra system is designed to have "total finality" and
does not include a transaction type to reverse a payment. In the United Kingdom
all payments between £100 and £30,000 are governed Consumer Credit Act. This
means the payment provider has equal responsibility and liability with the
seller if there’s a problem with the items bought or the payee fails to
render services. There are similar regulations across EU, Asia and North
America.

The current Libra design includes no protocol to comply with consumer protection
laws and has no clear plan to build one. Even worse, from an data architecture
the finality of the core authenticated data structure based on a Merkle
accumulator state admits no mechanism to build this into the core ledger without
a redesign of the core.

***

The final conclusion one must take away after doing technical due diligence on
this project is this simply that it would not pass muster in any respected
journal on distributed systems research or financial engineering. Before trying
to disrupt global monetary policy there is a massive amount of a technical work
needed to build a reliable network the public and regulators could trust to
securely handle user data.

I see no reason to believe that Facebook has done the technical work needed to
overcome these technical issues in their project, not does it have any technical
advantage over existing infrastructure that already works. <b style="color:
#650000">Claiming one's company needs regulatory flexibility to explore
innovation is not an excuse for not doing it in the first place.</b>
