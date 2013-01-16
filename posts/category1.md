---
title: Categorical Programming: Introduction
date: September 8, 2012
---

### Forward

This is a multipart blog post on the structure of the mathematical
field of Category theory and how it relates to real world
programming.

Category theory distills the essence of a large variety of constructions
in traditional set theory to more abstract context which can be used
to reason about general concepts in computer science and mathematics.

If one were to try and describe the essence of category theory, it is
that it is study of relationships between between mappings of elements,
but at a level of generality where there is no notion of individual
elements. Instead elements arise only out of their relationships between
each other.

This tutorial is a humble attempt to provide a bridge between the dense
literature and an easy introduction to category theory for Haskell
programmers.

### Categories

A **category**  $ \\C $ is a construction with four definitions:

1. A collection of **objects**. Written $ \\text{ob}(\\C) $.

![Illustration](/images/objects.svg).

2. A collection of **morphisms**. Written $ \\text{hom}(\\C) $.

![Illustration](/images/morphisms.svg).

3. A **composition** operation $ ( \\circ ) $ or written in
   Haskell as $ ( . ) $. The composition of morphsism yields morphisms in $ \\C $.

    $$ ( f . g ) $$

    $$ ( f \circ g ) $$

![Illustration](/images/composition2.svg).

4. For each object $ A $ there is an **identity** morphism .

    $$ \text{id}_A $$

![Illustration](/images/identity.svg).


#### Category of Haskell types

In Haskell we're constantly dealing with categorical structures. In fact
they're baked the very foundations of the language translate, and a
large part of Haskell can be cast nicely into categorical terms. We call
this category **HASK**.

<table>
<th>Category Theory</th>
<th>Haskell</th>
<th>Examples</th>
<tr>
    <td>Objects</td>
    <td>Haskell Types</td>
    <td>

```haskell
Int
Bool
String
Tuple
```

</td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Haskell Functions</td>
    <td>
```haskell
head :: [Int] -> Int
map :: (Char -> Int) -> [Char] -> [Int]
```

</td>
</tr>

<tr>
    <td>Composition</td>
    <td>Function Composition</td>
    <td>

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

</td>
</tr>
</table>

The set of all non-polymorphic Haskell types forms a category
with Haskell functions as morphisms. For example:

```haskell
head :: [Int] -> Int
head (x:_) =  x

Prelude> head [1,2,3] = 1
```

\

The Haskell prelude also contains a function composition operator
which should be familiar to all.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
g . f =  \x -> g (f x)
```

\

And similarly a polymorphic identity function.

```haskell
id :: forall a. a -> a
id x =  x
```

\

Like most statically typed languages Haskell enforces types in
composition.

```haskell
f :: A -> B
g :: B -> C

g . f :: A -> C
```

#### Homsets

A category is constructed out a collection of morphisms, over
objects in the categotry. The collection of other morphisms in 
a category is called a **homset** and written as:

$$ \hom{\C} $$ 

A individual a morphism $ f $ has two associated values, its **domain**
$ \\text{dom}(f) $ and **codomain** $ \\text{cod}(f) $.

Elements in the homset are morphisms between objects in $ \\C $,
namely if $ A,B \\in \\ob{\\C} $ then the set of morphisms in
$ \\C $ between $ A $ and $ B $ is written:

$$ \hom{A,B} $$

For composition to be well defined we require that composition
itself be a mapping only defined for:

$$ ( \circ ) : \hom{B,C} \rightarrow \hom{A,B} \rightarrow \hom{A,C} $$ 

#### Formal Definition of Categories

I will use the following tabular format for the definition of categories
from here out. The first column is the type of construction, the second
column is the constraints on the objects involved in the definition, and
the third column is the definition of the construction.

<table>
<th>**Category**</th>
<tr>
    <td>Objects</td>
    <td> $$ X, Y \in \C $$ </td>
    <td></td>
</tr>

<tr>
    <td>Morphisms</td>
    <td> $$ f \in \hom{\C} $$ </td>
    <td> $$ f: X \rightarrow Y $$ </td>
</tr>

<tr>
    <td>Composition</td>
    <td>
    $$ 
        f : X \rightarrow Y \\
        g : Y \rightarrow Z  
    $$
    </td>
    <td> $$ g . f : X \rightarrow Z $$ </td>
</tr>

<tr>
    <td>Identities</td>
    <td> $$ \text{For all } X \in \C $$ </td>
    <td> $$ \text{id}_A : X \rightarrow X $$ </td>
</tr>

</table>


![Illustration](/images/composition.svg).

The corresponding definition table for a category is that of the
laws for the category. 

<table>
<th>**Category Laws**</th>
<tr>
    <td> Identity </td>
    <td> $$ 
        A \in \C \\
        f : A \rightarrow B
    $$</td>
    <td> $$ f . \text{id}_A  = \text{id}_B . f = f $$ </td>
</tr>
<tr>
    <td> Associativity </td>
    <td> $$ f,g,h \in \hom{\C} $$</td>
    <td> $$ ( h . g ) . f = h . ( g . f ) $$ </td>
</tr>
</table>

![Illustration](/images/category_law_1.svg).

![Illustration](/images/category_law_2.svg).

In textbooks these diagrams are often written in the common
diagramatic form:

![Illustration](/images/category_alt_law_1.svg).

![Illustration](/images/category_alt_law_2.svg).

#### SET

Categories are often written in bold. For example the category
**SET** is often a motivating topic of discussion since classical
set theoretic definitions are often generalized.

<table>
<th>**Set**</th>
<tr>
    <td>Objects</td>
    <td>Set: $ S $ </td>
    <td>........................</td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Total functions over $ S $ </td>
    <td>........................</td>
</tr>

<tr>
    <td>Composition</td>
    <td> $$ 
        f : A \rightarrow B \\ 
        g : B \rightarrow C 
    $$
    </td>
    <td> $$ 
        g \circ f : A \rightarrow C \\
        g \circ f = \lambda a. g (f a) 
    $$
    </td>
</tr>

<tr>
    <td>Identities</td>
    <td> $$ A \in S $$ </td>
    <td> 
    $$
        \text{id}_A :: A \rightarrow A \\
        \text{id}_A = \lambda x . x
    $$
    </td>
</tr>

</table>

With the usual properties:

<table>
<th>**Set**</th>
<tr>
    <td> Associativity </td>
    <td> $ ( f \\circ g ) \\circ h = f \\circ ( g \\circ h ) $ </td>
</tr>

<tr>
    <td> Identities </td>
    <td> $ \\text{id}\_B \\circ f = f \\circ \\text{id}\_A = f $ </td>
</tr>

</table>


If we define a toy ``Cat`` typeclass with the above definition
and define an instance for Haskell (->) from ``GHC.Prim`` we
have:

```haskell
type Hask = (->)

class Cat cat where
    ident :: cat a a
    comp  :: cat b c -> cat a b -> cat a c

instance Cat Hask where
    ident x  = x
    comp f g = \x -> f (g x)
```

We see that we of course recover the identitiy function and
compostion from the Prelude.

```haskell
instance Cat Hask where
    ident = id
    comp  = (.)

-- Equivalent definition from Control.Category

instance Category (->) where
    id = Prelude.id
    (.) = (Prelude..)
```

It is worth noting the common confusion that morphisms are not
functions. It is the other way around, in the category SET functions
are morphisms with objects as sets but this is a special case. In
general morphisms are a pure abstraction which have structural
similarity to functions.

#### Classes of categories

There are three classes of categories that are studied:

- **Concrete categories** - Model mathematical structures with
  structure-preserving mappings between objects. Examples: ( **Vec** )

- **Real world categories** - Model real world systems such as
  type systems and physical processes. Examples: ( **Hask** )

- **Abstract categories** - Categories studied for relationship
  to other categories or motivating the discussion of the large
  scale structure of mathematics. Examples: ( **Cat** )

\

Four out purposes we'll discuss a small set of **concrete categories**
that don't require much knowledge of abstract algebra.

THe notion of a **subcategory** is also important, it is a category
contained within another category which also satisfies the category
construction.

It is are illuminating to look at the trivial categories , namely
categories with small amounts of objects and morphisms to gain a general
understanding of their structure even though they are not of much
practical interest.

There are also some simple categories, the simplest being the
**Zero** category, which is the category with no objects and no morphisms.

<table>
<th>**Zero**</th>
<tr>
    <td>Objects</td>
    <td> </td>
    <td> $$ \emptyset $$ </td>
</tr>

<tr>
    <td>Morphisms</td>
    <td> </td>
    <td> $$ \emptyset $$ </td>
</tr>

<tr>
    <td>Composition</td>
    <td> \* </td>
    <td> \* </td>
</tr>

<tr>
    <td>Identities</td>
    <td> $$ \text{For all } A $$ </td>
    <td> $$ \text{id}_A : A \rightarrow A $$ </td>
</tr>

</table>

The category laws are vacuously true for this category.

A slightly more interesting ( only slightly! ) is the **One**
category.


![Illustration](/images/one.svg).

<table>
<th>**One**</th>
<tr>
    <td>Objects</td>
    <td> singleton set </td>
    <td> $$ {X} $$ </td>
</tr>

<tr>
    <td>Morphisms</td>
    <td> </td>
    <td> $$ f : X \rightarrow X $$ </td>
</tr>

<tr>
    <td>Composition</td>
    <td> </td>
    <td> $$ (f . f) = \text{id}_X $$ </td>
</tr>

<tr>
    <td>Identities</td>
    <td> $$ \text{For all } A $$ </td>
    <td>
    $$ 
        \text{id}_X : X \rightarrow X  \\
        \text{id}_X = \lambda x. X
    $$ </td>
</tr>

</table>

Since the only morphism in the category is also the identity all the
laws hold merely by substitution.

It is also trivially true that **Zero** is a subcategory of **One**.


<table>
<th>**Two**</th>
<tr>
    <td>Objects</td>
    <td> $$ {X,Y} $$ </td>
</tr>

<tr>
    <td>Morphisms</td>
    <td> </td>
    <td> 
    $$
    f : X \rightarrow X \\
    g : Y \rightarrow Y
    $$
    </td>
</tr>

<tr>
    <td>Composition</td>
    <td> </td>
    <td> $$ (f . f) = \text{id}_X $$ </td>
</tr>

<tr>
    <td>Identities</td>
    <td> $$ \text{For all } A $$ </td>
    <td>
    $$ 
        \text{id}_X : X \rightarrow X  \\
        \text{id}_X = \lambda x. X \\
        \text{id}_Y : Y \rightarrow Y  \\
        \text{id}_Y = \lambda x. X
    $$
    </td>
</tr>

</table>

![Illustration](/images/two.svg).

```haskell
{-# LANGUAGE GADTs #-}

import Prelude hiding (id, (.))
import Control.Category

data Zero a b where
    Idz :: Zero () ()

data One a b where
    Ida :: One a ()


instance Category Zero where
    id = Idz
    Idz . Idz = Idz

instance Category One where
    id = Ida
    Ida . Ida = Ida
```

#### Discrete Categories

Discrete categories are categories where the only morphisms are
identity morphisms.

```haskell
{-# LANGUAGE GADTs #-}

import Prelude hiding (id, (.))
import Control.Category

data Discrete n n where
    Auto :: Discrete n n

instance Category Discrete where
    id = Auto
    Auto . Auto = Auto
```

In a discrete category it follows that if there exists a
morphisms between objects $ A $ and $ B $ then $ A $ must equal $
B $. Discrete categories are often indexed by the cardinality of the
number of singleton objects in the category.

#### Algebraic Categories

From algebra there are several examples of categories.

<table>
<th>**Vec**</th>
<tr>
    <td>Objects</td>
    <td>Vector spaces</td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Linear mappings</td>
</tr>

<tr>
    <td>Composition</td>
    <td>Composition of linear mappings</td>
</tr>

<tr>
    <td>Identities</td>
    <td></td>
</tr>
</table>



<table>
<th>**Grp**</th>
<tr>
    <td>Objects</td>
    <td>Groups</td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Group homomorphisms</td>
</tr>

<tr>
    <td>Composition</td>
    <td>Composition of group homomorphsisms</td>
</tr>

<tr>
    <td>Identities</td>
    <td> Identity mappings </td>
</tr>
</table>

<table>
<th>**Mon**</th>
<tr>
    <td>Objects</td>
    <td>Monoids</td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Monoid homomorphisms</td>
</tr>

<tr>
    <td>Composition</td>
    <td>Composition of monoid homomorphsisms</td>
</tr>

<tr>
    <td>Identities</td>
    <td>Identity mappings</td>
</tr>
</table>

For example considering the Monoid type of Haskell string types
we find that this gives rise to a subcategory of **Hask**.
Consider the type definition for Monoid.

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

The instance for String types is:

```haskell
type String = [Char]

instance Monoid String where
        mempty  = []
        mappend = (++)
```

<table>
<th>**Str**</th>
<tr>
    <td>Objects</td>
    <td>Characters</td>
    <td>```Char```
</tr>

<tr>
    <td>Morphisms</td>
    <td>Strings</td>
    <td>```[Char]```</td>
</tr>

<tr>
    <td>Composition</td>
    <td>String concatention</td>
    <td>```(++)```</td>
</tr>

<tr>
    <td>Identities</td>
    <td>Empty string</td>
    <td>```""```</td>
</tr>
</table>

It is easy to show that the identity laws hold.

```haskell
a ++ ""  = "" ++ a == a
```

Concatetention is a linear mapping so it is also easy to show
that composition laws must also hold for all characters.

```haskell
(a ++ b) ++ c = a ++ (b ++ c)
```

In short we epxect order-independent append operations. For
example:

```haskell
"foo" ++ "bar" + "" ++ "baz" == "foobarbaz"
"foo" ++ "bar" ++ "baz"      == "foobarbaz"
"foo" ++ ("bar" ++ "baz")    == "foobarbaz"
("foo" ++ "bar") ++ "baz"    == "foobarbaz"
```

#### Logic

Deductive logic also forms a category with propositions as
objects and proof trees as morphisms between objects.

<table>
<th>Deductive Logic ( **Prf** )<th>
<tr>
    <td>Objects</td>
    <td>Propositions</td>
    <td>$ \\alpha,\\beta,\\gamma $</td>
</tr>

<tr>
    <td>Morphisms</td>
    <td>Proofs</td>
    <td>$ \\alpha \\vdash \\beta $</td>
</tr>

<tr>
    <td>Composition</td>
    <td>Proof Trees</td>
    <td>$$
    \frac{\alpha \vdash \beta \hspace{1em} \beta \vdash \gamma}{\alpha \vdash \gamma} 
    $$
    </td>
</tr>

<tr>
    <td>Identities</td>
    <td>Tautologies</td>
    <td>$$
    \frac{}{\alpha \vdash \alpha} 
    $$
    </td>
</tr>
</table>

#### Duality

The important results of category is the notion of **duality**. Simply
put for any theorem about a category $ \\C $ we can obtain a new theorem
by swapping domain and codomain of each morphism and changing the
argument order of composition we obtain a result that also holds over in
category $ \\C^\\text{op} $ referred to the **dual theorem**.

1) Objects of $ \\C^\\text{op} $ are identical to $ \\C $.
2) Morphisms of the form $ f : B \\rightarrow A $ in $ \\C^\\text{op} $ are the morphisms
$ f : A \\rightarrow B $ in $ \\C $. 
3) Compositions of the form $ g \\circ f $ in $ \\C^\\text{op} $ are of the form $ f \\circ g $ in $ \\C $.
4) Identities in $ \\C^\\text{op} $  are the same as in $ \\C $.

We can build dual categories in Haskell from categories

```haskell
{-# LANGUAGE TypeOperators #-}

data Op k a b = Op { unOp :: k b a }

instance Category k => Category (Op k) where
  id = Op id
  (Op f) . (Op g) = Op (g . f)
```

#### Terminal and Initial

Many categories have a special elements or classes of elements
where morphisms between objects are uniquely identified. For
example a the case where all objects in the category have a
single morphism between a single element, such an element is
called **initial**. For an initial object $ I \\in \\text{obj}(\\C) $
to be initial we have:

$$
\forall A \in \ob{\C} \hspace{1em} | \text{hom}(0, A) | = 1
$$

This is often written as an arrow with an excalamation point to
indicate a unique morphism.

$$
0 \xrightarrow{\hspace{1em}!\hspace{1em}} A.
$$

![Illustration](/images/initial.svg).

The dual notion is that of a **terminal** element $ 1 \\in \\ob{\\C} $.

$$
\forall A \in \ob{\C}  \hspace{1em} | \text{hom}(A, 1) | = 1
$$

$$
A \xrightarrow{\hspace{1em}!\hspace{1em}} 1.
$$

![Illustration](/images/final.svg).

Initial and terminal objects are unique up to isomorphism. For example
in the category **Set** the initial element is the null set while the
terminal element is the singleton set collapsing any set to a
singleton set.

$$
\begin{align}
x &\xrightarrow{\hspace{1em}!\hspace{1em}} \{x\} \\
\{\} &\xrightarrow{\hspace{1em}!\hspace{1em}} x
\end{align}
$$

In the category **One** the terminal object is also the initial
object, such a category is said to have a **zero object**.

In the category of vector spaces **Vec** the zero object is the
zero dimensional vector space.

The subject of whether **Hask** has initial and terminal objects is
a hairy issue that divides the implementation of Haskell language
from the category theoretic interpretation of Haskell. Although
the definitions below satisfy the requirements it is possible to
find counter examples in the Haskell runtime that would break the
neccessary properties. See the 
[void package](http://hackage.haskell.org/package/void) for more 
details.

**Initial**

```haskell
newtype Void = Void Void

absurd :: Void -> a
absurd (Void a) = absurd a
```

**Terminal**

```haskell
data () = ()	

term :: forall a. a -> ()
term _ = ()	
```


The [category](https://github.com/ekmett/categories/tree/master/Control/Category)
library defines this notation using fairly exotic type structures but
this concept can be modeled in Haskell's type system!

```haskell
class Category k => HasTerminalObject k where
    type Terminal k :: *
    terminate :: a `k` Terminal k

class Category k => HasInitialObject k where
    type Initial k :: *
    initiate :: Initial k `k` a
```
