---
title: Cooking Classes with Datatype Generic Programming
date: March 3, 2016
---

### Cooking Classes with Datatype Generic Programming

Haskell Generics are a somewhat misunderstood topic but are an extremely
powerful technique for writing reusable and comparable interfaces across an
enormous universe of types with very little effort. They are probably my
favorite example of the advantages of an expressive type system endows us with.

Source for examples code is [available
here](https://github.com/sdiehl/cooking-generics).

Generics are a form of *datatype-generic programming*, which although the
namesake has some similarity to Java Generics they are different concepts
entirely. GHC's implementation of Generics fall out of the simple observation
that all datatypes in Haskell can be written as a combination of a *sum of
products*.

A **sum type**, is a data structure used to hold a value that could take on
several different, but fixed, types. For example:

```haskell
data Pastry
  = Turnover
  | Macaroon
  | Brownie
  | Cookie
```

A **product type**, is a data structure used to hold a fixed ordered set of
several types. Selecting a single field is called projection.

```haskell
data Person = Person
  { firstName       :: String
  , lastName        :: String
  , age             :: Int
  , height          :: Float
  , phoneNumber     :: String
  , flavor          :: String
  }
```

<!--
In C the approximation of these concepts is in *struct* and *union* types.

<img src="/images/memory_layout.png"></img>
-->

In Haskell all datatypes can be expressed as sums of products:

```haskell
data Expr
  = Add { l :: Expr, r :: Expr }
  | Mul { l :: Expr, r :: Expr }
  | Sub { l :: Expr, r :: Expr }
  | Div { l :: Expr, r :: Expr }
  | Number { val :: Int }
```

During compilation most of the information about the structure of the datatypes
is thrown out, by design Haskell erases all type information. Prior to
type-checking a phase known as **elaboration** expands out all record selectors
into toplevel functions which extract the named fields of a product. 

```haskell
data Point a b = Point { x :: a, y :: b }
```

```haskell
x :: Point a b -> a
x (Point a _) = a

y :: Point a b -> b
y (Point _ b) = b
```

The rest of the information about products largely gets thrown out after
compilation, and the product just get expanded into pattern matching code. For
sum types the only information that is kept around is the tag for each
constructor of the sum type. For instance ``Add`` is assigned the tag 1, ``Mul``
is assigned 2 etc. In a case statement the only information that is available at
runtime is which branch we're scrutinizing.

So what if consider not tossing out all this information and instead exposed it
to our program so that we could write generic logic that can introspect the
"structure" of our datatypes.

<!--
For instance if we were to translate the above expression language into a rough
approximation of how it's evaluated by the GHC runtime it would look like the
following C code:

```cpp
typedef struct T {
    enum { ADD, MUL, DIV, SUB, NUM } tag;
    union {
        struct {
            struct T *left, *right;
        } node;
        int value;
    };
} Expr;

int eval(Expr t)
{
    switch (t.tag) {
        case ADD:
            return eval(*t.node.left) + eval(*t.node.right);
            break;
        case MUL:
            return eval(*t.node.left) * eval(*t.node.right);
            break;
        case DIV:
            return eval(*t.node.left) / eval(*t.node.right);
            break;
        case SUB:
            return eval(*t.node.left) - eval(*t.node.right);
            break;
        case NUM:
            return t.value;
            break;
    }
}
```
-->

#### Compiler Hooks

Since GHC 6.10 we've had type families which, among other things, allow us to
associate data types with our typeclass. So the structure of our Generic class
can have a associated ``Rep`` type which can carry information along with the
typeclass.

```haskell
class Generic a where
  type Rep a :: * -> *
  from :: a -> (Rep a) x
  to :: (Rep a) x -> a
```

To represent the structure of our datatype we need to set up several datatypes
to encode, sums, products, empty branches and various metadata about the names
of fields, constructors and their types. All of which have a free parameter
``p`` which is bound to the head of typeclass instance when used in the
associated datatype ``Rep a``.

```haskell
data    V1        p                       -- Empty
data    U1        p = U1                  -- ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- Sum
data    (:*:) f g p = (f p) :*: (g p)     -- Product
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
newtype M1  i t f p = M1 { unM1 :: f p }  -- metadata wrapper
```

Now we could write this instance by hand for all of our datatypes, and for a
simple enumeration it would look like the following:

```haskell
data Ingredient
  = Flour
  | Sugar

instance Generic Ingredient where
  type Rep Ingredient = M1 D (T_Ingredient ((M1 C (C_Flour U1)) :+: (M1 C (C_Sugar U1))))

  from Flour = M1 (L1 (M1 U1))
  from Sugar = M1 (R1 (M1 U1))

  to (M1 (L1 (M1 U1))) = Flour
  to (M1 (R1 (M1 U1))) = Sugar

data T_Ingredient
data C_Flour
data C_Sugar
```

The instance here is purely mechanical and can be derived from GHC's internal
representation of it's syntax tree, namely the types
[GHC.DataCon](https://downloads.haskell.org/~ghc/7.10.2/docs/html/libraries/ghc-7.10.2/DataCon.html)
and
[GHC.TypeCon](https://downloads.haskell.org/~ghc/7.10.2/docs/html/libraries/ghc-7.10.2/TyCon.html).
Using the ``-XDeriveGeneric`` extension we can have GHC crank this typeclass out
automatically: 

```haskell
{-# LANGUAGE DeriveGeneric #-}

data Ingredient
  = Flour
  | Sugar
  deriving (Generic)
```

Lest we not handwave away the work that GHC is doing, let's actually recreate
the introspection logic that GHC uses when instantiating a Generic class from a
module's data definitions. Let's load a module dynamically, intercept the
compilation and dump out the internal structure of the it's datatypes to see how
this would be mechanically translated into a typeclass instance.


```haskell
import GHC
import GHC.Paths as Paths

import Name
import TyCon
import TypeRep
import DataCon
import HscTypes

import Text.Show.Pretty

main :: IO ()
main = do

  -- Inside the GHC Monad
  rep <- runGhc (Just Paths.libdir) $ do

    -- Spin up a GHC compiler environment
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags

    -- Make a dummy module to inject
    let mn = mkModuleName "Test"

    -- Make a dummy target
    addTarget Target {
      targetId = TargetModule mn
    , targetAllowObjCode = True
    , targetContents = Nothing
    }

    -- Run the GHC pipeline
    load LoadAllTargets
    modSum <- getModSummary mn
    p <- parseModule modSum
    t <- typecheckModule p

    -- Pluck out the module tycons after we're done type-checking
    DesugaredModule tcmod modguts <- desugarModule t
    let tycons = mg_tcs modguts

    -- Deconstruct all datatypes into their sums-of-products.
    return (deconstruct tycons)

  putStrLn (ppShow rep)
```

Now that we have access to GHC's internal representation of the "module guts" we
can write our deconstructor logic. The logic is a slim few hundred lines of
mostly ADT munging.

```haskell
deconstruct :: [TyCon] -> [Data]
deconstruct = fmap go
  where
    go x
      | isProduct x = M1 $ D Datatype
        { dataTypeName = getOccString (tyConName x)
        , modName      = modString x
        , isNewtype    = isNewTyCon x
        , datatype     = Product (mkProduct x)
        , recursive    = isRecursiveTyCon x
        }

      | isVoid x = M1 $ D Datatype
        { dataTypeName = getOccString (tyConName x)
        , modName      = modString x
        , isNewtype    = isNewTyCon x
        , datatype     = V1
        , recursive    = isRecursiveTyCon x
        }

      | otherwise = M1 $ D Datatype
        { dataTypeName = getOccString (tyConName x)
        , modName      = modString x
        , isNewtype    = isNewTyCon x
        , datatype     = Sum (mkProduct x)
        , recursive    = isRecursiveTyCon x
        }

mkRecord :: TyCon -> [Data]
mkRecord x = concatMap mkRProduct (tyConDataCons x)

mkProduct :: TyCon -> [Data]
mkProduct x = fmap go (tyConDataCons x)
  where
    go :: DataCon -> Data
    go x | isRecord x   = Product (mkRProduct x)
    go x | isDProduct x = Product (mkDProduct x)
    go x                = M1 (C (Constructor (conNames x)))

mkDProduct :: DataCon -> [Data]
mkDProduct xs = [K1 (showType x) | x <- dataConOrigArgTys xs]

mkRProduct :: DataCon -> [Data]
mkRProduct x = [M1 (S (Selector (getOccString fld)) ty) | (fld, ty) <- zip (fieldNames x) (mkDProduct x)]
```

Setting up the dummy module ``Test.hs`` to run our decompilation script:


```haskell
data PlatonicSolid
  = Tetrahedron
  | Cube
  | Octahedron
  | Dodecahedron
  | Icosahedron

data Person = Person
  { firstName       :: String
  , lastName        :: String
  , age             :: Int
  , height          :: Float
  , phoneNumber     :: String
  , flavor          :: String
  } deriving (Show)

data T
  = T1 { a :: Int, b :: Float }
  | T2 { c :: Int, d :: Double }
```

For ``PlatonicSolid`` we get the representation:

```haskell
M1
   (D Datatype
        { dataTypeName = "PlatonicSolid"
        , modName = "Test"
        , isNewtype = False
        , datatype =
            Sum
              [ M1 (C Constructor { conName = "Tetrahedron" })
              , M1 (C Constructor { conName = "Cube" })
              , M1 (C Constructor { conName = "Octahedron" })
              , M1 (C Constructor { conName = "Dodecahedron" })
              , M1 (C Constructor { conName = "Icosahedron" })
              ]
        , recursive = False
        })
```

For ``Person`` we get the representation:

```haskell
M1
    (D Datatype
         { dataTypeName = "Person"
         , modName = "Test"
         , isNewtype = False
         , datatype =
             Product
               [ M1 (S Selector { selName = "firstName" } (K1 "String"))
               , M1 (S Selector { selName = "lastName" } (K1 "String"))
               , M1 (S Selector { selName = "age" } (K1 "Int"))
               , M1 (S Selector { selName = "height" } (K1 "Float"))
               , M1 (S Selector { selName = "phoneNumber" } (K1 "String"))
               , M1 (S Selector { selName = "flavor" } (K1 "String"))
               ]
         , recursive = False
       })
```

For the sum of products ``T`` we get the representation:

```haskell
M1
  (D Datatype
       { dataTypeName = "T"
       , modName = "Test"
       , isNewtype = False
       , datatype =
           Sum
             [ Product
                 [ M1 (S Selector { selName = "a" } (K1 "Int"))
                 , M1 (S Selector { selName = "b" } (K1 "Float"))
                 ]
             , Product
                 [ M1 (S Selector { selName = "c" } (K1 "Int"))
                 , M1 (S Selector { selName = "d" } (K1 "Double"))
                 ]
             ]
       , recursive = False
       })
```

These data points are then used to generate the Rep instance in the derived
Generic instances. So that's a *rough approximation* of how ``-XDeriveGeneric``
works under the hood, nothing terribly complicated just book keeping.

#### GHC.Generics

From the internal representation we crank out several typeclass instances which
store the metadata about the various constructors.

```haskell
class Datatype d where
  datatypeName :: t d f a -> String
  moduleName   :: t d f a -> String
  isNewtype    :: t d f a -> Bool
  isNewtype _ = False

class Selector s where
  selName :: t s f a -> String

class Constructor c where
  conName :: t c f a -> String

  conFixity :: t c f a -> Fixity
  conFixity _ = Prefix

  conIsRecord :: t c f a -> Bool
  conIsRecord _ = False
```

For example, for ``Ingredient`` example from before, we'd have several
constructor instances automatically generated by which we could query the names
from the AST.

```haskell
type Rep Ingredient = M1 D (T_Ingredient ((M1 C (C_Flour U1)) :+: (M1 C (C_Sugar U1))))

data T_Ingredient
data C_Flour
data C_Sugar

instance Datatype T_Ingredient where
  datatypeName _ = "Ingredient"
  moduleName _ = "Main"

instance Constructor C_Flour where
  conName _ = "Flour"

instance Constructor C_Sugar where
  conName _ = "Sugar"
```

Unlike reflection in languages like Java, Generics are not pushing type
information into the runtime. Apart from a dictionary lookup for they are a
effectively free abstraction that has no overhead. We're simply making more
information from the compiler manifest in the types during the type-checking
phase, all of which gets erased during compilation.

#### Example

I tried to come up a non-contrived example for illustrating the usefulness of
generics, and there are plenty of examples (serializes for JSON, Protocol
Buffers, SQL Generation, traversals, command line parsers, etc) that are
well-documented elsewhere on the web. So let's consider an example based on the
silly pun in the title of this article, namely cooking typeclasses.

So we have a Pie type, naturally. 

```haskell
data Pie = Pie
  { filling :: Filling
  , topping :: Maybe Topping
  } deriving (Show, Generic)

data Filling = Apple | Cherry | Pumpkin
  deriving (Show, Generic)

data Topping = IceCream | WhipCream
  deriving (Show, Generic)
```

<a href="https://www.flickr.com/photos/theleggett/4409912246"><img src="/images/pie.jpg"></img></a>

Using generics we'd like to a generate a list of the types of pie that we can
put on a menu from the structure of the Haskell types. Records will denote named
variations ("filling" vs "topping") of the menu item, while sum types denote the
various options in the variations ("cherry filling" vs "apple filling").

```haskell
data Item
  = Item Text [Item]
  | Variant Text [Item]
  | Choice Text
  deriving (Show, Generic)
```

We implement a typeclass with a **default signature** which gives us the option
to manually specify how a type gets converted into a menu item, or fall back on
using it's generic representation to automatically generate it.

```haskell
class Menu a where
  menu :: a -> [Item]
  default menu :: (Generic a, GMenu (Rep a)) => a -> [Item]
  menu _ = gmenu (Proxy :: Proxy a)

gmenu :: forall a. (Generic a, GMenu (Rep a)) => Proxy a -> [Item]
gmenu _ = gopts (Proxy :: Proxy (Rep a))
```

Our generic menu operates over various GHC.Generics types to expand out the
sums and products into the Item categories that correspond to the menu. The
instance for ``GMenu (K1 R f)`` has a Menu constraint which allows manual
override for specific datatypes. Since we're passing around a proxy we'll have
to manually thread the dictionary around sometimes by passing an ``undefined``
cast to the type of the instance we need to resolve.

```haskell
-- Generic Menu
class GMenu a where
  gopts :: Proxy a -> [Item]

-- Datatype
instance GMenu f => GMenu (M1 D x f) where
  gopts _ = gopts (Proxy :: Proxy f)

-- Constructor Metadata
instance (GMenu f, Constructor c) => GMenu (M1 C c f) where
  gopts x
    | conIsRecord (undefined :: t c f a) =
      [Item (pack (conName m)) (gopts (Proxy :: Proxy f))]

    | otherwise = [Choice (pack (conName m))]
    where m = (undefined :: t c f a)

-- Selector Metadata
instance (GMenu f, Selector c) => GMenu (M1 S c f) where
  gopts _ = [Variant (pack (selName m)) (gopts (Proxy :: Proxy f))]
    where m = (undefined :: t c f a)

-- Constructor Paramater
instance (GMenu (Rep f), Menu f) => GMenu (K1 R f) where
  gopts _ = menu (undefined :: f)

-- Sum branch
instance (GMenu a, GMenu b) => GMenu (a :+: b) where
  gopts _ = gopts (Proxy :: Proxy a) ++ gopts (Proxy :: Proxy b)

-- Product branch
instance (GMenu a, GMenu b) => GMenu (a :*: b) where
  gopts _ = gopts (Proxy :: Proxy a) ++ gopts (Proxy :: Proxy b)

-- Void branch
instance GMenu U1 where
  gopts _ = []
```

Specifically we'll override the ``Maybe`` type so that it simply expands out to
a choice of either "AsIs" of the variant or just the list of choices endowed by
the inner parameter.

```haskell
instance Menu a => Menu (Maybe a) where
  menu _ = [Choice (pack "AsIs")] ++ (menu (undefined :: a))
```

As an example ``Maybe Topping`` expands out into three choices.

```haskell
menu (a :: Maybe Topping) ~ [Choice "AsIs", Choice "IceCream", Choice "whipCream"]
```

Using GHC 7.10's new ``-XDeriveAnyClass`` extension we can actually go back and
automatically derive Menu inside the deriving clause.

```haskell
data Pie = Pie
  { filling :: Filling
  , topping :: Maybe Topping
  } deriving (Show, Generic, Menu)
```


Now synthesizing a dictionary for Pie we can get a menu

```haskell
menu (undefined :: Pie)
```

And voila:

```haskell
[ Item
    "Pie"
    [ Variant
        "filling" [ Choice "Apple" , Choice "Cherry" , Choice "Pumpkin" ]
    , Variant
        "topping"
        [ Choice "AsIs" , Choice "IceCream" , Choice "WhipCream" ]
    ]
]
```

Since our logic is datatype generic, any Haskell we can write down can be
automatically translated to a Menu just by ``deriving Menu``. So now we can a
new ``Crisp``  desert (my favorite!) *and we get everything for free*!

```haskell
data Crisp = Crisp
  { contents :: Filling
  , temperature :: Temperature
  } deriving (Show, Generic, Menu)

data Temperature = Warm | Cold
  deriving (Show, Generic, Menu)

-- Add an instance for a pair of menu items. That expands into multiple items.
instance (Menu a, Menu b) => Menu (a,b) where
  menu _ = menu (undefined :: a) ++ menu (undefined :: b)
```

And we can generate the composite menu of both deserts:

```haskell
menu (undefined :: (Pie, Crisp))
```

```haskell
[ Item
    "Pie"
    [ Variant
        "filling" [ Choice "Apple" , Choice "Cherry" , Choice "Pumpkin" ]
    , Variant
        "topping"
        [ Choice "AsIs" , Choice "IceCream" , Choice "WhipCream" ]
    ]
, Item
    "Crisp"
    [ Variant
        "contents" [ Choice "Apple" , Choice "Cherry" , Choice "Pumpkin" ]
    , Variant "temperature" [ Choice "Warm" , Choice "Cold" ]
    ]
]
```

So that's generics. One of the best goto examples of how an expressive type
system and a few clever compiler hooks can make programmers lives easier by
cooking our boilerplate for us and giving tastier more correct code.

<a href="https://www.flickr.com/photos/theleggett/4409912246"><img src="/images/pie2.jpg"></img></a>
