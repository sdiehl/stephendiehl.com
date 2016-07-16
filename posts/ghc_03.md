---
title: Dive into GHC: Targeting Core
date: July 12, 2016
---

### Dive into GHC: Targeting Core

In the [last blog post](http://www.stephendiehl.com/posts/ghc_01.html), we
discussed the intermediate structures used in the  GHC compiler. This time let's
discuss the Core language.

<p style="text-align:center">
**[Accompaying Source Code](https://github.com/sdiehl/dive-into-ghc/tree/master/03-core)**
</p>

#### Core

GHC's most central data types. Core is a very small,
explicitly-typed, variant of System FC; a typed lambda calculus that differs
from the simply typed lambda calculus by the introduction of a mechanism of
universal quantification over types denoted as capital lambda $\Lambda$.

```haskell
-- System-F Notation
Λ b c a. λ (f1 : b -> c) (g : a -> b) (x1 : a). f1 (g x1)

-- Haskell Core
\ (@ b) (@ c) (@ a) (f1 :: b -> c) (g :: a -> b) (x1 :: a) -> f1 (g x1)
```

While the Haskell frontend is an implicitly-typed source language, Core is an
explicitly-typed language. Every binder has an explicit type, and terms include
explicit type abstractions and applications. 

To inspect the core from GHCi we can invoke it using the following flags and the
following shell alias. We have explicitly disable the printing of certain
metadata and longform names to make the representation easier to read.

```bash
alias ghci-core="ghci -ddump-simpl -dsuppress-idinfo \
-dsuppress-coercions -dsuppress-type-applications \
-dsuppress-uniques -dsuppress-module-prefixes"
```

At the interactive prompt we can then explore the core representation
interactively:

```bash
$ ghci-core
```

Then for example we can type in normal expressions and see their translation
into Core.

```bash
λ: let f x = x + 2 ; f :: Int -> Int

==================== Simplified expression ====================
returnIO
  (: ((\ (x :: Int) -> + $fNumInt x (I# 2)) `cast` ...) ([]))

λ: let f x = (x, x)

==================== Simplified expression ====================
returnIO (: ((\ (@ t) (x :: t) -> (x, x)) `cast` ...) ([]))
```

#### Core Syntax

Core is defined in the
[CoreSyn](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/CoreSyn.html)
module. The central datatype is ``Expr`` which holds the 10 core datatypes that
all Haskell expressions can be condensed down into.

```haskell
data Expr b
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion	
```

The pattern match logic for Core is broken down into several datatypes which
discriminate on at most one branch of a constructor. The process of translating
frontend case statements into Core case statements involves the process of
expanding pattern matches out into their a "splitting tree" of cases. The case
for the wild card pattern match is ``(DEFAULT, [], rhs)``.

```haskell
type Arg b = Expr b

type Alt b = (AltCon, [b], Expr b)

data AltCon 
  = DataAlt DataCon
  | LitAlt  Literal
  | DEFAULT
```

Notably on all the types there is a parameter ``b`` is the type of binders. The
binder type is a sum type containing a recursive binder and a non-recursive
binder. Bindings that are mutually recursive are encoded as a list of binders.

```haskell
data Bind b 
  = NonRec b (Expr b)
  | Rec [(b, (Expr b))]
```

For example the factorial function:

```haskell
fac :: Int -> Int -> Int
fac a 0 = a
fac a n = fac (n*a) (n-1)
```

... is expanded out into it's core binders is enclosed in a recursive binding.

```haskell
Rec {
fac :: Int -> Int -> Int
fac =
  \ (a :: Int) (ds :: Int) ->
    case ds of wild { I# ds1 ->
    case ds1 of _ {
      __DEFAULT ->
        fac (* @ Int $fNumInt wild a) (- @ Int $fNumInt wild (I# 1));
      0 -> a
    }
    }
end Rec }
```

There are several type synonyms provided by the ``CoreSyn`` module that expand
out the binder parameter into a convenient synonym.

```haskell
type TyVar = Var
type CoreExpr = Expr Var
type CoreBndr = Var
type CoreBind = Bind CoreBndr
type CoreProgram = [CoreBind]
```

As an aside the notation used in papers on the typing judgements for Haskell
syntax typically uses the following convention and has a one-to-one mapping to
each of the Haskell datatypes in ``Expr``.

$$
\begin{aligned}
e ::= \ & n                                              & \mathtt{Var}      &\quad \text{Variable} \\
    |\ & \mathtt{lit}                                   & \mathtt{Lam}      &\quad \text{Literal} \\
    |\ & e_1 \ e_2                                      & \mathtt{App}      &\quad \text{Application} \\
    |\ & \lambda n . e                                  & \mathtt{Lam}      &\quad \text{Abstraction} \\
    |\ & \textbf{let}\ \mathit{binding}\ \textbf{in}\ e & \mathtt{Let}      &\quad \text{Variable binding} \\
    |\ & \textbf{case}\ e\ \textbf{as}\ n\ \textbf{return}\ \tau\ \textbf{of}\ \overline{alt} & \mathtt{Case}     &\quad \text{Pattern match} \\
    |\ & e \triangleright \gamma                        & \mathtt{Cast}     &\quad \text{Cast} \\
    |\ & e_{\lbrace \textit{tick} \rbrace }             & \mathtt{Tick}     &\quad \text{Internal note} \\
    |\ & \tau                                           & \mathtt{Type}     &\quad \text{Type} \\
    |\ & \gamma                                         & \mathtt{Coercion} &\quad \text{Coercion} \\
\end{aligned}
$$

#### Var

The ``Var`` type is central to most of the core definitions, it is the primary
name type used in the later half of the compiler and contains is a synonym for
the ``Id`` type but it may additionally potentially contain type variables,

```haskell
data Var
  -- Type and kind variables
  = TyVar 
    { varName       :: Name
    , realUnique    :: FastInt
    , varType       :: Kind
    }

  -- Internal type variables used by inference algorithm
  | TcTyVar 
    { varName       :: Name
    , realUnique    :: FastInt
    , varType       :: Kind
    , tc_tv_details :: TcTyVarDetails
    }

  -- Value level identifiers
  | Id 
    { varName       :: !Name
    , realUnique    :: FastInt
    , varType       :: Type
    , idScope       :: IdScope
    , id_details    :: IdDetails
    , id_info       :: IdInfo
    }
```

The fields for Var also contain and important type which indicates the
provenance of the identifier.  A ``LocalId`` is bound within an expression such
as a lambda, case, or let binding. A ``GlobalId`` is either a top-level
expression or a imported data constructor, class, etc.

```haskell
data IdScope
  = GlobalId
  | LocalId ExportFlag
```

In addition to the scope there are two metadata records that give additional
information about the usage and type of the identifier. These are the ``IdInfo``
and ``IdDetails``. The ``IdDetails`` primary contains information about why the
variable is introduced while ``IdInfo`` contains metadata about optimizations
that may be applied during the core to core passes.

```haskell
data IdDetails
  = VanillaId
  | RecSelId {sel_tycon :: TyCon, sel_naughty :: Bool}
  | DataConWorkId DataCon.DataCon
  | DataConWrapId DataCon.DataCon
  | ClassOpId Class.Class
  | PrimOpId PrimOp.PrimOp
  | FCallId ForeignCall.ForeignCall
  | TickBoxOpId TickBoxOp
  | DFunId Int Bool
```

The structure of ``IdInfo`` is a set flags.

```haskell
data IdInfo = IdInfo 
  { arityInfo       :: ArityInfo
  , specInfo        :: SpecInfo
  , unfoldingInfo   :: Unfolding
  , cafInfo         :: CafInfo
  , oneShotInfo     :: OneShotInfo
  , inlinePragInfo  :: InlinePragma
  , occInfo         :: OccInfo
  , strictnessInfo  :: StrictSig
  , demandInfo      :: Demand
  , callArityInfo   :: !ArityInfo
  }
```

For the identifiers we'll construct we'll simply use the vanilla flavor of id as
they are simply from lambda expressions and toplevel functions.

```haskell
vanillaIdInfo :: IdInfo
vanillaIdInfo = IdInfo 
  { cafInfo           = vanillaCafInfo,
  , arityInfo         = unknownArity,
  , specInfo          = emptySpecInfo,
  , unfoldingInfo     = noUnfolding,
  , oneShotInfo       = NoOneShotInfo,
  , inlinePragInfo    = defaultInlinePragma,
  , occInfo           = NoOccInfo,
  , demandInfo        = topDmd,
  , strictnessInfo    = nopSig,
  , callArityInfo     = unknownArity
  }
```

A name combined with this metadata and a type uniquely constructs and Id/Var and
there are several helper functions that combine them together. 

```haskell
mkTyVar :: Name -> Kind -> TyVar
mkLocalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalVar :: IdDetails -> Name -> Type -> IdInfo -> Id
```
#### Example

Consider the following incredibly simple module:

```haskell
module Example (f) where

f :: a -> a
f x = x
```

Let's recreate it using Core constructions, as the functions we mentioned above.
We'll simulate the unique supply in a contrived way using ``mkUnique``, in
practice one shouldn't actually do this and instead simply initialize the supply
inside of IO.

```haskell
mkName :: Int -> String -> Name
mkName i n = mkInternalName (mkUnique 'n' i) (mkOccName OccName.varName n) noSrcSpan

xn :: Name
xn = mkName 0 "x"

an :: Name
an = mkName 1 "a"

fn :: Name
fn = mkExternalName (mkUnique 'n' 2) modl (mkOccName OccName.varName "f") noSrcSpan

-- a :: *
a :: TyVar
a = mkTyVar an anyKind

-- x :: a
x :: Var
x = mkLocalVar VanillaId xn (TyVarTy a) vanillaIdInfo

-- f :: a -> a
fv :: Var
fv = mkGlobalVar VanillaId fn (FunTy (TyVarTy a) (TyVarTy a)) vanillaIdInfo

def :: [Syn.CoreBind]
def = [Syn.NonRec fv f]

f :: Syn.Expr Var
f = Syn.Lam x (Syn.Var x)

modl :: Module
modl = mkModule mainPackageKey (mkModuleName "Example")
```

So now we have a synthetic Core module that we can package up into a ``ModGuts``
and ``ModSummary`` just like what we'd get from the top half of the compiler
after Typechecking. For most of these fields we simply initialize them with
default dummy values.

```haskell
guts :: ModGuts
guts = ModGuts
  {
      mg_module          = modl,
      mg_exports         = [Avail fn],
      mg_deps            = noDependencies,
      mg_dir_imps        = emptyModuleEnv,
      mg_used_names      = mkNameSet [fn],
      mg_used_th         = False,
      mg_rdr_env         = emptyGlobalRdrEnv,
      mg_fix_env         = emptyFixityEnv,
      mg_tcs             = [],
      mg_insts           = [],
      mg_fam_insts       = [],
      mg_patsyns         = [],
      mg_rules           = [],
      mg_binds           = def,                  -- our binding
      mg_foreign         = NoStubs,
      mg_warns           = NoWarnings,
      mg_hpc_info        = NoHpcInfo False,
      mg_modBreaks       = emptyModBreaks,
      mg_vect_decls      = [],
      mg_vect_info       = noVectInfo,
      mg_boot            = False,
      mg_anns            = [],
      mg_inst_env        = emptyInstEnv,
      mg_fam_inst_env    = emptyFamInstEnv,
      mg_safe_haskell    = Sf_None,
      mg_trust_pkg       = False,
      mg_dependent_files = []
  }
```

```haskell
summ :: DynFlags -> ModSummary
summ dflags = ModSummary 
  {
      ms_mod          = modl,
      ms_hsc_src      = HsSrcFile,
      ms_location     = ModLocation {
          ml_hs_file  = Nothing
      ,   ml_hi_file  = "Example.hi"
      ,   ml_obj_file = "Example.o"
      },
      ms_hs_date      = UTCTime (toEnum 0) 0,
      ms_obj_date     = Nothing,
      ms_iface_date   = Nothing,
      ms_srcimps      = [],
      ms_textual_imps = [],
      ms_hspp_file    = "Example.hs",
      ms_hspp_opts    = dflags,
      ms_hspp_buf     = Nothing
  }
```

#### Code Generation

So now that we have our synthetic core module we can run the normal compiler
pipeline on it and compile it just like we would with source code. The
transformation of Core to native code goes through several processes:

1. [CorePrep](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/CorePrep.html)
1. [CoreToStg](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/CoreToStg.html)
1. [Core Linting (Optional)](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/CoreLint.html)
1. [Stg2Stg](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/SimplStg.html)
1. [Code Generation](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/AsmCodeGen.html)

The main pass is called the Simplifier, which maps Core to Core. It's job it is
to perform a large collection of correctness-preserving transformations, with
the goal of producing a more efficient program. 

1. Constant folding
1. Applying the rewrite rules
1. Inlining
1. Case of case optimization
1. Case of known constructor optimization
1. Eta expansion and Eta reduction

In addition to optimization GHC contains a pass called the Core Linter which
does a variety of internal consistency checks on a given Core program. Since
Core is explicitly typed, typechecking a given program is trivial as we simply
check that the type of identifiers matches with their bindings and that the
types are internally consistent. The Core linter is extremely useful if you're
writing optimization passes that transform Core or are targeting it from another
language (other than vanilla Haskell). The linter pass can prevent a whole slew
of bugs before they manifest as code generation errors.

So now we'll run the rest of the compiler pipeline and generate assembly code
from our module. There's a bit of song and dance in converting between types but
the process is basically just connecting the inputs and outputs of the above
passes together.

```haskell
main :: IO ()
main = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags

  setSessionDynFlags $ dflags { hscTarget = HscAsm, ghcLink = LinkBinary }

  dflags <- getSessionDynFlags
  env <- getSession

  setTargets [Target 
    { targetId = TargetModule (mkModuleName "Example")
    , targetAllowObjCode = True
    , targetContents = Nothing }]

  -- repares for code generation.
  prep <- liftIO $ corePrepPgm env (ms_location (summ dflags)) (mg_binds guts) (mg_tcs guts)

  -- Transform Core into STG
  stg <- liftIO $ coreToStg dflags (mg_module guts) prep

  -- STG Transformer
  (stg_binds2, cost_centre_info) <- liftIO $ stg2stg dflags (mg_module guts) stg

  -- Generated Cmm
  let cmms = codeGen dflags (mg_module guts) (mg_tcs guts) cost_centre_info stg_binds2 (mg_hpc_info guts)

  -- Initialize a name supply for the Cmm pipeline
  us <- liftIO $ mkSplitUniqSupply 'S'
  let initTopSRT = initUs_ us emptySRT
      run_pipeline = cmmPipeline env

  -- Collect the Cmm code stream after running the pipeline.
  let cmmstream = do
       a <- Stream.mapAccumL run_pipeline initTopSRT cmms
       Stream.yield (srtToData a)

  -- Prepare the Cmm for 
  genraw <- liftIO $ cmmToRawCmm dflags cmmstream

  -- Initialize name supply for the native code generator and generate x86 to a
  -- file from the prepared Cmm.
  ncg_uniqs <- liftIO $ mkSplitUniqSupply 'n'
  fname <- liftIO $ (openFile "Example.asm" WriteMode)
  liftIO $ nativeCodeGen dflags (mg_module guts) modloc fname ncg_uniqs genraw

  -- Dump the outputted Stg and  Cmm out
  gen <- liftIO $ Stream.collect cmmstream
  liftIO $ putStrLn "=== STG ==="
  liftIO $ putStrLn $ showGhc stg_binds2

  liftIO $ putStrLn "=== CMM ==="
  liftIO $ putStrLn $ showGhc gen
```

And then we get a ``Example.asm`` file outputted from our synthetic core module.
A next step project would be to target a more complicated language to GHC Core
to take advantage of it's compiler optimizations and native code generation.

#### Summary & Next Steps

We mentioned Stg and Cmm only briefly. The next time we'll dive into the
structure of these languages, how to read their output, the big ideas that
inform their design and their construction as Haskell types.
