---
title: Dive into GHC: Intermediate Forms
date: July 11, 2016
---

### Dive into GHC: Intermediate Forms

In the [last blog post](http://www.stephendiehl.com/posts/ghc_01.html), we
discussed the high-level API used inside the GHC compiler. This time let's zoom
into the pieces that comprise the frontend of the compilation pipeline, namely:

1. **Parsing**
1. **Renaming**
1. **Typechecking**
1. **Desugaring**
1. **Simplification**
1. Code Generation

<p style="text-align:center">
**[Accompaying Source Code](https://github.com/sdiehl/dive-into-ghc/tree/master/02-parser)**
</p>

#### Passes

These steps are that are used inside of the main compiler driver functions in
``HscMain`` , and are comprised of the following interstitial transformations
which manipulate the internal representation of Haskell module into a form that
can be compiled into machine code or interpreted.

```haskell
-- Parse a module.
parseModule :: GhcMonad m => ModSummary -> m ParsedModule

-- Typecheck and rename a parsed module.
typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule

-- Desugar a typechecked module.
desugarModule :: GhcMonad m => TypecheckedModule -> m DesugaredModule

-- Extract ModGuts
coreModule :: DesugaredMod m => m -> ModGuts

-- Generated STG bindings from Core module
coreToStg :: DynFlags -> Module -> CoreProgram -> IO [StgBinding]
```

#### Parser

The frontend syntax for Haskell is fairly large and is partially defined in the
[Haskell 2010
specification](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010)
and extended by GHC. The parser is written using two custom tools:

1. *Alex*, for lexical analysis. See: [Lexer.x](https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x)
2. *Happy*, for the parser itself. See: [Parser.y](https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y)

Happy is a parser generator system for Haskell, similar to the tool `yacc' for
C. It works as a preprocessor with it's own syntax that generates a parse table
from two specifications,  a lexer file and parser file. The Happy parser
consists of a list of a production rules and a monad to running the parser in.
Production rules consist of a set of options on the left and generating Haskell
expressions on the right with indexed variables (``$1``, ``$2``, ...) mapping to
the ordered terms on the left (i.e. in the second term ``term`` ~ ``$1``,
``term`` ~ ``$2``).

```perl
terms
    : term                   { [$1] }
    | term terms             { $1 : $2 }
```

The most complicated part of the lexer is with regards to whitespace-sensitive
layout. To handle whitespace the lexer will analyze the line position of certain
productions and will output *virtual tokens* (vbrace, vsemi, etc) which are used
to canonicalize the syntax. For example the use of layout rule in do-notation:

```haskell
main = do
  putStrLn "Hello"
  putStrLn "Haskell"
```

Is converted into explicit virtual braces and semicolons at parse time.

```haskell
main = do { putStrLn "Hello"; putStrLn "Haskell" }
```

The main entry points are defined in the
[Parser](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/Parser.html)
and resolve into a monad ``P``  which holds state about the source buffer, and
filename and location. The resulting AST is called HsSyn and contains position
information preserved from the tokens.

```haskell
parseType :: P (LHsType RdrName)
parseExpression :: P (LHsExpr RdrName)
parseModule :: P (Located (HsModule RdrName))
```

#### Located

Frontend syntax in GHC carries position information along with it that can be
used in error reporting. Location information is defined by a ``Located`` type
which has a single type paramater for the AST element to be "located". Typically
type synonyms follow the convention that a "L" before the type indicates a
synonym for a located element. So a located ``HsExpr`` is a ``LHsExxpr``.

```haskell
data GenLocated l e = L l e
type Located e = GenLocated SrcSpan e
```

```haskell
unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenLocated l e -> l
getLoc (L l _) = l

noLoc :: e -> Located e
noLoc e = L noSrcSpan e
```

Most notably there is a functor instance for ``fmap``ing over a located element,
which modifies the underlying type preserving the location.

```haskell
instance Functor (GenLocated l) where
  fmap f (L l e) = L l (f e)
```

#### Frontend Syntax

The [frontend
syntax](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/HsSyn.html)
(called HsSyn) is spread across several data types which contain all possible
syntax elements generated from the parser. These are parameterized over the name
type so that transformations (renamer, etc) can alter the structure of bindings
and identifiers while preserving the syntax tree structure.

1. **HsModule** - Top-level structure for a module. Contains module name,
   exports, imports, and toplevel declarations.
1. **HsDecl** - Type, class, value, and interface signature decls
1. **HsBind** - Value bindings used for both top level and nested bindings.
1. **HsGroup** - A HsGroup is a group of related HsDecls.
1. **HsLit** - Literal value (Int, Char, etc)
1. **HsExpr** - A Haskell expression.
1. **HsType** - Frontend type annotation.
1. **Pat** - Pattern match in a case statement or toplevel binding.

There are many times of
[identifiers](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/Var.html#t:Var)
used in different phases of compilation. In order of generality:

1. **OccName** - Simplest name used, it holds the names as strings with the namespace that the name came from and position.
1. **RdrName** - Is the type of names that come directly from the parser. Contains information whether thane is unqualified or qualified (with module name).
1. **Name** -  Is the type of names that have had their scoping and binding resolved. They have an OccName but also a Unique that disambiguates Names
1. **Var** - Is the type of names that not only have a Name but also a Type. Vars are either global or local.

Names are disambiguated from each other by an internal unique numerical key
called a
[Unique](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/Unique.html#t:Unique)
, which are generated from a
[UniqSupply](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/UniqSupply.html)
. These are often embedded within the monad transformer ``MonadUnique`` layer of
the pass.

After typechecking we deal with Var and Id names which have the property that
the Name and Type can always be recovered using the following functions.

```haskell
varName :: Var -> Name
varType :: Var -> Kind
```

In addition, many lookup functions use a sum type ``TyThing`` which wraps
"things" in scope which have types, such as identifiers and type constructors.

```haskell
data TyThing
  = AnId Id
  | AConLike ConLike.ConLike
  | ATyCon TyCon
  | ACoAxiom (CoAxiom Branched)
```

#### Outputable

Inside the GHC API is a pretty printer which can be used to dump out a pretty
printed summary of a large number of the internal types. The structure of the
class is similar to ``Show`` typeclass.

```haskell
class Outputable a where
  ppr :: a -> SDoc
  pprPrec :: Rational -> a -> SDoc

showPpr :: Outputable a => DynFlags -> a -> String
```

To simplify our life a bit we can write a wrapper ``showGhc`` which uses the
default dynflags to dump out an instance of ``Outputable``.

```haskell
showGhc :: (Outputable a) => a -> String
showGhc = showPpr unsafeGlobalDynFlags
```

#### Tracing Compilation

Given the following example module we'd like to trace the transformation of the
intermediate forms as it's run through the compiler.

```haskell
module Example where

data Aniaml = Cat | Dog

add :: Int -> Int -> Int
add x y = x + y 
```

Using the machinery of the last post we import the module target, load it, and
then instead of adding it to an interactive context we explicitly call the
functions of the individual passes and dump our their structure to the screen.

```haskell
main :: IO ()
main = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted }

  target <- guessTarget "Example.hs" Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName "Example"

  pmod <- parseModule modSum      -- ModuleSummary
  tmod <- typecheckModule pmod    -- TypecheckedSource
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = coreModule dmod      -- ModGuts
  stg <- liftIO $ coreToStg dflags (mg_module core) (mg_binds core)

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc ( parsedSource pmod )

  liftIO $ banner "Renamed Module"
  liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

  liftIO $ banner "Typechecked Module"
  liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

  liftIO $ banner "Typed Toplevel Definitions"
  liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

  liftIO $ banner "Typed Toplevel Exports"
  liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )

  liftIO $ banner "Core Module"
  liftIO $ putStrLn $ showGhc ( mg_binds core )

  liftIO $ banner "STG"
  liftIO $ putStrLn $ showGhc stg
```

#### Parsing

From the module summary we can extract the underlying text stream of the source
file ``Example.hs``. Running the parser over this generates a *ParsedSource*
datatype which contains the HsSyn AST with raw OccNames.

```haskell
data ParsedModule = ParsedModule 
  { pm_mod_summary :: ModSummary
  , pm_parsed_source :: ParsedSource
  , pm_extra_src_files :: [FilePath]
  , pm_annotations :: ApiAnns
  }
```

```haskell
===============================Parsed Source===============================
module Example where
data Aniaml = Cat | Dog
add :: Int -> Int -> Int
add x y = x + y
```

***

#### Renaming

After parsing we run the renamer pass which is responsible for resolving scopes,
operator precedences, and unique'ifying all names so that clashes cannot occur.
The renamed resolves the ``RdrName`` into ``Name`` across all syntax elements.
In addition it annotates each name with whether they are qualified and global or
local to the current module. If a name is not in scope, compilation simply halts
here and reports the missing name.

```haskell
===============================Renamed Module===============================
Just (add :: Int -> Int -> Int
      add x y = x + y
      
      data Aniaml = Cat | Dog,
      [import (implicit) Prelude],
      Nothing,
      Nothing)
```

***

#### Typechecking

The renamed AST is then run through the typechecker which maps ``Name``s into
``Id`` and enriches them with inferred types using type inference. 

```haskell
data TypecheckedModule = TypecheckedModule 
  { tm_parsed_module :: ParsedModule
  , tm_renamed_source :: Maybe RenamedSource
  , tm_typechecked_source :: TypecheckedSource
  , tm_checked_module_info :: ModuleInfo
  , tm_internals_ :: (TcRnTypes.TcGblEnv, HscTypes.ModDetails)
  }
```

The reuslting structure is a a typechecked module which contains the toplevel
types of all bindings and all subexpressions.

```haskell
=============================Typechecked Module=============================
{AbsBinds [] []
   {Exports: [add <= add
                <>]
    Exported types: add :: Int -> Int -> Int
                    [LclId, Str=DmdType]
    Binds: add x y = (+) x y}}
```

The resulting datatypes has a field for ``ModuleInfo`` which contains various
metadata about the typing environment, the names in scope, and typeclasses
exported.

```haskell
data ModuleInfo = ModuleInfo 
  { minf_type_env :: HscTypes.TypeEnv
  , minf_exports :: NameSet.NameSet
  , minf_rdr_env :: Maybe RdrName.GlobalRdrEnv
  , minf_instances :: [ClsInst]
  , minf_iface :: Maybe ModIface
  , minf_safe :: SafeHaskellMode
  , minf_modBreaks :: ModBreaks
  }
```

Using the ``coreModule`` we can also construct the gruesomely named ``ModGuts``
datatype which contains an expanded form of all the various exports and types
with associated metadata.

```haskell
data ModGuts = ModGuts 
  { mg_module :: !Module,
  , mg_boot :: IsBootInterface
  , mg_exports :: ![Avail.AvailInfo]
  , mg_deps :: !Dependencies
  , mg_dir_imps :: !ImportedMods
  , mg_used_names :: !NameSet.NameSet
  , mg_used_th :: !Bool
  , mg_rdr_env :: !RdrName.GlobalRdrEnv
  , mg_fix_env :: !FixityEnv
  , mg_tcs :: ![TyCon]
  , mg_insts :: ![ClsInst]
  , mg_fam_insts :: ![FamInst]
  , mg_patsyns :: ![PatSyn.PatSyn]
  , mg_rules :: ![CoreSyn.CoreRule]
  , mg_binds :: !CoreSyn.CoreProgram
  , mg_foreign :: !ForeignStubs
  , mg_warns :: !Warnings
  , mg_anns :: [Annotations.Annotation]
  , mg_hpc_info :: !HpcInfo
  , mg_modBreaks :: !ModBreaks
  , mg_vect_decls :: ![CoreSyn.CoreVect]
  , mg_vect_info :: !VectInfo
  , mg_inst_env :: InstEnv.InstEnv
  , mg_fam_inst_env :: FamInstEnv.FamInstEnv
  , mg_safe_haskell :: SafeHaskellMode
  , mg_trust_pkg :: Bool
  , mg_dependent_files :: [FilePath]
  }
```

The ModuleInfo can be queried for any type of information about types or names
at this point. For instance if we were interesed in the types of a global name
we could look it up as a ``TyThing`` inside the typing environment.

```haskell
modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoExports :: ModuleInfo -> [Name]
```

```haskell
=========================Typed Toplevel Definitions=========================
[Type constructor ‘Aniaml’, Data constructor ‘Cat’,
 Data constructor ‘Dog’, Identifier ‘add’, Identifier ‘Cat’,
 Identifier ‘Dog’]
```

```haskell
===========================Typed Toplevel Exports===========================
[Aniaml, Cat, Dog, add]
```

***

#### Core

The Core language is GHC's most central data types. Core is a very small,
explicitly-typed, variant of System FC. The output of the ``typecheckModule``
function is a datatype ``CoreModule`` which contains the Core program as the
typing environment.

```haskell
data CoreModule = CoreModule 
  { cm_module :: !Module
  , cm_types :: !HscTypes.TypeEnv
  , cm_binds :: CoreSyn.CoreProgram
  , cm_safe :: SafeHaskellMode
  }
```

Looking at the core module we see that each name in the program is now annotated
with an explicit type and typeclass dictionaries have been inserted.

```haskell
================================Core Module================================
[add :: Int -> Int -> Int
 [LclIdX, Str=DmdType]
 add = \ (x :: Int) (y :: Int) -> + @ Int $fNumInt x y]
```

Core then is translated into a intermediate form that is amenable to various
analysis passes that optimize the program for code generation. This form is an
abstract machine known a *Spineless Tagless G-Machine*.  In this syntax every
closure has an associated *Static Reference Table* or SRT. More on this later.

```haskell
====================================STG====================================
[add :: Int -> Int -> Int
 [LclIdX, Str=DmdType] =
     \r srt:SRT:[r6ry :-> $fNumInt] [x y] + $fNumInt x y;]
```

***

#### Summary & Next Steps

We mentioned Core and STG only briefly. The process by which frontend syntax is
transformed into Core will be our next topic. The design of Core defines a great
deal of aspects about how the language is structured and informs the type system
as well as code-generation.
