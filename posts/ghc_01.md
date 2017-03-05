---
title: Dive into GHC: Pipeline
date: June 21, 2016
---

### Dive into GHC: Pipeline

After reading [Simon's](https://ghc.haskell.org/trac/ghc/blog/ContributingToGhc)
call for more volunteer writing about GHC I thought it would be timely to share
some knowledge I've accumulated over the years about working with the with GHC
internals.

I'm by no means an expert on GHC internals, but I have worked with them a fair
bit for several projects and the deep dive style of blog posts tends to be a
good format for helping ease into exploring the code for themselves.  Often
times simply a high-level overview and a small bit of runnable example code is
enough to encourage further involvement with an open source project and this
what I aim to write.

So begins a multipart writeup on the structure of GHC structured around several
examples that use the GHC API for some small project that shows off some
internal structure of the compiler.

<p style="text-align:center">
**[Accompaying Source Code](https://github.com/sdiehl/dive-into-ghc/tree/master/01-pipeline)**
</p>

***

#### Official Commentary

GHC core developers have actually spent a great deal of time over the sharing
knowledge about the design of the compiler. Some good places to start are the
following:

1. [GHC Commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary)
1. [OPLSS: Adventures in Types](https://www.youtube.com/watch?v=6COvD8oynmI)
1. [2006 GHC Hackathon Videos](https://ghc.haskell.org/trac/ghc/wiki/AboutVideos)
1. [David Terei's Notes](https://davidterei.com/talks/2016-03-cs240h/ghc-compiler.html)
1. [Takenobu Tani's GHC Illustrated](https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf)
1. [Edward Yang's Blog](http://blog.ezyang.com/2011/04/tracing-the-compilation-of-hello-factorial/)

On top of this there is a literature trail going back 25 years that shows how
the historical context and the research that led up to GHC today. 

1. [GHC Reading List](https://ghc.haskell.org/trac/ghc/wiki/ReadingList)

#### Toplevel

GHC is a quirky beast of a codebase, but as far compilers go it is a fairly
well-engineered and documented project if you know where to look. Yes, it uses a
somewhat idiosyncratic convention in places, but after all it is a 20-year old
codebase.

To get the source for the compiler clone the official repo:

```bash
$ git clone --recursive git://git.haskell.org/ghc.git
$ cd ghc/
```

There are many utilities included with the compiler the encompass documentation
and the build system, but the important toplevel directories for the compiler
itself are primarily:

```bash
├── rts          # The Haskell runtime systems
├── compiler     # The Haskell compiler logic
├── includes     # Header files for runtime and code generation
└── libraries    # The base libraries and Prelude source
```

For this post we'll concern ourselves with the ``compiler`` folder.

```bash
├── basicTypes   # Types used across all modules
├── cbits        # Misc C utilities
├── cmm          # Cmm langauge definitions
├── codeGen      # Cmm Compilers
├── coreSyn      # Core language definitions
├── deSugar      # Desugarer
├── ghci         # Interactive shell
├── hsSyn        # Frontend syntax
├── iface        # Interface files
├── llvmGen      # LLVM Code generator
├── main         # Compiler driver logic and options
├── nativeGen    # Assemblers for x86 / SPARC / PPC
├── parser       # Frontend Parser for HsSyn
├── prelude      # Wired-In Types /  Primops and Builtins
├── profiling    # Runtime profiing tools
├── rename       # Frontend renamer
├── simplCore    # Core-To-Core simplifier
├── simplStg     # Stg-To-Stg simplifier
├── specialise   # Specialisation pass ( Eliminates Overloading )
├── stgSyn       # Stg Core Language
├── stranal      # Strictness Analyzer
├── typecheck    # Typechecker
├── types        # Type language, data constructors, and type families
├── utils        # Misc functions and core data sstructures
└── vectorise    # Vectorisation optimiations
```

#### GHC API

Since GHC is itself written in Haskell, GHC is effectively a large library the
encompasses the [GHC
API](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/index.html).
The toplevel module is simply called
[GHC](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/GHC.html)
and contains a namespace dump of many of the core types that drive the
*compilation pipeline*.

Beneath this is the main API for compiling plain Haskell source code called
[HscMain](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/HscMain.html)
which contains the various drivers for different passes within the compilation.
The six core passes make up the [compilation
pipeline](http://www.aosabook.org/images/ghc/hscpipe2.png):

1. Parsing
1. Renaming
1. Typechecking
1. Desugaring
1. Simplification
1. Code Generation

The result of this compilation is several artificats which are *object files*
(.o), *interface files* (.hi) and *executables*.

#### GHC Monad

The heart of the compilation process is stored within the GHC Monad, a state
monad that handles the internal session state of the compilation pipeline, error
handling and sequencing of multi-module compilation.

```haskell
newtype Ghc a = Ghc { unGhc :: Session -> IO a }
```

The abstract class ``GhcMonad`` provides a lifted version of the GHC monad
functions to get at the internal session objects from within the various
submonads used throughout compilation (renamer, typechecker, etc).

```haskell
class (Functor m, MonadIO m, ExceptionMonad m, HasDynFlags m) => GhcMonad m where
  getSession :: m HscEnv
  setSession :: HscEnv -> m ()
```

The evaluation function takes in a path to the ``libdir`` and returns the result
inside of ``IO``.

```haskell
runGhc :: Maybe FilePath -> Ghc a -> IO a
```

The filepaths are installation specific paths indicating the local installation
and paths to the GHC compiler. These are provided by the [ghc-paths](https://hackage.haskell.org/package/ghc-paths)
package.

```haskell
import GHC.Paths

libdir, docdir, ghc, ghc_pkg :: FilePath
```

At the heart of the session object is a very important structure called
``HscEnv`` which holds the internal state of compilation. 

```haskell
data HscEnv
  = HscEnv 
  { hsc_dflags :: DynFlags
  , hsc_targets :: [Target]
  , hsc_mod_graph :: ModuleGraph
  , hsc_IC :: InteractiveContext
  , hsc_HPT :: HomePackageTable
  -- Many more ... (truncated for brevity)
  }
```

The ``hsc_dlfags`` holds the settings objects (more on this next). The
``hsc_targets`` holds the roots of the Module graph which are traversed
bottom-up to build up the entire set of modules needed for compilation of the
current package. The entire set of modules involved in this (roots and
non-roots) is stored in ``hsc_mod_graph`` which holds the whole ModuleGraph,
which is not necessarily in topological order. The ``hsc_IC`` field contains the
interactive context which is used for the interactive shell and for when the end
targets are linked in memory. Specific commands in GHCi such as adding modules
to the top-level scope modifying this structure state fully.

The ``hsc_HPT`` holds the home package table which describes already-compiled
home-package modules, When a module done being compiled, and is loaded with
``loadModule`` it is internally added to this mapping.

#### DynFlags

[DynFlags](https://downloads.haskell.org/~ghc/7.10.2/docs/html/libraries/ghc-7.10.2/DynFlags.html#t:DynFlags)
contains a collection of flags relating to the compilation of a single file or
GHC session. This is the core datatype that informs how compilation occurs and
is passed to most of the various pass functions.

```haskell
data DynFlags
  = DynFlags 
  { ghcMode :: GhcMode
  , ghcLink :: GhcLink
  , hscTarget :: HscTarget
  , settings :: Settings

  , flags :: [DynFlag]
  , extensionFlags :: [ExtensionFlag]

  , pkgState :: PackageState
  , pkgDatabase :: Maybe [PackageConfig]
  , packageEnv :: Maybe FilePath
  , packageFlags :: [PackageFlag]
  , extraPkgConfs :: [PkgConfRef] -> [PkgConfRef]
  -- Many more flags... (truncated for brevity)
  }
```
The ``GhcMode`` informs whether we're doing multi-module compilation or one-shot
single-file compilation. In the case of multi-module the ModuleGraph is built up
via the
[Finder](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/Finder.html)
function which searches the home package for the dependent modules.

**GhcMode**
--------------  ------------------
CompManager     ``--make``
OneShot         ``ghc -c Foo.hs``
MkDepend        ``ghc -M``


The ``HscTarget`` datatype defines the target code type of the compilation. By
default this is ``HscAsm``.

**HscTarget**
-------------- -----------------
HscC           Generate C code.
HscAsm         Generate assembly using the native code generator.
HscLlvm        Generate assembly using the llvm code generator.
HscInterpreted Generate bytecode.
HscNothing     Don't generate any code.  See notes above.


After compilation is done (for multi-module) GHC then begins the linker phase
and the ``GhcLink`` setting determines what to do with the resulting object
files.

**GhcLink**
--------------  ------------------
NoLink          Don't link at all
LinkBinary      Link object code into a binary
LinkInMemory    Use the in-memory dynamic linker (works for both bytecode and object code).
LinkDynLib      Link objects into a dynamic lib (DLL on Windows, DSO on ELF platforms)
LinkStaticLib   Link objects into a static lib

The simplest initializer of a GHC session simply uses the defaults and sets up a
interpreted session that links any modules it is given in memory.

```haskell
example :: IO ()
example = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              }
```


GHC exposes many compiler flags on the commandline and these are themselves
reflected in various subfields of the ``DynFlags`` struct. The three major
classes of flags are ``DumpFlag`` (example: ``-ddump-simpl``), GeneralFlag
(example: ``-fspec-constr``) and ExtensionFlag (example: ``-XTypeInType``).
There are various helper functions that modifying the DynFlags to twiddle these
flags on or off.

```haskell
dopt_set :: DynFlags -> DumpFlag -> DynFlags
gopt_set :: DynFlags -> GeneralFlag -> DynFlags
xopt_set :: DynFlags -> ExtensionFlag -> DynFlags
```

Through the compilation GHC will query the state of these flags to dispatch to
different codepaths based on whether a language extension is set or other flag
behavior. This is done through querying the ``GhcMonad`` instance to get the
dynflags and using one of the various flag specific functions. 

```haskell
xopt :: ExtensionFlag -> DynFlags -> Bool
gopt :: GeneralFlag -> DynFlags -> Bool
dopt :: DumpFlag -> DynFlags -> Bool
```

To enable various flags we use modify the current ``dflags`` object using the
flag set functions.

```haskell
example :: IO ()
example = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let dflags' = dflags { hscTarget = HscInterpreted , ghcLink = LinkInMemory }
               `dopt_set` Opt_D_dump_BCOs        -- Set Dump Flag
               `xopt_set` Opt_OverloadedStrings  -- Set Language Extension Flag
```

#### Compilation

To start compilation we first add a target to the state. This modifies the
``hsc_targets`` field of the environment. To two types of targets are either
module names or filenames. The ``guessTarget`` will discriminate on the given
string's extension it to determine which target object to create. 

```haskell
addTarget :: GhcMonad m => Target -> m ()
guessTarget :: GhcMonad m => String -> Maybe Phase -> m Target
```

Targets specify the source files or modules at the top of the dependency tree.
For a executable program there is just a single target ``Main.hs``, for a
library the targets are visible module in the library.

**Target**
--------------       ------------------
TargetModule         A module name: search for the file
TargetFile FilePath  A filename: preprocess and parse it to find the module name. 

If with the modules added to the state we can then perform dependency analsysis
to determine the module graph to proceed with multi-module compilation.
Dependency analysis entails parsing the import directives of the module and
resolving the ``ModuleGraph`` which is a type alias for a list of
``ModuleSummary`` which contains the targets.  This is performed by the
``depanal`` function.

```haskell
depanal :: GhcMonad m => [ModuleName] -> Bool -> m ModuleGraph
```

After a target is created the compiler is then run on the module yielding the
resulting artifacts and it is loaded into the home package table. This is
accomplished via the ``load`` command.

```haskell
load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
```

**LoadHowMuch**
--------------      ------------------
LoadAllTargets      Load all targets and its dependencies.
LoadUpTo            Load only the given module and its dependencies.
LoadDependenciesOf  Load only the dependencies of the given module, but not the module itself.


A full example of this would be the compilation of a module ``Example.hs`` in
the current working directory that is interpreted and linked in memory.

```haskell
example :: IO ()
example = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              }

  target <- guessTarget "Example.hs" Nothing
  addTarget target
  load LoadAllTargets
```


#### Interactive Context

On top of simply generating compiler artifacts. GHC can compile and link code
into memory to be evaluated interactively. The state of the interpreter backing
this is held in the
[InteractiveContext](https://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/HscTypes.html#t:InteractiveContext).

The set of modules in the interactive scope can be modified by the
``setContext`` function.

```haskell
getContext :: GhcMonad m => m [InteractiveImport]
setContext :: GhcMonad m => [InteractiveImport] -> m ()
```

When a module is interpreted and loaded as an interactive import it has its full
top-level scope available. We can manipulate, query and extend this scope using
various function.

``parseName`` can be used to resolve a name (or names) from a given string to a
set of symbols in the interactive context. This returns a ``Name`` object (more
on this later) which is GHC's internal name type that holds position and a
unique identifier.

```haskell
parseName :: GhcMonad m => String -> m [Name]
```

To resolve the type of an given expression the ``exprType`` can be used to
extract the type information within the current context.

```haskell
exprType :: GhcMonad m => String -> m Type
```

And within the entire interactive context we can query the set of all names that
have been brought into scope by imports. This is used for the interactive
``:browse`` command.

```haskell
getNamesInScope :: GhcMonad m => m [Name]
```

And the most important function is evaluation of arbitrary expressions with in
the interactive context. Which is accomplished via ``dynCompileExpr ``. This
returns a ``Dynamic`` which can be safely cast using ``fromDynamic`` for any
instance of ``Typeable``. This is used to dynamically evaluate a string
expression within the interactive context. 

```haskell
dynCompileExpr :: GhcMonad m => String -> m Dynamic
fromDynamic :: Typeable a => Dynamic -> a -> Maybe a
```

#### Package Database

In it's default state GHC is aware of two package databases: the global package
database in ``/usr/lib/ghc-x.x.x/`` and the user database in ``~/.ghc/lib``. 

This however can be extended via the “GHC_PACKAGE_PATH” environment variable
which reads the path variable and applies the ``extraPkgConfs`` function to add
it to the package database. This is used in the various modern sandboxing
techniques used in tools like ``cabal`` and ``stack``.

```haskell
extraPkgConfs :: [PkgConfRef] -> [PkgConfRef]
```

To modify the given dynflags with a filepath, the following function can be
used to extend the state.

```haskell
addPkgDbs :: GhcMonad m => [FilePath] -> m ()
addPkgDbs fps = do 
  dfs <- getSessionDynFlags
  let pkgs = map PkgConfFile fps
  let dfs' = dfs { extraPkgConfs = (pkgs ++) . extraPkgConfs dfs }
  setSessionDynFlags dfs'
  _ <- initPackages dfs'
  return ()
```

Stack sets this when launching the shell with ``stack repl``. More on modifying
this will be discussed later.

#### Mini GHCi

Ok, so let's a build a very small interactive shell for GHC. If you're not
familiar with [Haskeline](http://dev.stephendiehl.com/hask/#haskeline) (the
platform-agnostic [readline][readline] abstraction) then read up on that first. 

[readline]:https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html

The Haskeline interface is exposed as a monad transformer ``InputT`` which
inside of IO gives us our interactive repl monad.

```haskell
type Repl a = InputT IO a
```

To set up the initial session set get the default dynflags, set the target to be
interpreted and memory-linked and twiddle the ``-XExtendedDefaultRules`` flag.
We set the interactive shell to import the ``Prelude`` and then monadically
return the resulting session so that we can progressively add to it on each
shell commnad.

```haskell
initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  liftIO $ putStrLn "Setting up HscEnv"
  dflags <- getSessionDynFlags
  let dflags' = dflags { hscTarget = HscInterpreted , ghcLink = LinkInMemory }
                `xopt_set` Opt_ExtendedDefaultRules
  setSessionDynFlags dflags'
  setContext [ IIDecl $ simpleImportDecl (mkModuleName "Prelude") ]
  env <- getSession
  return env
```

Each our interactive shell commands is then wrapped in a helper function
``session`` which spins up a new Ghc monad but restores the session from the
last compilation. The monadic action is then evaluated and the resulting session
afterwards is returned as a value to be reused.

```haskell
session :: HscEnv -> Ghc a -> IO HscEnv
session env m = runGhc (Just libdir) $ do
  setSession env 
  m
  env <- getSession
  return env
```

The evaluator function tries two different compilation steps. First it tries to
compile the expression as is to see if it evaluates to a ``IO a`` action. If it
does it is then evaluated directly within the monad. If it does not then the
``fromDyanamic`` cast will simply yield a Nothing and we'll try to wrap the
expression in a print statement. The resulting compiled expression is guaranteed
to be an ``IO a`` so we unsafely coerce the compiled code pointer that GHC gives
us into IO and run it.

```haskell
eval :: String -> Ghc ()
eval inp = do
  dyn <- fromDynamic <$> dynCompileExpr inp
  case dyn of
    Nothing -> do
      act <- compileExpr ("Prelude.print (" <> inp <> ")")
      -- 'print' is constrained to 'IO ()' so unsafeCoerce is "safe"
      liftIO (unsafeCoerce act)
    Just act -> liftIO $ act
```

To add an import we simply cons the import as a module name to the context and
then yield the new state.

```haskell
addImport :: String -> Ghc ()
addImport mod = do
  ctx <- getContext
  setContext ( (IIDecl $ simpleImportDecl (mkModuleName mod)) : ctx )
```

Then we do the naughty thing of catching all exceptions that are thrown and just
printing them out. This is fairly justified in the case that if expression
compilation fails we have to just trap and report the failure in the embedded
interpreter logic.

```haskell
ghcCatch :: MonadIO m => IO a -> m (Maybe a)
ghcCatch m = liftIO $ do
  mres <- try m
  case mres of
    Left (err :: SomeException) -> do
      liftIO $ print err
      return Nothing
    Right res -> return (Just res)
```

The REPL then just reads the user's input and dispatch based on whether the line
starts with the keyword ``import``. Depending on the expression line it then
spins up a GHC session with the currently held HscEnv from the last line and
trys to compile it. If succesfully it then calls ``repl`` with the new ``env``
state ad-infinitum. Unix signals for aborting are handled by Haskeline monad.

```haskell
repl :: HscEnv -> Repl ()
repl env = do
  minput <- getInputLine ">>> "
  case minput of
    Nothing -> outputStrLn "Goodbye."

    Just input | "import" `isPrefixOf` input -> do
      let mod = concat $ tail $ words input
      env' <- ghcCatch (session env (addImport mod))
      maybe (repl env) repl env'

    Just input -> do
      env' <- ghcCatch (session env (eval input))
      maybe (repl env) repl env'
```

Then putting it all together.

```haskell
main :: IO ()
main = do
  env <- initSession
  runInputT defaultSettings (repl env)
```

We can then run our little shell.

```bash
$ stack build dive
$ stack exec dive 

Setting up HscEnv
>>> fmap (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
>>> import Data.Text
>>>
Goodbye.
```

So that's our custom Mini GHCi. In practice [real
GHCi](https://github.com/ghc/ghc/tree/master/libraries/ghci) does things a
little differently, but some underlying machinery remains the same. Other
features like name lookup and introspection are left as an exercise to the
reader. A fun next project would be to create tiny shell with an introspection
tool querying the original source code of any definition in scope.

#### Summary & Next Steps

This is the "Very High Level" API we can use to interact with GHC. Next we'll
concern ourselves with the guts of the internal artifacts used and how to
introspect and build them programatically.
