---
title: A Vim + Haskell Workflow
date: October 18, 2013
---

### A Vim + Haskell Workflow

**Hoogle**

[Hoogle](http://www.haskell.org/hoogle/) is a Haskell type search
engine which can be used online or installed locally.

```scala
$ cabal install hoogle
```

Hoogle can be used from the command line as well from GHCi by
adding the following lines to your ``.ghc/ghci.conf``

```scala
:def hoogle \s -> return $ ":! hoogle --count=15 \"" ++ s ++ "\""
```

\

For instance if we forgot the name of the function associated with
``(a -> m b) -> [a] -> m b``) we could ask hoogle for functions
matching. Indeed we see that ``mapM_`` is in the list.

<p>
<img src="/images/hoogle.png"/>
</p>

**syntastic**

[Syntastic](https://github.com/scrooloose/syntastic) is a
syntax checking plugin for a variety of languages, including
Haskell. It integrates with a either ghcmod or hdevtools to
provide type errors inline.

<p>
<img src="/images/loclist.png"/>
</p>

<p>
<img src="/images/errors.png"/>
</p>


To toggle between active or passive type checking we can enable
the following key bindings:

```scala
map <silent> <Leader>e :Errors<CR>
map <Leader>s :SyntasticToggleMode<CR>
```

To always show the errors list when editing we can set the
following flag: 

```python
let g:syntastic_auto_loc_list=1
```

**ghc-mod**

ghcmod is a command line tool to analyze Haskell source. It
integrates with syntastic to provide integration with GHCi.

```scala
$ cabal install ghc-mod
```

Pressing ( `tu` ) can then be used to update the background GHC
process with the file (from disk). This updates the tagbar and
allows you to fill in any missing types by highlighting the
toplevel function definition and pressing ( `tw` ) to infer the
corresponding type signature of the highlighted toplevel function
and add it to the line above. As usual the signature is the
inferred by GHC is guaranteed to be the *most general type*, not
necessarily the most usefull one.

```scala
" Reload
map <silent> tu :call GHC_BrowseAll()<CR>
" Type Lookup
map <silent> tw :call GHC_ShowType(1)<CR>
```


\

**hdevtools**

```scala
$ cabal install hdevtools
```

To enable, first install syntastic and then add the following to
your ``.vimrc``.

```scala
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>
```

Pressing ( `F1` ) in Normal mode shows the type under the cursor.
Pressing it repeatedly expands the selection to the parent
expression up to the toplevel function definition.

<p>
<img src="/images/f1.png"/>
</p>

Pressing ( `F3` ) in Normal mode will show further information about
type classes, data constructors or functions, including the
source location of definition.

<p>
<img src="/images/f3.png"/>
</p>

**hlint**

Hlint is a source linter for Haskell which can provide a
selection of hints for helping improve your code stylistic and
functionally. For instance if you happen to implement one of many
Prelude functions it can suggest that you use the builtin
instead.

```scala
$ cabal install hlint
```

<p>
<img src="/images/hlint.png"/>
</p>

**tagbar**

[Tagbar](http://majutsushi.github.io/tagbar/) is a plugin for
browsing the toplevel definitions in a file. It integrates with
ghcmod to generate the tags from 

<p>
<img src="/images/tagbar.png"/>
</p>

The tags are updated upon write, to open the tagbar bind
``:TagbarToggle`` to your key of choice;

```scala
nmap <leader>= :TagbarToggle<CR>
let g:tagbar_autofocus = 1
```

**vim-slime**

`vim-slime` is a plugin to allow one-way communication between
vim and a tmux shell, allowing you to interactively send code to
ghci. To integrate with tmux add the following to your `.vimrc`,
then start a separate terminal process running ``tmux`` and
``ghci``.

```scala
let g:slime_target = "tmux"
let g:slime_paste_file = tempname()
```

In visual mode you now press ( `Ctrl-C Ctrl-C` ) to send the current
block to the tmux session. Upon initial load it will prompt you
for the tmux session name and vim panel.

<p>
<img src="/images/slime.png"/>
</p>

**pointfree**

Pointfree is a syntax rewriter to eliminate unneccesary free
variables from an expression.

```scala
$ cabal install pointfree
```

\

Then add the following to your `.vimrc`.


```scala
autocmd BufEnter *.hs set formatprg=pointfree
```

\


In visual mode you can now press `gq` to convert a expression to
it's pointfree form. Though quite often the resulting form is
more obfuscated than the original.


<p>
<img src="/images/pointfree.png"/>
</p>
