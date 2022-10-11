A Paper Template using Ott and LaTeX
---

The point of this template is to start a paper using Ott and LaTeX
quickly.  This is really easy to get compiling, because it compiles as
is! However, if you are like me, and like to name things, then change
the following.

First, in [Makefile](Makefile) change the following lines:

```
# Name of the note:
Name := paper
# Name of the language (ott spec name):
OTTPrefix := spec
```

The first names the LaTeX files, and the second names the language and
prefixes all Ott commands with `$(OTTPrefix)`.  I usually set the
latter to my language name.

Then in [paper.tex](paper.tex) update the line

```
%% Change "spec" to the language prefix.
\input{spec-inc}  
```

with your new `$(OTTPrefix)`.

That's it, you can now build the paper with custom names.