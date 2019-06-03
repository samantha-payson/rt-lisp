# RT Lisp

This repository provides an implementation of *RT Lisp*, a simple Lisp dialect
similar to Clojure. The emphasis is on a small memory footprint, simple language
definition, and easy usage from C.

RT Lisp is being developed with game scripting in mind as its primary use case.

> Note: the name RT Lisp is terrible, it stands for "Runtime Lisp", because it's
> meant to be used in my game engine at runtime... It does NOT stand for "Real
> Time Lisp", the garbage collector likely prevents this system from being used
> in true realtime systems.
>
> Who knows, maybe I'll rename it ...

# The State of the Implementation

At this point, RT Lisp is feature complete except for the standard
library. Notable features include:

## Stack Based VM

RT Lisp uses a stack-based virtual machine with a pretty nicely pipelined
interpreter loop. Profiling is still needed, but anecdotally the performance is
good enough for relatively complex game scripting with hundreds of C to VM calls
per frame at this point.

## Bytecode Compiler

Instructions for the VM are generated by a straightforward syntax-directed
multi-pass compiler. It currently doesn't do much optimizing, but it does support
the tail call optimization.

## Generational Garbage Collection

RT Lisp uses a very responsive and cache-friendly generational garbage
collector. It uses compact rank/select bitmaps to perform efficient mark/sweep
passes.

## Lexically Scoped Packages

Unlike many other Lisp package systems, packages in RT Lisp are lexically
scoped, allowing for imports only within a block of code. This also allows for
macros that implicitly switch packages.

Is this a good idea? I don't know. But in the worst case just don't use it :).

```
(in-package foo
  (defun hello ()
    'hello-foo)
  (export hello))

=> T

(foo:hello)

=> hello-foo

(use-package foo
  (hello))

=> hello-foo
```

Right now the implementation is incomplete, but I'm putting consistent effort
into getting the basic features done, improving usability, and cleaning up the
code.

## Common Lisp-style Macros

Symbols are resolved in a manner similar to Common Lisp (except that the current
package is determined lexically). This allows for practically conflict-free
macro implementations with little effort.

That said, macros in RT Lisp are simply functions on S-Expressions, there are no
beaurocratic hygiene rules or other limits imposed.

## Selectors

Unlike common Lisp keywords (but a bit like Clojure keywords), RT Lisp uses
selectors. They are like self-evaluating symbols that begin with a dot
(e.g. `.x`, `.foo`). Selectors may be part of a package, as in `.foo:something`.
Unlike symbols, selectors need to be explicitly annotated with a package, no
resolution is performed; `.foo` is the same value no matter what package you are
in.

## Immutable Maps

Like Clojure, RT Lisp supports immutable HAMT-based maps. Maps may be "called"
as if they were functions, for example:

```
(let ((m { .x 1 .y 2 }))
  (m .x))

=> 1
```

Similarly, a selector may be "called" on a map as if it were a function.

```
(let ((m { .x 1 .y 2 }))
  (.y m))

=> 2
```

Both of these forms are convenient for use with higher-order functions.

## Lists

Obviously, RT Lisp supports standard Lisp cons lists.

## Tuples

Tuples are fixed-size zero-indexed vectors. Similar to maps, they can be called
as a unary function of an integer argument.

```
(let ((t [7 8 9]))
  (t 1))

=> 8
```

A string in RT Lisp is simply a tuple of characters. They are pretty-printed in
double quote syntax.

```
['L' 'I' 'S' 'P']

=> "LISP"
```

This syntax is also accepted by the reader.

```
("LISP" 2)

=> 'S'
```

## Lexically Scoped Closures

RT Lisp supports modern lexically scoped closures

```
(let ((c (let ((x 1)
               (y 2)
               (z 3))
           (lambda ()
             (list x y z)))))
  (c))

=> (1 2 3)
```

## Quasi-Quote Syntax

RT Lisp supports modern nesting quasi-quote syntax.

```
(let ((x 1)
      (y 2))
  `(~x y `(~x ~(~y))))

=> (1 y `(~x ~(2)))
```

## Exceptions, Stack Traces

RT Lisp throws exceptions and prints stack traces for unhandled exceptions.

```
(+ 1 '2')

  Exception .expected-int28:

      Wrong type, expected 'Int28', got 'Char'.

     { .object '2', .type .expected-int28, .message "Wrong type, expected \'Int28\', got \'Char\'.", }

  Stack Trace (most recent at bottom):

       0: repl:code-page                   #0302
            @0004   iadd


  ERROR!
```

Exceptions can be propagated easily through C code and back into Lisp for
arbitrary nesting of C and Lisp function calls.

Exception handling is possible, but messy at the moment.

> **TODO**: document exception handler macros.

## UTF-8 Encoded Chars/Strings

The Reader actually doesn't fully support UTF-8 yet (I'm working on it...), so
currently you can only read strings as UTF-8. The VM has full UTF-8 support
though, it's just a matter of allowing multibyte characters in the reader.

```
("カタカナ" 2)

=> 'カ'
```

## Multiple Machine Instances

A single C program can have multiple VM instances with seperate heaps, stacks,
etc. These VMs may share bytecode, to avoid replicating the same functions over
and over in memory.

Currently shared code isn't thread safe (though it is if you avoid defining
functions or macros in seperate threads), but I'm working on it.

# Work in Progress

Soon to be implemented/added are (in rough order of priority):

  - Standard library (partly done).
  - Better Code Organization (right now way too much code is in `rt-lisp.c`.
  - Bytecode store/load to files
  - Thread safety between VMs sharing code.
  - Proper REPL line editing.
  - Documentation for users (starting w/ tutorial)

Long term plans include:

  - Documentation for developers
  - Runtime performance work
  - Bytecode compiler optimization work (the compiler is *extremely simple*
    right now)

The VM's performance is currently pretty good, but there are two big
issues I'm aware of.

  1. Large data structures (e.g. maps with thousands of entries) can slow
  things down, but that hasn't been an issue for me working on game
  scripting.

  2. Vectors with more than 100 or so entries will crash the VM! This is
  problematic for working with strings, obviously... The same bug can arise when
  creating a `(labels ...)` form with more than 20 or so functions.

Of these two problems, 1 is unlikely to be fixed (by me...) unless it becomes an
issue. 2 is a bigger problem, but it might require some large
performance-affecting changes to the garbage collector to fix. There are easy
work-arounds for working with large strings.

> TODO: Document use of erlang-style i/o strings.


## A note about error handling

The error handling system has been improved massively, and at this point there
are only a few calls to `abort()` left in the VM. They are there for dealing with
some unlikely and catastrophic scenarios (e.g. VM Out of Memory).

There is still room to handle these errors more gracefully, and it's on my TODO
list.

# License

RT Lisp is free software: you can redistribute it and/or modify it under the
terms of the GNU Lesser General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

RT Lisp is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program.  If not, see <https://www.gnu.org/licenses/>.

## Submodule Licenses

All git submodules in this repository (listed in the .gitmodules file) have
their own licenses. The LGPLv3 license does not necessarily apply to those
submodules, and the only license which applies to the main RT Lisp repository
(excluding submodules) is the LGPLv3.
