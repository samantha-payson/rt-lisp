# RT Lisp

This repository provides an implementation of *RT Lisp*, a simple Lisp dialect
with immutable data types. The emphasis is on a small memory footprint, simple
language definition, and easy usage from C.

RT Lisp is being developed with game scripting in mind as its primary use case.

> Note: the name RT Lisp is terrible, it stands for "Runtime Lisp", because it's
> meant to be used in my game engine at runtime... It does NOT stand for "Real
> Time Lisp", the garbage collector likely prevents this system from being used
> in true realtime systems.
>
> Who knows, maybe I'll rename it ...

# This is a First Draft

Right now the implementation is incomplete, but I'm putting consistent effort
into getting the basic features done, improving usability, and cleaning up the
code.

Currently implemented are:

  - Stack based VM, w/ bytecode instruction set
  - Bytecode compiler
  - Generational garbage collector
  - Lexically scoped package system (really cool!)
  - Immutable HAMT-based map type (like Clojure)
  - Common Lisp-ish macros
  - Basic data types:
    - 28-bit int
    - 14.14-bit fixpoint
    - cons lists
    - tuples (i.e. immutable vectors)
    - symbols
    - selectors (like keywords)
  - Closures
  - Quasi-quote syntax (nesting doesn't work yet, though)

Soon to be implemented/added are (in rough order of priority):

  - Strings
  - Builtin Functions (i.e. calling C from Lisp)
  - Standard library
  - Proper Error Handling.
  - Proper REPL
  - Code sharing between VMs
  - Bytecode store/load to files
  - Documentation for users (starting w/ tutorial)

Long term plans include:

  - Documentation for developers
  - Runtime performance work
  - Bytecode compiler optimization work (the compiler is *extremely simple*
    right now)

The VM's performance is currently abysmal, especially if you start making maps
with a lot of entries (making a map with 64K entries takes about 21 seconds on
my machine with GCCs optimizations maxed out). There are lots of opportunities
for optimization in the VMs code, as well as the bytecode compiler output, so
it's entirely reasonable to expect that this will get much much faster over
time.

## A note about error handling

In the current implemenation, lots of places where an error might reasonably be
handled at runtime have a simple call to C's `abort()` function. This means that
the entire process can be crashed by you entering a bad line at the REPL.

As mentioned in the previous section, I *fully intend* to introduce a proper
error handling system allowing errors to be handled safely and efficiently from
C and/or Lisp.

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
