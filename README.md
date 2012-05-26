"Safety shouldn't be expensive."

splinter-core
=============

Core of the Splinter language.

goals
=====

* simplicity
* modularity
* safety
* educational

definitions
===========

* cell - traditionally called an LVALUE in C. something that can be assgined to

milestones
==========

Each milestone adds features or modifies previous features.

V0
--

* Modules
* Monomorphic
* Static Types
* Type Inference
* Stateful function notation

V1
--

* Polymorphic instead of Monomorphic
* Introduce Pointers

V2
--

* Operators
* User defined operators

V?
--

* References
* Composite Types
* Arrays
* Strings

annoying bits
=============

* assignment l-values
* references are AST nodes. references to references are bad.
* ref :: Cell -> Ptr
* deref :: Ptr -> Cell
* namespacing
* dots (.)
* indexing
* Cells get injected into the type system
  * Funny things happen because of this
