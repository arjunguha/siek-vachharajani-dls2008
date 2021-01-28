This repository contains the code from the following paper:

Jeremy G. Siek and Manish Vachharajani. Gradual Typing with Unification-based 
Inference. Dynamic Languages Symposium (DLS) 2008.

The directory /gtubi contains code downloaded from:

http://ece.colorado.edu/~siek/gtubi.tar.gz

With two omissions:

- I deleted the .svn directories
- There were two copies of the tool. As far as I could tell, the only difference
  was that one desugared let-expressions to applications, whereas the other
  did not. The copy in the repository supports let-expressions directly.


The code in the root directory uses Docker to create an operating environment
that supports an older version of OCaml that can compile and run the code.
The main reason this code no longer works is that OCaml 4.02 introduced a
distinction between immutable strings and mutable bytes. It is probably
fairly easy to fix.

To build the environment:

make

To access the environment:

./gtubi.sh