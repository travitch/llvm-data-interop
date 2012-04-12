This package converts the LLVM IR (either bitcode files or LLVM
assembly) into a Haskell data type (defined in llvm-base-types).  The
conversion uses the LLVM libraries to read bitcode.  It then traverses
the IR and produces a Haskell structure that can be pattern matched
against in pure code.

After the conversion, no C++ data is referenced and no C++ code is
called.  This library does link against LLVM directly, and therefore
also uses libstdc++.  This can make linking tricky sometimes.  For
example, mixing template-haskell with C++-linked code does not seem to
work.

Note that this library only works with LLVM 3.0.  An attempt will be
made to support versions later than 3.0.
