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

Note that this library only works with LLVM 3.0-3.2, and will try to
retain as much compatibility with future and past versions as is
reasonable.

# Installation Notes

 * Currently, this library is set up to link against the LLVM shared
   library (libLLVM-3.X.so).  Most distro packages build the LLVM
   shared library.  If you make a custom build, be sure to configure
   with `--enable-shared`.

## Debian and Ubuntu

You need the development libraries and headers, so be sure to install
the llvm-3.X-dev package, where X is the version you would like to
use.  You will also need clang.
