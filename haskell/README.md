Haskell module example
----------------------

*Note: in the following commands RTS version `8.2.2` needs to be adjusted as required*

On Linux with [stack](https://docs.haskellstack.org/en/stable/README/) installed:

Build the [shim library](https://github.com/wilton-iot/wilton_ghc/blob/master/resources/wilton_ghcshim.c),
that allows to init GHC runtime from wilton:

    curl -LO https://raw.githubusercontent.com/wilton-iot/wilton_ghc/master/resources/wilton_ghcshim.c
    stack ghc -- --make -dynamic -shared -fPIC -threaded -lHSrts_thr-ghc8.2.2 wilton_ghcshim.c -o libwilton_ghcshim.so

Build the example with `stack`, to ensure that all dependencies are fetched and built by `stack`:

    stack build

Build the example module as a shared library:

    stack ghc -- --make -dynamic -shared -fPIC -threaded -lHSrts_thr-ghc8.2.2 src/Lib.hs -o libwilton_haskell_example.so

Run the example:

    wilton ./test.js
    > Calling Haskell lib ...
    > Input object: MyObjIn {foo = "Hello", bar = 42}
    > Call response: [{"baa":43,"baz":"Hello from Haskell lib!"}]
