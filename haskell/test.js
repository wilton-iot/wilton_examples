/*
 * Copyright 2018, alex at staticlibs.net
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

define([
    "wilton/dyload",
    "wilton/wiltoncall"
], function(dyload, wiltoncall) {
    "use strict";

    return {
        main: function() {
            // load built-in wilton module that allows to init
            // GHC runtime using wilton_ghcshim shared lib
            dyload({
                name: "wilton_ghc"
            });

            // init GCH runtime in the current wilton process
            // this needs to be done before loading any haskell modules
            wiltoncall("ghc_init", {
                shimLibDirectory: ".",
                initArgs: ["+RTS", "-N"]
            });

            // load example module
            dyload({
                name: "wilton_haskell_example",
                directory: "."
            });

            print("Calling Haskell lib ...");
            var res = wiltoncall("example_hello", {
                foo: "Hello",
                bar: 42
            });
            print("Call response: [" + res + "]");

            // this call throws exception in Haskell, can be caught with try-catch
            /*
            wiltoncall("example_hello_exception", {
                foo: "Hello",
                bar: 42
            });
            */

            // shutdown GHC runtime
            wiltoncall("ghc_shutdown");
        }
    };
});
