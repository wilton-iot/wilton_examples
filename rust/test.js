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
            // load rust library
            dyload({
                name: "example_rust",
                directory: "target/debug"
            });

            print("Calling Rust lib ...");
            var res = wiltoncall("example_hello", {
                foo: "Hello",
                bar: 42
            });
            print("Call response: [" + res + "]");

            // this call panics in Rust, can be caught with try-catch
            /*
            var res = wiltoncall("example_hello_panic", {
                foo: "Hello panic",
                bar: 43
            });
            */
        }
    };
});
