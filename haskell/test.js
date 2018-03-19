
define([
    "wilton/dyload",
    "wilton/wiltoncall"
], function(dyload, wiltoncall) {
    "use strict";

    return {
        main: function() {
            dyload({
                name: "wilton_ghc"
            });

            wiltoncall("ghc_init", {
                shimLibDirectory: "/home/alex/projects/fe/",
                initArgs: ["+RTS", "-N"]
            });

            dyload({
                name: "wilton_haskell_example",
                directory: "."
            });

            var resp = wiltoncall("foo", {
                foo: 41,
                bar: 42
            });
            print("[" + resp + "]");

            wiltoncall("ghc_shutdown");

            print("success1");

        }
    };
})
