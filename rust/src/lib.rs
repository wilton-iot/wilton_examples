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

#[macro_use]
extern crate serde_derive;
extern crate wilton_rust;


// structs to be passed to/from JavaScript

#[derive(Deserialize)]
struct MyObj1 {
    foo: String,
    bar: i32,
}

#[derive(Serialize)]
struct MyObj2 {
    boo: String,
    baz: i32,
}


// functions with some logic

fn hello(obj: MyObj1) -> MyObj2 {
    MyObj2 {
        boo: obj.foo + " from Rust lib!",
        baz: obj.bar + 1
    }
}

fn hello_panic(_: MyObj1) -> MyObj2 {
    panic!("Some error message");
}


// this function is called on module load

#[no_mangle]
pub extern "C" fn wilton_module_init() -> *mut std::os::raw::c_char {

    // register a call for JavaScript
    let res1 = wilton_rust::register_wiltocall("example_hello", |obj: MyObj1| { hello(obj) });

    // check for error
    if res1.is_err() {
        return wilton_rust::create_wilton_error(res1.err());
    }
    
    // register some other call
    let res2 = wilton_rust::register_wiltocall("example_hello_panic", |obj: MyObj1| { hello_panic(obj) });

    // check for error
    if res2.is_err() {
        return wilton_rust::create_wilton_error(res2.err());
    }

    // return success
    wilton_rust::create_wilton_error(None)
}
