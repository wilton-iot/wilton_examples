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

/* 
 * File:   example.cpp
 * Author: alex
 *
 * Created on February 12, 2018, 9:22 PM
 */

#include <cstring>
#include <stdexcept>
#include <string>

#include "wilton/wiltoncall.h"

namespace example {

// js call helper, throws on JS error
std::string call_js(const std::string& engine, const std::string& call_desc) {
    char* out = nullptr;
    int out_len = 0;
    char* err = wiltoncall_runscript(engine.data(), static_cast<int>(engine.length()), // engine name
            call_desc.data(), static_cast<int>(call_desc.length()), // input data
            std::addressof(out), std::addressof(out_len)); // output data
    if (nullptr != err) {
        auto msg = std::string(err);
        wilton_free(err);
        throw std::runtime_error(std::string() + "Error calling JS module, message: [" + msg + "]");
    }
    auto out_str = std::string();
    if(out_len > 0) {
        out_str = std::string(out, out_len);
        wilton_free(out);
    }
    return out_str;
}

// helper function for registering wiltoncalls
char* wrapper_fun(void* ctx, const char* data_in, int data_in_len, char** data_out, int* data_out_len) {
    try {
        auto fun = reinterpret_cast<std::string(*)(const std::string&)> (ctx);
        auto input = std::string(data_in, static_cast<size_t>(data_in_len));
        std::string output = fun(input);
        if (!output.empty()) {
            *data_out = wilton_alloc(static_cast<int>(output.length()));
            std::memcpy(*data_out, output.data(), output.length());
        } else {
            *data_out = nullptr;
        }
        *data_out_len = static_cast<int>(output.length());
        return nullptr;
    } catch (const std::exception& e) {
        //auto what = std::string("CALL ERROR"); // std::string(e.what());
        auto what = std::string(e.what());
        // returned error must be NUL-terminated
        char* err = wilton_alloc(static_cast<int>(what.length()) + 1);
        std::memcpy(err, what.c_str(), what.length() + 1);
        return err;
    }
}

std::string hello(const std::string& input) {
    // arbitrary C++ code here
    return input + " from C++!";
}

std::string hello_cb(const std::string& input) {
    // need to know JS engine name
#ifdef _WIN32
    auto engine = std::string("chakra");
#else 
    // auto engine = std::string("duktape");
    auto engine = std::string("jsc");
#endif

    // some JSON lib may be convenient here
    auto call_desc = R"(
{
    "module": "dist/callback",
    "func": "hello",
    "args": [")" + input + R"("]
}
)";

    // call JS module
    std::string out_str = call_js(engine, call_desc);

    // return result to original JS caller
    return out_str;
}


} // namespace

// this function is called on module load,
// must return NULL on success
extern "C"
#ifdef _WIN32
__declspec(dllexport)
#endif
char* wilton_module_init() {
    char* err = nullptr;

    // register 'hello' function
    auto name_hello = std::string("example_hello");
    err = wiltoncall_register(name_hello.data(), static_cast<int> (name_hello.length()), 
            reinterpret_cast<void*> (example::hello), example::wrapper_fun);
    if (nullptr != err) return err;

    // register 'hello_cb' function
    auto name_hello_cb = std::string("example_hello_cb");
    err = wiltoncall_register(name_hello_cb.data(), static_cast<int> (name_hello_cb.length()), 
            reinterpret_cast<void*> (example::hello_cb), example::wrapper_fun);
    if (nullptr != err) return err;

    // return success
    return nullptr;
}
