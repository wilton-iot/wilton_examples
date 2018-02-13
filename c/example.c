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

#include <string.h>

#include "wilton/wiltoncall.h"

#define RESPONSE_STRING " from C!"
#define CALL_NAME "example_hello"

// result must be allocated with 'wilton_alloc' and assigned to 'data_out'
char* hello(void* ctx, const char* data_in, int data_in_len, char** data_out, int* data_out_len) {
    // arbitrary C code here
    (void) ctx;
    size_t resp_len = strlen(RESPONSE_STRING);
    // allocate and return result
    *data_out = wilton_alloc(data_in_len + resp_len + 1);
    memcpy(*data_out, data_in, data_in_len);
    memcpy(*data_out + data_in_len, RESPONSE_STRING, resp_len);
    *(*data_out + data_in_len + resp_len) = '\0';
    *data_out_len = data_in_len + resp_len;
    return NULL;
}

// this function is called on module load,
// must return NULL on success
#ifdef _WIN32
__declspec(dllexport)
#endif
char* wilton_module_init() {
    // register 'hello' function
    char* err = wiltoncall_register(CALL_NAME, strlen(CALL_NAME), NULL, hello);
    if (err) return err;

    // return success
    return NULL;
}
