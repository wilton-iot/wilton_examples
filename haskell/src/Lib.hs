--
-- Copyright 2018, alex at staticlibs.net
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson
import Data.Data
import Data.Maybe
import GHC.Generics
import Foreign.C.String
import Foreign.Wilton.FFI


-- structs to be passed to/from JavaScript

data MyObjIn = MyObjIn {
    foo :: String,
    bar :: Int
} deriving (Typeable, Data, Generic, Show)
instance FromJSON MyObjIn

data MyObjOut = MyObjOut {
    baz :: String,
    baa :: Int
} deriving (Generic, Show)
instance ToJSON MyObjOut


-- functions with some logic

hello :: MyObjIn -> IO MyObjOut
hello obj = do
    putStrLn ("Input object: " ++ (show obj))
    return MyObjOut {
        baz = (foo obj ++ " from Haskell lib!"),
        baa = (bar obj + 1) }

hello_exception :: MyObjIn -> IO MyObjOut
hello_exception = error "Error message from Haskell"


-- this function is called on module load

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    -- register a call for JavaScript
    fooErr <- registerWiltoncall "example_hello" hello
    -- check for error
    if (isJust fooErr) then createWiltonError fooErr
    else do
        -- register some other call
        barErr <- registerWiltoncall "example_hello_exception" hello_exception
        -- check for error
        if (isJust barErr) then createWiltonError fooErr
        -- return success
        else createWiltonError Nothing
