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

data MyObjIn = MyObjIn {
    foo :: Int,
    bar :: Int
} deriving (Typeable, Data, Generic, Show)
instance FromJSON MyObjIn

data MyObjOut = MyObjOut {
    baz :: Int,
    baa :: Int
} deriving (Generic, Show)
instance ToJSON MyObjOut


fun1 :: MyObjIn -> IO MyObjOut
fun1 obj = do
    print obj
    return MyObjOut { baz = (foo obj + 1), baa = (bar obj + 1) }

fun2 :: MyObjIn -> IO MyObjOut
fun2 = error "Oops!"

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    fooErr <- registerWiltoncall "foo" fun1
    if (isJust fooErr) then createWiltonError fooErr
    else do
        barErr <- registerWiltoncall "bar" fun2
        if (isJust barErr) then createWiltonError fooErr
        else createWiltonError Nothing
