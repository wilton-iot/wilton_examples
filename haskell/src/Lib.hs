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
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Maybe (isJust)
import Data.Text (Text)
import Foreign.C.String (CString)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Data as Data
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Foreign.Wilton.FFI as WiltonFFI


-- structs to be passed to/from JavaScript

data MyObjIn = MyObjIn {
    foo :: String,
    bar :: Int
} deriving (Data.Typeable, Data.Data, Generics.Generic, Show)
instance Aeson.FromJSON MyObjIn

data MyObjOut = MyObjOut {
    baz :: String,
    baa :: Int
} deriving (Generics.Generic, Show)
instance Aeson.ToJSON MyObjOut

-- backcall to JS

capitalize :: Text -> IO String
capitalize msg = do
    let callDesc = Aeson.Object (Map.fromList [
            ("module", "lodash/string"),
            ("func", "capitalize"),
            ("args", Aeson.Array (Vector.fromList [Aeson.String msg]))
            ])
    respEither <- WiltonFFI.runWiltonScript callDesc
    either
        (\err -> do
            error err)
        (\respBs -> do
            return (UTF8.toString respBs))
        respEither


-- functions with some logic

hello :: MyObjIn -> IO MyObjOut
hello obj = do
    st <- capitalize "haskell"
    return MyObjOut {
        baz = (foo obj ++ " from " ++ st ++ " lib!"),
        baa = (bar obj + 1) }

hello_exception :: MyObjIn -> IO MyObjOut
hello_exception = error "Error message from Haskell"


-- this function is called on module load

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    -- register a call for JavaScript
    fooErr <- WiltonFFI.registerWiltonCall "example_hello" hello
    -- check for error
    if (isJust fooErr) then WiltonFFI.createWiltonError fooErr
    else do
        -- register some other call
        barErr <- WiltonFFI.registerWiltonCall "example_hello_exception" hello_exception
        -- check for error
        if (isJust barErr) then WiltonFFI.createWiltonError fooErr
        -- return success
        else WiltonFFI.createWiltonError Nothing
