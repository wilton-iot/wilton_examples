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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Foreign.C.String (CString)
import GHC.Generics (Generic)
import Foreign.Wilton.FFI as WiltonFFI


-- shortcut helper
encodeBytes :: ToJSON a => a -> ByteString
encodeBytes = ByteString.concat . ByteStringLazy.toChunks . Aeson.encode

-- structs to be passed to/from JavaScript

data MyObjIn = MyObjIn
    { foo :: String
    , bar :: Int
    } deriving (Generic, Show, Typeable)
instance FromJSON MyObjIn

data MyObjOut = MyObjOut
    { baz :: String
    , baa :: Int
    } deriving (Generic, Show)
instance ToJSON MyObjOut


-- backcall to JS

capitalize :: Text -> IO String
capitalize msg = do
    -- use runscript_chakra on windows
    -- bytestring version is used because result is a plain string
    respEither <- invokeWiltonCallByteString "runscript_jsc" (encodeBytes (Aeson.object 
        [ "module" .= ("lodash/string" :: Text)
        , "func" .= ("capitalize" :: Text)
        , "args" .= [msg]
        ]))
    case respEither of
        Left err -> error (UTF8.toString err)
        Right respBs -> return (UTF8.toString respBs)

-- functions with some logic

hello :: MyObjIn -> IO MyObjOut
hello obj = do
    st <- capitalize "haskell"
    return MyObjOut
        { baz = (foo obj ++ " from " ++ st ++ " lib!")
        , baa = (bar obj + 1)
        }

hello_exception :: MyObjIn -> IO MyObjOut
hello_exception = error "Error message from Haskell"


-- this function is called on module load

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    -- register a call for JavaScript
    fooErr <- registerWiltonCall "example_hello" hello
    -- check for error
    if (isJust fooErr) then createWiltonError fooErr
    else do
        -- register some other call
        barErr <- registerWiltonCall "example_hello_exception" hello_exception
        -- check for error
        if (isJust barErr) then createWiltonError fooErr
        -- return success
        else createWiltonError Nothing
