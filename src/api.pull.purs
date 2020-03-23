module Api.Pull
  ( Pull(..)
  , encodePull
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (concatAll)

data Pull = Ping

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.uint32 10, encodePing ]

encodePing :: Uint8Array
encodePing = Encode.uint32 0
