{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Pipes.Cereal ( encode,
                      encodePut,
                      decode,
                      decodeGet,
                      Put,
                      Get,
                      Serialize ) where

import Control.Monad.Error.Class
import Data.Maybe
import Data.Monoid
import Data.Serialize hiding ( encode, decode )
import Pipes
import Pipes.ByteString
import Pipes.Parse

-- |Encode a value to a byte stream.
encode :: (Serialize a, Monad m) => a -> Producer' ByteString m ()
encode = encodePut . put

-- |Encode an explicit 'Put'.
encodePut :: Monad m => Put -> Producer' ByteString m ()
encodePut = Pipes.ByteString.fromLazy . runPutLazy

-- |Decode a value from a byte stream.
decode :: (Serialize a, Monad m) => Parser ByteString m (Either String a)
decode = decodeGet get

-- |Decode a value using an explicit 'Get'.
decodeGet :: Monad m => Get a -> Parser ByteString m (Either String a)
decodeGet = go . runGetPartial
  where go f = do next <- draw
                  case f (fromMaybe mempty next) of
                   Fail e bs -> unDraw bs >> return (Left e)
                   Partial f' -> go f'
                   Done a bs -> unDraw bs >> return (Right a)
