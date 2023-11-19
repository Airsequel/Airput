{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Protolude (Text, (<>))

hello :: Text -> Text
hello name =
  "Hello " <> name <> "!"
