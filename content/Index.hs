{-# LANGUAGE OverloadedStrings #-}

module Index (index) where

import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Util

-- | the index page to be used
--   change to `index = File "index.html"` if you want `index.html` as the index page
index :: Index
index = Index title (generate content)

title :: String
title = "meowcolm024"

-- content of the home page
content :: Content ()
content = do
  section "welcome" Nothing . htmlLines $
    [ "hi, this is just a homepage" <> H.br,
      "click " <> lightSwitch <> " to toggle between light (dark) mode"
    ]
  posts "posts"
  section "tags" (Just "/tags") mempty
  section "about" (Just "/about.html") mempty
  section "github" (Just "https://github.com/Meowcolm024") mempty
