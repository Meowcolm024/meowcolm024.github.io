{-# LANGUAGE OverloadedStrings #-}

module Index (index) where

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util

-- | the index page to be used
--   use `index = File "index.html"` if you want `index.html` as the index page
index :: Index
index = Index title content

-- title of the site
title :: String
title = "meowcolm024"

-- content of the home page
content :: Content ()
content = do
  section "welcome" Nothing $ do
    txt "hi, this is just a homepage"
    html $ H.p $ "click " <> lightSwitch <> " to toggle between light (dark) mode"
    markdown "checkout the [github repo](https://github.com/Meowcolm024/meowcolm024.github.io) :D"
  posts "posts"
  section "tags" (Just "/tags") none
  section "about" (Just "/about.html") none
  section "github" (Just "https://github.com/Meowcolm024") none
