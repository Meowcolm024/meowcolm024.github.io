{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Util where

import Control.Monad.Trans.Writer
import Data.String
import Data.Text (Text)
import Text.Blaze.Html ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import qualified Text.Pandoc as P

-- | different types of index page
data Index = Index String String | File String

type Content = Writer H.Html

generate :: Content a -> String
generate = renderHtml . execWriter

-- | add a section to index page
section :: String -> Maybe String -> H.Html -> Content ()
section title url content = do
  let h2 s = case url of
        Just u -> H.a (H.h2 s) ! A.href (fromString u)
        Nothing -> H.h2 s
  tell $ h2 $ H.toHtml title
  tell content

-- | add the post section with recent post lists
posts :: String -> Content ()
posts title = do
  tell $ H.h2 $ H.toHtml title
  tell $ H.preEscapedToHtml @String "$partial(\"templates/partials/recent-list.html\")$"

-- | a list of raw htmls
htmlLines :: [H.Html] -> H.Html
htmlLines = H.p . mconcat

-- | light (dark) mode toggle
lightSwitch :: H.Html
lightSwitch = aslightSwitch (H.a "")

-- | set an html element to light (dark) mode toggle
aslightSwitch :: H.Html -> H.Html
aslightSwitch h = h ! A.id "dark" ! A.onclick "dark_mode()"

-- | raw markdown
markdown :: Text -> H.Html
markdown s = case result of
  Right h -> h
  Left err -> error $ show err
  where
    result = P.runPure $ P.readMarkdown P.def s >>= P.writeHtml5 P.def
