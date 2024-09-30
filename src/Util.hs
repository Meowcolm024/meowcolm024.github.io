{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Util where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS
import Data.String
import Data.Text (Text)
import Data.Void (Void)
import Text.Blaze.Html ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P

-- | different types of index page
data Index = Index String (Content ()) | File String

type Content = RWST Void H.Html Int (Except String)

-- | add a section to index page
section :: String -> Maybe String -> Content () -> Content ()
section title url content = do
  h' <- h
  let hd s = case url of
        Just u -> H.a (h' s) ! A.href (fromString u)
        Nothing -> h' s
  tell $ hd $ H.toHtml title
  nested content

-- | add the post section with recent post lists
posts :: String -> Content ()
posts title = do
  h' <- h
  tell $ h' $ H.toHtml title
  tell $ H.preEscapedToHtml @String "$partial(\"templates/partials/recent-list.html\")$"

-- | empty content
none :: Content ()
none = pure ()

-- | raw html
html :: H.Html -> Content ()
html = tell

-- | simple text
txt :: String -> Content ()
txt = html . H.pre . H.toHtml

-- | from markdown
markdown :: Text -> Content ()
markdown s = case result of
  Right h -> html h
  Left err -> error $ show err
  where
    result = P.runPure $ P.readMarkdown P.def s >>= P.writeHtml5 P.def

-- | light (dark) mode toggle
lightSwitch :: H.Html
lightSwitch = H.a "" ! A.id "dark" ! A.onclick "dark_mode()"

-- generate the index page
generate :: Content a -> String
generate c = case runExcept $ runRWST c undefined 2 of
  Right (_, _, h) -> renderHtml h
  Left err -> error err

-- increase heading level for nested content
nested :: Content a -> Content a
nested inner = do
  lv <- get
  put (lv + 1)
  r <- inner
  put lv
  pure r

-- heading indexed by level
h :: Content (H.Html -> H.Html)
h = do
  lv <- get
  if lv < 2 || lv > 6
    then lift $ throwE "invalid heading level"
    else
      pure $ [H.h2, H.h3, H.h4, H.h5, H.h6] !! (lv - 2)
