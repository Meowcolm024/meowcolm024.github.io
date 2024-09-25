--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (toLower)
import Data.Text (Text)
import Hakyll
import Text.Pandoc
import Text.Pandoc.Walk (walk)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "scripts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateBodyCompiler

  match "templates/partials/*" $ compile templateCompiler

  match (fromList ["about.md"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/_*.html" . map toLower)

  tagsRules tags $ \tagStr tagsPattern -> do
    route idRoute
    compile $ do
      posts <- loadAll tagsPattern >>= recentFirst
      let postsCtx =
            constField "title" ("tag/" <> tagStr)
              <> listField "posts" postCtx (return posts)
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

  -- tags page
  create ["tags/index.html"] $ do
    route idRoute
    let tagsCtx =
          tagCloudField "body" 100.0 300.0 tags
            <> constField "title" "tags"
            <> defaultContext
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    let tagsCtx = tagsField "tags" tags <> postCtx
    compile $
      pandocCompiler'
        >>= loadAndApplyTemplate "templates/post.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

  -- archive page
  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let archiveCtx =
            listField "posts" postCtx (pure posts)
              <> constField "title" "archives"
              <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let indexCtx =
            listField "posts" postCtx (pure (take 5 posts)) <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs"
    }

--------------------------------------------------------------------------------
pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Nothing,
        writerHTMLMathMethod = MathML -- use Mathjax
      }
    pandocTransformer

pandocTransformer :: Pandoc -> Pandoc
pandocTransformer = walk go
  where
    go (CodeBlock (_, classes, _) contents) = addPrism classes contents
    go (Header 2 attr contents) = addH2Link attr contents
    go x = x

-- generate correct code block for prism.js
addPrism :: [Text] -> Text -> Block
addPrism classes contents =
  RawBlock (Format "html") $
    "<pre>" <> code classes <> contents <> "</code></pre>"
  where
    -- generate language tag for prism
    code [] = "<code>"
    code (h : _) = "<code class=\"language-" <> h <> "\">"

-- add section link to <h2> only
addH2Link :: (Text, [Text], [(Text, Text)]) -> [Inline] -> Block
addH2Link attr@(idAttr, _, _) contents =
  Header 2 attr ([link, Space] <> contents)
  where
    link = Link nullAttr [Str "#"] ("#" <> idAttr, "")
