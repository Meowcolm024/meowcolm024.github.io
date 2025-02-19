--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.Text (Text)
import Hakyll
import Index (index)
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import qualified Util

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

  match "font/*.woff2" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateBodyCompiler

  match "templates/partials/*" $ compile templateCompiler

  tags <- createTags

  match "posts/*" $ do
    route $ setExtension "html"
    let tagsCtx = tagsField "tags" tags <> postCtx
    compile $
      postCompiler
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

  -- handle other pages in `content/`
  match "content/*.md" $ do
    route contentRoute
    compile $
      postCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  createIndexPage index

--------------------------------------------------------------------------------
createTags :: Rules Tags
createTags = do
  -- tags generation (add `-` to avoid confusion with `index` tag)
  tags <- buildTags "posts/*" $ fromCapture "tags/-*-.html" . map toLower

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

  create ["tags/index.html"] $ do
    route idRoute
    let tagsCtx =
          tagCloudField "body" 100.0 300.0 tags
            <> constField "title" "tags"
            <> defaultContext
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

  pure tags

createIndexPage :: Util.Index -> Rules ()
createIndexPage index = case index of
  Util.Index title content -> create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let indexCtx =
            constField "title" title
              <> constField "body" (Util.generate content)
              <> listField "posts" postCtx (pure (take 5 posts))
              <> defaultContext
      makeItem "" >>= postProc indexCtx
  Util.File name -> match (fromGlob $ "content/" <> name) $ do
    route contentRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let indexCtx =
            listField "posts" postCtx (pure (take 5 posts))
              <> defaultContext
      getResourceBody >>= postProc indexCtx
  where
    postProc indexCtx =
      loadAndApplyTemplate "templates/index.html" indexCtx
        >=> applyAsTemplate indexCtx
        >=> relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

contentRoute :: Routes
contentRoute = gsubRoute "content/" (const "") `composeRoutes` setExtension "html"

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs" -- for GitHub pages
    }

--------------------------------------------------------------------------------
postCompiler :: Compiler (Item String)
postCompiler =
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
