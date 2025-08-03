--------------------------------------------------------------------------------
import Control.Monad (forM)
import Data.Aeson (encode, object, (.=))
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll
import Text.Blaze (ToValue (toValue), (!))
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
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

  match "font/*.woff2" $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateBodyCompiler

  match "templates/partials/*" $ compile templateCompiler

  tagsCtx' <- createTagsCtx

  match "posts/*" $ do
    route $ setExtension "html"
    let tagsCtx = tagsCtx' <> postCtx
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

  -- index page
  match (fromGlob "content/index.html") $ do
    route contentRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.md"
      let indexCtx =
            listField "posts" postCtx (pure (take 5 posts))
              <> defaultContext
      getResourceBody
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= applyAsTemplate indexCtx
        >>= relativizeUrls

--------------------------------------------------------------------------------
createTagsCtx :: Rules (Context a)
createTagsCtx = do
  tags <- tagsMap <$> buildTags "posts/*" undefined

  create ["tags.json"] $ do
    route idRoute
    let insertTag (tag, postObj) = M.insertWith (++) tag [postObj]
    let createEntry tag id' = do
          metadata <- getMetadata id'
          let title = fromMaybe "untitled" (lookupString "title" metadata)
          url <- getRoute id'
          time <- getItemUTC defaultTimeLocale id'
          let date = formatTime defaultTimeLocale "%B %e, %Y" time
          pure (tag, object ["title" .= title, "url" .= url, "date" .= date])
    compile $ do
      allPostMetadata <- sequence [createEntry tag id' | (tag, ids) <- tags, id' <- ids]
      makeItem (encode $ foldr insertTag M.empty allPostMetadata)

  match (fromGlob "content/tags.html") $ do
    route contentRoute
    compile $ do
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  let simpleRenderLink tag = fmap $ \url ->
        H.a
          ! A.title (H.stringValue ("All pages tagged '" <> tag <> "'."))
          ! A.href (toValue url)
          $ toHtml tag

  pure $ field "tags" $ \item -> do
    tags' <- getTags $ itemIdentifier item
    links <- forM tags' $ \tag -> do
      route' <- getRoute "content/tags.html"
      pure $ simpleRenderLink tag ((<> "?tag=" <> tag) . toUrl <$> route')
    pure $ renderHtml $ (mconcat . intersperse ", ") $ catMaybes links

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
