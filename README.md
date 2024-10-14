# Homepage

> This theme is based on [no style, please!](https://github.com/riggraz/no-style-please), and heavily modified for [Hakyll](https://jaspervdj.be/hakyll/).

## Setup

Project built using Cabal 3.10.3 and GHC 9.4.8

```sh
# build site
$ cabal run site build
```

## Features

1. Syntax highlighting via [Prism.js](https://prismjs.com)
2. Section hyperlink (check this [article](https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html))
3. Tags support (check this [article](https://myme.no/posts/2023-01-13-adding-tags-to-hakyll.html))
4. Dark mode support with toggle button (mostly usable)
5. Use `Noto Sans Mono` font from [Google Fonts](https://fonts.google.com/noto/specimen/Noto+Sans+Mono)
6. A simple _eDSL_ to configure the _index_ page (or use your own)

## Notes

- `content/Index.hs`: the (default) _index_ page of the site
- `content/*.md`: other (non-post) pages of the site (like _about_, _contact_ etc)
- `posts/yyyy-mm-dd-*.md`: all your posts here (prefixed with dates)
- `images/favicon.ico`: the _favicon_ of the site

## Homepage customization

To customize homepage contents, modify the `content/Index.hs` file (checkout the file for example). __Note: you probably need to rebuild the site after modifying `Index.hs`__.

- To change the title, modify `title :: String`.
- To modify the contents, modify `content :: Content ()`. You can find a simple _eDSL_ guide below:
  - `section :: String -> Maybe String -> Content () -> Content ()` Creates an entry with a title, optionally an URL, and some contents in the section
  - `posts :: String -> Content ()` Creates an entry of post list with a title
  - `none :: Content ()` Empty content
  - `html :: Html -> Content ()` Raw html contents (checkout [blaze-html](https://hackage.haskell.org/package/blaze-html))
  - `txt :: Text -> Content ()` Text contents (no fancy markups)
  - `markdown :: Text -> Content ()` Markdown contents

If you don't want to use the _eDSL_, you can always create your own index page (e.g. `content/index.html`), and set `index = File "index.html"`.

> Q: Why an eDSL?
>
> A: Building an eDSL is just fun OwO (or I need to write a bunch of code to handle yaml, and it could be less flexible)

## License

Licensed under [MIT](https://opensource.org/license/MIT).
