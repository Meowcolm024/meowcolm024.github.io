# homepage

> The theme is based on [no style, please!](https://github.com/riggraz/no-style-please), and heavily modified for [Hakyll](https://jaspervdj.be/hakyll/).

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
6. A simple _eDSL_ to configure the homepage

## Notes

- `content/Index.hs`: the _index_ page of the site
  - alternatively, you can write your own index page (e.g. `content/index.htm`) and modify the `index :: Index` binding in the `hs` file
- `content/about.md`: the _about_ page of the site
- `posts/yyyy-mm-dd-*.md`: all your posts here (prefixed with dates)
- `images/favicon.ico`: the _favicon_ of the site

## TODO

- [ ] Add more configs to the _eDSL_
