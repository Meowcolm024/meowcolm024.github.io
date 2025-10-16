# Homepage

> This theme is based on [no style, please!](https://github.com/riggraz/no-style-please), and heavily modified for [Hakyll](https://jaspervdj.be/hakyll/).

## Setup

Project built using Stack

```sh
# build site
$ stack run build
```

## Features

1. Syntax highlighting via [Prism.js](https://prismjs.com)
2. Section hyperlink (check this [article](https://frasertweedale.github.io/blog-fp/posts/2020-12-10-hakyll-section-links.html))
3. Tags support (JavaScript required)
4. Dark mode support with toggle button (mostly usable)
5. Fonts:
  - `Fira Mono` for texts (check [this](https://github.com/mozilla/Fira))
  - `Julia Mono` for code blocks (check [this](https://github.com/cormullion/juliamono))

## Notes

- `content/index.html`: the _index_ page of the site
- `content/tags.html`: dummy for the tags page
- `content/*.md`: other (non-post) pages of the site (like _about_, _contact_ etc)
- `posts/yyyy-mm-dd-*.md`: all your posts here (prefixed with dates)
- `images/favicon.ico`: the _favicon_ of the site

## License

Licensed under [MIT](https://opensource.org/license/MIT).
