---
title: markdown example
tags: test
---

------

## Emphasis

**This is bold text**

*This is italic text*

~~Strikethrough~~

------

## Lists

1. First ordered list item
   1. inside a list
2. Another item
   - but not ordered

- Unorder item 1
- Another one

------

## Images

Image test:

![Stormtroopocat](https://octodex.github.com/images/stormtroopocat.jpg "The Stormtroopocat")

------

## Footnotes

Footnote 1 link[^first].

Footnote 2 link[^second].

Duplicated footnote reference[^second].

[^first]: Footnote **can have markup**

    and multiple paragraphs.

[^second]: Footnote text.

------

## Code and Syntax Highlighting

Inline: use the command `$ cabal build` to build the project.

Code blocks:

```haskell
primes :: [Integer]
primes = filterPrime [2..] where
  filterPrime (p:xs) =
    p : filterPrime [x | x <- xs, x `mod` p /= 0]
```

```coq
Theorem add_assoc : ∀ n m p : nat,
  n + (m + p) = (n + m) + p.
Proof.
  intros n m p. induction n as [| n' IHn'].
  - (* n = 0 *)
    reflexivity.
  - (* n = S n' *)
    simpl. rewrite IHn'. reflexivity. 
Qed.
```

------

## Mathematics

The expression $\sum_{i=1}^{\infty} i^{-2}$ is inlined.

Math blocks:

$$
\frac{1}{1 + \sqrt[3]{2} + \sqrt[3]{4}} = \sqrt[3]{2} - 1
$$

------

## Tables

Colons can be used to align columns.

| Tables        |      Are      |  Cool |
| ------------- | :-----------: | ----: |
| col 3 is      | right-aligned | $1600 |
| col 2 is      |   centered    |   $12 |
| zebra stripes |   are neat    |    $1 |

------

## Blockquotes

> Blockquotes are very handy in email to emulate reply text.
> This line is part of the same quote.
