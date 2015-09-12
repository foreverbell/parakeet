# obtuse-parakeet

## Disclaimer

I know **nothing** about Compiler Theory, Haskell and Japanese. This repository is purely for fun, and serving as a test-bed for some Haskell experiments. 

So this repository is not intended to upload to Hackage.

## Introduction

Input:
* https://raw.githubusercontent.com/foreverbell/obtuse-parakeet/master/tests/Butter-fly/Butter-fly.j
* https://raw.githubusercontent.com/foreverbell/obtuse-parakeet/master/tests/Butter-fly/Butter-fly.r

Output:
![](https://raw.githubusercontent.com/foreverbell/obtuse-parakeet/master/tests/Butter-fly/Butter-fly.png)

Rōmaji should follow [Hepburn romanization](https://en.wikipedia.org/wiki/Hepburn_romanization), which is used by Google translate. And also it is the most commonly used rōmaji in China.

## Installation

To build, at least ghc **7.10.2** is required.

```bash
$ cabal install obtuse-parakeet.cabal
```

## Development

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

## Usage

* XeLaTex package dependencies: xeCJK, ruby
* Font dependencies: MS Mincho, MS Gothic

```bash
$ obtuse-parakeet -j Butter-fly.j -r Butter-fly.r -o Butter-fly.tex
$ xelatex Butter-fly.tex
```

You should guarantee that the two input files are encoded in UTF-8.

## Limitations

* Kanji matching is based on the `try` combinator of Haskell library `Parsec`, enumerating every possible matching (an exponential algorithm of course). So the program will get extremely slow when there is a mistake in a long line of rōmaji.
* The long vowel `ō` is ambiguous in Hepburn romanization, which is interpreted to `ou` or `oo`. To resolve this, we always pick the former one. For example, `東京(Tōkyō)` is correctly translated to `とうきょう`, while `大阪(Ōsaka)` is wrongly translated to `おうさか`.
* Kana iteration mark `ゝ` and `ヽ` is not supported.
* .. and other things.

## TODO List

* Ambiguous `ō` warning.

