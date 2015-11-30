# parakeet

## Disclaimer

I know **nothing** about compilers, Haskell and Japanese. This repository is purely for fun, and serving as a test-bed for some Haskell experiments. 

## Introduction

**GOOD NEWS!** We have an experimential online demo powered by GHCJS, see [here](https://foreverbell.github.io/parakeet) for more details.

Build the most convenient tool for Japanese beginners!

Input:
* https://raw.githubusercontent.com/foreverbell/parakeet/master/tests/Butter-fly/Butter-fly.j
* https://raw.githubusercontent.com/foreverbell/parakeet/master/tests/Butter-fly/Butter-fly.r

Output:
![](https://raw.githubusercontent.com/foreverbell/miscellaneous/master/resource/parakeet/Butter-fly.png)

Full output:
* tex: https://raw.githubusercontent.com/foreverbell/miscellaneous/master/resource/parakeet/Butter-fly.tex
* pdf: https://raw.githubusercontent.com/foreverbell/miscellaneous/master/resource/parakeet/Butter-fly.pdf

Romaji should follow [Hepburn romanization](https://en.wikipedia.org/wiki/Hepburn_romanization).

## Installation

To build, at least ghc **7.10.2** is required.

```bash
$ cabal install parakeet.cabal
```

For stack users,  **lts-3.13** resolver is recommended.

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
$ parakeet -j Butter-fly.j -r Butter-fly.r -o Butter-fly.tex
$ xelatex Butter-fly.tex
```

You should guarantee that the two input files are encoded in UTF-8.

## Limitations

* The parsing algorithm is essentially LL(infinity), it is an exponential algorithm of course! So the program may get extremely slow when there is a mistake in a long line of romaji. A proper use of separator `$` can avoid this trap.
* The long vowel `ō` is ambiguous in Hepburn romanization, which is interpreted to `ou` or `oo`. To resolve this, we always pick the former one. For example, `東京(Tōkyō)` is correctly translated to `とうきょう`, while `大阪(Ōsaka)` is wrongly translated to `おうさか`.
* There are two `zu`s and `ji`s in romanization, namely `ずづ` and `じぢ` in hiragana respectively. We always pick `ずじ` when translating `zu` and `ji` into furigana. If you want `づぢ`, use `du(dzu)` and `di(dji)` instead.

## Document

Since I haven't find any potential users, so there will be no document available, please create an issue if you have trouble using it.

## TODO List

* Ambiguous `ō` warning.
* Extended katakana support.
* Exception type for robustness.
