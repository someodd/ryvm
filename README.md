# ryvm: Rank You Very Much

It's like Google, but for your file system.

Search using standard input, get results via standard output in TSV format.

This project is in alpha. It is not optimized, the code is not clean, etc. It's a mess. The API may change drastically.

This is being ported over my [bore](https://github.com/someodd/bore) project. I eventually want it to be a dependency of `bore`.

For now try this command:

```
echo "/home/tilde/Projects/gopherhole_bore/\nlinux" | stack run > out.log
```

## Why not `grep`

AFAIK, `grep` doesn't do anything beyond simple pattern matching, whereas this does stuff like actually ranking relevance based on things like proximity of keywords.

## Features

* Formally verified with LiquidHaskell
* Property-based testing (currently not working.)