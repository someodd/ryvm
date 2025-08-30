# ryvm: Rank You Very Much.

**ryvm** is a command-line tool that searches your local files and ranks the results by relevance. Think of it as Google, but for your own file system.

It is **formally verified** using LiquidHaskell to ensure correctness.

### Why use `ryvm` instead of `grep`?

  * `grep` finds lines in files that match a pattern.
  * `ryvm` finds files that are *relevant* to a search query. It scores results based on things like how close keywords are to each other.

### Quick Start

Search for files containing "your query" in the current directory (`.`):

```bash
ryvm . "your query"
```

The output is a Tab-Separated Value (TSV) list.

### Usage

```
ryvm [OPTIONS] LOCATION "QUERY"
```

  * **`LOCATION`**: The directory path to search in.
  * **`"QUERY"`**: The search term, in quotes.

**Options:**

  * `--stdin`: Read `LOCATION` and `QUERY` from standard input instead of arguments.
  * `--make-relative`: Make result file paths relative to the `LOCATION`.
  * `--ext-whitelist="txt,md"`: A comma-separated list of file extensions to search inside. The default is `"txt"`.

### Examples

**1. Search for a keyword in all Haskell files**

This searches for the keyword "deriving" in all files ending with `.hs` in the current directory.

```bash
ryvm --ext-whitelist="hs" . "deriving"
```

**2. Format search results for easy reading**

This command searches for "deriving" and formats the output with a header and clear labels for score and context.

```bash
q="deriving"; ryvm --ext-whitelist hs . "$q" | awk -F'\t' -v q="$q" '{t[NR]=$2;s[NR]=$3;c[NR]=$4} END {if(NR>0){printf "search results for (%s): %d hits\n\n",q,NR; for(i=1;i<=NR;i++) printf "%s\nHit Score: %s\n%s\n\n",t[i],s[i],c[i]} else {printf "no hits found for (%s)\n",q}}'
```

**3. Generate a Gophermap**

This command's output can be used to create a menu for the [Internet Gopher Protocol](https://en.wikipedia.org/wiki/Gopher_\(protocol\)). Using `--make-relative` is recommended for Gopher selectors.

```bash
ryvm --make-relative . "deriving" | awk -F'\t' '{print "0"$2"\t"$1"\tgopher.example.com\t70"}'
```