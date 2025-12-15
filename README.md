# xreferee

Validate cross references throughout a git repo.

It's often useful to link two different locations in a codebase, and it might not always be possible to enforce it by importing a common source of truth. Some examples:
* Keeping two constants in sync across files in two different languages
* Linking an implementation to markdown files or comments documenting the design
* Referencing an invariant documented on a field definition at the call-site

See GHC's wiki on how they've found cross references helpful: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/coding-style#2-using-notes.

You can use this tool in CI or pre-commit hooks to validate that cross references across a repository are valid. For example:

```markdown
This is a _markdown_ **file** documenting a feature.
We can mark this as a source of truth with a Markdown comment:
<!-- #(ref:my-feature) -->
```

```python
# In my Python code, add a reference to the markdown document, where
# you know you can just search for a matching anchor tag
# See @(ref:my-feature)
def my_feature():
    pass

# Maybe the Python file is also the source of truth for a constant:
# #(ref:my-version-123)
MY_VERSION = 123
```

```javascript
// Then in my Javascript file, we can use a cross reference to ensure they're
// kept in sync. If the label above is updated to `my-version-124`, then this
// cross reference will be broken, and xreferee will flag it.
// @(ref:my-version-123)
const MY_VERSION = 123
```

## Installation

Go to the GitHub releases page and download an artifact.

Alternatively, clone this repo, install the Haskell toolchain, and build it.

## Usage

```shell
# Run this command in CI or pre-commit hooks to validate all cross references
xreferee
```

## Comparison with other tools

### [tagref](https://github.com/stepchowfun/tagref)

`xreferee` uses `git grep` internally, which does the heavy lifting of searching, while `tagref` walks the file tree itself. Since `git grep` is heavily optimized, `xreferee` should have better performance.

Using a git repo provisioned from the largest file in `data/fixtures/` with [hyperfine](https://github.com/sharkdp/hyperfine):

```console
$ hyperfine -N --warmup 10 --runs 200 xreferee tagref
Benchmark 1: xreferee
  Time (mean ± σ):     165.6 ms ±   5.6 ms    [User: 219.5 ms, System: 246.2 ms]
  Range (min … max):   156.2 ms … 229.8 ms    200 runs

Benchmark 2: tagref
  Time (mean ± σ):     487.9 ms ±   9.4 ms    [User: 876.3 ms, System: 91.7 ms]
  Range (min … max):   453.5 ms … 522.0 ms    200 runs

Summary
  xreferee ran 2.95 ± 0.12 times faster than tagref
```
