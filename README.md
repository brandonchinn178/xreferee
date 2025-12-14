# xreferee

Validate cross references throughout a git repo.

It's often useful to link two different locations in a codebase, and it might not always be possible to enforce it by importing a common source of truth. Some examples:
* Keeping two constants in sync across files in two different languages
* Linking an implementation to markdown files or comments documenting the design

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
