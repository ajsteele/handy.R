handy.R
=======

An R script containing handy functions to save time and typing.

## Style guide

Where possible, follow the [Google Code R style guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml).

Here is a sample function to act as a style guide. Function names should be camelCase unless you have a good reason to name them otherwise (for example, in analogy with existing R functions). If you're making a wrapper for an existing R function, add 'Plus' on the end; for example, the enhanced read.table function is called readTablePlus.

````R
myNewFunction <- function(x, df) {
  # Description of what the function does.
  #
  # Args:
  #     x: Standard name for a scalar or vector.
  #    df: Standard name for a data frame.
  #
  # Returns:
  #   The word 'hello' as a character vector.
  #
  # Globals:
  #  myGlobal: A global variable set by this function. Avoid these whereever possible!
  myGlobal <<- 42
  "hello"
}
```
