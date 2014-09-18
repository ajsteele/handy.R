################################################################################
###  VARIABLE DEFINITION  ######################################################
################################################################################
default.logfile.name <- 'log.txt'

################################################################################
###  DATA FRAMES  ##############################################################
################################################################################

sample.df <- function(x, size = 1, replace = FALSE, prob = NULL) {
  # Samples rows from a data frame.
  #
  # Args:
  #       x: A data frame.
  #   size, replace, prob: Arguments from the sample function.
  #
  # Returns:
  #   size rows from this data frame (default 1), with or without replacement
  #   and with an optional probability vector.
  x[sample(nrow(x), size, replace, prob), ]
}

withoutCols <- function(df, cols) {
  # Returns a vector to allow certain columns to be excluded from a
  # data frame. The case where the columns being excluded are referred to
  # numerically is trivial, but is included as well for generality.
  #
  # Args:
  #      data: a data frame.
  #      cols: the columns to be excluded, as a vector or scalar of number(s) or
  #            name(s).
  #
  # Returns:
  #      In the text case, a vector of booleans; TRUE for columns to include.
  #      In the numerical case, R understands df[-3,], so just minus the input.
  if(is.character(cols)) { return(!(names(df) %in% cols)) }
  else { return(-cols) }
}

explodeByCol <- function(df, col, sep=',', regex = NULL,
                         fixed = is.null(regex)) {
  # If your data frame contains multiple values in a single column, this splits
  # multiple values across different rows, using either a separator character or
  # a regular expression.
  #
  # Args:
  #       df: a data frame.
  #      col: the column to explode by.
  #      sep: the separator of the multiple values.
  #    regex: a regular expression which matches the values you're looking for;
  #           overrides sep.
  #    fixed: whether a strsplit or a regexp, this determines whether to search
  #           for a pattern (TRUE) or a fixed string (FALSE). Defaults to TRUE
  #           where regex is NULL, and FALSE if not, to coincide with the most
  #           likely use-cases for each.
  #
  # Returns:
  #      A data frame with new rows, one for each value in the exploded column.
  
  # data frames fairly often come in with 'character' columns which are factors,
  # and these string-based functions can't handle that, so convert with a
  # warning
  if(is.factor(df[, col])) {
    warning(
      paste0('The column you passed is a factor, and has been coerced',
             ' to a character for exploding.')
    )
    df[, col] <- as.character(df[, col])
  } else if(!is.character(df[, col])) {
    # if it's not character data, it won't work, so pass an error
    stop(
      paste0('The column passed to explodeByType should be character ',
             'data; it is of class ', class(df[, col]), '.'
      )
    )
  }
  
  # if regex is NULL, use the separator provided
  if(is.null(regex)) {
    exploded <- strsplit(df[, col], sep, fixed = fixed)
  } else {
    exploded <- regmatches(df[, col], gregexpr(regex, df[, col]), fixed = fixed)
  }
  # how many of each row should I create? ie 1,1,2,1,0
  n.exploded <- sapply(exploded, length)
  # turn that into a list of data frame row indices, ie 1,2,3,3,4
  n.exploded.rows <- unlist(
    sapply(1:length(n.exploded), # loop over i, the row index
           function(i) {
             rep(i, n.exploded[i])  # the row number i, n times
           }
           )
    )
  # take the data frame and repeat rows the relevant number of times
  df <- df[n.exploded.rows, ]
  # fill its RS ID column with the appropriate IDs
  df[, col] <- unlist(exploded)
  df
}

################################################################################
###  CHARACTERS  ###############################################################
################################################################################

removeWhitespace <- function(x) { gsub("\\s","", x) }
# For a character or vector of characters x, removes all spaces and line
# breaks.
#
# Args:
#      x: A character or vector of characters.
#
# Returns:
#      The character with whitespace removed.

pastePlus <- function(..., sep=" ", collapse = NULL, recycleZeroLength = TRUE) {
  # Version of the base R paste function which optionally returns nothing if any
  # of the ...s being concatenated have zero length. (Default behaviour is to
  # recycle them to "".)
  #
  # Args:
  # ..., sep, collapse: as paste in base R
  #   ignoreZeroLength: 
  #
  # Returns:
  #      If any of the passed objects has zero length, NULL; otherwise, the
  #      result of the paste function.
  if(!recycleZeroLength &
       any(lapply(list(...), length) == 0)) {
    return(NULL);
  }
  paste(..., sep = sep, collapse = collapse)
}

paste0Plus <- function(..., collapse = NULL, recycleZeroLength = TRUE) {
  pastePlus(..., sep="", collapse = collapse,
            recycleZeroLength = recycleZeroLength)
}

################################################################################
###  FACTORS  ##################################################################
################################################################################

concatFactors <- function(...) {
  # Takes some factors and concatenates them. R coerces factors to integers if
  # you don't convert them to character vectors at the intermediate stage, so
  # this saves typing that every time.
  #
  # Args:
  #      ...: Some factors
  #
  # Returns:
  #      A big factor.
  factor(unlist(lapply(list(...), FUN=as.character)))
}

factorChooseFirst <- function(x, first) {
  # Move a chosen level to be the first in a factor.
  #
  # Args:
  #         x: A factor.
  #     first: The level in the factor you want to be first.
  #
  # Returns:
  #      A factor with the first level redefined to be the one specified.
  
  # if the level requested to be first isn't present, this ain't gonna work
  if (!(first %in% levels(x))) {
    stop(paste("Error: the level", first, "doesn't appear in the factor",
               deparse(substitute(x))))
  }
  factor(x, levels = c(first, levels(x)[levels(x) != first]))
}

allSameLength <- function(x) {
  # Work out whether all elements of a list are the same length.
  #
  # Args:
  #         x: A list.
  #
  # Returns:
  #      TRUE or FALSE, depending.
  length(unique(lapply(x, length))) == 1
}

################################################################################
###  FILES  ####################################################################
################################################################################

list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      ignore.case=FALSE) {
  # Lists the directories present within a path.
  # Credit: http://stackoverflow.com/questions/4749783
  #
  # Args:
  #      See list.files
  #
  # Returns:
  #      A vector of directories within the path being searched.
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  all[file.info(all)$isdir]
}

writeTablePlus <- function(data, filename, comment='', sep='\t',
                           comment.char='#', col.names=NA,  ...) {
  # A wrapper for the write.table function which adds a comment of your choice
  # at the top of the file.
  #
  # Args:
  #     filename: The name of the file to be written.
  #      comment: The comment to be added at the top of the file.
  #          sep: The separator for the data, tab by default.
  # comment.char: The character denoting comments, # by default.
  #    col.names: write.able argument. The NA default here ensures that the
  #               header row is correctly offset given that the first column is
  #               row names.
  #         ... : Allows arbitrary extra arguments relevant to write.table.
  #
  # Returns:
  #      Nothing!
  f <- file(filename, open="wt") # open a connection to the file
  # if there's a comment, write that first
  if(nchar(comment) > 0) {
    # wrap the comment at 80 lines prefixed the comment character plus space
    comment <- strwrap(comment, width=80, prefix=paste(comment.char,' ',sep=''))
    writeLines(comment, f)
  }
  write.table(data, f, sep=sep, col.names=col.names, ...)
  close(f)
}

readTablePlus <- function(filename, sep='\t', comment.char='#', header=TRUE,
                          ...) {
  # Handy wrapper for the read.table function to make it compatible with the
  # writeTablePlus function with its default options.
  read.table(filename, sep=sep, comment.char=comment.char, header=header, ...)
}

justFilename <- function(x) {
  # Returns filenames without extensions.
  #
  # Args:
  #       x: A character or vector of characters containing filenames, with or
  #          without paths.
  #
  # Returns:
  #   The string or vector with everything after and including a final full stop
  #   removed.
  sapply(strsplit(basename(x),"\\."),
         function(x) paste(x[1:(length(x)-1)],
                           collapse=".")
         )
}

fileExt <- function(x) {
  # Returns just extensions from filenames.
  #
  # Args:
  #       x: A character or vector of characters containing filenames, with or
  #          without paths.
  #
  # Returns:
  #   Everything after a final full stop.
  
  # split the strings by full stops, and only take the final element
  extensions <- sapply(strsplit(basename(x),"\\."),
                       function(x) tail(x, 1)
                      )
  # where the extension is the same as the input filename, there is no extension
  extensions[extensions == x] <- ''
  
  extensions
}

suffixFilename <- function(x, suffix = '_1') {
  # Returns a filename with a suffix appended before its extension.
  #
  # Args:
  #       x: A character or vector of characters containing filenames.
  #
  # Returns:
  #   filename_suffix.ext
  paste0(justFilename(x), suffix, '.', fileExt(x))
}

################################################################################
###  MATHEMATICS  ##############################################################
################################################################################

tri <- function(x) {
  # Calculates the xth triangular number.
  #
  # Args:
  #       x: A number.
  #
  # Returns:
  #   The xth triangular number.
  x * (x + 1) / 2
}

trirt <- function(x) {
  # Calculates the triangular root of a number.
  #
  # Args:
  #       x: A number.
  #
  # Returns:
  #   Its triangular root.
  (sqrt(8*x + 1) - 1) / 2
}

# A series of functions which allow arithmetic on quantities with uncertainty.
# Create a quantity by passing values or vectors to unum(x, dx), and then add,
# subtract, multiply or divide with the functions below.
unum <- function(x, dx) { data.frame(x=x, dx=dx) }
  # Calculates the triangular root of a number.
  #
  # Args:
  #       x: A number or vector of numbers.
  #      dx: A number or vector of numbers representing the uncertainty on x.
  #
  # Returns:
  #   A data frame with columns x and dx which can be used for further
  #   operations.
uadd <- function(a, b) {
  z <- a$x + b$x
  dz <- sqrt(a$dx^2 + b$dx^2)
  unum(z, dz)
}
usub <- function(a, b) {
  z <- a$x - b$x
  dz <- sqrt(a$dx^2 + b$dx^2)
  unum(z, dz)
}
umul <- function(a, b) {
  z <- a$x * b$x
  dz <- z * sqrt((a$dx/a$x)^2 + (b$dx/b$x)^2)
  unum(z, dz)
}
udiv <- function(a, b) {
  z <- a$x / b$x
  dz <- z * sqrt((a$dx/a$x)^2 + (b$dx/b$x)^2)
  unum(z, dz)
}

normalise <- function(x, FUN = sum) {
  # Returns a vector normalised by the function FUN, default being sum so the
  # vector would now sum to 1. Another example would be max, so the largest
  # value in x becomes 1.
  #
  # Args:
  #       x: A vector.
  #     FUN: A function which returns a single value when applied to a vector.
  #
  # Returns:
  #   A vector, normalised appropriately.
  if(!is.function(FUN)) stop('Passed FUN is not a function')
  x / FUN(x)
}

################################################################################
###  STATISTICS  ###############################################################
################################################################################

stdErr <- function(x) { sqrt(var(x)/length(x)) }
  # For a vector x, returns the standard error on the mean.
  #
  # Args:
  #      x: A vector.
  #
  # Returns:
  #      The standard error on the mean.

cv <- function(x) { sd(x)/mean(x) }
  # For a vector x, returns the coefficient of variation.
  #
  # Args:
  #      x: A vector.
  #
  # Returns:
  #      The coefficient of variation.

covar <- function(x) {
  # Wrapper function which returns the variance for a single-column vector and
  # a covariance matrix for a multi-column vector.
  #
  # Args:
  #      x: Some data.
  #
  # Returns:
  #      The covariance matrix.
  if(is.null(dim(x))) {
    return(var(x))
  } else {
    return(cov(x))
  }
}

popvar <- function(x, na.rm = FALSE) {
  # Calculates population variance instead of sample variance (which is the
  # default of the var() function in R).
  # 
  # Args:
  #      x: a vector of the population data.
  #  na.rm: a logical value indicating whether NA values should be stripped
  #         before the computation proceeds.
  #
  # Returns:
  #      The population variance.
  if(na.rm) {
    x   <- x[!is.na(x)]
  } else if(any(is.na(x))) {
    return(NA)
  }
  mean((x-mean(x))^2)
}

################################################################################
###  MISCELLANEOUS  ############################################################
################################################################################

NA2val <- function(x, val = 0) {
  # Wrapper to turn NAs in an object into a value of your choice.
  #
  # Args:
  #       x: The object containing errant NA values.
  #     val: The value to replace the NAs with, default 0.
  #
  # Returns:
  #   The object with the NAs replaced appropriately.
  x[is.na(x)] <- val
  x
}

firstElement <- function(x) {
  # Function for apply-ing to lists which will return the first element of a
  # list element
  # Args:
  #       x: An object with elements.
  #
  # Returns:
  #   The first element of that object.
  x[1]
}

initParallel <- function(cores = NULL) {
  # Wrapper to initialise parallel computing functionality.
  #
  # Args:
  #   cores: The number of cores to use simultaneously. If absent, use the
  #          default from registerDoMC, 'approximately half the number of
  #          cores detected by the parallel package'.
  #
  # Returns:
  #   Nothing.
  require(doMC)
  registerDoMC(cores)
  require(foreach)
}

logfileStart <- function(filename = default.logfile.name) {
  # Wrapper for creating a new blank log file during script execution.
  # NB This will silently overwrite existing files!
  #
  # Args:
  #  filename: The name of the file to create.
  #
  # Returns:
  #   Nothing.
  #
  # Globals:
  #   Creates a global variable called logfileName so that related functions
  #   know where to write to.
  logfileName <<- filename
  cat('', file = filename)
}

logfileCat <- function(...,
                       newline = TRUE, sep = "", fill = FALSE,
                       filename = logfileName
                       ) {
  # Wrapper for adding an entry to a log file.
  #
  # Args:
  #      ... : Stuff to write to the file
  #   newline: Whether to start a new line after the entry.
  #       sep: Separator between objects to write.
  #      fill: The fill option for cat().
  #  filename: The name of the file to write to; default being the global
  #            variable set by logfileStart.
  #
  # Returns:
  #   Nothing.
  if(newline & !fill) append.me <- "\n" else append.me <- NULL
  cat(..., append.me, file = filename, sep = sep, fill = fill,
      append = TRUE)
}

logfileEnd <- function() {
  # Wrapper for blanking the existing logfileName such that no further entries
  # are written to it given the default options for logfileCat.
  #
  # Args:
  #  None.
  #
  # Returns:
  #   Nothing.
  #
  # Globals:
  #   Sets logfileName  to "".
  logfileName <<- ""
}

unixTimestamp <-function() {
  # Quick function to generate the UNIX timestamp.
  #
  # Args:
  #   None.
  #
  # Returns:
  #   Time in whole seconds since the start of the Unix epoch (01/01/1970 UTC)
  as.numeric(Sys.time())
}
