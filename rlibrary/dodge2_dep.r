#### https://github.com/tidyverse/ggplot2/blob/master/R/position-dodge2.r
#' @export
#' @rdname position_dodge
#' @param padding Padding between elements at the same position. Elements are
#'   shrunk by this proportion to allow space between them. Defaults to 0.1.
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'   This is useful if you're rotating both the plot and legend.
position_dodge2 <- function(width = NULL, preserve = c("total", "single"),
                            padding = 0.1, reverse = FALSE) {
  ggproto(NULL, PositionDodge2,
          width = width,
          preserve = match.arg(preserve),
          padding = padding,
          reverse = reverse
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodge2 <- ggproto("PositionDodge2", PositionDodge,
                          preserve = "total",
                          padding = 0.1,
                          reverse = FALSE,

                          setup_params = function(self, data) {
                            if (is.null(data$xmin) &&
                              is.null(data$xmax) &&
                              is.null(self$width)) {
                              warning("Width not defined. Set with `position_dodge2(width = ?)`",
                                      call. = FALSE)
                            }

                            if (identical(self$preserve, "total")) {
                              n <- NULL
                            } else {
                              panels <- unname(split(data, data$PANEL))
                              if ("x" %in% names(data)) {
                                # Point geom
                                groups <- lapply(panels, function(panel) table(panel$x))
                              } else {
                                # Interval geom
                                groups <- lapply(panels, find_x_overlaps)
                              }
                              n_groups <- vapply(groups, max, double(1))
                              n <- max(n_groups)
                            }

                            list(
                              width = self$width,
                              n = n,
                              padding = self$padding,
                              reverse = self$reverse
                            )
                          },

                          compute_panel = function(data, params, scales) {
                            collide2(
                              data,
                              params$width,
                              name = "position_dodge2",
                              strategy = pos_dodge2,
                              n = params$n,
                              padding = params$padding,
                              check.width = FALSE,
                              reverse = params$reverse
                            )
                          }
)

pos_dodge2 <- function(df, width, n = NULL, padding = 0.1) {
  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  # xid represents groups of boxes that share the same position
  df$xid <- find_x_overlaps(df)

  # based on xid find newx, i.e. the center of each group of overlapping
  # elements. for boxes, bars, etc. this should be the same as original x, but
  # for arbitrary rects it may not be
  newx <- (tapply(df$xmin, df$xid, min) + tapply(df$xmax, df$xid, max)) / 2
  df$newx <- newx[df$xid]

  if (is.null(n)) {
    # If n is null, preserve total widths of elements at each position by
    # dividing widths by the number of elements at that position
    n <- table(df$xid)
    df$new_width <- (df$xmax - df$xmin) / as.numeric(n[df$xid])
  } else {
    df$new_width <- (df$xmax - df$xmin) / n
  }

  # Find the total width of each group of elements
  group_sizes <- stats::aggregate(
    list(size = df$new_width),
    list(newx = df$newx),
    sum
  )

  # Starting xmin for each group of elements
  starts <- group_sizes$newx - (group_sizes$size / 2)

  # Set the elements in place
  for (i in seq_along(starts)) {
    divisions <- cumsum(c(starts[i], df[df$xid == i, "new_width"]))
    df[df$xid == i, "xmin"] <- divisions[-length(divisions)]
    df[df$xid == i, "xmax"] <- divisions[-1]
  }

  # x values get moved to between xmin and xmax
  df$x <- (df$xmin + df$xmax) / 2

  # If no elements occupy the same position, there is no need to add padding
  if (!any(duplicated(df$xid))) {
    return(df)
  }

  # Shrink elements to add space between them
  df$pad_width <- df$new_width * (1 - padding)
  df$xmin <- df$x - (df$pad_width / 2)
  df$xmax <- df$x + (df$pad_width / 2)

  df$xid <- NULL
  df$newx <- NULL
  df$new_width <- NULL
  df$pad_width <- NULL

  df
}

# Find groups of overlapping elements that need to be dodged from one another
find_x_overlaps <- function(df) {
  overlaps <- numeric(nrow(df))
  overlaps[1] <- counter <- 1

  for (i in seq_asc(2, nrow(df))) {
    if (df$xmin[i] >= df$xmax[i - 1]) {
      counter <- counter + 1
    }
    overlaps[i] <- counter
  }
  overlaps
}


##### https://github.com/tidyverse/ggplot2/blob/master/R/position-collide.r
# Detect and prevent collisions.
# Powers dodging, stacking and filling.
collide_setup <- function(data, width = NULL, name, strategy,
                          check.width = TRUE, reverse = FALSE) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin <- data$x
      data$xmax <- data$x
    }

    # Width determined from data, must be floating point constant
    widths <- unique(data$xmax - data$xmin)
    widths <- widths[!is.na(widths)]

    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(widths))) {
    #       warning(name, " requires constant width: output may be incorrect",
    #         call. = FALSE)
    #     }
    width <- widths[1]
  }

  list(data = data, width = width)
}

collide <- function(data, width = NULL, name, strategy,
                    ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$xmin, data$group),]
  } else {
    data <- data[order(data$xmin, -data$group),]
  }

  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping x intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$ymax)) {
    dapply(data, "xmin", strategy, ..., width = width)
  } else if (!is.null(data$y)) {
    data$ymax <- data$y
    data <- dapply(data, "xmin", strategy, ..., width = width)
    data$y <- data$ymax
    data
  } else {
    stop("Neither y nor ymax defined")
  }
}

# Alternate version of collide() used by position_dodge2()
collide2 <- function(data, width = NULL, name, strategy,
                     ..., check.width = TRUE, reverse = FALSE) {
  dlist <- collide_setup(data, width, name, strategy, check.width, reverse)
  data <- dlist$data
  width <- dlist$width

  # Reorder by x position, then on group. The default stacking order is
  # different than for collide() because of the order in which pos_dodge2 places
  # elements
  if (reverse) {
    data <- data[order(data$x, -data$group),]
  } else {
    data <- data[order(data$x, data$group),]
  }

  pos <- match.fun(strategy)
  pos(data, width, ...)
}


#### https://github.com/tidyverse/ggplot2/blob/master/R/utilities.r

#' @export
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(alpha = 0.5, colour = "blue")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(colour = alpha("blue", 0.5))
scales::alpha

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

# Check required aesthetics are present
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @param character vector of required aesthetics
# @param character vector of present aesthetics
# @param name of object for error message
# @keyword internal
check_required_aesthetics <- function(required, present, name) {
  missing_aes <- setdiff(required, present)
  if (length(missing_aes) == 0) return()

  stop(name, " requires the following missing aesthetics: ",
       paste(missing_aes, collapse = ", "), call. = FALSE)
}

# Concatenate a named list for output
# Print a `list(a=1, b=2)` as `(a=1, b=2)`
#
# @param list to concatenate
# @keyword internal
#X clist(list(a=1, b=2))
#X clist(par()[1:5])
clist <- function(l) {
  paste(paste(names(l), l, sep = " = ", collapse = ", "), sep = "")
}


# Test whether package `package` is available. `fun` provides
# the name of the ggplot2 function that uses this package, and is
# used only to produce a meaningful error message if the
# package is not available.
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun, "`.\n",
       "Please install and try again.", call. = FALSE)
}

# Return unique columns
# This is used for figuring out which columns are constant within a group
#
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, function(x) length(unique(x)) == 1), drop = FALSE]
  rownames(df) <- 1:nrow(df)
  df
}

#' Convenience function to remove missing values from a data.frame
#'
#' Remove all non-complete rows, with a warning if `na.rm = FALSE`.
#' ggplot is somewhat more accommodating of missing values than R generally.
#' For those stats which require complete data, missing values will be
#' automatically removed with a warning. If `na.rm = TRUE` is supplied
#' to the statistic, the warning will be suppressed.
#'
#' @param df data.frame
#' @param na.rm If true, will suppress warning message.
#' @param vars Character vector of variables to check for missings in
#' @param name Optional function name to improve error message.
#' @param finite If `TRUE`, will also remove non-finite values.
#' @keywords internal
#' @export
remove_missing <- function(df, na.rm = FALSE, vars = names(df), name = "",
                           finite = FALSE) {
  stopifnot(is.logical(na.rm))

  vars <- intersect(vars, names(df))
  if (name != "") name <- paste(" (", name, ")", sep = "")

  if (finite) {
    missing <- !cases(df[, vars, drop = FALSE], is_finite)
    str <- "non-finite"
  } else {
    missing <- !cases(df[, vars, drop = FALSE], is_complete)
    str <- "missing"
  }

  if (any(missing)) {
    df <- df[!missing,]
    if (!na.rm) {
      warning_wrap(
        "Removed ", sum(missing), " rows containing ", str, " values", name, ""
      )
    }
  }

  df
}

# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(ok)) {
    all(ok)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

# Wrapper around is.finite to handle list cols
is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    is.finite(x)
  }
}

is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}


#' Used in examples to illustrate when errors should occur.
#'
#' @param expr code to evaluate.
#' @export
#' @keywords internal
#' @examples
#' should_stop(stop("Hi!"))
#' should_stop(should_stop("Hi!"))
should_stop <- function(expr) {
  res <- try(print(force(expr)), TRUE)
  if (!inherits(res, "try-error")) stop("No error!", call. = FALSE)
  invisible()
}


#' A waiver object.
#'
#' A waiver is a "flag" object, similar to `NULL`, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (`NULL`) and
#' displaying a default value calculated elsewhere (`waiver()`)
#'
#' @export
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Give a deprecation error, warning, or message, depending on version number.
#'
#' This function is deprecated.
#'
#' @param version The last version of ggplot2 where this function was good
#'   (in other words, the last version where it was not deprecated).
#' @param msg The message to print.
#' @keywords internal
#' @export
gg_dep <- function(version, msg) {
  .Deprecated()
  v <- as.package_version(version)
  cv <- utils::packageVersion("ggplot2")

  # If current major number is greater than last-good major number, or if
  #  current minor number is more than 1 greater than last-good minor number,
  #  give error.
  if (cv[[1, 1]] > v[[1, 1]] || cv[[1, 2]] > v[[1, 2]] + 1) {
    stop(msg, " (Defunct; last used in version ", version, ")",
         call. = FALSE)

    # If minor number differs by one, give warning
  } else if (cv[[1, 2]] > v[[1, 2]]) {
    warning(msg, " (Deprecated; last used in version ", version, ")",
            call. = FALSE)

    # If only subminor number is greater, give message
  } else if (cv[[1, 3]] > v[[1, 3]]) {
    message(msg, " (Deprecated; last used in version ", version, ")")
  }

  invisible()
}

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(rep(FALSE, length(x)))
  }

  !is.na(nms) & nms != ""
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  stop('Please use `to_lower_ascii()`, which works fine in all locales.', call. = FALSE)
}

toupper <- function(x) {
  stop('Please use `to_upper_ascii()`, which works fine in all locales.', call. = FALSE)
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub("", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

# This function checks that all columns of a dataframe `x` are data and
# returns the names of any columns that are not.
# We define "data" as atomic types or lists, not functions or otherwise
check_nondata_cols <- function(x) {
  idx <- (vapply(x, function(x) rlang::is_vector(x), logical(1)))
  names(x)[which(!idx)]
}

compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

is.formula <- function(x) inherits(x, "formula")

deparse2 <- function(x) {
  y <- deparse(x, backtick = TRUE)
  if (length(y) == 1) {
    y
  } else {
    paste0(y[[1]], "...")
  }
}

message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

warning_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  warning(paste0(wrapped, collapse = "\n"), call. = FALSE)
}

var_list <- function(x) {
  x <- encodeString(x, quote = "`")
  if (length(x) > 5) {
    x <- c(x[1:5], paste0("and ", length(x) - 5, " more"))
  }

  paste0(x, collapse = ", ")
}

dispatch_args <- function(f, ...) {
  args <- list(...)
  formals <- formals(f)
  formals[names(args)] <- args
  formals(f) <- formals
  f
}

is_missing_arg <- function(x) identical(x, quote(expr =))
# Get all arguments in a function as a list. Will fail if an ellipsis argument
# named .ignore
# @param ... passed on in case enclosing function uses ellipsis in argument list
find_args <- function(...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))

  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]

  modify_list(vals, list(..., `...` = NULL))
}

# Used in annotations to ensure printed even when no
# global data
dummy_data <- function() new_data_frame(list(x = NA), n = 1)

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

seq_asc <- function(to, from) {
  if (to > from) {
    integer()
  } else {
    to:from
  }
}

# Needed to trigger package loading
#' @importFrom tibble tibble
NULL

# Check inputs with tibble but allow column vectors (see #2609 and #2374)
as_gg_data_frame <- function(x) {
  x <- lapply(x, validate_column_vec)
  new_data_frame(tibble::as_tibble(x))
}

validate_column_vec <- function(x) {
  if (is_column_vec(x)) {
    dim(x) <- NULL
  }
  x
}

is_column_vec <- function(x) {
  dims <- dim(x)
  length(dims) == 2L && dims[[2]] == 1L
}

# Parse takes a vector of n lines and returns m expressions.
# See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#
# parse(text = c("alpha", "", "gamma"))
# #> expression(alpha, gamma)
#
# parse_safe(text = c("alpha", "", "gamma"))
# #> expression(alpha, NA, gamma)
#
parse_safe <- function(text) {
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}