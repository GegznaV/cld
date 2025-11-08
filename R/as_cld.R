#' Convert objects to cld_object
#'
#' Generic function to convert various objects to `cld_object` format.
#' This is useful when you have CLD results from other packages or
#' custom formats and want to use them with cld's methods.
#'
#' @param x Object to convert to `cld_object`
#' @param ... Additional arguments passed to methods
#'
#' @return A `cld_object` (data frame with class `c("cld_object", "data.frame")`)
#'
#' @details
#' The `as_cld()` function provides a way to convert various formats to
#' the standard `cld_object` structure. This is particularly useful when:
#' * Working with CLD results from other packages
#' * Converting custom formats to use cld's print and coercion methods
#' * Creating CLD objects programmatically
#'
#' @export
#'
#' @examples
#' # Convert from data frame
#' df <- data.frame(
#'   group = c("A", "B", "C"),
#'   cld = c("a", "b", "b")
#' )
#' as_cld(df)
#'
#' # Convert from named character vector
#' letters_vec <- c(A = "a", B = "b", C = "b")
#' as_cld(letters_vec)
as_cld <- function(x, ...) {
  UseMethod("as_cld")
}

#' @rdname as_cld
#' @export
as_cld.data.frame <- function(x, ...) {
  # Validate required columns
  if (!"group" %in% names(x)) {
    stop("Data frame must have a 'group' column.", call. = FALSE)
  }
  if (!"cld" %in% names(x)) {
    stop("Data frame must have a 'cld' column.", call. = FALSE)
  }

  # Create spaced_cld if not present
  if (!"spaced_cld" %in% names(x)) {
    # Calculate spaced version
    max_len <- max(nchar(x$cld))
    x$spaced_cld <- vapply(x$cld, function(letters) {
      spaces_needed <- max_len - nchar(letters)
      paste0(strrep("_", floor(spaces_needed / 2)),
        letters,
        strrep("_", ceiling(spaces_needed / 2)))
    }, character(1))
  }

  # Ensure correct column order
  x <- x[, c("group", "cld", "spaced_cld")]

  # Add class if not already present
  if (!"cld_object" %in% class(x)) {
    class(x) <- c("cld_object", class(x))
  }

  x
}

#' @rdname as_cld
#' @export
as_cld.character <- function(x, ...) {
  # Assume x is a named character vector where names are groups
  if (is.null(names(x))) {
    stop("Character vector must be named (names = groups, values = letters).",
      call. = FALSE)
  }

  df <- data.frame(
    group = names(x),
    cld = as.character(x)
  )

  as_cld.data.frame(df)
}

#' @rdname as_cld
#' @export
as_cld.cld_object <- function(x, ...) {
  # Already a cld_object, just return it
  x
}
