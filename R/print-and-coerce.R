#' Print and coercion methods for cld_object
#'
#' @param x A `cld_object` to print or convert
#' @param ... Additional arguments passed to print methods
#'
#' @return
#' * `print.cld_object()`: Invisibly returns the input object
#' * `as.data.frame.cld_object()`: Returns a plain data frame
#' * `as.character.cld_object()`: Returns a named character vector of letters
#' * `as_tibble.cld_object()`: Returns a tibble
#'
#' @name cld_methods
#' @export
#'
#' @examples
#' obj <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
#' result <- make_cld(obj)
#'
#' # Print method
#' print(result)
#'
#' # Convert to plain data frame
#' as.data.frame(result)
#'
#' # Convert to named character vector
#' as.character(result)
#'
#' # Convert to tibble
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   tibble::as_tibble(result)
#' }
print.cld_object <- function(x, ...) {
  cat("Compact Letter Display (CLD)\n")

  alpha_val <- attr(x, "alpha")
  if (!is.null(alpha_val)) {
    cat("Signif. level (alpha): ", alpha_val, "\n")
  }

  method_val <- attr(x, "method")
  if (!is.null(method_val)) {
    cat("Method: ", method_val, "\n\n")
  }

  # Print as data frame without row names
  print(as.data.frame(x), row.names = FALSE, ...)

  invisible(x)
}

#' @rdname cld_methods
#' @export
#' @importFrom stats setNames
as.data.frame.cld_object <- function(x, ...) {
  # Remove cld_object class to return plain data frame
  class(x) <- setdiff(class(x), "cld_object")
  x
}

#' @rdname cld_methods
#' @export
#' @importFrom stats setNames
as.character.cld_object <- function(x, ...) {
  # Return named character vector: names are groups, values are letters
  setNames(x$cld, x$group)
}

#' @rdname cld_methods
#' @exportS3Method tibble::as_tibble
as_tibble.cld_object <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required for as_tibble(). Please install it.",
      call. = FALSE)
  }
  # Convert to plain data frame first, then to tibble
  df <- as.data.frame.cld_object(x)
  tibble::as_tibble(df, ...)
}
