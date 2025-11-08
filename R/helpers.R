# Helper function: Extract data from formula --------------------------------
#
#' Extract data from formula
#'
#' Internal helper function to extract data from a formula object,
#' using either the provided data or the formula's environment.
#' This function wraps [stats::model.frame()] and handles the case where
#' data is not explicitly provided by using the formula's environment.
#'
#' @param formula An R formula object specifying the model structure.
#' @param data Optional data frame containing the variables in the formula.
#'   If `NULL`, the formula's environment is used to find the variables.
#' @param ... Additional arguments passed to [stats::model.frame()].
#'
#' @return A data frame extracted using [stats::model.frame()] containing
#'   the variables referenced in the formula.
#'
#' @importFrom stats model.frame
#'
#' @noRd
#' @keywords internal
extract_data <- function(formula, data = NULL, ...) {
  if (is.null(data)) {
    data <- rlang::f_env(formula)
  }
  stats::model.frame(formula, data, ...)
}


# Helper function: Check if matrix is square --------------------------------
#
#' Check if matrix is square
#'
#' Internal helper function to check if a matrix has equal number of rows and columns.
#'
#' @param x A matrix.
#'
#' @return Logical value: `TRUE` if the matrix is square, `FALSE` otherwise.
#'
#' @note Adapted from **matrixcalc** package version 1.0-3.
#'
#' @noRd
#' @keywords internal
is_square_matrix <- function(x) {
  is.matrix(x) && nrow(x) == ncol(x)
}


# Helper function: Check if matrix is symmetric -----------------------------
#
#' Check if matrix is symmetric
#'
#' Internal helper function to check if a matrix is symmetric (equal to its transpose).
#' The matrix must be square and numeric. A symmetric matrix satisfies the condition
#' that `x[i,j] == x[j,i]` for all i and j.
#'
#' @param x A numeric matrix to be checked for symmetry.
#'
#' @return Logical value: `TRUE` if the matrix is symmetric, `FALSE` otherwise.
#'
#' @note This function will throw an error if the matrix is not square.
#'
#' @noRd
#' @keywords internal
is_symetric_matrix <- function(x) {
  checkmate::assert_matrix(x, mode = "numeric")
  if (!is_square_matrix(x)) {
    stop("Matrix is not square", call. = FALSE)
  }
  all(x == t(x))
}
