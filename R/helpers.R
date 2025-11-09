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
is_symmetric_matrix <- function(x) {
  checkmate::assert_matrix(x, mode = "numeric")
  if (!is_square_matrix(x)) {
    stop("Matrix is not square", call. = FALSE)
  }
  all(x == t(x))
}


# Helper function: Detect hyphens in group names ----------------------------
#
#' Detect if group names contain hyphens
#'
#' Internal helper function to check if any group names contain hyphens,
#' which would conflict with the hyphen separator used by multcompView.
#'
#' @param group_names Character vector of group names.
#'
#' @return Logical value: `TRUE` if any group name contains a hyphen, `FALSE` otherwise.
#'
#' @noRd
#' @keywords internal
has_hyphens_in_names <- function(group_names) {
  any(grepl("-", group_names, fixed = TRUE))
}


# Helper function: Find suitable replacement for hyphens --------------------
#
#' Find a suitable replacement character for hyphens
#'
#' Internal helper function to find a character that can replace hyphens
#' in group names without causing conflicts. Tries multiple replacement
#' characters in priority order.
#'
#' @param group_names Character vector of group names.
#'
#' @return Character string with the replacement character.
#'   Priority order:
#'   1. underscore "_"
#'   2. en-dash "–" (U+2013)
#'   3. em-dash "—" (U+2014)
#'   4. semicolon ";"
#'   5. colon ":"
#'   6. vertical bar "|"
#'   7. Error if all are present
#'
#' @noRd
#' @keywords internal
find_hyphen_replacement <- function(group_names) {
  # List of replacement candidates in priority order
  candidates <- list(
    list(char = "_", name = "underscore"),
    list(char = "\u2013", name = "en-dash"),
    list(char = "\u2014", name = "em-dash"),
    list(char = ";", name = "semicolon"),
    list(char = ":", name = "colon"),
    list(char = "|", name = "vertical bar")
  )
  
  # Find the first candidate not present in group names
  for (candidate in candidates) {
    if (!any(grepl(candidate$char, group_names, fixed = TRUE))) {
      return(candidate$char)
    }
  }
  
  # If all candidates are present, we can't proceed safely
  stop(
    "Group names contain hyphens ('-'), but all replacement characters ",
    "(underscore, en-dash, em-dash, semicolon, colon, vertical bar) ",
    "are already present in the names. ",
    "Cannot automatically replace hyphens. ",
    "Please manually replace hyphens in group names with a unique character.",
    call. = FALSE
  )
}


# Helper function: Replace hyphens in group names ---------------------------
#
#' Replace hyphens in group names with alternative separator
#'
#' Internal helper function to replace hyphens in group names with an
#' alternative character that won't conflict with the comparison separator.
#'
#' @param group_names Character vector of group names.
#' @param replacement Character string to use as replacement. If NULL,
#'   automatically determines the best replacement.
#'
#' @return List with:
#'   * `names` - Character vector with hyphens replaced
#'   * `replacement` - Character string that was used for replacement
#'   * `had_hyphens` - Logical indicating if replacement was performed
#'
#' @noRd
#' @keywords internal
replace_hyphens_in_names <- function(group_names, replacement = NULL) {
  if (!has_hyphens_in_names(group_names)) {
    return(list(
      names = group_names,
      replacement = NULL,
      had_hyphens = FALSE
    ))
  }

  if (is.null(replacement)) {
    replacement <- find_hyphen_replacement(group_names)
  }

  new_names <- gsub("-", replacement, group_names, fixed = TRUE)

  list(
    names = new_names,
    replacement = replacement,
    had_hyphens = TRUE
  )
}


# Helper function: Restore hyphens in group names ---------------------------
#
#' Restore hyphens in group names
#'
#' Internal helper function to restore original hyphens in group names
#' after CLD generation.
#'
#' @param group_names Character vector of group names with replacements.
#' @param replacement Character string that was used to replace hyphens.
#'
#' @return Character vector with original hyphens restored.
#'
#' @noRd
#' @keywords internal
restore_hyphens_in_names <- function(group_names, replacement) {
  if (is.null(replacement)) {
    return(group_names)
  }
  gsub(replacement, "-", group_names, fixed = TRUE)
}
