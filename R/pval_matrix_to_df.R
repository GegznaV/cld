#' Convert matrix with p-values to data frame
#'
#' This function converts a matrix of p-values into a data frame with three columns:
#' `gr1` (column names), `gr2` (row names), and `p_values` (the p-values).
#' This is particularly useful when you have a symmetric matrix of p-values from
#' pairwise comparisons and need to convert it to a long-format data frame for
#' further analysis or for use with [make_cld()].
#'
#' @param x A numeric matrix containing p-values. If the matrix has no column or row names,
#'   they will be assigned sequential numbers. The matrix can be symmetric (with diagonal
#'   and upper/lower triangle) or triangular. `NA` values are removed from the output.
#'
#' @return A data frame of class `c("pairwise_pval_df", "data.frame")` with columns:
#' * `gr1` - names from the columns of the matrix
#' * `gr2` - names from the rows of the matrix
#' * `p_values` - the corresponding p-values from the matrix (excluding `NA` values)
#'
#' @family helper functions
#' @family data conversion functions
#' @concept helper_functions
#' @concept data_conversion
#' @keywords manip
#'
#' @seealso
#' * [make_cld()] for creating compact letter displays from the resulting data frame
#' * [as_cld()] for converting other formats to cld_object
#'
#' @export
#'
#' @examples
#' # Create a matrix of p-values
#' m <- matrix(c(NA, 0.05, 0.01, 0.05, NA, 0.03, 0.01, 0.03, NA), nrow = 3)
#' colnames(m) <- c("A", "B", "C")
#' rownames(m) <- c("A", "B", "C")
#'
#' # Convert to data frame
#' cld::pval_matrix_to_df(m)
#' @importFrom stats complete.cases
pval_matrix_to_df <- function(x) {
  if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
  if (is.null(rownames(x))) rownames(x) <- seq_len(nrow(x))

  df <- data.frame(
    gr1 = colnames(x)[col(x)],
    gr2 = rownames(x)[row(x)],
    p_values = c(x)
  )

  df <- df[stats::complete.cases(df), ]

  structure(df, class = c("pairwise_pval_df", "data.frame"))
}
