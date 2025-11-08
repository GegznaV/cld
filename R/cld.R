#' Make a compact letter display (CLD) for pair-wise comparisons
#'
#' Creates a compact letter display for results of pair-wise comparisons
#' (e.g., ANOVA post-hoc tests, Kruskal-Wallis post-hoc tests, and others).
#' Groups that share the same letter are not significantly different from each other.
#' This provides a visual summary of which groups differ significantly, commonly used
#' in agricultural, biological, and statistical reporting.
#'
#' @param obj Object with pair-wise comparisons (e.g., post-hoc test results).
#'   Currently supported object classes:
#'   * `PMCMR` - from packages **PMCMR** and **PMCMRplus**
#'   * `pairwise.htest` - from base R (e.g., `pairwise.t.test`, `pairwise.wilcox.test`)
#'   * `posthoc_anova` - from package **rstatix** (e.g., `games_howell_test`, `tukey_hsd`)
#'   * `PostHocTest` - from package **DescTools** (e.g., `ConoverTest`, `DunnettTest`)
#'   * `DunnTest` - from package **DescTools**
#'   * `matrix` - symmetric matrices of p-values
#'   * `data.frame` - with comparison results (requires `gr1_col`, `gr2_col`, `p_val_col` specification)
#'   * `formula` - interface for data frames
#'   * `pairwise_pval_df` - output from [pval_matrix_to_df()]
#'
#' @param ... Further arguments passed to internal methods. These may include:
#'   * `reversed` - Logical. If `TRUE`, reverses the letter ordering (default: `FALSE`)
#'   * `swap_compared_names` - Logical. If `TRUE`, swaps group order in comparisons (default: `FALSE`)
#'   * `print.comp` - Logical. If `TRUE`, prints comparison names (default: `FALSE`)
#'   Additional cleaning options (default: `TRUE` for most methods):
#'   * `remove.space` - Removes spaces from comparison strings
#'   * `remove.equal` - Removes equal signs from comparison strings
#'   * `swap.colon` - Replaces colons with hyphens
#'   * `swap.vs` - Replaces "vs" with hyphens (default: `FALSE`)
#'
#' @param data A data frame with p-values and names of comparisons. This argument
#'   is required when `obj` is a formula. The data frame should contain at least
#'   two columns: one for p-values and one for comparison labels. See examples for details.
#'
#' @param formula An R model [stats::formula()] where the left-hand side
#'   term indicates the variable with p-values and the right-hand side term defines
#'   the variable with comparisons, e.g., `p.adjust ~ Comparison`. This provides
#'   a flexible way to specify which columns in `data` contain the relevant information.
#'   Usually used in combination with `data`.
#'
#' @param alpha Numeric value between 0 and 1. The significance level (alpha) for determining
#'   which comparisons are significantly different. Comparisons with p-values below this
#'   threshold are considered significant. Default is 0.05.
#'   Note: For `posthoc_anova` objects, the default is calculated as `1 - conf_level`.
#' @param gr1_col Character string. Name of the column in the data frame containing
#'   the first group names in each pairwise comparison. Default is `"group1"`.
#'   Only used for the data.frame method. The function will construct comparisons
#'   in the format `gr2-gr1`.
#' @param gr2_col Character string. Name of the column in the data frame containing
#'   the second group names in each pairwise comparison. Default is `"group2"`.
#'   Only used for the data.frame method.
#' @param p_val_col Character string. Name of the column in the data frame containing
#'   the p-values for each comparison. Default is `"p.adj"` (adjusted p-values).
#'   Only used for the data.frame method. Can also be `"p.value"` or any other
#'   column name containing numeric p-values.
#' @param remove.space Logical. If `TRUE`, removes spaces from comparison strings.
#'   Default is `FALSE` for the data.frame method to preserve original formatting.
#'   Set to `TRUE` if your group names contain spaces and you want compact comparisons.
#'
#' @return A data frame of class `c("cld_object", "data.frame")` with three columns:
#' * `group` - Character. The names of the groups being compared
#' * `cld` - Character. The compact letter display for each group. Groups sharing
#'   at least one letter are not significantly different from each other
#' * `spaced_cld` - Character. A monospaced version of the CLD where spaces are
#'   replaced with underscores, useful for maintaining alignment in output
#'
#' The rows are ordered according to the group names. Groups with the same letter(s)
#' do not differ significantly at the specified alpha level.
#'
#' @seealso
#' * [pval_matrix_to_df()] for converting p-value matrices to data frames
#' * [multcompView::multcompLetters()] for the underlying letter generation algorithm
#'
#' @references
#' Piepho HP (2004). An algorithm for a letter-based representation of
#' all-pairwise comparisons. Journal of Computational and Graphical Statistics,
#' 13(2), 456-466. \doi{10.1198/1061860043515}, available at 
#' https://www.tandfonline.com/doi/abs/10.1198/1061860043515
#'
#' @export
#'
#' @examples
#' # Example 1: Using pairwise.htest (Wilcoxon test)
#' obj1 <- stats::pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
#' cld::make_cld(obj1)
#'
#' # Example 2: Using pairwise.htest (t-test)
#' obj2 <- with(OrchardSprays, stats::pairwise.t.test(decrease, treatment))
#' cld::make_cld(obj2)
#'
#' # Example 3: Using pairwise.htest (proportion test)
#' \donttest{
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' obj3 <- stats::pairwise.prop.test(smokers, patients)
#' cld::make_cld(obj3)
#' }
#'
#' # Example 4: Using PMCMR objects
#' \donttest{
#' obj4 <- PMCMRplus::kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
#' cld::make_cld(obj4)
#' }
#'
#' # Example 5: Using DescTools objects
#' \donttest{
#' obj5 <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)
#' cld::make_cld(obj5)
#' }
#'
#' # Example 6: Using rstatix objects
#' \donttest{
#' obj6 <- rstatix::games_howell_test(PlantGrowth, weight ~ group)
#' cld::make_cld(obj6)
#' }
#'
#' # Example 7: Using formula interface
#' my_dataframe <- utils::read.table(
#'   text = c(
#'     'Comparison     p.value p.adjust
#'     "EE - GB = 0"        1 1.000000
#'     "EE - CY = 0" 0.001093 0.003279
#'     "GB - CY = 0" 0.005477 0.008216'
#'   ),
#'   header = TRUE
#' )
#' cld::make_cld(p.adjust ~ Comparison, data = my_dataframe)
#'
#' # Example 8: Using a symmetric matrix of p-values
#' # Create matrix
#' m <- c(
#'   1.00, 0.22, 0.05, 0.00,
#'   0.22, 1.00, 0.17, 0.01,
#'   0.05, 0.17, 1.00, 0.22,
#'   0.00, 0.01, 0.22, 1.00
#' )
#' obj8 <- matrix(m, nrow = 4)
#' rownames(obj8) <- colnames(obj8) <- c("P", "O", "I", "U")
#' obj8
#'
#' # Make CLD
#' cld::make_cld(obj8)
#'
#' # Example 9: Using data.frame method with custom column names
#' my_data <- data.frame(
#'   group1 = c("A", "A", "B"),
#'   group2 = c("B", "C", "C"),
#'   p.adj = c(0.001, 0.045, 0.892)
#' )
#' cld::make_cld(my_data, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj")
#'
make_cld <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  UseMethod("make_cld", obj)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.pairwise.htest <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  m1 <- obj$p.value
  df <- pval_matrix_to_df(m1)
  res <- make_cld_df(
    comparison   = paste0(df$gr1, "-", df$gr2),
    p.value      = df$p_values,
    threshold    = alpha,
    remove.space = TRUE,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
#' @details
#' ## Method for rstatix objects (`posthoc_anova`)
#'
#' The `posthoc_anova` method supports objects from the **rstatix** package,
#' specifically results from `games_howell_test()` and `tukey_hsd()`.
#' The significance level is automatically derived from the confidence level
#' used in the test (alpha = 1 - conf_level), but can be overridden.
make_cld.posthoc_anova <- function(obj, ..., alpha = 1 - obj$input$conf_level) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  which_posthoc <-
    switch(tolower(obj$input$method),
      "games-howell" = "games.howell",
      "tukey" = "tukey",
      stop("Unsupported method: ", obj$input$method, call. = FALSE)
    )

  obj2 <- obj$output$result

  res <- make_cld_df(
    comparison          = obj2$groups,
    p.value             = obj2$p_adjusted,
    threshold           = alpha,
    swap_compared_names = TRUE,
    ...
  )
  res
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.PMCMR <- function(obj, ..., alpha = 0.05) {
  make_cld.pairwise.htest(obj, ..., alpha = alpha)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.PostHocTest <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  # DescTools PostHocTest objects have results in the first element as a matrix
  # with p-values in the 'pval' column
  p_values <- obj[[1]][, "pval"]

  res <- make_cld_df(
    comparison   = names(p_values),
    p.value      = p_values,
    threshold    = alpha,
    remove.space = TRUE,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.DunnTest <- function(obj, ..., alpha = 0.05) {
  # DunnTest from DescTools has the same structure as PostHocTest
  make_cld.PostHocTest(obj, ..., alpha = alpha)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.formula <- function(obj, ..., data = NULL, alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  data <- extract_data(obj, data)
  res <- make_cld_df(
    obj,
    data      = data,
    threshold = alpha,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.matrix <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_matrix(obj, mode = "numeric")

  if (!is_square_matrix(obj)) {
    stop("This function works with square matrices only.", call. = FALSE)
  }

  if (!all(colnames(obj) == rownames(obj))) {
    stop("Matrix is square but its column and row names do not match.", call. = FALSE)
  }

  obj[upper.tri(obj, diag = TRUE)] <- NA
  df <- pval_matrix_to_df(obj)
  make_cld.pairwise_pval_df(df, ..., alpha = alpha)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.data.frame <- function(
  obj, ..., alpha = 0.05,
  gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj",
  remove.space = FALSE
) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_string(gr1_col)
  checkmate::assert_string(gr2_col)
  checkmate::assert_string(p_val_col)

  # Check that required columns exist
  if (!gr1_col %in% names(obj)) {
    stop(
      "Column '", gr1_col, "' not found in data frame.\n",
      "Available columns: ", paste(names(obj), collapse = ", "),
      call. = FALSE
    )
  }
  if (!gr2_col %in% names(obj)) {
    stop(
      "Column '", gr2_col, "' not found in data frame.\n",
      "Available columns: ", paste(names(obj), collapse = ", "),
      call. = FALSE
    )
  }
  if (!p_val_col %in% names(obj)) {
    stop(
      "Column '", p_val_col, "' not found in data frame.\n",
      "Available columns: ", paste(names(obj), collapse = ", "),
      call. = FALSE
    )
  }

  res <- make_cld_df(
    comparison   = paste0(obj[[gr2_col]], "-", obj[[gr1_col]]),
    p.value      = obj[[p_val_col]],
    threshold    = alpha,
    remove.space = remove.space,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname make_cld
#' @export
make_cld.pairwise_pval_df <- function(obj, ..., alpha = 0.05) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)

  res <- make_cld_df(
    comparison = paste0(obj$gr1, " - ", obj$gr2),
    p.value    = obj$p_values,
    threshold  = alpha,
    ...
  )
  res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
