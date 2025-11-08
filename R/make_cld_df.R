#' Make a data frame with compact letter display (internal)
#'
#' Computes a compact letter display (CLD) of statistically significant
#' differences in pairwise comparisons and outputs the results as a data frame.
#' This is an internal workhorse function that powers all the various `make_cld()`
#' methods. It handles the actual computation of letter assignments based on
#' significant differences, using the algorithm from [multcompView::multcompLetters()].
#'
#' @param formula An optional formula specifying the relationship between p-values
#'   and comparisons (e.g., `p.value ~ comparison`). When provided, `data` must
#'   also be supplied.
#' @param data A data frame containing the comparison data when using formula interface.
#'   Required when `formula` is provided.
#' @param comparison Character vector of comparison labels (e.g., "A-B", "A-C").
#'   Each element should specify a pair of groups being compared, separated by
#'   a delimiter (typically "-").
#' @param p.value Numeric vector of p-values corresponding to each comparison.
#'   Must be the same length as `comparison`.
#' @param threshold Numeric value between 0 and 1. The significance threshold
#'   (alpha level) for determining significant differences. Comparisons with
#'   p-values below this threshold are marked as significant. Default is 0.05.
#' @param print.comp Logical. If `TRUE`, prints the comparison names. Default is `FALSE`.
#' @param remove.space Logical. If `TRUE`, removes spaces from comparison strings.
#'   Default is `TRUE`.
#' @param remove.equal Logical. If `TRUE`, removes equal signs from comparison strings.
#'   Default is `TRUE`.
#' @param swap.colon Logical. If `TRUE`, replaces colons with hyphens in comparison strings.
#'   Default is `TRUE`.
#' @param swap.vs Logical. If `TRUE`, replaces "vs" with hyphens in comparison strings.
#'   Default is `FALSE`.
#' @param reversed Logical. Passed to [multcompView::multcompLetters()]. If `TRUE`,
#'   reverses the letter ordering. Default is `FALSE`.
#' @param swap_compared_names Logical. If `TRUE`, group names are swapped
#'   from "2-1" or "second-first" to "1-2" or "first-second" in the comparison strings.
#'   This may lead to different ordering of CLD letters and compared groups,
#'   but doesn't affect the statistical interpretation. Default is `FALSE`.
#' @param ... Additional arguments (currently unused, reserved for future extensions).
#'
#' @return A data frame of class `c("cld_object", "data.frame")` with columns:
#' * `group` - Character. The names of the groups extracted from the comparisons
#' * `cld` - Character. Compact letter display for each group
#' * `spaced_cld` - Character. Monospaced version of the CLD (spaces replaced
#'   with underscores for alignment)
#'
#' @note
#' This function is inspired by and based on code from [rcompanion::cldList()]
#' in the **rcompanion** package by Salvatore Mangiafico. The underlying
#' letter-assignment algorithm is implemented in [multcompView::multcompLetters()].
#'
#' @noRd
#' @keywords internal
make_cld_df <- function(
  formula      = NULL,
  data         = NULL,
  comparison   = NULL,
  p.value      = NULL,
  threshold    = 0.05,
  print.comp   = FALSE,
  remove.space = TRUE,
  remove.equal = TRUE,
  swap.colon   = TRUE,
  swap.vs      = FALSE,
  reversed     = FALSE,
  swap_compared_names = FALSE,
  ...
) {
  # Extract variables from formula if provided
  if (!is.null(formula)) {
    p.value    <- data[[all.vars(formula[[2]])[1]]]
    comparison <- data[[all.vars(formula[[3]])[1]]]
  }

  # Identify significant differences
  significant_difference <- p.value < threshold

  # Clean up comparison strings
  if (remove.space) {
    comparison <- gsub(" ", "", comparison)
  }
  if (remove.equal) {
    comparison <- gsub("=", "", comparison)
  }
  if (swap.colon) {
    comparison <- gsub(":", "-", comparison)
  }
  if (swap.vs) {
    comparison <- gsub("vs", "-", comparison)
  }

  # Swap group order in comparisons if requested
  if (swap_compared_names) {
    part_1 <- sub("-.*$", "", comparison)
    part_2 <- sub("^.*?-", "", comparison)
    comparison <- paste0(part_2, "-", part_1)
  }

  # Name the significant differences vector
  names(significant_difference) <- comparison

  # Optionally print comparisons
  if (print.comp) {
    Y <- data.frame(Comparisons = names(significant_difference))
    cat("\n\n")
    print(Y)
    cat("\n\n")
  }

  # Generate compact letter display
  # Generate compact letter display
  MCL <- multcompView::multcompLetters(significant_difference, reversed = reversed)

  regular_cld <- as.character(MCL$Letters)
  spaced_cld  <- as.character(MCL$monospacedLetters)

  # Use regular CLD if monospaced is not available
  if (is.null(MCL$monospacedLetters)) {
    spaced_cld <- regular_cld
  }

  # Build result data frame
  res <- data.frame(
    group            = names(MCL$Letters),
    cld              = regular_cld,
    spaced_cld       = gsub(" ", "_", spaced_cld),
    stringsAsFactors = FALSE
  )

  # Store metadata as attributes
  attr(res, "alpha") <- threshold
  attr(res, "n_comparisons") <- length(comparison)
  attr(res, "n_significant") <- sum(significant_difference, na.rm = TRUE)

  structure(res, class = c("cld_object", class(res)))
}
