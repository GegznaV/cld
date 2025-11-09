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
#' @param quiet_hyphen_warning Logical. If `TRUE`, suppresses the informational message
#'   when hyphens in group names are automatically replaced. Default is `FALSE`.
#'   Note: The hyphen `-` is the default separator used by multcompView to distinguish
#'   group pairs (e.g., "GroupA-GroupB"). When group names contain hyphens (e.g.,
#'   "Plant-based"), they are temporarily replaced with alternative characters
#'   (underscore, en-dash, etc.) and restored after processing.
#' @param gr1 Character vector of first group names in each comparison. Used internally
#'   as an alternative to the `comparison` parameter for better hyphen handling.
#' @param gr2 Character vector of second group names in each comparison. Used internally
#'   as an alternative to the `comparison` parameter for better hyphen handling.
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
  gr1          = NULL,
  gr2          = NULL,
  threshold    = 0.05,
  print_comp   = FALSE,
  remove_space = TRUE,
  remove_equal = TRUE,
  swap_colon   = TRUE,
  swap_vs      = FALSE,
  reversed     = FALSE,
  swap_compared_names = FALSE,
  quiet_hyphen_warning = FALSE,
  sep          = "-",
  ...
) {
  # Extract variables from formula if provided
  if (!is.null(formula)) {
    p.value    <- data[[all.vars(formula[[2]])[1]]]
    comparison <- data[[all.vars(formula[[3]])[1]]]
  }

  # Handle case where gr1 and gr2 are provided instead of comparison
  hyphen_info <- list(had_hyphens = FALSE, replacement = NULL)

  if (!is.null(gr1) && !is.null(gr2)) {
    # Check if group names contain hyphens
    all_groups <- unique(c(gr1, gr2))
    hyphen_info <- replace_hyphens_in_names(all_groups)

    if (hyphen_info$had_hyphens) {
      # Show informational message unless suppressed
      if (!quiet_hyphen_warning) {
        replacement_desc <- switch(
          hyphen_info$replacement,
          "_" = "underscore ('_')",
          "\u2013" = "en-dash ('\u2013')",
          "\u2014" = "em-dash ('\u2014')",
          ";" = "semicolon (';')",
          ":" = "colon (':')",
          "|" = "vertical bar ('|')",
          sprintf("'%s'", hyphen_info$replacement)  # fallback
        )

        message(
          "Group names contain hyphens ('-'), which conflict with the comparison separator.\n",
          "Hyphens have been automatically replaced with ", replacement_desc, " for processing.\n",
          "The original hyphens will be restored in the output.\n",
          "To suppress this message, use quiet_hyphen_warning = TRUE."
        )
      }

      # Create mapping for replacement (always do this when hyphens are present)
      names_map <- stats::setNames(hyphen_info$names, all_groups)

      # Replace hyphens in gr1 and gr2
      gr1 <- names_map[gr1]
      gr2 <- names_map[gr2]
    }

    # Create comparison strings from gr1 and gr2 - ALWAYS use hyphen for multcompLetters
    comparison <- paste0(gr1, "-", gr2)
  } else if (!is.null(comparison) && sep != "-") {
    # If we have comparison strings with a custom separator, convert them to use hyphens
    # Split on custom separator, reassemble with hyphens
    comparison <- gsub(sep, "-", comparison, fixed = TRUE)
  }

  # Identify significant differences
  significant_difference <- p.value < threshold

  # Clean up comparison strings (only apply these if not already processed gr1/gr2)
  if (is.null(gr1) && is.null(gr2)) {
    if (remove_space) {
      comparison <- gsub(" ", "", comparison)
    }
    if (remove_equal) {
      comparison <- gsub("=", "", comparison)
    }
    if (swap_colon) {
      comparison <- gsub(":", "-", comparison)
    }
    if (swap_vs) {
      comparison <- gsub("vs", "-", comparison)
    }
  }
  
  # Check for problematic separators in comparison strings (formula method case)
  # Only relevant when we DON'T have gr1/gr2 (which handle hyphens automatically)
  if (is.null(gr1) && is.null(gr2) && !is.null(comparison)) {
    # After cleaning, check if any comparison has multiple hyphens
    # This suggests hyphens within group names (after conversion to hyphen separator)
    hyphen_counts <- vapply(gregexpr("-", comparison, fixed = TRUE), 
                           function(x) sum(x > 0), integer(1))
    
    if (any(hyphen_counts > 1)) {
      # Detected multiple hyphens - this is problematic
      warning(
        "Comparison strings contain multiple hyphens, which may indicate group names ",
        "with hyphens or other special characters.\n",
        "The formula method cannot reliably distinguish separator hyphens from hyphens ",
        "within group names.\n",
        "This may produce incorrect results.\n\n",
        "Recommended solutions:\n",
        "1. Use the data.frame method with gr1_col and gr2_col parameters\n",
        "2. Use two-variable formula: p.value ~ group1 + group2\n",
        "3. Try a different separator that doesn't appear in group names\n\n",
        "See ?make_cld section 'Handling Group Names with Hyphens' for details.",
        call. = FALSE
      )
    }
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
  if (print_comp) {
    Y <- data.frame(Comparisons = names(significant_difference))
    cat("\n\n")
    print(Y)
    cat("\n\n")
  }

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

  # Restore original hyphens in group names
  if (hyphen_info$had_hyphens) {
    res$group <- restore_hyphens_in_names(res$group, hyphen_info$replacement)
  }

  # Store metadata as attributes
  attr(res, "alpha") <- threshold
  attr(res, "n_comparisons") <- length(comparison)
  attr(res, "n_significant") <- sum(significant_difference, na.rm = TRUE)

  structure(res, class = c("cld_object", class(res)))
}
