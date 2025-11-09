# Tests for groups with hyphens in their names ============================
#
# IMPORTANT NOTE:
# The underlying multcompView::multcompLetters() function uses hyphens as
# separators between group names in comparison strings (e.g., "A-B"). This
# creates a conflict when group names themselves contain hyphens.
#
# These tests document:
# 1. The limitation with hyphenated group names when using comparison-based methods
# 2. Workarounds using alternative separators or pre-processing
# 3. Methods that work correctly (as_cld, coercion methods, print)
#
# ============================================================================

# Test data with hyphenated group names
create_hyphenated_data <- function() {
  list(
    groups = c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)", "Control"),
    values = c(
      rep(5.2, 10),  # Plant-based
      rep(7.8, 10),  # Synthetic (K-6)
      rep(7.5, 10),  # Synthetic (A-9)
      rep(3.1, 10)   # Control
    )
  )
}

# Helper function to replace hyphens in group names with alternative separator
replace_hyphens_in_groups <- function(group_names, replacement = "_") {
  gsub("-", replacement, group_names, fixed = TRUE)
}

# ============================================================================
# make_cld.matrix tests - LIMITATION DOCUMENTED
# ============================================================================

test_that("make_cld.matrix handles hyphenated names with automatic replacement", {
  # Hyphenated group names are now automatically handled with message

  pval_matrix <- matrix(c(
    1.00, 0.001, 0.002, 0.001,
    0.001, 1.00, 0.850, 0.001,
    0.002, 0.850, 1.00, 0.001,
    0.001, 0.001, 0.001, 1.00
  ), nrow = 4, byrow = TRUE)

  rownames(pval_matrix) <- colnames(pval_matrix) <-
    c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)", "Control")

  # Should work now with a message
  expect_message(
    result <- make_cld(pval_matrix, alpha = 0.05),
    "Group names contain hyphens"
  )

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 4)

  # Original hyphens should be restored in output
  expect_true("Plant-based" %in% result$group)
  # Spaces are removed by default
  expect_true("Synthetic(K-6)" %in% result$group)
  expect_true("Synthetic(A-9)" %in% result$group)
})

test_that("make_cld.matrix without hyphens in names produces no message", {
  # No message when group names don't contain hyphens
  pval_matrix <- matrix(c(
    1.00, 0.001, 0.002, 0.001,
    0.001, 1.00, 0.850, 0.001,
    0.002, 0.850, 1.00, 0.001,
    0.001, 0.001, 0.001, 1.00
  ), nrow = 4, byrow = TRUE)

  rownames(pval_matrix) <- colnames(pval_matrix) <-
    c("Plantbased", "Synthetic(K_6)", "Synthetic(A_9)", "Control")

  expect_silent(result <- make_cld(pval_matrix, alpha = 0.05))

  expect_s3_class(result, "cld_object")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("quiet_hyphen_warning parameter suppresses message", {
  pval_matrix <- matrix(c(
    1.00, 0.001,
    0.001, 1.00
  ), nrow = 2, byrow = TRUE)

  rownames(pval_matrix) <- colnames(pval_matrix) <- c("Plant-based", "Control")

  # With quiet = FALSE (default), should show message
  expect_message(
    make_cld(pval_matrix, quiet_hyphen_warning = FALSE),
    "Group names contain hyphens"
  )

  # With quiet = TRUE, should be silent
  expect_silent(
    result <- make_cld(pval_matrix, quiet_hyphen_warning = TRUE)
  )

  # Result should still be correct
  expect_s3_class(result, "cld_object")
  expect_true("Plant-based" %in% result$group)
  expect_equal(nrow(result), 2)  # 2 groups: Plant-based and Control
})

# ============================================================================
# make_cld.data.frame tests - LIMITATION DOCUMENTED
# ============================================================================

test_that("make_cld.data.frame handles hyphenated names with warning", {
  # Hyphenated group names are automatically handled
  df <- data.frame(
    group1 = c("Plant-based", "Plant-based", "Synthetic (K-6)"),
    group2 = c("Synthetic (K-6)", "Synthetic (A-9)", "Synthetic (A-9)"),
    p.adj = c(0.001, 0.002, 0.850),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- make_cld(df, gr1_col = "group1", gr2_col = "group2",
      p_val_col = "p.adj", alpha = 0.05),
    "Group names contain hyphens"
  )

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)

  # Original hyphens should be restored
  expect_true("Plant-based" %in% result$group)
  expect_true(any(grepl("K-6", result$group)))
  expect_true(any(grepl("A-9", result$group)))

  # Verify structure
  expect_true(all(c("group", "cld", "spaced_cld") %in% names(result)))
})

test_that("make_cld.data.frame preserves parentheses when no hyphens in names", {
  # Parentheses alone (without hyphens) work fine
  df <- data.frame(
    group1 = c("Control", "Control"),
    group2 = c("Synthetic (K6)", "Synthetic (A9)"),
    p.adj = c(0.001, 0.001),
    stringsAsFactors = FALSE
  )

  result <- make_cld(
    df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"
  )

  expect_s3_class(result, "cld_object")
  # Check that parentheses are preserved
  expect_true(any(grepl("\\(K6\\)", result$group)))
  expect_true(any(grepl("\\(A9\\)", result$group)))
})

# ============================================================================
# make_cld.formula tests - LIMITATION DOCUMENTED
# ============================================================================

test_that("make_cld.formula with hyphenated names still has limitation", {
  # Formula method works with comparison strings, so it still has limitations
  # when parsing group names from comparison strings with multiple hyphens
  my_data <- data.frame(
    Comparison = c(
      "Plant-based - Synthetic (K-6)",
      "Plant-based - Synthetic (A-9)",
      "Synthetic (K-6) - Synthetic (A-9)"
    ),
    p.adjust = c(0.001, 0.002, 0.850),
    stringsAsFactors = FALSE
  )

  # Formula method doesn't have gr1/gr2, so it's harder to handle
  # For now, this still has limitations with complex cases
  skip("Formula method with complex hyphenated names needs special handling")
})

test_that("make_cld.formula workaround: use alternative separator", {
  # WORKAROUND: Use a different separator like " vs " or " | "
  my_data <- data.frame(
    Comparison = c(
      "Plant_based vs Synthetic (K_6)",
      "Plant_based vs Synthetic (A_9)",
      "Synthetic (K_6) vs Synthetic (A_9)"
    ),
    p.adjust = c(0.001, 0.002, 0.850),
    stringsAsFactors = FALSE
  )

  result <- make_cld(p.adjust ~ Comparison, data = my_data,
    swap.vs = TRUE, alpha = 0.05)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true("Plant_based" %in% result$group)
})

# ============================================================================
# make_cld.pairwise_pval_df tests
# ============================================================================

test_that("make_cld.pairwise_pval_df handles hyphenated names with warning", {
  # Now works with automatic hyphen replacement
  pval_df <- data.frame(
    gr1 = c("Plant-based", "Plant-based", "Synthetic (K-6)"),
    gr2 = c("Synthetic (K-6)", "Synthetic (A-9)", "Synthetic (A-9)"),
    p_values = c(0.001, 0.002, 0.850),
    stringsAsFactors = FALSE
  )
  class(pval_df) <- c("pairwise_pval_df", "data.frame")

  expect_message(
    result <- make_cld(pval_df, alpha = 0.05),
    "Group names contain hyphens"
  )

  expect_s3_class(result, "cld_object")
  # Original hyphens restored
  expect_true("Plant-based" %in% result$group)
  expect_true(any(grepl("K-6", result$group)))
  expect_true(any(grepl("A-9", result$group)))
})

# ============================================================================
# pval_matrix_to_df with hyphenated names - THIS WORKS!
# ============================================================================

test_that("pval_matrix_to_df preserves hyphenated group names", {
  # pval_matrix_to_df just converts format, doesn't call multcompLetters
  # so it works fine with hyphens
  pval_matrix <- matrix(c(
    NA, 0.001, 0.002,
    NA, NA, 0.850,
    NA, NA, NA
  ), nrow = 3, byrow = TRUE)

  rownames(pval_matrix) <- c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)")
  colnames(pval_matrix) <- c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)")

  result <- pval_matrix_to_df(pval_matrix)

  expect_s3_class(result, "pairwise_pval_df")
  expect_true("Plant-based" %in% c(result$gr1, result$gr2))
  expect_true(any(grepl("K-6", c(result$gr1, result$gr2))))
  expect_true(any(grepl("A-9", c(result$gr1, result$gr2))))
})

# ============================================================================
# as_cld tests with hyphenated names
# ============================================================================

test_that("as_cld.data.frame handles hyphenated group names", {
  df <- data.frame(
    group = c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)"),
    cld = c("a", "b", "b"),
    stringsAsFactors = FALSE
  )

  result <- as_cld(df)

  expect_s3_class(result, "cld_object")
  expect_equal(result$group[1], "Plant-based")
  expect_true(any(grepl("\\(K-6\\)", result$group)))
  expect_true(any(grepl("\\(A-9\\)", result$group)))
})

test_that("as_cld.character handles hyphenated group names", {
  vec <- c(
    "Plant-based" = "a",
    "Synthetic (K-6)" = "b",
    "Synthetic (A-9)" = "b"
  )

  result <- as_cld(vec)

  expect_s3_class(result, "cld_object")
  expect_equal(result$group[1], "Plant-based")
  expect_true("Synthetic (K-6)" %in% result$group)
  expect_true("Synthetic (A-9)" %in% result$group)
})

# ============================================================================
# Coercion methods with hyphenated names
# ============================================================================

test_that("as.data.frame.cld_object preserves hyphenated group names", {
  df <- data.frame(
    group = c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)"),
    cld = c("a", "b", "b"),
    stringsAsFactors = FALSE
  )

  cld_obj <- as_cld(df)
  result <- as.data.frame(cld_obj)

  expect_false("cld_object" %in% class(result))
  expect_equal(result$group[1], "Plant-based")
  expect_true(any(grepl("K-6", result$group)))
  expect_true(any(grepl("A-9", result$group)))
})

test_that("as.character.cld_object preserves hyphenated group names", {
  df <- data.frame(
    group = c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)"),
    cld = c("a", "b", "b"),
    stringsAsFactors = FALSE
  )

  cld_obj <- as_cld(df)
  result <- as.character(cld_obj)

  expect_type(result, "character")
  expect_true("Plant-based" %in% names(result))
  expect_true("Synthetic (K-6)" %in% names(result))
  expect_true("Synthetic (A-9)" %in% names(result))

  # Check values are correct (unname to compare just values)
  expect_equal(unname(result["Plant-based"]), "a")
  expect_equal(unname(result["Synthetic (K-6)"]), "b")
  expect_equal(unname(result["Synthetic (A-9)"]), "b")
})

test_that("as_tibble.cld_object preserves hyphenated group names", {
  skip_if_not_installed("tibble")

  df <- data.frame(
    group = c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)"),
    cld = c("a", "b", "b"),
    stringsAsFactors = FALSE
  )

  cld_obj <- as_cld(df)
  result <- tibble::as_tibble(cld_obj)

  expect_s3_class(result, "tbl_df")
  expect_equal(result$group[1], "Plant-based")
  expect_true("Synthetic (K-6)" %in% result$group)
  expect_true("Synthetic (A-9)" %in% result$group)
})

# ============================================================================
# print.cld_object with hyphenated names
# ============================================================================

test_that("print.cld_object displays hyphenated group names correctly", {
  df <- data.frame(
    group = c("Plant-based", "Synthetic (K-6)", "Synthetic (A-9)", "Control"),
    cld = c("a", "b", "b", "c"),
    stringsAsFactors = FALSE
  )

  cld_obj <- as_cld(df)
  output <- capture.output(print(cld_obj))

  # Check that hyphenated names appear in output
  output_text <- paste(output, collapse = " ")
  expect_true(grepl("Plant-based", output_text, fixed = TRUE))
  expect_true(grepl("Synthetic \\(K-6\\)", output_text))
  expect_true(grepl("Synthetic \\(A-9\\)", output_text))
})

# ============================================================================
# Edge cases and special scenarios - LIMITATION DOCUMENTED
# ============================================================================

test_that("multiple hyphens in group names are handled automatically", {
  # Multiple hyphens now handled with warning
  df <- data.frame(
    group1 = c("Type-A-1-X", "Type-A-1-X"),
    group2 = c("Type-B-2-Y", "Type-C-3-Z"),
    p.adj = c(0.001, 0.002),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"),
    "Group names contain hyphens"
  )

  expect_s3_class(result, "cld_object")
  expect_true("Type-A-1-X" %in% result$group)
  expect_true("Type-B-2-Y" %in% result$group)
  expect_true("Type-C-3-Z" %in% result$group)
})

test_that("groups with only hyphens now work", {
  # Simple hyphenated names now work
  df <- data.frame(
    group1 = c("A-B", "A-B"),
    group2 = c("C-D", "E-F"),
    p.adj = c(0.001, 0.002),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"),
    "Group names contain hyphens"
  )

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true(all(grepl("-", result$group)))
})

test_that("hyphens at start or end are handled", {
  df <- data.frame(
    group1 = c("-Start", "End-"),
    group2 = c("Normal", "Normal"),
    p.adj = c(0.001, 0.002),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"),
    "Group names contain hyphens"
  )

  expect_true("-Start" %in% result$group)
  expect_true("End-" %in% result$group)
})

test_that("mixed special characters with hyphens", {
  df <- data.frame(
    group = c("Plant-based (2023)", "Synthetic-K6 [v2]", "Control_A-1"),
    cld = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )

  result <- as_cld(df)

  expect_equal(result$group[1], "Plant-based (2023)")
  expect_equal(result$group[2], "Synthetic-K6 [v2]")
  expect_equal(result$group[3], "Control_A-1")
})

# ============================================================================
# Integration tests - demonstrating successful workarounds
# ============================================================================

test_that("complete workflow with automatic hyphen handling", {
  # Demonstrate full workflow with automatic hyphen replacement

  # 1. Create matrix with hyphenated names
  pval_matrix <- matrix(c(
    1.00, 0.001,
    0.001, 1.00
  ), nrow = 2, byrow = TRUE)

  rownames(pval_matrix) <- colnames(pval_matrix) <-
    c("Plant-based", "Synthetic (K-6)")

  # 2. Generate CLD - should work with warning
  expect_message(
    result <- make_cld(pval_matrix, alpha = 0.05),
    "Group names contain hyphens"
  )

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 2)

  # 3. Original hyphens are restored in output
  expect_true("Plant-based" %in% result$group)
  expect_true(any(grepl("K-6", result$group)))

  # 4. Convert to different formats
  df <- as.data.frame(result)
  expect_false("cld_object" %in% class(df))
  expect_true("Plant-based" %in% df$group)

  char_vec <- as.character(result)
  expect_type(char_vec, "character")
  expect_length(char_vec, 2)
  expect_true("Plant-based" %in% names(char_vec))
})

test_that("reversed parameter works with hyphenated group names", {
  pval_matrix <- matrix(c(
    1.00, 0.001,
    0.001, 1.00
  ), nrow = 2, byrow = TRUE)

  rownames(pval_matrix) <- colnames(pval_matrix) <-
    c("Plant-based", "Synthetic (K-6)")

  expect_message(result_normal <- make_cld(pval_matrix, reversed = FALSE))
  expect_message(result_reversed <- make_cld(pval_matrix, reversed = TRUE))

  expect_s3_class(result_normal, "cld_object")
  expect_s3_class(result_reversed, "cld_object")
  expect_equal(result_normal$group, result_reversed$group)
  # Letters should be different when reversed
  expect_false(identical(result_normal$cld, result_reversed$cld))

  # Original hyphens preserved
  expect_true("Plant-based" %in% result_normal$group)
  expect_true("Plant-based" %in% result_reversed$group)
})
