# Tests for new formula approaches to handle hyphenated group names ==========
#
# This test file documents two new formula method approaches that solve
# the hyphen conflict issue:
#
# 1. Two-variable formula: p_value ~ group1 + group2
#    - Avoids parsing comparison strings entirely
#    - Automatically handles hyphens via gr1/gr2 parameters
#
# 2. Custom separator: p_value ~ comparison with sep parameter
#    - Uses alternative separator (e.g., ":", ";", "|")
#    - Avoids hyphen conflicts when group names contain hyphens
#
# ============================================================================

# Test data with hyphenated group names
create_hyphenated_pval_df <- function() {
  data.frame(
    group1 = c("Plant-based", "Plant-based", "Plant-based", 
               "Synthetic (K-6)", "Synthetic (K-6)", 
               "Synthetic (A-9)"),
    group2 = c("Synthetic (K-6)", "Synthetic (A-9)", "Control",
               "Synthetic (A-9)", "Control",
               "Control"),
    p_value = c(0.001, 0.002, 0.001, 0.850, 0.001, 0.001)
  )
}

# ============================================================================
# Two-variable formula approach tests
# ============================================================================

test_that("make_cld.formula with two-variable syntax handles hyphenated groups", {
  df <- create_hyphenated_pval_df()
  
  # Two-variable formula should work without any warnings or messages
  # because it uses gr1/gr2 parameters which handle hyphens automatically
  expect_silent(
    result <- make_cld(p_value ~ group1 + group2, data = df, 
                      quiet_hyphen_warning = TRUE)
  )
  
  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 4)  # 4 groups
  expect_equal(ncol(result), 3)  # group, cld, and spaced_cld columns
  
  # Check that all group names with hyphens are preserved
  expect_true("Plant-based" %in% result$group)
  expect_true("Synthetic (K-6)" %in% result$group)
  expect_true("Synthetic (A-9)" %in% result$group)
  expect_true("Control" %in% result$group)
  
  # Check CLD assignments make sense
  # Plant-based and Control should be different (p=0.001)
  # Synthetic (K-6) and Synthetic (A-9) should be similar (p=0.850)
  cld_df <- as.data.frame(result)
  expect_true(cld_df$cld[cld_df$group == "Plant-based"] != 
              cld_df$cld[cld_df$group == "Control"])
})

test_that("two-variable formula method produces message about hyphen handling", {
  df <- create_hyphenated_pval_df()
  
  # Should show message about automatic hyphen handling
  expect_message(
    result <- make_cld(p_value ~ group1 + group2, data = df),
    "Group names contain hyphens"
  )
  
  # Verify result is correct
  expect_s3_class(result, "cld_object")
  expect_true("Plant-based" %in% result$group)
})

test_that("two-variable formula with quiet_hyphen_warning suppresses message", {
  df <- create_hyphenated_pval_df()
  
  # Should be completely silent
  expect_silent(
    result <- make_cld(p_value ~ group1 + group2, data = df, 
                      quiet_hyphen_warning = TRUE)
  )
  
  expect_s3_class(result, "cld_object")
})

test_that("two-variable formula works with threshold parameter", {
  df <- create_hyphenated_pval_df()
  
  expect_silent(
    result <- make_cld(p_value ~ group1 + group2, data = df, 
                      threshold = 0.01, quiet_hyphen_warning = TRUE)
  )
  
  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 4)
})

test_that("two-variable formula works with reversed parameter", {
  df <- create_hyphenated_pval_df()
  
  expect_silent(
    result_normal <- make_cld(p_value ~ group1 + group2, data = df, 
                             reversed = FALSE, quiet_hyphen_warning = TRUE)
  )
  
  expect_silent(
    result_reversed <- make_cld(p_value ~ group1 + group2, data = df, 
                               reversed = TRUE, quiet_hyphen_warning = TRUE)
  )
  
  expect_s3_class(result_normal, "cld_object")
  expect_s3_class(result_reversed, "cld_object")
  
  # Groups should be the same
  expect_equal(result_normal$group, result_reversed$group)
  
  # Letters might be different when reversed
  # (depends on the specific p-values and algorithm)
})

# ============================================================================
# Custom separator (sep parameter) tests
# ============================================================================
#
# NOTE: The sep parameter can only work for groups WITHOUT hyphens in their names!
# This is because multcompView::multcompLetters() internally requires hyphens
# as separators. When we convert a custom separator to hyphens, any existing
# hyphens in group names create ambiguity.
#
# For groups WITH hyphens, use the two-variable formula approach instead.
# ============================================================================

test_that("sep parameter works for groups without hyphens (colon separator)", {
  # Create data with simple group names (no hyphens)
  df <- data.frame(
    comparison = c(
      "GroupA:GroupB",
      "GroupA:GroupC",
      "GroupB:GroupC"
    ),
    p_value = c(0.001, 0.05, 0.3)
  )
  
  # Use colon separator
  expect_silent(
    result <- make_cld(p_value ~ comparison, data = df, sep = ":")
  )
  
  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true("GroupA" %in% result$group)
  expect_true("GroupB" %in% result$group)
  expect_true("GroupC" %in% result$group)
})

test_that("sep parameter works for groups without hyphens (semicolon separator)", {
  df <- data.frame(
    comparison = c(
      "Treatment1;Treatment2",
      "Treatment1;Control"
    ),
    p_value = c(0.001, 0.001)
  )
  
  expect_silent(
    result <- make_cld(p_value ~ comparison, data = df, sep = ";")
  )
  
  expect_s3_class(result, "cld_object")
  expect_true("Treatment1" %in% result$group)
  expect_true("Treatment2" %in% result$group)
  expect_true("Control" %in% result$group)
})

test_that("sep parameter works for groups without hyphens (vertical bar separator)", {
  df <- data.frame(
    comparison = c(
      "A|B",
      "A|C"
    ),
    p_value = c(0.001, 0.05)
  )
  
  expect_silent(
    result <- make_cld(p_value ~ comparison, data = df, sep = "|")
  )
  
  expect_s3_class(result, "cld_object")
  expect_true("A" %in% result$group)
  expect_true("B" %in% result$group)
  expect_true("C" %in% result$group)
})

test_that("sep parameter does NOT work for groups WITH hyphens", {
  # This test documents the LIMITATION of the sep parameter
  # Even with a custom separator, if group names contain hyphens, it fails
  df <- data.frame(
    comparison = c(
      "Plant-based:Synthetic",  # "Plant-based" contains hyphen
      "Plant-based:Control"
    ),
    p_value = c(0.001, 0.001)
  )
  
  # This WILL fail because converting ":" to "-" creates multiple hyphens
  # "Plant-based:Synthetic" → "Plant-based-Synthetic" (has 2 hyphens)
  # Expect both warning about multiple hyphens AND error from multcompView
  expect_warning(
    expect_error(
      make_cld(p_value ~ comparison, data = df, sep = ":"),
      "Names must contain exactly one"
    ),
    "multiple hyphens"
  )
})

test_that("warning issued when formula method used with groups containing hyphens", {
  # Using default separator with hyphenated group names should warn
  df <- data.frame(
    comparison = c(
      "Plant-based-Synthetic",  # Multiple hyphens
      "Plant-based-Control"
    ),
    p_value = c(0.001, 0.001)
  )
  
  # Should warn about multiple hyphens AND error from multcompView
  expect_warning(
    expect_error(
      make_cld(p_value ~ comparison, data = df),
      "Names must contain exactly one"
    ),
    "multiple hyphens"
  )
})

# ============================================================================
# Comparison of old vs new approaches
# ============================================================================

test_that("two-variable formula is the solution for hyphenated group names", {
  df <- create_hyphenated_pval_df()
  
  # New two-variable approach works perfectly!
  expect_silent(
    new_result <- make_cld(p_value ~ group1 + group2, data = df, 
                          quiet_hyphen_warning = TRUE)
  )
  
  expect_s3_class(new_result, "cld_object")
  expect_equal(nrow(new_result), 4)
  
  # All hyphenated names are preserved
  expect_true(all(c("Plant-based", "Synthetic (K-6)", 
                   "Synthetic (A-9)", "Control") %in% new_result$group))
})

test_that("two-variable formula avoids old formula method problems", {
  df <- create_hyphenated_pval_df()
  
  # Old approach with single-variable formula would warn AND error
  df_old <- data.frame(
    comparison = c(
      "Plant-based-Synthetic (K-6)",
      "Plant-based-Synthetic (A-9)",
      "Plant-based-Control",
      "Synthetic (K-6)-Synthetic (A-9)",
      "Synthetic (K-6)-Control",
      "Synthetic (A-9)-Control"
    ),
    p_value = c(0.001, 0.002, 0.001, 0.850, 0.001, 0.001)
  )
  
  # Old method: Warning about multiple hyphens AND error from multcompView
  expect_warning(
    expect_error(
      make_cld(p_value ~ comparison, data = df_old),
      "Names must contain exactly one"
    ),
    "multiple hyphens"
  )
  
  # New two-variable approach works without issues!
  expect_silent(
    new_result <- make_cld(p_value ~ group1 + group2, data = df, 
                          quiet_hyphen_warning = TRUE)
  )
  
  expect_s3_class(new_result, "cld_object")
})

# ============================================================================
# Edge cases
# ============================================================================

test_that("two-variable formula handles groups with no hyphens", {
  # Should work fine even without hyphens
  df <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p_value = c(0.001, 0.05, 0.3)
  )
  
  expect_silent(
    result <- make_cld(p_value ~ group1 + group2, data = df)
  )
  
  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
})

test_that("custom sep handles groups with no special characters", {
  df <- data.frame(
    comparison = c("A:B", "A:C", "B:C"),
    p_value = c(0.001, 0.05, 0.3)
  )
  
  expect_silent(
    result <- make_cld(p_value ~ comparison, data = df, sep = ":")
  )
  
  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
})

test_that("two-variable formula with single hyphen in group names", {
  # Edge case: only one group has hyphen
  df <- data.frame(
    group1 = c("Plant-based", "Plant-based", "Normal"),
    group2 = c("Normal", "Control", "Control"),
    p_value = c(0.001, 0.001, 0.05)
  )
  
  # Should still show message because at least one name has hyphen
  expect_message(
    result <- make_cld(p_value ~ group1 + group2, data = df),
    "Group names contain hyphens"
  )
  
  expect_true("Plant-based" %in% result$group)
  expect_true("Normal" %in% result$group)
  expect_true("Control" %in% result$group)
})
