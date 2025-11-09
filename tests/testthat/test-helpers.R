# Tests for helper functions ================================================

test_that("`pval_matrix_to_df` works correctly", {
  m <- matrix(c(NA, 0.05, 0.01, 0.05, NA, 0.03, 0.01, 0.03, NA), nrow = 3)
  colnames(m) <- c("A", "B", "C")
  rownames(m) <- c("A", "B", "C")

  result <- pval_matrix_to_df(m)

  expect_s3_class(result, "pairwise_pval_df")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("gr1", "gr2", "p_values"))
  expect_equal(nrow(result), 6) # 9 - 3 NA values
  expect_true(all(!is.na(result$p_values)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`pval_matrix_to_df` handles missing names", {
  m <- matrix(c(NA, 0.05, 0.01, 0.05, NA, 0.03, 0.01, 0.03, NA), nrow = 3)

  result <- pval_matrix_to_df(m)

  expect_s3_class(result, "pairwise_pval_df")
  expect_equal(result$gr1, c("1", "1", "2", "2", "3", "3"))
  expect_equal(result$gr2, c("2", "3", "1", "3", "1", "2"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`pval_matrix_to_df` preserves row/column names correctly", {
  m <- matrix(c(NA, 0.05, 0.01, 0.05, NA, 0.03, 0.01, 0.03, NA), nrow = 3)
  colnames(m) <- c("Group1", "Group2", "Group3")
  rownames(m) <- c("Group1", "Group2", "Group3")

  result <- pval_matrix_to_df(m)

  expect_true(all(result$gr1 %in% c("Group1", "Group2", "Group3")))
  expect_true(all(result$gr2 %in% c("Group1", "Group2", "Group3")))
})


# Tests for hyphen handling helper functions ================================

test_that("`has_hyphens_in_names` detects hyphens correctly", {
  # Should detect hyphens
  expect_true(cld:::has_hyphens_in_names("Plant-based"))
  expect_true(cld:::has_hyphens_in_names(c("A", "B-C")))
  expect_true(cld:::has_hyphens_in_names(c("Test-1", "Test-2")))
  
  # Should not detect hyphens
  expect_false(cld:::has_hyphens_in_names("Plant_based"))
  expect_false(cld:::has_hyphens_in_names(c("A", "B", "C")))
  expect_false(cld:::has_hyphens_in_names(c("Test1", "Test2")))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`has_hyphens_in_names` handles edge cases", {
  expect_false(cld:::has_hyphens_in_names(character(0)))  # empty vector
  expect_false(cld:::has_hyphens_in_names(""))  # empty string
  expect_true(cld:::has_hyphens_in_names("-"))  # just hyphen
  expect_true(cld:::has_hyphens_in_names("--"))  # multiple hyphens
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` prioritizes underscore", {
  # When nothing conflicts, should return underscore
  result <- cld:::find_hyphen_replacement(c("A-B", "C-D"))
  expect_equal(result, "_")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` tries en-dash when underscore present", {
  result <- cld:::find_hyphen_replacement(c("A-B", "C_D"))
  expect_equal(result, "\u2013")  # en-dash
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` tries em-dash when en-dash present", {
  result <- cld:::find_hyphen_replacement(c("A-B", "C_D", "E\u2013F"))
  expect_equal(result, "\u2014")  # em-dash
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` tries semicolon when dashes present", {
  result <- cld:::find_hyphen_replacement(c("A-B", "C_D", "E\u2013F", "G\u2014H"))
  expect_equal(result, ";")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` tries colon when semicolon present", {
  result <- cld:::find_hyphen_replacement(c("A-B", "C_D", "E\u2013F", "G\u2014H", "I;J"))
  expect_equal(result, ":")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` tries vertical bar when colon present", {
  result <- cld:::find_hyphen_replacement(c("A-B", "C_D", "E\u2013F", "G\u2014H", "I;J", "K:L"))
  expect_equal(result, "|")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`find_hyphen_replacement` errors when all replacements present", {
  expect_error(
    cld:::find_hyphen_replacement(c("A-B", "C_D", "E\u2013F", "G\u2014H", "I;J", "K:L", "M|N")),
    "all replacement characters.*are already present"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`replace_hyphens_in_names` returns unchanged when no hyphens", {
  names <- c("A", "B", "C")
  result <- cld:::replace_hyphens_in_names(names)
  
  expect_equal(result$names, names)
  expect_null(result$replacement)
  expect_false(result$had_hyphens)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`replace_hyphens_in_names` replaces with underscore by default", {
  names <- c("A-B", "C-D")
  result <- cld:::replace_hyphens_in_names(names)
  
  expect_equal(result$names, c("A_B", "C_D"))
  expect_equal(result$replacement, "_")
  expect_true(result$had_hyphens)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`replace_hyphens_in_names` handles multiple hyphens per name", {
  names <- c("A-B-C", "D-E-F-G")
  result <- cld:::replace_hyphens_in_names(names)
  
  expect_equal(result$names, c("A_B_C", "D_E_F_G"))
  expect_equal(result$replacement, "_")
  expect_true(result$had_hyphens)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`replace_hyphens_in_names` uses en-dash when underscore present", {
  names <- c("A-B", "C_D")
  result <- cld:::replace_hyphens_in_names(names)
  
  expect_equal(result$names, c("A\u2013B", "C_D"))
  expect_equal(result$replacement, "\u2013")
  expect_true(result$had_hyphens)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`replace_hyphens_in_names` accepts manual replacement", {
  names <- c("A-B", "C-D")
  result <- cld:::replace_hyphens_in_names(names, replacement = "~")
  
  expect_equal(result$names, c("A~B", "C~D"))
  expect_equal(result$replacement, "~")
  expect_true(result$had_hyphens)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`restore_hyphens_in_names` restores original hyphens", {
  # Test with underscore
  names <- c("A_B", "C_D")
  result <- cld:::restore_hyphens_in_names(names, "_")
  expect_equal(result, c("A-B", "C-D"))
  
  # Test with en-dash
  names <- c("A\u2013B", "C\u2013D")
  result <- cld:::restore_hyphens_in_names(names, "\u2013")
  expect_equal(result, c("A-B", "C-D"))
  
  # Test with custom replacement
  names <- c("A~B", "C~D")
  result <- cld:::restore_hyphens_in_names(names, "~")
  expect_equal(result, c("A-B", "C-D"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`restore_hyphens_in_names` handles NULL replacement", {
  names <- c("A", "B", "C")
  result <- cld:::restore_hyphens_in_names(names, NULL)
  expect_equal(result, names)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`restore_hyphens_in_names` handles multiple replacements per name", {
  names <- c("A_B_C", "D_E_F_G")
  result <- cld:::restore_hyphens_in_names(names, "_")
  expect_equal(result, c("A-B-C", "D-E-F-G"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("round-trip replacement and restoration preserves original", {
  original <- c("Plant-based", "Synthetic (K-6)", "Test-A-B")
  
  # Replace hyphens
  replaced <- cld:::replace_hyphens_in_names(original)
  expect_true(replaced$had_hyphens)
  expect_false(cld:::has_hyphens_in_names(replaced$names))
  
  # Restore hyphens
  restored <- cld:::restore_hyphens_in_names(replaced$names, replaced$replacement)
  expect_equal(restored, original)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("helper functions work together in complex scenario", {
  # Complex names with various special characters
  names <- c("Plant-based", "Synthetic_(K-6)", "Control")
  
  # Check for hyphens
  expect_true(cld:::has_hyphens_in_names(names))
  
  # Find replacement (should use en-dash since underscore is present)
  replacement <- cld:::find_hyphen_replacement(names)
  expect_equal(replacement, "\u2013")
  
  # Replace hyphens
  replaced <- cld:::replace_hyphens_in_names(names)
  expect_equal(replaced$replacement, "\u2013")
  expect_equal(replaced$names, c("Plant\u2013based", "Synthetic_(K\u20136)", "Control"))
  
  # Restore hyphens
  restored <- cld:::restore_hyphens_in_names(replaced$names, replaced$replacement)
  expect_equal(restored, names)
})

# Tests for other helper functions ==========================================

test_that("extract_data works when data is NULL", {
  # Create a formula with data in the environment
  x <- 1:10
  y <- 11:20
  f <- y ~ x
  
  # extract_data should find x and y in the formula environment
  result <- extract_data(f, data = NULL)
  
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("is_symetric_matrix detects symmetric matrices", {
  # Symmetric matrix
  sym_mat <- matrix(c(
    1, 2, 3,
    2, 4, 5,
    3, 5, 6
  ), nrow = 3, byrow = TRUE)
  
  expect_true(is_symetric_matrix(sym_mat))
})

test_that("is_symetric_matrix detects non-symmetric matrices", {
  # Non-symmetric matrix
  non_sym_mat <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
  ), nrow = 3, byrow = TRUE)
  
  expect_false(is_symetric_matrix(non_sym_mat))
})

test_that("is_symetric_matrix errors with non-square matrix", {
  # Non-square matrix
  non_square <- matrix(1:6, nrow = 2, ncol = 3)
  
  expect_error(
    is_symetric_matrix(non_square),
    "Matrix is not square"
  )
})
