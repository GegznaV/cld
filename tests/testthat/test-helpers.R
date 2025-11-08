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
