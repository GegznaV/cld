# Tests for make_cld.formula and make_cld.data.frame methods ================

test_that("`make_cld.formula` works with data frame", {
  my_dataframe <- data.frame(
    Comparison = c("A-B", "A-C", "B-C"),
    p_value = c(0.9, 0.02, 0.03),
    p.adjust = c(1.0, 0.04, 0.06)
  )

  result <- make_cld(p.adjust ~ Comparison, data = my_dataframe)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true(all(c("A", "B", "C") %in% result$group))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.data.frame` works with custom column names", {
  df <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.9, 0.02, 0.03)
  )

  result <- make_cld(df, alpha = 0.05)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.data.frame` respects alpha parameter", {
  df <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.9, 0.02, 0.03)
  )

  result_005 <- make_cld(df, alpha = 0.05)
  result_001 <- make_cld(df, alpha = 0.01)

  expect_s3_class(result_005, "cld_object")
  expect_s3_class(result_001, "cld_object")
  # With alpha=0.01, fewer comparisons are significant
  expect_false(identical(result_005$cld, result_001$cld))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.data.frame` works with alternative column names", {
  df <- data.frame(
    grp1 = c("X", "X", "Y"),
    grp2 = c("Y", "Z", "Z"),
    pval = c(0.8, 0.01, 0.02)
  )

  result <- make_cld(df, gr1_col = "grp1", gr2_col = "grp2", p_val_col = "pval")

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true(all(c("X", "Y", "Z") %in% result$group))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.data.frame works with fully custom column names", {
  # Test with non-default column names
  df <- data.frame(
    treatment1 = c("A", "A", "B"),
    treatment2 = c("B", "C", "C"),
    pvalue = c(0.01, 0.05, 0.3)
  )
  
  result <- make_cld(
    df,
    gr1_col = "treatment1",
    gr2_col = "treatment2",
    p_val_col = "pvalue"
  )
  
  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true(all(c("A", "B", "C") %in% result$group))
})

