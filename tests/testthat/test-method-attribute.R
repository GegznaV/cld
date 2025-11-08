# Tests for 'method' attribute =============================================

test_that("make_cld.pairwise.htest stores 'method' attribute", {
  obj <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )

  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "Wilcoxon rank sum test with continuity correction")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.pairwise.htest stores correct method for t-test", {
  obj <- pairwise.t.test(
    PlantGrowth$weight,
    PlantGrowth$group
  )

  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "t tests with pooled SD")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld with rstatix games_howell_test stores 'method' attribute", {
  skip_if_not_installed("rstatix")

  obj <- rstatix::games_howell_test(PlantGrowth, weight ~ group)

  result <- make_cld(obj)

  # rstatix objects are handled by data.frame method
  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "data.frame")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld with rstatix tukey_hsd stores 'method' attribute", {
  skip_if_not_installed("rstatix")

  obj <- rstatix::tukey_hsd(PlantGrowth, weight ~ group)

  result <- make_cld(obj)

  # rstatix objects are handled by data.frame method
  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "data.frame")
}) # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.PMCMR stores 'method' attribute", {
  skip_if_not_installed("PMCMRplus")

  suppressWarnings(
    obj <- PMCMRplus::kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
  )

  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "Conover's all-pairs test")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.PostHocTest stores 'method' attribute", {
  skip_if_not_installed("DescTools")

  obj <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)

  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "ConoverTest")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.DunnTest stores 'method' attribute", {
  skip_if_not_installed("DescTools")

  obj <- DescTools::DunnTest(weight ~ group, data = PlantGrowth)

  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "DunnTest")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.formula stores 'method' attribute", {
  my_dataframe <- data.frame(
    Comparison = c("EE - GB = 0", "EE - CY = 0", "GB - CY = 0"),
    p.value = c(1, 0.001093, 0.005477),
    p.adjust = c(1.000000, 0.003279, 0.008216)
  )

  result <- make_cld(p.adjust ~ Comparison, data = my_dataframe)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "formula")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.matrix stores 'method' attribute", {
  m <- c(
    1.00, 0.22, 0.05, 0.00,
    0.22, 1.00, 0.17, 0.01,
    0.05, 0.17, 1.00, 0.22,
    0.00, 0.01, 0.22, 1.00
  )
  obj <- matrix(m, nrow = 4)
  rownames(obj) <- colnames(obj) <- c("P", "O", "I", "U")

  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "matrix")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.data.frame stores 'method' attribute", {
  my_data <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.001, 0.045, 0.892)
  )

  result <- make_cld(my_data, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj")

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "data.frame")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld.pairwise_pval_df stores 'method' attribute", {
  m <- matrix(
    c(NA, 0.22, 0.05, 0.22, NA, 0.17, 0.05, 0.17, NA),
    nrow = 3
  )
  rownames(m) <- colnames(m) <- c("A", "B", "C")

  df <- pval_matrix_to_df(m)
  result <- make_cld(df)

  expect_true(!is.null(attr(result, "method")))
  expect_equal(attr(result, "method"), "pairwise_pval_df")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("method attribute is preserved through print", {
  obj <- pairwise.t.test(
    PlantGrowth$weight,
    PlantGrowth$group
  )

  result <- make_cld(obj)
  method_before <- attr(result, "method")

  # Capture print output (should not affect attributes)
  capture.output(print(result))

  method_after <- attr(result, "method")

  expect_equal(method_before, method_after)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("method attribute appears in print output", {
  obj <- pairwise.t.test(
    PlantGrowth$weight,
    PlantGrowth$group
  )

  result <- make_cld(obj)
  output <- capture.output(print(result))

  # Check that method appears in the output
  expect_true(any(grepl("Method:", output)))
  expect_true(any(grepl("t tests with pooled SD", output)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("all methods have consistent method attribute behavior", {
  # Test that all methods return character strings

  # pairwise.htest
  obj1 <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result1 <- make_cld(obj1)
  expect_type(attr(result1, "method"), "character")

  # matrix
  m <- matrix(c(1, 0.22, 0.22, 1), nrow = 2)
  rownames(m) <- colnames(m) <- c("A", "B")
  result2 <- make_cld(m)
  expect_type(attr(result2, "method"), "character")

  # data.frame
  df <- data.frame(
    group1 = c("A"),
    group2 = c("B"),
    p.adj = c(0.05)
  )
  result3 <- make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj")
  expect_type(attr(result3, "method"), "character")

  # formula
  my_data <- data.frame(
    Comparison = c("A - B = 0"),
    p.adjust = c(0.05)
  )
  result4 <- make_cld(p.adjust ~ Comparison, data = my_data)
  expect_type(attr(result4, "method"), "character")
})
