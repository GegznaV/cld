# Tests for metadata attributes ============================================

test_that("make_cld stores alpha attribute correctly", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)

  result1 <- make_cld(obj, alpha = 0.05)
  expect_equal(attr(result1, "alpha"), 0.05)

  result2 <- make_cld(obj, alpha = 0.01)
  expect_equal(attr(result2, "alpha"), 0.01)

  result3 <- make_cld(obj, alpha = 0.10)
  expect_equal(attr(result3, "alpha"), 0.10)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld stores n_comparisons attribute", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  # PlantGrowth has 3 groups, so 3 comparisons
  expect_true(!is.null(attr(result, "n_comparisons")))
  expect_equal(attr(result, "n_comparisons"), 3)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("make_cld stores n_significant attribute", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj, alpha = 0.05)

  expect_true(!is.null(attr(result, "n_significant")))
  expect_type(attr(result, "n_significant"), "integer")
  expect_true(attr(result, "n_significant") >= 0)
  expect_true(attr(result, "n_significant") <= attr(result, "n_comparisons"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("n_significant changes with alpha threshold", {
  obj <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )

  result1 <- make_cld(obj, alpha = 0.05)
  result2 <- make_cld(obj, alpha = 0.01)

  # Stricter alpha should result in fewer or equal significant comparisons
  expect_true(attr(result2, "n_significant") <= attr(result1, "n_significant"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metadata attributes work with matrix input", {
  m <- c(
    1.00, 0.22, 0.05, 0.00,
    0.22, 1.00, 0.17, 0.01,
    0.05, 0.17, 1.00, 0.22,
    0.00, 0.01, 0.22, 1.00
  )
  obj <- matrix(m, nrow = 4)
  rownames(obj) <- colnames(obj) <- c("P", "O", "I", "U")

  result <- make_cld(obj, alpha = 0.05)

  expect_equal(attr(result, "alpha"), 0.05)
  expect_true(!is.null(attr(result, "n_comparisons")))
  expect_true(!is.null(attr(result, "n_significant")))

  # 4 groups = 6 pairwise comparisons
  expect_equal(attr(result, "n_comparisons"), 6)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metadata attributes work with data.frame input", {
  my_data <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.001, 0.045, 0.892)
  )

  result <- make_cld(my_data, gr1_col = "group1", gr2_col = "group2",
    p_val_col = "p.adj", alpha = 0.05)

  expect_equal(attr(result, "alpha"), 0.05)
  expect_equal(attr(result, "n_comparisons"), 3)
  expect_equal(attr(result, "n_significant"), 2)  # 0.001 and 0.045 are < 0.05
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metadata attributes work with formula input", {
  my_dataframe <- data.frame(
    Comparison = c("EE - GB = 0", "EE - CY = 0", "GB - CY = 0"),
    p.value = c(1, 0.001093, 0.005477),
    p.adjust = c(1.000000, 0.003279, 0.008216)
  )

  result <- make_cld(p.adjust ~ Comparison, data = my_dataframe, alpha = 0.01)

  expect_equal(attr(result, "alpha"), 0.01)
  expect_equal(attr(result, "n_comparisons"), 3)
  expect_equal(attr(result, "n_significant"), 2)  # 0.003279 and 0.008216 are < 0.01
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metadata attributes work with PMCMR objects", {
  skip_if_not_installed("PMCMRplus")

  suppressWarnings(
    obj <- PMCMRplus::kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
  )
  result <- make_cld(obj, alpha = 0.05)

  expect_equal(attr(result, "alpha"), 0.05)
  expect_true(!is.null(attr(result, "n_comparisons")))
  expect_true(!is.null(attr(result, "n_significant")))

  # 6 groups = 15 pairwise comparisons
  expect_equal(attr(result, "n_comparisons"), 15)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metadata attributes work with rstatix objects", {
  skip_if_not_installed("rstatix")
  
  obj <- rstatix::games_howell_test(PlantGrowth, weight ~ group)
  result <- make_cld(obj, alpha = 0.05)
  
  expect_equal(attr(result, "alpha"), 0.05)
  expect_equal(attr(result, "n_comparisons"), 3)
  expect_true(!is.null(attr(result, "n_significant")))
})# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("metadata attributes work with DescTools objects", {
  skip_if_not_installed("DescTools")

  obj <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)
  result <- make_cld(obj, alpha = 0.05)

  expect_equal(attr(result, "alpha"), 0.05)
  expect_equal(attr(result, "n_comparisons"), 3)
  expect_true(!is.null(attr(result, "n_significant")))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("all three metadata attributes are always present", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  expect_true(!is.null(attr(result, "alpha")))
  expect_true(!is.null(attr(result, "method")))
  expect_true(!is.null(attr(result, "n_comparisons")))
  expect_true(!is.null(attr(result, "n_significant")))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("n_significant is 0 when no comparisons are significant", {
  # Create data where all p-values > alpha
  my_data <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.5, 0.7, 0.9)
  )

  result <- make_cld(my_data, gr1_col = "group1", gr2_col = "group2",
    p_val_col = "p.adj", alpha = 0.05)

  expect_equal(attr(result, "n_significant"), 0)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("n_significant equals n_comparisons when all are significant", {
  # Create data where all p-values < alpha
  my_data <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.001, 0.002, 0.003)
  )

  result <- make_cld(my_data, gr1_col = "group1", gr2_col = "group2",
    p_val_col = "p.adj", alpha = 0.05)

  expect_equal(attr(result, "n_significant"), attr(result, "n_comparisons"))
  expect_equal(attr(result, "n_significant"), 3)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("attributes are preserved after coercion to data.frame", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  alpha_before <- attr(result, "alpha")
  method_before <- attr(result, "method")
  n_comp_before <- attr(result, "n_comparisons")
  n_sig_before <- attr(result, "n_significant")

  df <- as.data.frame(result)

  expect_equal(attr(df, "alpha"), alpha_before)
  expect_equal(attr(df, "method"), method_before)
  expect_equal(attr(df, "n_comparisons"), n_comp_before)
  expect_equal(attr(df, "n_significant"), n_sig_before)
})
