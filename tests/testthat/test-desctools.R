# Tests for DescTools package integration ===================================

test_that("`make_cld.PostHocTest` works with ConoverTest", {
  skip_if_not_installed("DescTools")

  data(PlantGrowth, package = "datasets")
  obj1 <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)

  result <- make_cld(obj1)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  # Check that all expected groups are present (order may vary)
  expect_setequal(as.character(result$group), c("ctrl", "trt1", "trt2"))
  expect_true(all(c("group", "cld", "spaced_cld") %in% names(result)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.PostHocTest` works with DunnettTest", {
  skip_if_not_installed("DescTools")

  data(PlantGrowth, package = "datasets")
  obj2 <- DescTools::DunnettTest(weight ~ group, data = PlantGrowth)

  result <- make_cld(obj2)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true(all(c("group", "cld", "spaced_cld") %in% names(result)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.DunnTest` works with DunnTest", {
  skip_if_not_installed("DescTools")

  data(PlantGrowth, package = "datasets")
  obj3 <- DescTools::DunnTest(weight ~ group, data = PlantGrowth)

  result <- make_cld(obj3)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  # Check that all expected groups are present (order may vary)
  expect_setequal(as.character(result$group), c("ctrl", "trt1", "trt2"))
  expect_true(all(c("group", "cld", "spaced_cld") %in% names(result)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.PostHocTest` respects alpha parameter", {
  skip_if_not_installed("DescTools")

  data(PlantGrowth, package = "datasets")
  obj <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)

  result_005 <- make_cld(obj, alpha = 0.05)
  result_001 <- make_cld(obj, alpha = 0.01)

  expect_s3_class(result_005, "cld_object")
  expect_s3_class(result_001, "cld_object")

  # With stricter alpha, we might get different groupings
  # (not necessarily, depends on p-values)
  expect_equal(nrow(result_005), nrow(result_001))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.PostHocTest` works with ScheffeTest", {
  skip_if_not_installed("DescTools")

  data(PlantGrowth, package = "datasets")
  obj <- DescTools::ScheffeTest(aov(weight ~ group, data = PlantGrowth))

  result <- make_cld(obj)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 3)
  expect_true(all(c("group", "cld", "spaced_cld") %in% names(result)))
})
