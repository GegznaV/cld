# Tests for make_cld.pairwise.htest method ==================================

test_that("`make_cld.pairwise.htest` works with pairwise.wilcox.test", {
  obj1 <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )

  result <- make_cld(obj1)

  # Check structure
  expect_s3_class(result, "cld_object")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("group", "cld", "spaced_cld"))
  expect_equal(nrow(result), 6)

  # Check CLD values
  expect_equal(
    as.character(result$cld),
    c("a", "b", "bc", "ac", "c", "a")
  )

  # Check group names
  expect_equal(
    result$group,
    c("casein", "horsebean", "linseed", "meatmeal", "soybean", "sunflower")
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.pairwise.htest` works with pairwise.t.test", {
  obj2 <- with(OrchardSprays, pairwise.t.test(decrease, treatment))

  result <- make_cld(obj2)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 8)
  expect_equal(
    as.character(result$cld),
    c("a", "a", "a", "ab", "bc", "c", "c", "c")
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.pairwise.htest` works with insignificant results", {
  smokers <- c(83, 90, 129, 70)
  patients <- c(86, 93, 136, 82)
  expect_warning(expect_warning(expect_warning(
    obj3 <- pairwise.prop.test(smokers, patients)
  )))

  result <- make_cld(obj3)

  expect_s3_class(result, "cld_object")
  expect_equal(as.character(result$cld), c("a", "a", "a", "a"))
  expect_equal(nrow(result), 4)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.pairwise.htest` respects alpha parameter", {
  obj1 <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )

  result_005 <- make_cld(obj1, alpha = 0.05)
  result_001 <- make_cld(obj1, alpha = 0.01)

  # More stringent alpha should result in fewer differences (more groups sharing letters)
  expect_s3_class(result_001, "cld_object")
  expect_false(identical(result_005$cld, result_001$cld))
})
