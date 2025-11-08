# Tests for make_cld.PMCMR method ===========================================

test_that("`make_cld.PMCMR` works with kwAllPairsConoverTest", {
  skip_if_not_installed("PMCMRplus")

  expect_warning(
    obj2a <- PMCMRplus::kwAllPairsConoverTest(
      weight ~ feed,
      data = chickwts
    )
  )

  result <- make_cld(obj2a)

  expect_s3_class(result, "cld_object")
  expect_equal(
    as.character(result$cld),
    c("a", "b", "bc", "ac", "c", "a")
  )
  expect_equal(nrow(result), 6)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.PMCMR` works with kwManyOneConoverTest", {
  skip_if_not_installed("PMCMRplus")

  expect_warning(
    obj2b <- PMCMRplus::kwManyOneConoverTest(
      weight ~ feed,
      data = chickwts
    )
  )

  result <- make_cld(obj2b)

  expect_s3_class(result, "cld_object")
  expect_equal(
    as.character(result$cld),
    c("a", "b", "b", "ab", "b", "ab")
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.PMCMR` respects alpha parameter", {
  skip_if_not_installed("PMCMRplus")

  expect_warning(
    obj <- PMCMRplus::kwAllPairsConoverTest(
      weight ~ feed,
      data = chickwts
    )
  )

  result_005 <- make_cld(obj, alpha = 0.05)
  result_001 <- make_cld(obj, alpha = 0.01)

  expect_s3_class(result_005, "cld_object")
  expect_s3_class(result_001, "cld_object")
})
