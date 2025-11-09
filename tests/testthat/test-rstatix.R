# Tests for rstatix package integration =====================================

test_that("`make_cld.data.frame` works with rstatix output", {
  skip_if_not_installed("rstatix")

  obj1 <- rstatix::games_howell_test(weight ~ feed, data = chickwts)

  result <- make_cld(obj1)

  expect_s3_class(result, "cld_object")
  expect_equal(
    as.character(result$group),
    c("horsebean", "linseed", "meatmeal", "soybean", "sunflower", "casein")
  )
  expect_equal(nrow(result), 6)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld` works with rstatix tukey_hsd", {
  skip_if_not_installed("rstatix")

  obj <- rstatix::tukey_hsd(chickwts, weight ~ feed)

  result <- make_cld(obj)

  expect_s3_class(result, "cld_object")
  expect_equal(nrow(result), 6)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("rstatix results work directly with data.frame method", {
  skip_if_not_installed("rstatix")
  
  # This test confirms that rstatix objects work with the data.frame method
  # rstatix results are data.frames with appropriate columns
  
  gh_result <- rstatix::games_howell_test(chickwts, weight ~ feed)
  
  result <- make_cld(gh_result)
  
  # Check that it works and has proper structure
  expect_s3_class(result, "cld_object")
  expect_s3_class(gh_result, "data.frame")  # rstatix results are data.frames
  expect_equal(nrow(result), 6)
})
