# Tests for parameter validation ============================================

test_that("alpha parameter validation works", {
  obj1 <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )

  expect_error(make_cld(obj1, alpha = -0.1), "Element 1 is not >= 0")
  expect_error(make_cld(obj1, alpha = 1.5), "Element 1 is not <= 1")
  expect_error(make_cld(obj1, alpha = "0.05"), "Must be of type 'number'")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("alpha parameter must be numeric", {
  df <- data.frame(
    group1 = c("A", "A", "B"),
    group2 = c("B", "C", "C"),
    p.adj = c(0.9, 0.02, 0.03)
  )

  expect_error(make_cld(df, alpha = "0.05"), "Must be of type 'number'")
  expect_error(make_cld(df, alpha = NULL), "Must be of type 'number'")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("alpha parameter works with valid values", {
  obj1 <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )

  # Should work with valid alpha values
  expect_s3_class(make_cld(obj1, alpha = 0.01), "cld_object")
  expect_s3_class(make_cld(obj1, alpha = 0.05), "cld_object")
  expect_s3_class(make_cld(obj1, alpha = 0.10), "cld_object")
  expect_s3_class(make_cld(obj1, alpha = 0.50), "cld_object")
  expect_s3_class(make_cld(obj1, alpha = 1.00), "cld_object")
  expect_s3_class(make_cld(obj1, alpha = 0.00), "cld_object")
})
