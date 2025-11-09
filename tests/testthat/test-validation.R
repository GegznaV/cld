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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tests for make_cld.data.frame column validation ---------------------------

test_that("make_cld.data.frame errors when gr1_col is missing", {
  df <- data.frame(
    group2 = c("A", "B"),
    p.adj = c(0.01, 0.05)
  )
  
  expect_error(
    make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"),
    "Column 'group1' not found"
  )
})

test_that("make_cld.data.frame errors when gr2_col is missing", {
  df <- data.frame(
    group1 = c("A", "B"),
    p.adj = c(0.01, 0.05)
  )
  
  expect_error(
    make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"),
    "Column 'group2' not found"
  )
})

test_that("make_cld.data.frame errors when p_val_col is missing", {
  df <- data.frame(
    group1 = c("A", "B"),
    group2 = c("B", "C")
  )
  
  expect_error(
    make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj"),
    "Column 'p.adj' not found"
  )
})
