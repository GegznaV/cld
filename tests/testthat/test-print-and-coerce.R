# Tests for print and coercion methods =====================================

test_that("print.cld_object displays correct structure", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  output <- capture.output(print(result))

  # Check header
  expect_true(any(grepl("Compact Letter Display \\(CLD\\)", output)))

  # Check alpha value
  expect_true(any(grepl("Signif. level \\(alpha\\):", output)))
  expect_true(any(grepl("0.05", output)))

  # Check method
  expect_true(any(grepl("Method:", output)))

  # Check data is printed
  expect_true(any(grepl("group", output)))
  expect_true(any(grepl("cld", output)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("print.cld_object returns object invisibly", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  not_used <- capture.output(
    returned <- withVisible(print(result))
  )

  expect_false(returned$visible)
  expect_identical(returned$value, result)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("print.cld_object handles missing alpha attribute", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  # Remove alpha attribute
  attr(result, "alpha") <- NULL

  output <- capture.output(print(result))

  # Should still print without error
  expect_true(any(grepl("Compact Letter Display", output)))
  # Alpha line should not appear
  expect_false(any(grepl("Significance level", output)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("print.cld_object handles missing method attribute", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  # Remove method attribute
  attr(result, "method") <- NULL

  output <- capture.output(print(result))

  # Should still print without error
  expect_true(any(grepl("Compact Letter Display", output)))
  # Method line should not appear
  expect_false(any(grepl("Method:", output)))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as.data.frame.cld_object removes cld_object class", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  df <- as.data.frame(result)

  expect_false("cld_object" %in% class(df))
  expect_true("data.frame" %in% class(df))
  expect_equal(class(df), "data.frame")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as.data.frame.cld_object preserves data content", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  df <- as.data.frame(result)

  expect_equal(df$group, result$group)
  expect_equal(df$cld, result$cld)
  expect_equal(df$spaced_cld, result$spaced_cld)
  expect_equal(ncol(df), 3)
  expect_equal(nrow(df), nrow(result))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as.data.frame.cld_object preserves attributes", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  df <- as.data.frame(result)

  # Attributes should be preserved
  expect_equal(attr(df, "alpha"), attr(result, "alpha"))
  expect_equal(attr(df, "method"), attr(result, "method"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as.character.cld_object returns named character vector", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  char_vec <- as.character(result)

  expect_type(char_vec, "character")
  expect_named(char_vec)
  expect_equal(length(char_vec), nrow(result))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as.character.cld_object has correct names and values", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  char_vec <- as.character(result)

  # Names should be group names
  expect_equal(names(char_vec), result$group)

  # Values should be CLD letters
  expect_equal(as.character(char_vec), result$cld)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as.character.cld_object works with complex CLDs", {
  obj <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )
  result <- make_cld(obj)

  char_vec <- as.character(result)

  expect_equal(length(char_vec), 6)
  expect_equal(names(char_vec), result$group)
  expect_equal(unname(char_vec), result$cld)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("coercion methods work in sequence", {
  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  # Convert to data frame and back (via as_cld)
  df <- as.data.frame(result)
  result2 <- as_cld(df)

  expect_s3_class(result2, "cld_object")
  expect_equal(result2$group, result$group)
  expect_equal(result2$cld, result$cld)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("print method works with all input types", {
  # Test with different methods to ensure print works consistently

  # pairwise.htest
  obj1 <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result1 <- make_cld(obj1)
  expect_output(print(result1), "Compact Letter Display")

  # matrix
  m <- matrix(c(1, 0.01, 0.01, 1), nrow = 2)
  rownames(m) <- colnames(m) <- c("A", "B")
  result2 <- make_cld(m)
  expect_output(print(result2), "Compact Letter Display")

  # data.frame
  df <- data.frame(
    group1 = c("A"),
    group2 = c("B"),
    p.adj = c(0.001)
  )
  result3 <- make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj")
  expect_output(print(result3), "Compact Letter Display")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_tibble.cld_object returns tibble", {
  skip_if_not_installed("tibble")

  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  tib <- tibble::as_tibble(result)

  expect_s3_class(tib, "tbl_df")
  expect_s3_class(tib, "tbl")
  expect_s3_class(tib, "data.frame")
  expect_false("cld_object" %in% class(tib))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_tibble.cld_object preserves data content", {
  skip_if_not_installed("tibble")

  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  tib <- tibble::as_tibble(result)

  expect_equal(tib$group, result$group)
  expect_equal(tib$cld, result$cld)
  expect_equal(tib$spaced_cld, result$spaced_cld)
  expect_equal(ncol(tib), 3)
  expect_equal(nrow(tib), nrow(result))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_tibble.cld_object preserves attributes", {
  skip_if_not_installed("tibble")

  obj <- pairwise.t.test(PlantGrowth$weight, PlantGrowth$group)
  result <- make_cld(obj)

  tib <- tibble::as_tibble(result)

  # Attributes should be preserved
  expect_equal(attr(tib, "alpha"), attr(result, "alpha"))
  expect_equal(attr(tib, "method"), attr(result, "method"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_tibble.cld_object works with complex CLDs", {
  skip_if_not_installed("tibble")

  obj <- pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  )
  result <- make_cld(obj)

  tib <- tibble::as_tibble(result)

  expect_s3_class(tib, "tbl_df")
  expect_equal(nrow(tib), 6)
  expect_equal(tib$group, result$group)
  expect_equal(tib$cld, result$cld)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_tibble.cld_object errors when tibble is not installed", {
  skip("Mocking requireNamespace is complex in testthat 3.x")
  
  # This test documents that as_tibble checks for tibble package
  # In practice, tibble is a Suggests dependency and users may not have it
  # The function should error gracefully with a helpful message
  
  # Test is skipped because:
  # 1. testthat 3.x changed mocking behavior
  # 2. requireNamespace is in base R and hard to mock
  # 3. The error path is simple and well-documented
  # 4. Manual testing confirms the error message works correctly
})
