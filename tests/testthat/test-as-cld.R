# Tests for as_cld() converter function ====================================

test_that("as_cld.data.frame works with valid input", {
  df <- data.frame(
    group = c("A", "B", "C"),
    cld = c("a", "b", "b")
  )

  result <- as_cld(df)

  # Check structure
  expect_s3_class(result, "cld_object")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("group", "cld", "spaced_cld"))
  expect_equal(nrow(result), 3)

  # Check values
  expect_equal(result$group, c("A", "B", "C"))
  expect_equal(result$cld, c("a", "b", "b"))
  expect_true(all(nchar(result$spaced_cld) == nchar(result$spaced_cld[1])))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.data.frame creates spaced_cld if missing", {
  df <- data.frame(
    group = c("A", "B", "C"),
    cld = c("ab", "b", "abc")
  )

  result <- as_cld(df)

  expect_true("spaced_cld" %in% names(result))
  expect_equal(nchar(result$spaced_cld[1]), nchar(result$spaced_cld[2]))
  expect_equal(nchar(result$spaced_cld[2]), nchar(result$spaced_cld[3]))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.data.frame preserves existing spaced_cld", {
  df <- data.frame(
    group = c("A", "B", "C"),
    cld = c("a", "b", "b"),
    spaced_cld = c("_a_", "_b_", "_b_")
  )

  result <- as_cld(df)

  expect_equal(result$spaced_cld, c("_a_", "_b_", "_b_"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.data.frame adds class if not present", {
  df <- data.frame(
    group = c("A", "B", "C"),
    cld = c("a", "b", "b")
  )

  result <- as_cld(df)

  expect_true("cld_object" %in% class(result))
  expect_equal(class(result), c("cld_object", "data.frame"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.data.frame errors when 'group' column is missing", {
  df <- data.frame(
    name = c("A", "B", "C"),
    cld = c("a", "b", "b")
  )

  expect_error(
    as_cld(df),
    "Data frame must have a 'group' column"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.data.frame errors when 'cld' column is missing", {
  df <- data.frame(
    group = c("A", "B", "C"),
    letters = c("a", "b", "b")
  )

  expect_error(
    as_cld(df),
    "Data frame must have a 'cld' column"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.character works with named character vector", {
  letters_vec <- c(A = "a", B = "b", C = "b")

  result <- as_cld(letters_vec)

  # Check structure
  expect_s3_class(result, "cld_object")
  expect_s3_class(result, "data.frame")
  expect_named(result, c("group", "cld", "spaced_cld"))

  # Check values
  expect_equal(result$group, c("A", "B", "C"))
  expect_equal(result$cld, c("a", "b", "b"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.character works with complex letter combinations", {
  letters_vec <- c(
    Treatment1 = "a",
    Treatment2 = "ab",
    Treatment3 = "bc",
    Treatment4 = "c"
  )

  result <- as_cld(letters_vec)

  expect_equal(result$group, c("Treatment1", "Treatment2", "Treatment3", "Treatment4"))
  expect_equal(result$cld, c("a", "ab", "bc", "c"))
  expect_equal(nrow(result), 4)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.character errors when vector is not named", {
  letters_vec <- c("a", "b", "b")

  expect_error(
    as_cld(letters_vec),
    "Character vector must be named"
  )
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.cld_object returns object unchanged", {
  # Create a cld_object
  obj <- make_cld(pairwise.wilcox.test(
    chickwts$weight,
    chickwts$feed,
    exact = FALSE
  ))

  result <- as_cld(obj)

  expect_identical(result, obj)
  expect_s3_class(result, "cld_object")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld works with output from make_cld", {
  # Test integration with make_cld
  original <- make_cld(pairwise.t.test(
    PlantGrowth$weight,
    PlantGrowth$group
  ))

  # Convert to data frame and back
  df <- as.data.frame(original)
  result <- as_cld(df)

  expect_s3_class(result, "cld_object")
  expect_equal(result$group, original$group)
  expect_equal(result$cld, original$cld)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("as_cld.data.frame handles extra columns gracefully", {
  df <- data.frame(
    group = c("A", "B", "C"),
    cld = c("a", "b", "b"),
    extra_col = c(1, 2, 3),
    another_col = c("x", "y", "z")
  )

  result <- as_cld(df)

  # Should only have the three required columns
  expect_named(result, c("group", "cld", "spaced_cld"))
  expect_equal(ncol(result), 3)
})
