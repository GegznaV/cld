# Tests for swap_compared_names functionality ==============================

test_that("swap_compared_names works with simple group names", {
  # Create test data
  df <- data.frame(
    comparison = c("A-B", "A-C", "B-C"),
    p_value = c(0.01, 0.05, 0.10)
  )

  # Test with swap_compared_names = FALSE (default)
  result1 <- make_cld(p_value ~ comparison, data = df, swap_compared_names = FALSE)

  # Test with swap_compared_names = TRUE
  result2 <- make_cld(p_value ~ comparison, data = df, swap_compared_names = TRUE)

  # Both should produce valid cld objects
  expect_s3_class(result1, "cld_object")
  expect_s3_class(result2, "cld_object")

  # Both should have the same groups
  expect_setequal(result1$group, result2$group)
})

test_that("swap_compared_names works with complex group names", {
  # Create test data with more complex names
  df <- data.frame(
    comparison = c("Group1-Group2", "Group1-Group3", "Group2-Group3"),
    p_value = c(0.01, 0.05, 0.10)
  )

  # Test with swap_compared_names = FALSE
  result1 <- make_cld(p_value ~ comparison, data = df, swap_compared_names = FALSE)

  # Test with swap_compared_names = TRUE
  result2 <- make_cld(p_value ~ comparison, data = df, swap_compared_names = TRUE)

  # Both should produce valid cld objects
  expect_s3_class(result1, "cld_object")
  expect_s3_class(result2, "cld_object")

  # Both should have the same groups
  expect_setequal(result1$group, result2$group)
  expect_true(all(result1$group %in% c("Group1", "Group2", "Group3")))
})

test_that("swap_compared_names works with names containing underscores", {
  # Create test data with underscores
  df <- data.frame(
    comparison = c(
      "Treatment_A-Treatment_B", "Treatment_A-Treatment_C",
      "Treatment_B-Treatment_C"
    ),
    p_value = c(0.01, 0.05, 0.10)
  )

  # Test with swap_compared_names = TRUE
  result <- make_cld(p_value ~ comparison, data = df, swap_compared_names = TRUE)

  # Should produce valid cld object
  expect_s3_class(result, "cld_object")

  # Should have correct groups
  expect_true(all(result$group %in% c("Treatment_A", "Treatment_B", "Treatment_C")))
})

test_that("swap_compared_names works with numeric group names", {
  # Create test data with numeric names
  df <- data.frame(
    comparison = c("1-2", "1-3", "2-3"),
    p_value = c(0.01, 0.05, 0.10)
  )

  # Test with swap_compared_names = TRUE
  result <- make_cld(p_value ~ comparison, data = df, swap_compared_names = TRUE)

  # Should produce valid cld object
  expect_s3_class(result, "cld_object")

  # Should have correct groups
  expect_true(all(result$group %in% c("1", "2", "3")))
})

test_that("base R string operations produce correct results", {
  # Test the actual string operations used in the code
  test_cases <- c(
    "A-B",
    "Group1-Group2",
    "Treatment_A-Treatment_B",
    "1-2",
    "Control-Treated"
  )

  for (comp in test_cases) {
    # Extract parts using base R (as in the updated code)
    part_1 <- sub("-.*$", "", comp)
    part_2 <- sub("^.*?-", "", comp)

    # Verify parts are non-empty
    expect_true(nchar(part_1) > 0,
      info = paste("part_1 should not be empty for:", comp)
    )
    expect_true(nchar(part_2) > 0,
      info = paste("part_2 should not be empty for:", comp)
    )

    # Verify swapping works
    swapped <- paste0(part_2, "-", part_1)
    expect_true(grepl("-", swapped),
      info = paste("swapped should contain hyphen:", swapped)
    )

    # Verify we can extract both parts from swapped version
    part_1_back <- sub("-.*$", "", swapped)
    part_2_back <- sub("^.*?-", "", swapped)
    expect_equal(part_1_back, part_2,
      info = paste("Round-trip should work for:", comp)
    )
    expect_equal(part_2_back, part_1,
      info = paste("Round-trip should work for:", comp)
    )
  }
})
