# Tests for make_cld.matrix method ==========================================

test_that("`make_cld.matrix` (symmetric) works", {
  m <- c(
    1.00, 0.22, 0.05, 0.00,
    0.22, 1.00, 0.17, 0.01,
    0.05, 0.17, 1.00, 0.22,
    0.00, 0.01, 0.22, 1.00
  )
  obj <- matrix(m, nrow = 4)
  rownames(obj) <- colnames(obj) <- c("P", "O", "I", "U")

  result <- make_cld(obj)

  expect_s3_class(result, "cld_object")
  expect_equal(
    as.character(result$cld),
    c("a", "a", "ab", "b")
  )
  # Groups are sorted alphabetically
  expect_equal(result$group, c("P", "O", "I", "U"))
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.matrix` validates input", {
  # Non-square matrix
  m <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(make_cld(m), "square")

  # Square matrix with mismatched names
  m2 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  rownames(m2) <- c("A", "B")
  colnames(m2) <- c("C", "D")
  expect_error(make_cld(m2), "do not match")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("`make_cld.matrix` respects alpha parameter", {
  # Create matrix with p-values strategically placed around alpha thresholds
  # P-O: 0.03 (sig at 0.05, not at 0.01)
  # P-I: 0.12 (sig at 0.20, not at 0.05)
  # P-U: 0.001 (always sig)
  # O-I: 0.15 (sig at 0.20, not at 0.05)
  # O-U: 0.002 (always sig)
  # I-U: 0.25 (not sig even at 0.20)
  m <- c(
    1.00, 0.03, 0.12, 0.001,
    0.03, 1.00, 0.15, 0.002,
    0.12, 0.15, 1.00, 0.25,
    0.001, 0.002, 0.25, 1.00
  )
  obj <- matrix(m, nrow = 4)
  rownames(obj) <- colnames(obj) <- c("P", "O", "I", "U")

  result_005 <- make_cld(obj, alpha = 0.05)
  result_020 <- make_cld(obj, alpha = 0.20)

  expect_s3_class(result_005, "cld_object")
  expect_s3_class(result_020, "cld_object")

  # At alpha=0.05: fewer significant differences
  # At alpha=0.20: more significant differences, so CLDs should differ
  expect_false(identical(as.character(result_005$cld), as.character(result_020$cld)))
})
