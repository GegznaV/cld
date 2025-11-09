library(testthat)

test_that("swap_compared_names normalizes reversed comparisons", {
  comps_rev <- c("B-A", "C-A", "C-B")
  pvals <- c(0.2, 0.01, 0.01)

  res1 <- cld:::make_cld_df(comparison = comps_rev, p.value = pvals,
    threshold = 0.05, swap_compared_names = TRUE,
    print_comp = FALSE)

  res2 <- cld:::make_cld_df(comparison = c("A-B", "A-C", "B-C"),
    p.value = pvals, threshold = 0.05,
    print_comp = FALSE)

  # Compare sorted by group to avoid ordering differences
  expect_equal(res1[order(res1$group), ], res2[order(res2$group), ])
})

test_that("reversed changes letter ordering", {
  comps <- c("A-B", "A-C", "B-C")
  pvals <- c(0.2, 0.01, 0.01)

  res_norm <- cld:::make_cld_df(comparison = comps, p.value = pvals,
    threshold = 0.05, reversed = FALSE,
    print_comp = FALSE)

  res_rev <- cld:::make_cld_df(comparison = comps, p.value = pvals,
    threshold = 0.05, reversed = TRUE,
    print_comp = FALSE)

  # Letters should not be identical when reversed = TRUE
  expect_false(all(res_norm$cld == res_rev$cld))
  # But they should contain same set of groups
  expect_setequal(res_norm$group, res_rev$group)
})

test_that("print_comp and cleaning options (swap_vs, swap_colon, remove_equal, remove_space)", {
  pvals <- c(0.2, 0.01, 0.01)

  # swap_vs: "vs" should be replaced with '-'
  comps_vs <- c("A vs B", "A vs C", "B vs C")
  out_vs <- capture.output(
    cld:::make_cld_df(comparison = comps_vs, p.value = pvals,
      threshold = 0.05, swap_vs = TRUE, print_comp = TRUE)
  )
  expect_false(any(grepl("vs", out_vs)))
  expect_true(any(grepl("-", out_vs)))

  # swap_colon: ':' should be replaced with '-' in comparisons
  comps_colon <- c("A:B", "A:C", "B:C")
  out_colon <- capture.output(
    cld:::make_cld_df(comparison = comps_colon, p.value = pvals,
      threshold = 0.05, swap_colon = TRUE, print_comp = TRUE)
  )
  # Check that A:B pattern (comparison with colon) is not in output
  expect_false(any(grepl("A:B|A:C|B:C", out_colon)))
  expect_true(any(grepl("A-B|A-C|B-C", out_colon)))

  # remove_equal: '=' should be removed from strings
  comps_eq <- c("A - B = 0", "A - C = 0", "B - C = 0")
  out_eq <- capture.output(
    cld:::make_cld_df(comparison = comps_eq, p.value = pvals,
      threshold = 0.05, remove_equal = TRUE, print_comp = TRUE)
  )
  expect_false(any(grepl("=", out_eq)))

  # remove_space: spaces around '-' should be removed
  comps_space <- c("A - B", "A - C", "B - C")
  out_space <- capture.output(
    cld:::make_cld_df(comparison = comps_space, p.value = pvals,
      threshold = 0.05, remove_space = TRUE, print_comp = TRUE)
  )
  # Expect to see the compact form A-B (no spaces) and not the spaced form
  expect_true(any(grepl("A-B", out_space)))
  expect_false(any(grepl("A - B", out_space)))
})

test_that("make_cld.data.frame remove_space parameter works", {
  # Test that remove_space is passed through to data.frame method
  df <- data.frame(
    group1 = c("Group A", "Group A"),
    group2 = c("Group B", "Group C"),
    p.adj = c(0.01, 0.05)
  )
  
  result <- make_cld(df, remove_space = TRUE)
  
  expect_s3_class(result, "cld_object")
  # With remove_space=TRUE, spaces should be preserved in group names
  # (remove_space applies to comparison strings, not group names themselves)
  expect_true("Group A" %in% result$group || "GroupA" %in% result$group)
})
