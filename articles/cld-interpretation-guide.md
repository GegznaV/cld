# Understanding and Interpreting Compact Letter Displays

``` r
library(cld)
```

## What is a Compact Letter Display?

A Compact Letter Display (CLD) visualizes multiple pairwise comparison
results in a compact format by assigning letters to groups:

- **Groups sharing at least one letter** are **NOT significantly
  different**
- **Groups with no shared letters** are **significantly different**

This convention is widely used in agricultural research, biology, and
statistics to present post-hoc test results.

## Basic Example

``` r
# Example with shared letters
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
cld_result <- make_cld(result)
cld_result
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  Wilcoxon rank sum test with continuity correction 
#> 
#>      group cld spaced_cld
#>     casein   a        a__
#>  horsebean   b        _b_
#>    linseed  bc        _bc
#>   meatmeal  ac        a_c
#>    soybean   c        __c
#>  sunflower   a        a__
```

**Interpretation:**

- Groups with shared letters → **not significantly different**, e.g.:
  - `casein` (a) and `sunflower` (a) share letter “a”
  - `linseed` (bc) and `meatmeal` (ac) share letter “c”
  - `horsebean` (b) and `linseed` (bc) share letter “b”
- Groups with no shared letters → **significantly different**, e.g.:
  - `horsebean` (b) and `soybean` (c) share no letters
  - `horsebean` (b) and `sunflower` (a) share no letters
  - `casein` (a) and `horsebean` (b) share no letters

## Common Patterns

**Pattern 1:** All different (a, b, c) → all groups significantly
different

``` r
set.seed(123)
data1 <- data.frame(
  value = c(rnorm(10, 10, 1), rnorm(10, 20, 1), rnorm(10, 30, 1)),
  group = rep(c("Low", "Medium", "High"), each = 10)
)
make_cld(pairwise.t.test(data1$value, data1$group))
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  t tests with pooled SD 
#> 
#>   group cld spaced_cld
#>    High   a        a__
#>     Low   b        _b_
#>  Medium   c        __c
```

**Pattern 2:** One different (a, a, b) → first two groups not different
from each other

``` r
set.seed(455)
data2 <- data.frame(
  value = c(rnorm(10, 10, 1.5), rnorm(10, 10.1, 1.5), rnorm(10, 20, 1.5)),
  group = rep(c("A", "B", "C"), each = 10)
)
make_cld(pairwise.t.test(data2$value, data2$group))
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  t tests with pooled SD 
#> 
#>  group cld spaced_cld
#>      A   a         a_
#>      B   a         a_
#>      C   b         _b
```

**Pattern 3:** Overlapping (a, ab, bc, c) → creates chain of
non-significant differences

``` r
set.seed(889)
data3 <- data.frame(
  value = c(rnorm(10, 10, 3), rnorm(10, 13, 3), rnorm(10, 16, 3), rnorm(10, 19, 3)),
  group = rep(c("G1", "G2", "G3", "G4"), each = 10)
)
make_cld(pairwise.t.test(data3$value, data3$group))
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  t tests with pooled SD 
#> 
#>  group cld spaced_cld
#>     G1   a        a__
#>     G2  ab        ab_
#>     G3  bc        _bc
#>     G4   c        __c
```

## Spaced CLD Format

The `spaced_cld` column aligns letters vertically using underscores
(`_`) for spaces, making patterns easier to visualize in monospaced
fonts.

## Statistical Considerations

### Significance Level (Alpha)

Lower alpha values are more conservative, resulting in fewer significant
differences:

``` r
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
```

``` r
make_cld(result, alpha = 0.05)   # Standard
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  Wilcoxon rank sum test with continuity correction 
#> 
#>      group cld spaced_cld
#>     casein   a        a__
#>  horsebean   b        _b_
#>    linseed  bc        _bc
#>   meatmeal  ac        a_c
#>    soybean   c        __c
#>  sunflower   a        a__
```

``` r
make_cld(result, alpha = 0.01)   # Stringent
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.01 
#> Method:  Wilcoxon rank sum test with continuity correction 
#> 
#>      group cld spaced_cld
#>     casein  ab        ab_
#>  horsebean   c        __c
#>    linseed  ac        a_c
#>   meatmeal  ab        ab_
#>    soybean  ab        ab_
#>  sunflower   b        _b_
```

``` r
make_cld(result, alpha = 0.001)  # Very stringent
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.001 
#> Method:  Wilcoxon rank sum test with continuity correction 
#> 
#>      group cld spaced_cld
#>     casein   a          a
#>  horsebean   a          a
#>    linseed   a          a
#>   meatmeal   a          a
#>    soybean   a          a
#>  sunflower   a          a
```

### Multiple Comparison Adjustment

P-value adjustment methods (Bonferroni, Holm, FDR, etc.) control Type I
error inflation and can significantly impact results. Use the
`p.adjust.method` argument in your pairwise test function.

## Common Misinterpretations

❌ **Wrong:** “Group ‘a’ has higher values than group ‘b’”  
✅ **Correct:** Letters indicate grouping, not magnitude or rank. They
only show which groups differ statistically.

💡 **Tip:** Sort groups by their original means/medians to understand
which groups have higher or lower values.

## Accessing Metadata

``` r
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
cld_result <- make_cld(result)

# Access stored information
attr(cld_result, "alpha")           # Significance level
#> [1] 0.05
attr(cld_result, "method")          # Statistical method
#> [1] "Wilcoxon rank sum test with continuity correction"
attr(cld_result, "n_comparisons")   # Total comparisons
#> [1] 15
attr(cld_result, "n_significant")   # Significant comparisons
#> [1] 8
```

## Quick Reference

| Comparison   | Shared Letters?  | Interpretation              |
|--------------|------------------|-----------------------------|
| “a” vs “a”   | Yes              | Not significantly different |
| “a” vs “b”   | No               | Significantly different     |
| “ab” vs “bc” | Yes (letter “b”) | Not significantly different |
| “ab” vs “cd” | No               | Significantly different     |

## Further Reading

- [`vignette("cld")`](https://gegznav.github.io/cld/articles/cld.md) –
  Basic usage guide
- [`vignette("cld-input-formats")`](https://gegznav.github.io/cld/articles/cld-input-formats.md)
  – Supported input types
- [`vignette("cld-advanced-features")`](https://gegznav.github.io/cld/articles/cld-advanced-features.md)
  – Advanced features and customization
- [`?cld::make_cld`](https://gegznav.github.io/cld/reference/make_cld.md)
  – Function documentation
