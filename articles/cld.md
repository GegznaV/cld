# Getting Started with cld

``` r
library(cld)
```

## Introduction

The **cld** package provides an easy and consistent way to create
compact letter displays (CLDs) for visualizing results of pairwise
statistical comparisons. Groups sharing the same letter are not
significantly different from each other — a convention widely used in
agricultural, biological, and statistical publications.

## Why Use cld?

🔄 **Universal compatibility** - Works with outputs from base R,
PMCMRplus, rstatix, DescTools, and custom data frames  
🎯 **One function** -
[`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
handles all input types automatically  
📊 **Publication-ready** - Generate clean, professional statistical
grouping labels  
📝 **Informative** - Stores metadata (alpha, method, comparison counts)
for transparency  
🛠️ **Well-tested** - 500+ tests ensuring reliability across all methods

## Interpretation Rules

❌ At least one **shared letter** → Groups are **NOT** significantly
different  
✅ **No shared letters** → Groups **ARE** significantly different

**Examples:**

- ❌ Groups with “c” and “c” share letter “c” → difference is not
  significant
- ❌ Groups with “a” and “ab” share letter “a” → difference is not
  significant
- ❌ Groups “ab” and “bc” share letter “b” → difference is not
  significant
- ❌ Groups “abc” and “bcd” share letters “b”, and “c” → difference is
  not significant
- ✅ Groups with “a” and “c” share no letters → significant difference
- ✅ Groups with “abd” and “ce” share no letters → significant
  difference

## Quick Start

The **cld** package works with various statistical test outputs. Here’s
a simple example:

``` r
# Run a pairwise test
test_result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)

# Generate compact letter display
make_cld(test_result)
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

**Interpretation**:

- Groups sharing at least one letter are **not** significantly different
  (e.g., casein and sunflower both have “a”);
- Groups with no shared letters **are** significantly different (e.g.,
  horsebean “b” and soybean “c”).

## Basic Usage with Base R Tests

### Pairwise Wilcoxon Test

``` r
# Pairwise Wilcoxon rank sum test
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
make_cld(result)
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

### Pairwise t-test

``` r
# Pairwise t-test
result2 <- pairwise.t.test(chickwts$weight, chickwts$feed)
make_cld(result2)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  t tests with pooled SD 
#> 
#>      group cld spaced_cld
#>     casein   a        a__
#>  horsebean   b        _b_
#>    linseed  bc        _bc
#>   meatmeal  ac        a_c
#>    soybean   c        __c
#>  sunflower   a        a__
```

## Understanding the Output

### Structure

The [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
function returns a `cld_object` (enhanced data frame) with:

**Columns:**

- **group** - Names of the groups being compared
- **cld** - Compact letter display (letters only)
- **spaced_cld** - Monospaced version for alignment (underscores replace
  spaces)

**Attributes (metadata):**

- **alpha** - Significance level used
- **method** - Statistical test/method name
- **n_comparisons** - Total number of pairwise comparisons
- **n_significant** - Number of significant differences found

``` r
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
cld_result <- make_cld(result)

# View result
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

# Access metadata
attributes(cld_result)[c("alpha", "method", "n_comparisons", "n_significant")]
#> $alpha
#> [1] 0.05
#> 
#> $method
#> [1] "Wilcoxon rank sum test with continuity correction"
#> 
#> $n_comparisons
#> [1] 15
#> 
#> $n_significant
#> [1] 8
```

## Working with the Output

### Convert to Other Formats

``` r
# Extract as named character vector
letters_only <- as.character(cld_result)
letters_only
#>    casein horsebean   linseed  meatmeal   soybean sunflower 
#>       "a"       "b"      "bc"      "ac"       "c"       "a"

# Convert back to plain data frame (removes metadata)
plain_df <- as.data.frame(cld_result)
class(plain_df)
#> [1] "data.frame"
```

### Adjust Significance Level

``` r
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)

# Standard (alpha = 0.05)
make_cld(result, alpha = 0.05)
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

# More stringent (alpha = 0.01)
make_cld(result, alpha = 0.01)
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
