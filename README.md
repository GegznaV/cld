
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cld: Compact Letter Display for Statistical Comparisons

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cld)](https://CRAN.R-project.org/package=cld)
[![GitHub
version](https://img.shields.io/badge/GitHub-0.0.0.9000-brightgreen.svg)](https://github.com/GegznaV/cld)
[![R-CMD-check](https://github.com/GegznaV/cld/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GegznaV/cld/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/GegznaV/cld/branch/master/graph/badge.svg)](https://codecov.io/gh/GegznaV/cld)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2025--11--08-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

> **Simplify your statistical reporting with compact letter displays**

The **cld** package provides an easy and consistent way to create
compact letter displays (CLDs) for visualizing results of pairwise
statistical comparisons. Groups sharing the same letter are not
significantly different from each other — a convention widely used in
agricultural, biological, and statistical publications.

## Why Use cld?

✅ **Universal compatibility** - Works with outputs from base R,
PMCMRplus, rstatix, DescTools, and custom data frames  
✅ **Consistent interface** - One function (`make_cld()`) handles all
input types  
✅ **Publication-ready** - Generate clean, professional statistical
grouping labels  
✅ **Well-tested** - 135+ tests ensuring reliability across all
methods  
✅ **Informative output** - Stores metadata (alpha, method, comparison
counts) for transparency

## Installation

Install the stable version from CRAN:

``` r
install.packages("cld")
```

Install the development version from
[GitHub](https://github.com/GegznaV/cld):

``` r
# install.packages("devtools")
devtools::install_github("GegznaV/cld")
```

## Quick Start

The **cld** package works with various statistical test outputs. Here’s
a simple example:

``` r
library(cld)

# Run a pairwise test
test_result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)

# Generate compact letter display
make_cld(test_result)
#>      group cld spaced_cld
#>     casein   a        a__
#>  horsebean   b        _b_
#>    linseed  bc        _bc
#>   meatmeal  ac        a_c
#>    soybean   c        __c
#>  sunflower   a        a__
```

**Interpretation**: Groups with the same letter (e.g., casein and
sunflower both have “a”) are not significantly different. Groups without
shared letters (e.g., horsebean “b” and soybean “c”) are significantly
different.

## Supported Input Formats

The `make_cld()` function works seamlessly with:

| Input Type            | Example Packages | Function Examples                             |
|-----------------------|------------------|-----------------------------------------------|
| `pairwise.htest`      | base R           | `pairwise.t.test()`, `pairwise.wilcox.test()` |
| `PMCMR` / `PMCMRplus` | PMCMR, PMCMRplus | `kwAllPairsConoverTest()`, `dunnTest()`       |
| `posthoc_anova`       | rstatix          | `games_howell_test()`, `tukey_hsd()`          |
| `PostHocTest`         | DescTools        | `ConoverTest()`, `DunnettTest()`              |
| `matrix`              | Custom           | Symmetric p-value matrices                    |
| `data.frame`          | Custom           | Custom comparison data frames                 |
| `formula`             | Custom           | Formula interface for data frames             |

## Examples

### Example 1: Base R Pairwise Tests

``` r
library(cld)

# Pairwise Wilcoxon rank sum test
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
make_cld(result)
#>       group cld spaced_cld
#> 1    casein   a        a__
#> 2 horsebean   b        _b_
#> 3   linseed  bc        _bc
#> 4  meatmeal  ac        a_c
#> 5   soybean   c        __c
#> 6 sunflower   a        a__

# Pairwise t-test
result2 <- pairwise.t.test(chickwts$weight, chickwts$feed)
make_cld(result2, alpha = 0.01)  # More stringent threshold
#>       group cld spaced_cld
#> 1    casein   a        a__
#> 2 horsebean   b        _b_
#> 3   linseed  bc        _bc
#> 4  meatmeal  ac        a_c
#> 5   soybean   c        __c
#> 6 sunflower   a        a__
```

### Example 2: P-value Matrices

``` r
# Create a symmetric matrix of p-values
m <- matrix(c(
  1.00, 0.22, 0.05, 0.00,
  0.22, 1.00, 0.17, 0.01,
  0.05, 0.17, 1.00, 0.22,
  0.00, 0.01, 0.22, 1.00
), nrow = 4)
rownames(m) <- colnames(m) <- c("GroupA", "GroupB", "GroupC", "GroupD")

# Generate CLD
make_cld(m, alpha = 0.05)
#>    group cld spaced_cld
#> 1 GroupA   a         a_
#> 2 GroupB   a         a_
#> 3 GroupC  ab         ab
#> 4 GroupD   b         _b
```

### Example 3: PMCMRplus (Non-parametric Tests)

``` r
library(PMCMRplus)

# Kruskal-Wallis post-hoc test
kw_result <- kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
make_cld(kw_result)
```

### Example 4: rstatix (Tidyverse-friendly)

``` r
library(rstatix)

# Games-Howell test (for unequal variances)
gh_result <- games_howell_test(PlantGrowth, weight ~ group)
make_cld(gh_result)

# Tukey HSD test
tukey_result <- tukey_hsd(PlantGrowth, weight ~ group)
make_cld(tukey_result)
```

### Example 5: Data Frames (Custom Results)

``` r
# Custom comparison results
comparisons <- data.frame(
  group1 = c("Treatment_A", "Treatment_A", "Treatment_B"),
  group2 = c("Treatment_B", "Treatment_C", "Treatment_C"),
  p.adj  = c(0.9, 0.02, 0.03)
)

make_cld(comparisons, alpha = 0.05)
#>         group cld spaced_cld
#> 1 Treatment_B   a         a_
#> 2 Treatment_C   b         _b
#> 3 Treatment_A   a         a_
```

### Example 6: Formula Interface

``` r
# Using formula for data frames with custom column names
my_data <- data.frame(
  Comparison = c("A-B", "A-C", "B-C"),
  p_value = c(0.12, 0.001, 0.045),
  p_adjusted = c(0.18, 0.003, 0.068)
)

make_cld(p_adjusted ~ Comparison, data = my_data)
#>   group cld spaced_cld
#> 1     A   a         a_
#> 2     B  ab         ab
#> 3     C   b         _b
```

## Understanding the Output

### Structure

The `make_cld()` function returns a `cld_object` (enhanced data frame)
with:

**Columns:** - **group** - Names of the groups being compared -
**cld** - Compact letter display (letters only) - **spaced_cld** -
Monospaced version for alignment (underscores replace spaces)

**Attributes (metadata):** - **alpha** - Significance level used -
**method** - Statistical test/method name - **n_comparisons** - Total
number of pairwise comparisons - **n_significant** - Number of
significant differences found

``` r
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
cld_result <- make_cld(result)

# View result
cld_result
#>       group cld spaced_cld
#> 1    casein   a        a__
#> 2 horsebean   b        _b_
#> 3   linseed  bc        _bc
#> 4  meatmeal  ac        a_c
#> 5   soybean   c        __c
#> 6 sunflower   a        a__

# Access metadata
attributes(cld_result)[c("alpha", "method", "n_comparisons", "n_significant")]
#> $<NA>
#> NULL
#> 
#> $<NA>
#> NULL
#> 
#> $<NA>
#> NULL
#> 
#> $<NA>
#> NULL
```

### Interpretation Rules

❌ At least one **shared letter** → Groups are **NOT** significantly
different  
✅ **No shared letters** → Groups **ARE** significantly different

**Example:** - ❌ Groups with “c” and “c” share letter “c” → difference
is not significant - ❌ Groups with “a” and “ab” share letter “a” →
difference is not significant - ❌ Groups “ab” and “bc” share letter “b”
→ difference is not significant - ❌ Groups “abc” and “bcd” share
letters “b”, and “c” → difference is not significant - ✅ Groups with
“a” and “c” share no letters → significantly different - ✅ Groups with
“abd” and “ce” share no letters → significantly different

## Working with the Output

### Convert to Other Formats

``` r
# Extract as named character vector
letters_only <- as.character(cld_result)
letters_only
#> [1] "c(\"casein\", \"horsebean\", \"linseed\", \"meatmeal\", \"soybean\", \"sunflower\")"
#> [2] "c(\"a\", \"b\", \"bc\", \"ac\", \"c\", \"a\")"                                      
#> [3] "c(\"a__\", \"_b_\", \"_bc\", \"a_c\", \"__c\", \"a__\")"

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
#>       group cld spaced_cld
#> 1    casein   a        a__
#> 2 horsebean   b        _b_
#> 3   linseed  bc        _bc
#> 4  meatmeal  ac        a_c
#> 5   soybean   c        __c
#> 6 sunflower   a        a__

# More stringent (alpha = 0.01)
make_cld(result, alpha = 0.01)
#>       group cld spaced_cld
#> 1    casein  ab        ab_
#> 2 horsebean   c        __c
#> 3   linseed  ac        a_c
#> 4  meatmeal  ab        ab_
#> 5   soybean  ab        ab_
#> 6 sunflower   b        _b_
```

## Common Use Cases

### 🌾 Agricultural Studies

Comparing crop yields across different treatments or varieties

``` r
# Example: Fertilizer treatments
fertilizer_test <- pairwise.t.test(yield, treatment, data = crop_data)
make_cld(fertilizer_test)
```

### 🧪 Biological Research

Post-hoc comparisons after ANOVA in experimental biology

``` r
# Example: Drug efficacy study
anova_result <- aov(response ~ drug_type, data = trial_data)
tukey_result <- TukeyHSD(anova_result)
# Convert and create CLD...
```

### 📊 Quality Control

Comparing multiple production methods or batches

``` r
# Example: Manufacturing process comparison
kruskal_result <- kruskal.test(strength ~ method, data = qc_data)
# Follow up with post-hoc test and CLD
```

### 📈 Survey Analysis

Displaying pairwise differences between groups in survey data

``` r
# Example: Satisfaction scores across departments
make_cld(comparison_results, alpha = 0.01)
```

## Advanced Features

### Custom Parameters

``` r
# Reverse letter ordering
make_cld(result, reversed = TRUE)

# Swap comparison name order
make_cld(result, swap_compared_names = TRUE)

# Print comparison names during processing
make_cld(result, print.comp = TRUE)

# Control string cleaning
make_cld(df, remove.space = TRUE, swap.colon = TRUE, remove.equal = TRUE)
```

### Integration with Plotting

``` r
library(ggplot2)

# Get CLD results
cld_result <- make_cld(test_result)

# Merge with summary statistics
plot_data <- merge(summary_stats, cld_result, by.x = "treatment", by.y = "group")

# Create plot with letter annotations
ggplot(plot_data, aes(x = treatment, y = mean)) +
  geom_col() +
  geom_text(aes(label = cld, y = mean + se), vjust = -0.5) +
  theme_minimal()
```

## Related Packages

| Package                                                         | Purpose               | Relationship to cld            |
|-----------------------------------------------------------------|-----------------------|--------------------------------|
| [multcompView](https://cran.r-project.org/package=multcompView) | CLD algorithm         | Used internally by **cld**     |
| [rcompanion](https://cran.r-project.org/package=rcompanion)     | Statistical functions | Alternative CLD implementation |
| [PMCMRplus](https://cran.r-project.org/package=PMCMRplus)       | Post-hoc tests        | Compatible input for **cld**   |
| [rstatix](https://cran.r-project.org/package=rstatix)           | Tidy statistics       | Compatible input for **cld**   |
| [DescTools](https://cran.r-project.org/package=DescTools)       | Statistical tools     | Compatible input for **cld**   |
| [emmeans](https://cran.r-project.org/package=emmeans)           | Marginal means        | Planned future support         |

## Getting Help

### Documentation

- 📖 Function reference: `?cld::make_cld`
- 📚 Package website: <https://gegznav.github.io/cld/>
- 💡 Examples: See function documentation with `example(cld::make_cld)`

### Issues & Contributions

- 🐛 Report bugs: <https://github.com/GegznaV/cld/issues>
- 💬 Ask questions: Open a [GitHub
  Discussion](https://github.com/GegznaV/cld/discussions)
- 🔧 Contribute: Pull requests welcome!

## Citation

To cite the **cld** package in publications:

``` r
citation("cld")
```

## License

GPL-3
