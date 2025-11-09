# Make a compact letter display (CLD) for pair-wise comparisons

Creates a compact letter display for results of pair-wise comparisons
(e.g., ANOVA post-hoc tests, Kruskal-Wallis post-hoc tests, and others).
Groups that share the same letter are not significantly different from
each other. This provides a visual summary of which groups differ
significantly, commonly used in agricultural, biological, and
statistical reporting.

## Usage

``` r
make_cld(obj, ..., alpha = 0.05)

# S3 method for class 'pairwise.htest'
make_cld(obj, ..., alpha = 0.05)

# S3 method for class 'PMCMR'
make_cld(obj, ..., alpha = 0.05)

# S3 method for class 'PostHocTest'
make_cld(obj, ..., alpha = 0.05)

# S3 method for class 'DunnTest'
make_cld(obj, ..., alpha = 0.05)

# S3 method for class 'formula'
make_cld(obj, ..., data = NULL, alpha = 0.05)

# S3 method for class 'matrix'
make_cld(obj, ..., alpha = 0.05)

# S3 method for class 'data.frame'
make_cld(
  obj,
  ...,
  alpha = 0.05,
  gr1_col = "group1",
  gr2_col = "group2",
  p_val_col = "p.adj",
  remove_space = FALSE
)

# S3 method for class 'pairwise_pval_df'
make_cld(obj, ..., alpha = 0.05)
```

## Arguments

- obj:

  Object with pair-wise comparisons (e.g., post-hoc test results).
  Currently supported object classes:

  - `PMCMR` - from packages **PMCMR** and **PMCMRplus**

  - `pairwise.htest` - from base R (e.g., `pairwise.t.test`,
    `pairwise.wilcox.test`)

  - `data.frame` - with comparison results from packages like
    **rstatix** (e.g., `games_howell_test`, `tukey_hsd`). Requires
    `gr1_col`, `gr2_col`, `p_val_col` specification.

  - `PostHocTest` - from package **DescTools** (e.g., `ConoverTest`,
    `DunnettTest`)

  - `DunnTest` - from package **DescTools**

  - `matrix` - symmetric matrices of p-values

  - `formula` - interface for data frames

  - `pairwise_pval_df` - output from
    [`pval_matrix_to_df()`](https://gegznav.github.io/cld/reference/pval_matrix_to_df.md)

- ...:

  Further arguments passed to internal methods. These may include:

  - `reversed` - Logical. If `TRUE`, reverses the letter ordering
    (default: `FALSE`)

  - `swap_compared_names` - Logical. If `TRUE`, swaps group order in
    comparisons (default: `FALSE`)

  - `print_comp` - Logical. If `TRUE`, prints comparison names (default:
    `FALSE`)

  - `sep` - Character. Custom separator for comparison strings (default:
    `"-"`). Use this when your group names contain hyphens. For example,
    use `sep = ":"` or `sep = ";"`. Only applies to single-variable
    formula method. Not needed for two-variable formula. Additional
    cleaning options (default: `TRUE` for most methods):

  - `remove_space` - Removes spaces from comparison strings

  - `remove_equal` - Removes equal signs from comparison strings

  - `swap_colon` - Replaces colons with hyphens (use `FALSE` if using
    `:` as separator)

  - `swap_vs` - Replaces "vs" with hyphens (default: `FALSE`)

- alpha:

  Numeric value between 0 and 1. The significance level (alpha) for
  determining which comparisons are significantly different. Comparisons
  with p-values below this threshold are considered significant. Default
  is 0.05.

- data:

  A data frame with p-values and names of comparisons. This argument is
  required when `obj` is a formula. The data frame should contain at
  least two columns: one for p-values and one for comparison labels. See
  examples for details.

- gr1_col:

  Character string. Name of the column in the data frame containing the
  first group names in each pairwise comparison. Default is `"group1"`.
  Only used for the data.frame method. The function will construct
  comparisons in the format `gr2-gr1`.

- gr2_col:

  Character string. Name of the column in the data frame containing the
  second group names in each pairwise comparison. Default is `"group2"`.
  Only used for the data.frame method.

- p_val_col:

  Character string. Name of the column in the data frame containing the
  p-values for each comparison. Default is `"p.adj"` (adjusted
  p-values). Only used for the data.frame method. Can also be
  `"p_value"` or any other column name containing numeric p-values.

- remove_space:

  Logical. If `TRUE`, removes spaces from comparison strings. Default is
  `FALSE` for the data.frame method to preserve original formatting. Set
  to `TRUE` if your group names contain spaces and you want compact
  comparisons.

- formula:

  An R model [`stats::formula()`](https://rdrr.io/r/stats/formula.html)
  with two possible formats:

  - **Two-variable formula** (recommended): `p_value ~ group1 + group2`
    where `group1` and `group2` are separate columns containing group
    names. This format automatically handles hyphens in group names.

  - **Single-variable formula**: `p_value ~ Comparison` where
    `Comparison` is a column with pre-formatted comparison strings
    (e.g., "A-B", "A-C"). This format has limitations with hyphenated
    group names. Usually used in combination with `data`.

## Value

A data frame of class `c("cld_object", "data.frame")` with three
columns:

- `group` - Character. The names of the groups being compared

- `cld` - Character. The compact letter display for each group. Groups
  sharing at least one letter are not significantly different from each
  other

- `spaced_cld` - Character. A monospaced version of the CLD where spaces
  are replaced with underscores, useful for maintaining alignment in
  output

The rows are ordered according to the group names. Groups with the same
letter(s) do not differ significantly at the specified alpha level.

## Handling Group Names with Hyphens

The underlying multcompView package uses hyphens (`-`) as the default
separator between group names in comparison strings (e.g.,
"GroupA-GroupB"). This creates a conflict when group names themselves
contain hyphens (e.g., "Plant-based", "Treatment-1").

**Automatic Handling:** Most methods (matrix, data.frame,
pairwise.htest, pairwise_pval_df, and others) automatically detect and
handle hyphens in group names by:

1.  Temporarily replacing hyphens with alternative characters
    (underscore, en-dash, etc.)

2.  Processing the comparisons

3.  Restoring the original hyphens in the output

An informational message is shown when this occurs. To suppress it, use
`quiet_hyphen_warning = TRUE`.

**Formula Method Limitation:** The formula method (e.g.,
`make_cld(p_value ~ comparison, data = df)`) has limited support for
group names with hyphens because it receives pre-formatted comparison
strings where the separator hyphens cannot be reliably distinguished
from hyphens within group names.

**Best Practice for Hyphenated Group Names:** Use the data.frame method
with `gr1_col` and `gr2_col` parameters. This method handles hyphens
automatically and seamlessly. For example:

    # Instead of: make_cld(p_value ~ comparison, data = df)
    # Use: make_cld(df, gr1_col = "group1", gr2_col = "group2", p_val_col = "p_value")

**Alternative Workarounds for Formula Method:**

- Convert your data to matrix format (also handles hyphens
  automatically)

- Replace hyphens in group names with underscores before creating
  comparisons

- Use a different separator in comparison strings (e.g., " vs " with
  `swap_vs = TRUE`)

## References

Piepho HP (2004). An algorithm for a letter-based representation of
all-pairwise comparisons. Journal of Computational and Graphical
Statistics, 13(2), 456-466.
[doi:10.1198/1061860043515](https://doi.org/10.1198/1061860043515) ,
available at https://www.tandfonline.com/doi/abs/10.1198/1061860043515

## See also

**Helper Functions:**

- [`pval_matrix_to_df()`](https://gegznav.github.io/cld/reference/pval_matrix_to_df.md)
  for converting p-value matrices to data frames

- [`as_cld()`](https://gegznav.github.io/cld/reference/as_cld.md) for
  converting other formats to cld_object

**Output Methods:**

- [`print.cld_object()`](https://gegznav.github.io/cld/reference/cld_methods.md)
  for displaying CLD results

- [cld_methods](https://gegznav.github.io/cld/reference/cld_methods.md)
  for coercion methods (as.data.frame, as.character, as_tibble)

**Underlying Algorithm:**

- [`multcompView::multcompLetters()`](https://rdrr.io/pkg/multcompView/man/multcompLetters.html)
  for the letter generation algorithm

## Examples

``` r
# Example 1: Using pairwise.htest (Wilcoxon test)
obj1 <- stats::pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
cld::make_cld(obj1)
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

# Example 2: Using pairwise.htest (t-test)
obj2 <- with(OrchardSprays, stats::pairwise.t.test(decrease, treatment))
cld::make_cld(obj2)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  t tests with pooled SD 
#> 
#>  group cld spaced_cld
#>      A   a        a__
#>      B   a        a__
#>      C   a        a__
#>      D  ab        ab_
#>      E  bc        _bc
#>      F   c        __c
#>      G   c        __c
#>      H   c        __c

# Example 3: Using pairwise.htest (proportion test)
# \donttest{
smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
obj3 <- stats::pairwise.prop.test(smokers, patients)
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
cld::make_cld(obj3)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  Pairwise comparison of proportions 
#> 
#>  group cld spaced_cld
#>      1   a          a
#>      2   a          a
#>      3   a          a
#>      4   a          a
# }

# Example 4: Using PMCMR objects
# \donttest{
obj4 <- PMCMRplus::kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
#> Warning: Ties are present. Quantiles were corrected for ties.
cld::make_cld(obj4)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  Conover's all-pairs test 
#> 
#>  group cld spaced_cld
#>      A   a        a__
#>      B   a        a__
#>      C   b        _b_
#>      D   c        __c
#>      E  bc        _bc
#>      F   a        a__
# }

# Example 5: Using DescTools objects
# \donttest{
obj5 <- DescTools::ConoverTest(weight ~ group, data = PlantGrowth)
cld::make_cld(obj5)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  ConoverTest 
#> 
#>  group cld spaced_cld
#>   trt1   a         a_
#>   trt2   b         _b
#>   ctrl  ab         ab
# }

# Example 6: Using rstatix data frames (via data.frame method)
# \donttest{
obj6 <- rstatix::games_howell_test(PlantGrowth, weight ~ group)
cld::make_cld(obj6, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj")
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  data.frame 
#> 
#>  group cld spaced_cld
#>   trt1   a         a_
#>   trt2   b         _b
#>   ctrl  ab         ab
# }

# Example 7: Using formula interface
my_dataframe <- utils::read.table(
  text = c(
    'Comparison     p_value p.adjust
    "EE - GB = 0"        1 1.000000
    "EE - CY = 0" 0.001093 0.003279
    "GB - CY = 0" 0.005477 0.008216'
  ),
  header = TRUE
)
cld::make_cld(p.adjust ~ Comparison, data = my_dataframe)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  formula 
#> 
#>  group cld spaced_cld
#>     EE   a         a_
#>     GB   a         a_
#>    GB0  ab         ab
#>    CY0   b         _b

# Example 8: Using a symmetric matrix of p-values
# Create matrix
m <- c(
  1.00, 0.22, 0.05, 0.00,
  0.22, 1.00, 0.17, 0.01,
  0.05, 0.17, 1.00, 0.22,
  0.00, 0.01, 0.22, 1.00
)
obj8 <- matrix(m, nrow = 4)
rownames(obj8) <- colnames(obj8) <- c("P", "O", "I", "U")
obj8
#>      P    O    I    U
#> P 1.00 0.22 0.05 0.00
#> O 0.22 1.00 0.17 0.01
#> I 0.05 0.17 1.00 0.22
#> U 0.00 0.01 0.22 1.00

# Make CLD
cld::make_cld(obj8)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  matrix 
#> 
#>  group cld spaced_cld
#>      P   a         a_
#>      O   a         a_
#>      I  ab         ab
#>      U   b         _b

# Example 9: Using data.frame method with custom column names
my_data <- data.frame(
  group1 = c("A", "A", "B"),
  group2 = c("B", "C", "C"),
  p.adj = c(0.001, 0.045, 0.892)
)
cld::make_cld(my_data, gr1_col = "group1", gr2_col = "group2", p_val_col = "p.adj")
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  data.frame 
#> 
#>  group cld spaced_cld
#>      B   a         a_
#>      C   a         a_
#>      A   b         _b
```
