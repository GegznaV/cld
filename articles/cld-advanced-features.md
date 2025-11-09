# Advanced Features and Customization

``` r
library(cld)
```

## Custom Parameters

The [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
function provides several parameters for customizing the output and
behavior.

### Reverse Letter Ordering

By default, letters are assigned with “a” representing the group with
the lowest mean/median. You can reverse this:

``` r
result <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)

# Default ordering
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

# Reversed ordering
make_cld(result, reversed = TRUE)
#> Compact Letter Display (CLD)
#> Signif. level (alpha):  0.05 
#> Method:  Wilcoxon rank sum test with continuity correction 
#> 
#>      group cld spaced_cld
#>     casein   c        __c
#>  horsebean   b        _b_
#>    linseed  ab        ab_
#>   meatmeal  ac        a_c
#>    soybean   a        a__
#>  sunflower   c        __c
```

### Swap Comparison Names

Sometimes the order of group names in comparisons needs to be swapped:

``` r
# Swap the order of compared groups
make_cld(result, swap_compared_names = TRUE)
```

### Print Comparisons

For debugging or understanding the comparison process:

``` r
# Print comparison names during processing
make_cld(result, print_comp = TRUE)
```

### String Cleaning Options

Control how comparison strings are processed:

``` r
# Remove spaces from comparison strings
make_cld(df, remove_space = TRUE)

# Replace colons with hyphens
make_cld(df, swap_colon = TRUE)

# Remove equal signs
make_cld(df, remove_equal = TRUE)

# Replace "vs" with hyphens
make_cld(df, swap_vs = TRUE)
```
