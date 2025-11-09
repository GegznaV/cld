# Print and coercion methods for cld_object

Print and coercion methods for cld_object

## Usage

``` r
# S3 method for class 'cld_object'
print(x, ...)

# S3 method for class 'cld_object'
as.data.frame(x, ...)

# S3 method for class 'cld_object'
as.character(x, ...)

# S3 method for class 'cld_object'
as_tibble(x, ...)
```

## Arguments

- x:

  A `cld_object` to print or convert

- ...:

  Additional arguments passed to print methods

## Value

- `print.cld_object()`: Invisibly returns the input object

- `as.data.frame.cld_object()`: Returns a plain data frame

- `as.character.cld_object()`: Returns a named character vector of
  letters

- `as_tibble.cld_object()`: Returns a tibble

## See also

- [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
  for creating compact letter displays

- [`as_cld()`](https://gegznav.github.io/cld/reference/as_cld.md) for
  converting objects to cld_object

## Examples

``` r
obj <- pairwise.wilcox.test(chickwts$weight, chickwts$feed, exact = FALSE)
result <- make_cld(obj)

# Print method
print(result)
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

# Convert to plain data frame
as.data.frame(result)
#>       group cld spaced_cld
#> 1    casein   a        a__
#> 2 horsebean   b        _b_
#> 3   linseed  bc        _bc
#> 4  meatmeal  ac        a_c
#> 5   soybean   c        __c
#> 6 sunflower   a        a__

# Convert to named character vector
as.character(result)
#>    casein horsebean   linseed  meatmeal   soybean sunflower 
#>       "a"       "b"      "bc"      "ac"       "c"       "a" 

# Convert to tibble
if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::as_tibble(result)
}
#> # A tibble: 6 × 3
#>   group     cld   spaced_cld
#>   <chr>     <chr> <chr>     
#> 1 casein    a     a__       
#> 2 horsebean b     _b_       
#> 3 linseed   bc    _bc       
#> 4 meatmeal  ac    a_c       
#> 5 soybean   c     __c       
#> 6 sunflower a     a__       
```
