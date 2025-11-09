# Convert matrix with p-values to data frame

This function converts a matrix of p-values into a data frame with three
columns: `gr1` (column names), `gr2` (row names), and `p_values` (the
p-values). This is particularly useful when you have a symmetric matrix
of p-values from pairwise comparisons and need to convert it to a
long-format data frame for further analysis or for use with
[`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md).

## Usage

``` r
pval_matrix_to_df(x)
```

## Arguments

- x:

  A numeric matrix containing p-values. If the matrix has no column or
  row names, they will be assigned sequential numbers. The matrix can be
  symmetric (with diagonal and upper/lower triangle) or triangular. `NA`
  values are removed from the output.

## Value

A data frame of class `c("pairwise_pval_df", "data.frame")` with
columns:

- `gr1` - names from the columns of the matrix

- `gr2` - names from the rows of the matrix

- `p_values` - the corresponding p-values from the matrix (excluding
  `NA` values)

## See also

- [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
  for creating compact letter displays from the resulting data frame

- [`as_cld()`](https://gegznav.github.io/cld/reference/as_cld.md) for
  converting other formats to cld_object

Other helper functions:
[`as_cld()`](https://gegznav.github.io/cld/reference/as_cld.md)

Other data conversion functions:
[`as_cld()`](https://gegznav.github.io/cld/reference/as_cld.md)

## Examples

``` r
# Create a matrix of p-values
m <- matrix(c(NA, 0.05, 0.01, 0.05, NA, 0.03, 0.01, 0.03, NA), nrow = 3)
colnames(m) <- c("A", "B", "C")
rownames(m) <- c("A", "B", "C")

# Convert to data frame
cld::pval_matrix_to_df(m)
#>   gr1 gr2 p_values
#> 2   A   B     0.05
#> 3   A   C     0.01
#> 4   B   A     0.05
#> 6   B   C     0.03
#> 7   C   A     0.01
#> 8   C   B     0.03
```
