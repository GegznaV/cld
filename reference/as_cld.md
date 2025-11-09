# Convert objects to cld_object

Generic function to convert various objects to `cld_object` format. This
is useful when you have CLD results from other packages or custom
formats and want to use them with cld's methods.

## Usage

``` r
as_cld(x, ...)

# S3 method for class 'data.frame'
as_cld(x, ...)

# S3 method for class 'character'
as_cld(x, ...)

# S3 method for class 'cld_object'
as_cld(x, ...)
```

## Arguments

- x:

  Object to convert to `cld_object`

- ...:

  Additional arguments passed to methods

## Value

A `cld_object` (data frame with class `c("cld_object", "data.frame")`)
containing three columns:

- `group` - Character. The names of the groups

- `cld` - Character. The compact letter display for each group

- `spaced_cld` - Character. A monospaced version of the CLD where spaces
  are replaced with underscores, useful for maintaining alignment in
  output

## Details

The `as_cld()` function provides a way to convert various formats to the
standard `cld_object` structure. This is particularly useful when:

- Working with CLD results from other packages

- Converting custom formats to use cld's print and coercion methods

- Creating CLD objects programmatically

## See also

- [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
  for creating compact letter displays

- [`pval_matrix_to_df()`](https://gegznav.github.io/cld/reference/pval_matrix_to_df.md)
  for converting p-value matrices

- [cld_methods](https://gegznav.github.io/cld/reference/cld_methods.md)
  for other cld_object methods

Other helper functions:
[`pval_matrix_to_df()`](https://gegznav.github.io/cld/reference/pval_matrix_to_df.md)

Other data conversion functions:
[`pval_matrix_to_df()`](https://gegznav.github.io/cld/reference/pval_matrix_to_df.md)

## Examples

``` r
# Convert from data frame
df <- data.frame(
  group = c("A", "B", "C"),
  cld = c("a", "b", "b")
)
as_cld(df)
#> Compact Letter Display (CLD)
#>  group cld spaced_cld
#>      A   a          a
#>      B   b          b
#>      C   b          b

# Convert from named character vector
letters_vec <- c(A = "a", B = "b", C = "b")
as_cld(letters_vec)
#> Compact Letter Display (CLD)
#>  group cld spaced_cld
#>      A   a          a
#>      B   b          b
#>      C   b          b
```
