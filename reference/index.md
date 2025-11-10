# Package index

## Main Functions

Core functions for creating compact letter displays from statistical
test results. These are the primary user-facing functions.

- [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md) :
  Make a compact letter display (CLD) for pair-wise comparisons

## Helper & Utility Functions

Functions for data conversion, preparation, and creating CLD objects
from other formats.

- [`as_cld()`](https://gegznav.github.io/cld/reference/as_cld.md) :
  Convert objects to cld_object
- [`pval_matrix_to_df()`](https://gegznav.github.io/cld/reference/pval_matrix_to_df.md)
  : Convert matrix with p-values to data frame

## Output Handling & Coercion Methods

Print methods and coercion functions for converting cld_object to other
formats.

- [`print(`*`<cld_object>`*`)`](https://gegznav.github.io/cld/reference/cld_methods.md)
  [`as.data.frame(`*`<cld_object>`*`)`](https://gegznav.github.io/cld/reference/cld_methods.md)
  [`as.character(`*`<cld_object>`*`)`](https://gegznav.github.io/cld/reference/cld_methods.md)
  [`as_tibble(`*`<cld_object>`*`)`](https://gegznav.github.io/cld/reference/cld_methods.md)
  : Print and coercion methods for cld_object
