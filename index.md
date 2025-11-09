# cld: Compact Letter Display for Statistical Comparisons

> **Simplify your statistical reporting with compact letter displays**

The **cld** package provides an easy and consistent way to create
compact letter displays (CLDs) for visualizing results of pairwise
statistical comparisons. Groups sharing the same letter are not
significantly different from each other — a convention widely used in
agricultural, biological, and statistical publications.

## Why Use cld?

Key Features:

- 🔄 **Universal compatibility** - Works with base R, PMCMRplus,
  rstatix, DescTools, and custom formats
- 🎯 **One function** -
  [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
  handles all input types automatically
- 📊 **Publication-ready** - Clean, professional statistical grouping
  labels
- 📝 **Informative** - Stores metadata (alpha, method, comparison
  counts)
- 🛠️ **Well-tested** - Comprehensive unit tests ensure reliability

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

**Interpretation**:

- Groups sharing at least one letter are **not** significantly different
  (e.g., casein and sunflower both have “a”);
- Groups with no shared letters **are** significantly different (e.g.,
  horsebean “b” and soybean “c”).

## Supported Input Formats

The [`make_cld()`](https://gegznav.github.io/cld/reference/make_cld.md)
function works seamlessly with:

| Input Type             | Example Packages | Function Examples                                                                                                                                  |
|------------------------|------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| `pairwise.htest`       | base R           | [`pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html), [`pairwise.wilcox.test()`](https://rdrr.io/r/stats/pairwise.wilcox.test.html) |
| `PMCMR` / `PMCMRplus`  | PMCMR, PMCMRplus | `kwAllPairsConoverTest()`, `dunnTest()`                                                                                                            |
| `data.frame` (rstatix) | rstatix          | `games_howell_test()`, `tukey_hsd()`                                                                                                               |
| `PostHocTest`          | DescTools        | `ConoverTest()`, `DunnettTest()`                                                                                                                   |
| `matrix`               | Custom           | Symmetric p-value matrices                                                                                                                         |
| `data.frame`           | Custom           | Custom comparison data frames                                                                                                                      |
| `formula`              | Custom           | Formula interface for data frames                                                                                                                  |

## Learn More

📚 **Comprehensive vignettes** (also available on [package
website](https://gegznav.github.io/cld/)):

- [`vignette("cld")`](https://gegznav.github.io/cld/articles/cld.md) -
  Complete introduction with examples
- [`vignette("cld-input-formats")`](https://gegznav.github.io/cld/articles/cld-input-formats.md) -
  Detailed examples for all input types
- [`vignette("cld-interpretation-guide")`](https://gegznav.github.io/cld/articles/cld-interpretation-guide.md) -
  How to correctly interpret CLDs
- [`vignette("cld-advanced-features")`](https://gegznav.github.io/cld/articles/cld-advanced-features.md) -
  Custom parameters and plotting

## Related Packages

| Package                                                         | Purpose               | Relationship to cld            |
|-----------------------------------------------------------------|-----------------------|--------------------------------|
| [multcompView](https://cran.r-project.org/package=multcompView) | CLD algorithm         | Used internally by **cld**     |
| [rcompanion](https://cran.r-project.org/package=rcompanion)     | Statistical functions | Alternative CLD implementation |
| [PMCMRplus](https://cran.r-project.org/package=PMCMRplus)       | Post-hoc tests        | Compatible input for **cld**   |
| [rstatix](https://cran.r-project.org/package=rstatix)           | Tidy statistics       | Compatible input for **cld**   |
| [DescTools](https://cran.r-project.org/package=DescTools)       | Statistical tools     | Compatible input for **cld**   |

## Getting Help

- 📖 Function reference:
  [`?cld::make_cld`](https://gegznav.github.io/cld/reference/make_cld.md)
- 📚 Package website: <https://gegznav.github.io/cld/>
- 🐛 Report bugs: <https://github.com/GegznaV/cld/issues>
- 💬 Ask questions: [GitHub
  Discussions](https://github.com/GegznaV/cld/discussions)

## Citation

To cite the **cld** package in publications:

``` r
citation("cld")
```

## License

GPL-3
