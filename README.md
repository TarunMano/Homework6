# Homework6: Sparse Numeric Vector Package <a href="https://github.com/YOUR-USERNAME/Homework6/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/YOUR-USERNAME/Homework6/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>

## Overview

**Homework6** is an R package created for **SDS 375** that implements an S4 class  
`sparse_numeric`, a lightweight representation of sparse numeric vectors.  

A `sparse_numeric` object stores only:
- the nonzero **values**
- their **positions**
- the full **length** of the vector  

The package includes:

- S4 class definition + validation
- Coercion to/from standard numeric vectors
- Arithmetic methods:  
  `sparse_add()`, `sparse_sub()`, `sparse_mult()`, `sparse_crossprod()`
- Operator overloading for `+`, `-`, `*`
- Methods for:  
  `mean()`, `norm()`, `standardize()`, `length()`
- A custom `show()` method and a visualization method using `plot()`
- Complete unit test suite with >90% coverage
- pkgdown website with documentation

---

## Installation

You can install the package from GitHub using:

```r
# install.packages("remotes")
remotes::install_github("YOUR-USERNAME/Homework6")
