## tests/testthat/test-sparse_numeric.R

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("valid sparse_numeric object passes validObject", {
  x <- new("sparse_numeric",
           value  = c(1, 2, 3, 1),
           pos    = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("invalid sparse_numeric object fails validObject", {
  x <- new("sparse_numeric",
           value  = c(1, 2, 3, 1),
           pos    = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

test_that("invalid because of duplicate positions", {
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(2L, 2L),
        length = 4L),
    "'pos' has duplicates"
  )
})

test_that("invalid because positions not strictly ascending", {
  expect_error(
    new("sparse_numeric",
        value  = c(1, 2),
        pos    = c(3L, 2L),
        length = 4L),
    "strictly ascending"
  )
})

test_that("check coercion return class", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("coercion back to numeric matches original", {
  x  <- c(0, 0, 0, 1, 2, 0)
  sx <- as(x, "sparse_numeric")
  expect_equal(as(sx, "numeric"), x)
})

test_that("show method exists and runs", {
  expect_no_error(getMethod("show", "sparse_numeric"))
  x <- as(c(0, 0, 1, 0, 2), "sparse_numeric")
  expect_no_error(show(x))
})

test_that("plot method exists", {
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("plot method runs without error", {
  x <- as(c(0, 1, 0, 2, 0), "sparse_numeric")
  y <- as(c(1, 0, 3, 0, 4), "sparse_numeric")
  expect_no_error(plot(x, y))
})

test_that("+ method exists", {
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
})

test_that("- method exists", {
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
})

test_that("* method exists", {
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

test_that("sparse_add generic exists", {
  expect_true(isGeneric("sparse_add"))
})

test_that("sparse_mult generic exists", {
  expect_true(isGeneric("sparse_mult"))
})

test_that("sparse_sub generic exists", {
  expect_true(isGeneric("sparse_sub"))
})

test_that("sparse_crossprod generic exists", {
  expect_true(isGeneric("sparse_crossprod"))
})

test_that("sparse_add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse_mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse_sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse_crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  z <- sparse_add(x, y)
  expect_s4_class(z, "sparse_numeric")
})

test_that("sparse_add basic correctness", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  x      <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y      <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  x      <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
  y      <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("all zero wrong length gives error", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9),  "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("sparse_sub correctness", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  z <- sparse_sub(x, y)
  expect_equal(as(z, "numeric"), c(-1, -1, 0, 1, -2))
})

test_that("sparse_mult correctness", {
  x <- as(c(0, 2, 0, 3, 0), "sparse_numeric")
  y <- as(c(1, 2, 0, 0, 4), "sparse_numeric")
  z <- sparse_mult(x, y)
  expect_equal(as(z, "numeric"), c(0, 4, 0, 0, 0))
})

test_that("sparse_crossprod matches dense inner product", {
  x <- c(0, 2, 0, 3, 0)
  y <- c(1, 2, 0, 0, 4)
  sx <- as(x, "sparse_numeric")
  sy <- as(y, "sparse_numeric")
  expect_equal(sparse_crossprod(sx, sy), sum(x * y))
})

test_that("length method returns underlying length", {
  x <- as(c(0, 1, 0, 0, 2), "sparse_numeric")
  expect_equal(length(x), 5L)
})


test_that("mean for sparse_numeric matches dense mean", {
  set.seed(1)
  x  <- rnorm(10)
  sx <- as(x, "sparse_numeric")
  expect_equal(mean(sx), mean(x))
})

test_that("mean of all-zero sparse vector is zero", {
  x  <- rep(0, 5)
  sx <- as(x, "sparse_numeric")
  expect_equal(mean(sx), 0)
})

test_that("mean of empty sparse vector is NaN", {
  sx <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 0L)
  expect_true(is.nan(mean(sx)))
})


test_that("norm for sparse_numeric matches dense L2 norm", {
  x  <- c(3, 4, 0, 0)
  sx <- as(x, "sparse_numeric")
  expect_equal(norm(sx), sqrt(sum(x^2)))
})

test_that("norm of all-zero sparse vector is zero", {
  x  <- rep(0, 5)
  sx <- as(x, "sparse_numeric")
  expect_equal(norm(sx), 0)
})

test_that("standardize matches dense scale() (mean and sd)", {
  set.seed(2)
  x  <- rnorm(20)
  sx <- as(x, "sparse_numeric")

  zx_dense  <- as.numeric(scale(x))
  zx_sparse <- as(standardize(sx), "numeric")

  expect_equal(mean(zx_sparse), mean(zx_dense), tolerance = 1e-8)
  expect_equal(sd(zx_sparse),   sd(zx_dense),   tolerance = 1e-8)
})

test_that("standardize on all-zero vector returns all zeros", {
  x  <- rep(0, 10)
  sx <- as(x, "sparse_numeric")
  zs <- as(standardize(sx), "numeric")
  expect_equal(zs, rep(0, 10))
})

test_that("standardize on constant non-zero vector returns all zeros", {
  x  <- rep(5, 8)
  sx <- as(x, "sparse_numeric")
  zs <- as(standardize(sx), "numeric")
  expect_equal(zs, rep(0, 8))
})

test_that("standardize on length-1 vector returns zero", {
  x  <- 7
  sx <- as(x, "sparse_numeric")
  zs <- as(standardize(sx), "numeric")
  expect_equal(zs, 0)
})

test_that("standardize on empty vector returns empty sparse", {
  sx <- new("sparse_numeric",
            value  = numeric(),
            pos    = integer(),
            length = 0L)
  zs <- standardize(sx)
  expect_s4_class(zs, "sparse_numeric")
  expect_equal(length(zs), 0L)
})
