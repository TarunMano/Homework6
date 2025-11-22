#' Class "sparse_numeric"
#'
#' S4 class for representing sparse numeric vectors by storing only
#' non-zero values and their positions.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos   Integer vector of 1-based positions of the non-zero values.
#' @slot length Single integer giving the full length of the underlying vector.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value  = "numeric",
    pos    = "integer",
    length = "integer"
  )
)

setValidity("sparse_numeric", function(object) {
  errs <- character(0)

  if (!is.numeric(object@value))
    errs <- c(errs, "'value' must be numeric")
  if (!is.integer(object@pos))
    errs <- c(errs, "'pos' must be integer")
  if (!is.integer(object@length) || length(object@length) != 1L)
    errs <- c(errs, "'length' must be a single integer")

  n <- if (length(object@length)) object@length[1L] else NA_integer_

  if (length(object@value) != length(object@pos))
    errs <- c(errs, "'value' and 'pos' lengths must match")

  if (length(object@pos)) {
    if (any(is.na(object@pos)))
      errs <- c(errs, "'pos' has NA")
    if (any(object@pos < 1L | object@pos > n))
      errs <- c(errs, "'pos' outside 1..length")
    if (anyDuplicated(object@pos))
      errs <- c(errs, "'pos' has duplicates")
    if (!all(diff(object@pos) > 0L))
      errs <- c(errs, "'pos' must be strictly ascending")
  }

  if (!is.na(n) && n < 0L)
    errs <- c(errs, "'length' must be >= 0")

  if (length(errs)) errs else TRUE
})

setAs("numeric", "sparse_numeric", function(from) {
  if (!length(from)) {
    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = 0L))
  }
  nz <- which(from != 0)
  new("sparse_numeric",
      value  = as.numeric(from[nz]),
      pos    = as.integer(nz),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  if (length(from@pos)) out[from@pos] <- from@value
  out
})


#' @rdname sparse_numeric-methods
#' @export
setMethod("show", "sparse_numeric", function(object) {
  nnz <- length(object@pos)
  cat("sparse_numeric (length =", object@length, ", nnz =", nnz, ")\n")
  if (nnz) {
    k  <- min(nnz, 8L)
    df <- data.frame(
      pos   = object@pos[seq_len(k)],
      value = object@value[seq_len(k)]
    )
    print(df, row.names = FALSE)
    if (nnz > k) cat("  ...", nnz - k, "more non-zeros\n")
  } else {
    cat("  <all zeros>\n")
  }
})

# helper: check same length ----------------------------------------------

.same_len <- function(x, y) {
  if (x@length != y@length)
    stop("Sparse vectors must have the same 'length'.")
  invisible(TRUE)
}


#' Add two sparse_numeric vectors
#'
#' Adds two \code{sparse_numeric} vectors of the same length and
#' returns their sum, represented as a \code{sparse_numeric}.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#'
#' @return A \code{sparse_numeric} object of the same length.
#' @export
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

#' @rdname sparse_numeric-methods
#' @export
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .same_len(x, y)
  if (!length(x@pos))
    return(new("sparse_numeric", value = y@value, pos = y@pos, length = y@length))
  if (!length(y@pos))
    return(x)

  all_pos <- sort(unique(c(x@pos, y@pos)))

  vx <- numeric(length(all_pos))
  vy <- numeric(length(all_pos))

  vx[match(x@pos, all_pos)] <- x@value
  vy[match(y@pos, all_pos)] <- y@value

  v    <- vx + vy
  keep <- v != 0

  new("sparse_numeric",
      value  = v[keep],
      pos    = as.integer(all_pos[keep]),
      length = x@length)
})


#' Subtract two sparse_numeric vectors
#'
#' Computes \code{x - y} for two \code{sparse_numeric} vectors of the
#' same length.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} object of the same length.
#' @export
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

#' @rdname sparse_numeric-methods
#' @export
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .same_len(x, y)
  if (!length(y@pos))
    return(x)
  if (!length(x@pos))
    return(new("sparse_numeric",
               value  = -y@value,
               pos    = y@pos,
               length = y@length))

  all_pos <- sort(unique(c(x@pos, y@pos)))

  vx <- numeric(length(all_pos))
  vy <- numeric(length(all_pos))

  vx[match(x@pos, all_pos)] <- x@value
  vy[match(y@pos, all_pos)] <- y@value

  v    <- vx - vy
  keep <- v != 0

  new("sparse_numeric",
      value  = v[keep],
      pos    = as.integer(all_pos[keep]),
      length = x@length)
})


#' Elementwise multiply two sparse_numeric vectors
#'
#' Computes the elementwise (Hadamard) product \code{x * y} for two
#' \code{sparse_numeric} vectors of the same length.
#'
#' @inheritParams sparse_add
#'
#' @return A \code{sparse_numeric} object of the same length.
#' @export
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' @rdname sparse_numeric-methods
#' @export
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .same_len(x, y)
  if (!length(x@pos) || !length(y@pos))
    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = x@length))

  both <- intersect(x@pos, y@pos)
  if (!length(both))
    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = x@length))

  ix <- match(both, x@pos)
  iy <- match(both, y@pos)
  v  <- x@value[ix] * y@value[iy]

  keep <- v != 0

  new("sparse_numeric",
      value  = v[keep],
      pos    = as.integer(both[keep]),
      length = x@length)
})


#' Inner product of two sparse_numeric vectors
#'
#' Computes the standard Euclidean inner product between two
#' \code{sparse_numeric} vectors of the same length.
#'
#' @inheritParams sparse_add
#'
#' @return A numeric scalar giving the inner product.
#' @export
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

#' @rdname sparse_numeric-methods
#' @export
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  .same_len(x, y)
  if (!length(x@pos) || !length(y@pos)) return(0.0)
  both <- intersect(x@pos, y@pos)
  if (!length(both)) return(0.0)
  ix <- match(both, x@pos)
  iy <- match(both, y@pos)
  sum(x@value[ix] * y@value[iy])
})



#' @rdname sparse_numeric-methods
#' @export
setMethod("+", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' @rdname sparse_numeric-methods
#' @export
setMethod("-", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' @rdname sparse_numeric-methods
#' @export
setMethod("*", c("sparse_numeric", "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))


#' Mean of a sparse_numeric vector
#'
#' Computes the mean of a \code{sparse_numeric} vector, counting all
#' implicit zeros.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return Numeric scalar mean.
#' @export
setMethod("mean", "sparse_numeric", function(x, ...) {
  n <- x@length
  if (n == 0L) {
    return(NaN)  # matches mean(numeric(0))
  }
  if (!length(x@value)) {
    return(0)
  }
  sum(x@value) / n
})


#' Euclidean norm of a sparse_numeric vector
#'
#' Computes the Euclidean (L2) norm of a \code{sparse_numeric} vector.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return Numeric scalar norm.
#' @export
setGeneric("norm", function(x, ...) standardGeneric("norm"))

#' @rdname sparse_numeric-methods
#' @export
setMethod("norm", "sparse_numeric", function(x, ...) {
  if (!length(x@value)) {
    return(0)
  }
  sqrt(sum(x@value^2))
})


#' Standardize a sparse_numeric vector
#'
#' Centers and scales a \code{sparse_numeric} vector by subtracting the
#' mean (over all entries, including zeros) and dividing by the sample
#' standard deviation. If the standard deviation is zero or not finite,
#' a vector of zeros is returned.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Ignored.
#'
#' @return A standardized \code{sparse_numeric} object of the same length.
#' @export
setGeneric("standardize", function(x, ...) standardGeneric("standardize"))

#' @rdname sparse_numeric-methods
#' @export
setMethod("standardize", "sparse_numeric", function(x, ...) {
  n <- x@length

  # empty vector
  if (n == 0L) {
    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = 0L))
  }

  k <- length(x@pos)
  v <- x@value


  if (!k) {
    m <- 0
  } else {
    m <- sum(v) / n
  }


  if (n < 2L) {
    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = n))
  }


  if (k) {
    dev_nz <- v - m
    ssd    <- sum(dev_nz^2) + (n - k) * m^2
  } else {
    ssd <- 0
  }

  var <- ssd / (n - 1L)
  sd  <- sqrt(var)

  if (!is.finite(sd) || sd == 0) {

    return(new("sparse_numeric",
               value  = numeric(),
               pos    = integer(),
               length = n))
  }

  v_std <- (v - m) / sd
  z0 <- (-m) / sd

  if (z0 == 0) {
    keep <- v_std != 0
    return(new("sparse_numeric",
               value  = v_std[keep],
               pos    = x@pos[keep],
               length = n))
  }

  val_all <- rep(z0, n)
  if (k) {
    val_all[x@pos] <- v_std
  }
  nz_idx <- which(val_all != 0)

  new("sparse_numeric",
      value  = as.numeric(val_all[nz_idx]),
      pos    = as.integer(nz_idx),
      length = as.integer(n))
})


#' @rdname sparse_numeric-methods
#' @export
setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y, ...) {
  .same_len(x, y)

  xmin <- 1
  xmax <- max(1L, x@length)

  vals <- c(0, x@value, y@value)
  ymin <- min(vals)
  ymax <- max(vals)
  if (ymin == ymax) {
    ymin <- ymin - 1
    ymax <- ymax + 1
  }

  plot(NA, xlim = c(xmin, xmax), ylim = c(ymin, ymax),
       xlab = "Position", ylab = "Value",
       main = "Sparse non-zeros", ...)

  if (length(x@pos)) points(x@pos, x@value, pch = 16)
  if (length(y@pos)) points(y@pos, y@value, pch = 1)

  ov <- intersect(x@pos, y@pos)
  if (length(ov)) {
    xv <- x@value[match(ov, x@pos)]
    yv <- y@value[match(ov, y@pos)]
    segments(ov, xv, ov, yv, lty = 2)
  }

  legend("topleft", bty = "n",
         legend = c("x (filled)", "y (open)", "overlap"),
         pch    = c(16, 1, NA),
         lty    = c(NA, NA, 2))
})


#' @rdname sparse_numeric-methods
#' @export
setMethod("length", "sparse_numeric", function(x) x@length)
