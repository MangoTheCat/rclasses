
#' Create a machine learning classifier
#'
#' @param data_source The data to use to build the classifier.
#'   Either a data frame, or a file name or a URL. The \code{rio}
#'   package is used to read the data from files and URLs, and
#'   the data is converted to a data frame after reading it.
#' @param method Name of the method to use. Currently \code{knn},
#'   the \eqn{k} nearest neighbors classifier, and \code{lm},
#'   linear models, are implemented.
#'
#' @export
#' @importFrom rio import

create_ml <- function(data_source, method) {

  ml <- structure(
    list(
      data = create_base(data_source),
      method = method
    ),
    class = c(paste0("ml_", method), "ml")
  )

  create(ml)
}

#' @export

create <- function(ml, ...) UseMethod("create")

# --------------------------------------------------------------------

#' @export
#' @method print ml

print.ml <- function(x, ...) {
  cat("Machine learning instance:", x$method %||% "unknown method", "\n")
  cat("Data: -----------------------------------------------------\n")
  if (is.null(x$data)) cat("(!) No data\n") else print(head(x$data))
  cat("...\n")
  cat("Parameters: -----------------------------------------------\n")
  print(get_params(x))
  cat("-----------------------------------------------------------\n")
  invisible(x)
}

# --------------------------------------------------------------------

#' @export

`split_data<-` <- function(ml, value) {
  UseMethod("split_data<-")
}

#' @export
#' @method split_data<- ml

`split_data<-.ml` <- function(ml, value) {
  percents <- value
  assert_that(identical(names(percents), c("training", "validation", "test")))
  training <- percents["training"]

  ml$split <- split_data_base(
    ml$data,
    percents["training"],
    percents["validation"],
    percents["test"]
  )
  ml
}

# --------------------------------------------------------------------

# @export

get_data <- function(ml) UseMethod("get_data")

#' @export
#' @method get_data ml

get_data.ml <- function(ml) {
  ml$data
}

# --------------------------------------------------------------------

#' @export

get_params <- function(ml) UseMethod("get_params")

#' @export
#' @method get_params ml

get_params.ml <- function(ml) {
  ml$params
}

#' @export

`params<-` <- function(ml, value) UseMethod("params<-")

#' @export
#' @method params<- ml

`params<-.ml` <- function(ml, value) {
  ml$params <- modifyList(ml$params, value)
  ml
}

# --------------------------------------------------------------------

#' @export

train <- function(ml, ...) UseMethod("train")

#' @export
#' @method train ml

train.ml <- function(ml, ...) {
  stop("Internal error, train method not implemented", call. = FALSE)
}

#' @export

# --------------------------------------------------------------------

evaluate <- function(ml, ...) UseMethod("evaluate")

#' @export
#' @method evaluate ml

evaluate.ml <- function(ml, ...) {
  stop("Internal error, evaluate method not implemented", call. = FALSE)
}

# --------------------------------------------------------------------

#' @export
#' @method predict ml

predict.ml <- function(object, ...) {
  stop("Internal error, predict method not implemented", call. = FALSE)
}

# --------------------------------------------------------------------

#' @export
#' @method plot ml

plot.ml <- function(x, y, ...) {
  stop("Internal error, plot method not implemented", call. = FALSE)
}
