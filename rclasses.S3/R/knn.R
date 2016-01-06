
#' @noRd
#' @export
#' @examples
#' knn_iris <- create_ml(iris, "knn")
#' split_data(knn_iris) <- c(training = 60, validation = 20, test = 20)
#' params(knn_iris) <- list(response = "Species", k = 1:3)
#' knn_iris_trained <- train(knn_iris)
#' evaluate(knn_iris_trained)
#'
#' cars <- mtcars
#' cars$gear <- as.factor(cars$gear)
#' knn_cars <- create_ml(cars, "knn")
#' split_data(knn_cars) <- c(training = 60, validation = 20, test = 20)
#' params(knn_cars) <- list(response = "gear", k = 1:3)
#' knn_cars_trained <- train(knn_cars)
#' evaluate(knn_cars_trained)

create.ml_knn <- function(ml, ...) {
  ml$params <- list(response = "y", k = 1:10)
  ml
}

#' @export
#' @method train ml_knn

train.ml_knn <- function(ml, ...) {

  res <- train_base_ml_knn(
    data = ml$data,
    response = ml$params$response,
    k = ml$params$k,
    split = ml$split
  )

  ml$trained <- res

  ml
}

#' @export
#' @method evaluate ml_knn

evaluate.ml_knn <- function(ml, ...) {

  assert_that(!is.null(ml$trained), msg = "Predictor is not trained")

  evaluate_base_ml_knn(
    data = ml$data,
    response = ml$params$response,
    k = ml$trained$k,
    split = ml$split
  )
}

#' @export
#' @method predict ml_knn

predict.ml_knn <- function(object, ...) {
  ## TODO
}

#' @export
#' @method plot ml_knn

plot.ml_knn <- function(x, y, ...) {
  ## TODO
}
