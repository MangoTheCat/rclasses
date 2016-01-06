
train_base_ml_knn <- function(data, response, k, split) {

  assert_that(
    response %in% names(data),
    !is.null(split),
    is.integerish(k)
  )

  feats <- setdiff(names(data), response)
  training_data <- data[split$split == "training", feats, drop = FALSE]
  validate_data <- data[split$split == "validation", feats, drop = FALSE]
  training_resp <- data[split$split == "training", response]
  validate_resp <- data[split$split == "validation", response]

  k_pred <- lapply(
    k,
    predict_base_ml_knn,
    training_data = training_data,
    training_resp = training_resp,
    predict_data = validate_data
  )

  k_err <- lapply(
    k_pred,
    validate_resp,
    FUN = jaccard_distance
  )

  best_k <- which.min(k_err)

  list(k = best_k, k_err = k_err, err = k_err[best_k], measure = "jaccard")
}

jaccard_distance <- function(s1, s2) {
  assert_that(length(s1) == length(s2))
  s1 <- as.integer(s1)
  s2 <- as.integer(s2)
  1 - sum(s1 == s2) / length(s1)
}

predict_base_ml_knn <- function(training_data, training_resp,
                                predict_data, k) {

  assert_that(
    is.data.frame(training_data),
    is.factor(training_resp) || is.integerish(training_resp),
    length(training_resp) == nrow(training_data),
    is.integerish(k),
    length(k) == 1
  )

  print(k)

  lapply(seq_len(nrow(predict_data)), function(i) {
    ## Search for closest k data points, and take the most common response
    dist <- euclidean_dist(predict_data[i, ], training_data)
    knn <- order(dist)[1:k]
    majority(training_resp[knn])
  })

}

euclidean_dist <- function(d1, d2) {
  sqrt(apply(d2, 1, function(r) sum((r - d1)^2)))
}

majority <- function(x) {
  tab <- tabulate(x)
  mysample(which(max(tab) == tab))
}

mysample <- function(x) {
  if (length(x) == 1) {
    x
  } else {
    sample(x, 1)
  }
}

evaluate_base_ml_knn <- function(data, response, k, split) {

  assert_that(
    is.data.frame(data),
    response %in% names(data),
    is.integerish(k),
    length(k) == 1
  )

  feats <- setdiff(names(data), response)
  training_data <- data[split$split == "training", feats, drop = FALSE]
  test_data <- data[split$split == "test", feats, drop = FALSE]
  training_resp <- data[split$split == "training", response]
  test_resp <- data[split$split == "test", response]

  pred <- predict_base_ml_knn(
    training_data = training_data,
    training_resp = training_resp,
    predict_data = test_data,
    k = k
  )

  err <- jaccard_distance(test_resp, pred)

  list(err = err, measure = "jaccard")
}
