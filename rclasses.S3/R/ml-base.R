
create_base <- function(data_source) {

  if (is.data.frame(data_source)) {
    data <- data_source

  } else if (is.character(data_source) && length(data_source) == 1) {
    data <- import(data_source)
  }

  as.data.frame(data)
}

split_data_base <- function(data, training, validation, test) {

  assert_that(
    is.data.frame(data),
    training + validation + test == 100
  )

  split_sizes <- c(
    training = training,
    validation = validation,
    test = test
  )

  split <- sample(
    c("training", "validation", "test"),
    nrow(data),
    replace = TRUE,
    prob = split_sizes / 100
  )

  list(sizes = split_sizes, split = split)
}
