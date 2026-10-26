# Run an expression while hiding progress output printed with cat() or message().
quiet <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(suppressWarnings(expr)))
  out
}

# mcccadata objects used in several test files
mealdrink_data <- function() {
  quiet(create.MCCCAdata(mealDrink[, 1:2], mealDrink[, 3:4]))
}

young_data <- function() {
  quiet(create.MCCCAdata(young[, 1:3], young[, 4:5]))
}
