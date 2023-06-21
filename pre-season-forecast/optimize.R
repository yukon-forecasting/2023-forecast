library(readr)

predictions <- read_csv("pre-season-forecast/output/predictions.csv")
predictions <- predictions$prediction

logi_fun <- function(x, mu, s) { 1 / (1 + exp(-((x - mu)/s))) }

opt_fun <- function(params) {
  residuals <- c(
    0.15 - logi_fun(predictions[1], params[1], params[2]),
    0.25 - logi_fun(predictions[2], params[1], params[2]),
    0.50 - logi_fun(predictions[3], params[1], params[2])
  )

  sum(residuals^2)
}

optim_result <- optim(c(mu=20, s=5), opt_fun)
optim_result

logistic_params <- data.frame(param = c("mu", "s"), value = optim_result$par)

write_csv(logistic_params, "pre-season-forecast/output/logistic_params.csv")
