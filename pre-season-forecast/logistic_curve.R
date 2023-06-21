#' logistic_curve.R
#'
#' Generate the logistic curve for the website using the predicted percentiles
#' from may_forecast.R

library(ggplot2)
library(readr)

predictions <- read_csv("pre-season-forecast/output/predictions.csv")
logi_fun <- function(x, mu, s) { 1 / (1 + exp(-((x - mu)/s))) }

logistic_params <- read_csv("pre-season-forecast/output/logistic_params.csv")
mu <- logistic_params$value[which(logistic_params$param == "mu")]
s <- logistic_params$value[which(logistic_params$param == "s")]

xrange <- -10:50
cpue <- data.frame(day = xrange,
                   date = as.Date(xrange, format = "%j", origin = as.Date("2016-05-31")),
                   pccpue = 100 * logi_fun(xrange, mu, s))

# Write out
write_csv(cpue, file = "pre-season-forecast/output/logistic_curve.csv")

predictions$percent <- c(15, 25, 50)
predictions$label <- paste0(c(15, 25, 50), "%")
predictions$date <- as.Date(predictions$prediction, format = "%j", origin = as.Date("2016-05-31"))

ggplot() +
  geom_bar(data = predictions, aes(date, percent), stat = "identity", fill = NA, color = "black") +
  geom_text(data = predictions, aes(date, percent, label = label), vjust = -1, size = 3) +
  geom_line(data = cpue, aes(date, pccpue)) +
  labs(x = "Date", y = "Cumulative % CPUE") +
  theme_bw()

ggsave("pre-season-forecast/figures/logistic_curve.png", width = 6, height = 3)
