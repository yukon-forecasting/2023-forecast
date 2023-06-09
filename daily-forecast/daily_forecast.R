#' daily_forecast.R
#'
#' Code for generating the daily forecast.

library(ggplot2)
library(readr)
library(dplyr)

# Load data
inseason <- read_csv("data/be_drift.csv") %>%
  mutate(ccpue = cumsum(cpue))
logistic_curve <- read_csv("pre-season-forecast/output/logistic_curve.csv")
predictions <- read_csv("pre-season-forecast/output/predictions.csv")

# Calculate estimated pccpue
today <- tail(inseason, n = 1)$day
ccpue <- tail(inseason, n = 1)$ccpue

final_ccpue <- (ccpue / (logistic_curve[logistic_curve$day == today,"pccpue"] / 100))[[1]]

estimated <- inseason %>%
  mutate(pccpue = ccpue / final_ccpue)

# Export estimated pccpues for Chart1
write.table(subset(estimated, day >= 1)$pccpue * 100, row.names = FALSE, col.names = FALSE, file = "daily-forecast/output/estimated.csv")

# Write cumulative CPUEs for Chart 2
write.table(inseason[inseason$day >= 1,"ccpue"], row.names = FALSE, col.names = FALSE, file = "daily-forecast/output/cumulative_cpue.csv")

# Subset the columns for later use
estimated <- estimated %>%
  select(day, pccpue)

# Plot
logistic_curve %>%
  mutate(date = as.Date(date))

predictions$percent <- c(15, 25, 50)
predictions$label <- paste0(c(15, 25, 50), "%")
predictions$date <- as.Date(predictions$prediction, format = "%j", origin = as.Date("2016-05-31"))

estimated$date <- as.Date(estimated$day, format = "%j", origin = as.Date("2016-05-31"))
estimated$pccpue <- estimated$pccpue * 100

logistic_curve$curve <- "Modeled"
estimated$curve <- "Estimated"
combined <- rbind(logistic_curve,
                  estimated)
combined$curve <- ordered(combined$curve, level = c("Modeled", "Estimated"))

# Chart 1

ggplot() +
  geom_line(data = combined, aes(date, pccpue, color = curve), size = 1) +
  geom_point(data = combined, aes(date, pccpue, shape = curve, color = curve)) +
  geom_ribbon(data = subset(combined, curve == "Estimated"), aes(x=date, ymin = 0, ymax = pccpue), fill = "cyan", alpha = 0.5) +
  geom_col(data = predictions, aes(x = date, y = percent), fill = "#4a86e8", width = 1) +
  scale_shape_manual(values = c(NA, 19)) +
  scale_color_manual(values = c("#4a86e8", "cyan")) +
  labs(x = "Date", y = "Percent of Run") +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text.align = 1,
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))

ggsave("daily-forecast/figures/chart_one.png", width = 6, height = 3)


# Chart 2: Finaly CPUE time series
final_cpue <- data.frame(day = inseason$day,
                         estimate = inseason$ccpue / (logistic_curve[logistic_curve$day %in% inseason$day,"pccpue"][[1]] / 100))
final_cpue$date <- as.Date(final_cpue$day, format = "%j", origin = as.Date("2016-05-31"))

ggplot(final_cpue, aes(date, estimate)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Estimated Total CPUE") +
  theme_bw()

ggsave("daily-forecast/figures/final_cpue.png", width = 6, height = 3)

