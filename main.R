library(pacman)

pacman::p_load(rio, heatmaply, dplyr, ggplot2, naniar)


dataset <- import("~/Documents/ADA Project/in-vehicle-coupon-recommendation.csv", na.strings = "")

head(dataset)

tail(dataset)

str(dataset)

colSums(is.na(dataset))

vis_miss(dataset)

missing_check <- is.na(dataset)

heatmaply(missing_check,
          col = c("white", "red"),    # Color scheme for missing (white) and non-missing (red) values
          main = "Missing Values Heatmap",  # Title of the heatmap
          xlab = "Columns",           # X-axis label
          ylab = "Rows"  )
