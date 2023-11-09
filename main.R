library(pacman)

pacman::p_load(rio, heatmaply, dplyr, ggplot2, naniar, tidyverse, miscset, DescTools)

dataset <- import("~/Documents/ADA Project/in-vehicle-coupon-recommendation.csv", na.strings = "")

dim(dataset)

head(dataset)

tail(dataset)

dim(dataset)

str(dataset)

summary(dataset)

colSums(is.na(dataset)) # calculate the number of missing values in every column

vis_miss(dataset)

# drop car column
drop <- c("car")
dataset = dataset[, !names(dataset) %in% drop]
print(dataset)

dim(dataset)

#check for dulpicates
dataset <- distinct(dataset)

dim(dataset)

#mode_val <- Mode(dataset$CarryAway, na.rm = TRUE)
#print(mode_val)

# Create a bar plot
barplot(table(dataset$Y), main = "Target Class Distribution")

# fill the missing values in columns using mode
Mode <- function(a) {
  u <- unique(a)
  u[which.max(tabulate(match(a, u)))]
}

dataset[is.na(dataset)] <- Mode(dataset)
dataset

#check if null values are replaced by mode
colSums(is.na(dataset)) 

#check for class imbalance in the dataset
#convert Y variable into factor variable
dataset$Y <- as.factor(dataset$Y)

barplot(prop.table(table(dataset$Y)),
        ylim = c(0, 0.7),
        main = "Class Distribution")

#univariate analysis
# Subset numeric columns with dplyr
num_cols <- select_if(dataset, is.numeric)

num_cols 

dim(num_cols)

#histograms for numerical columns
num_cols %>% gather() %>% head()

ggplot(gather(num_cols), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

# remove numerical columns from dataset to get categorical columns
cat_cols = dplyr::select(dataset, -c('direction_opp', 'direction_same', 'has_children',
                                     'temperature', 'toCoupon_GEQ15min', 'toCoupon_GEQ25min',
                                     'toCoupon_GEQ5min','Y'))
dim(cat_cols)
