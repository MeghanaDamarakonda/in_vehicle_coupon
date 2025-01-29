library(pacman)

pacman::p_load(rio, heatmaply, naniar, tidyverse, miscset, DescTools, reshape2, corrplot, rpart, rpart.plot, randomForest)

dataset <- import("~/Documents/ADA Project/in-vehicle-coupon-recommendation.csv", na.strings = "")

my_global_theme <- theme_light() + theme(
    text = element_text(color = "black"),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12)
  )

theme_set(my_global_theme)

dim(dataset) # 26 attributes, 12684 instances

head(dataset)

tail(dataset)

str(dataset)

column_types <- sapply(dataset, class)

# Gives the count of categorical and numerical columns in the dataset
type_counts <- table(column_types)

type_df <- data.frame(Type = c("Categorical", "Numerical"), Count = as.numeric(type_counts))

# Percentage of type of attributes
plt <- ggplot(type_df, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Types of attributes", y = "Count")

ggsave("attribute_type_plot.png", plt, width = 8, height = 6, dpi = 300)

summary(dataset)

# calculate the number of missing values in every column
colSums(is.na(dataset))

plt <- vis_miss(dataset)

ggsave("missing_val_plot.png", plt, width = 8, height = 6, dpi = 300)

# drop car column from original dataset and categorical dataframe
dataset <- dataset %>% select(-c(car))

# Remove rows with missing values
dataset <- na.omit(dataset)

dim(dataset)

column_types <- sapply(dataset, class)

categorical_col_names <- names(column_types[column_types == "character"])

categorical_df <- dataset[categorical_col_names]

dim(categorical_df) #we have 17 categorical features

# Start uni-variate analysis for categorical features
# Split dataframe into two parts based on unique categories >5 and <=5

cols_with_5_cats <- sapply(categorical_df, function(col) n_distinct(col, na.rm = TRUE) > 5)

col_names_with_5_cats <- names(cols_with_5_cats[cols_with_5_cats])

cat_df_with_5_cols <- categorical_df[col_names_with_5_cats]

cat_df_with_more_cols <- categorical_df %>% select(-c(col_names_with_5_cats))

for (col in names(cat_df_with_more_cols)) {
  plt <- ggplot(cat_df_with_more_cols, aes_string(x = col, fill = col)) +
    geom_bar(alpha = 1) +
    ggtitle(paste("Count Plot for", col)) +
    xlab(col) +
    ylab("Count")
  
  if (col == "coupon") {
    plt <- plt + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  ggsave(paste(col, "_count_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)
  
}

head(cat_df_with_5_cols)

for (col in names(cat_df_with_5_cols)) {
  
  if(col == "occupation") {
    
    plt <- ggplot(cat_df_with_5_cols, aes_string(y = col, fill = col)) +
      geom_bar(alpha = 1) +
      ggtitle(paste("Count Plot for", col)) +
      xlab("Count") +
      ylab(col) +
      theme(legend.position = "none")
    
    ggsave(paste(col, "_count_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)
    
  } else {
    
    plot_df <- cat_df_with_5_cols %>% 
      group_by(.data[[col]]) %>% 
      count() %>% ungroup() %>% mutate(perc = round(n / sum(n), 3))
    
    plt <- ggplot(plot_df, aes(x = "", y=perc, fill=.data[[col]])) + 
      geom_bar(stat="identity", width = 1) +
      geom_text(aes(label=scales::percent(perc)), position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      ggtitle(paste("Pie Plot for", col)) +
      theme_void()
    
    ggsave(paste(col, "_pie_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)
  }
  
}

# Uni-variate analysis for numerical columns
num_df <- select_if(dataset, is.numeric)

dim(num_df)

#histograms for numerical columns
plt <- ggplot(gather(num_df), aes(x = value)) + 
  geom_histogram(bins = 10, fill = "skyblue") + 
  facet_wrap(~key, scales = "free") +
  ggtitle("Frequency distribution of numerical features")

ggsave("hist_plot.png", plt, width = 8, height = 6, dpi = 300)

# Drop toCoupon_GEQ5min feature as there is no variance.
dataset <- dataset %>% select(-c(toCoupon_GEQ5min))

num_df <- num_df %>% select(-c(toCoupon_GEQ5min))

# Correlation between numerical features
corr_df <- as.data.frame(as.table(cor(num_df)))

colnames(corr_df) <- c("Var1", "Var2", "Correlation")

# Create a correlation heatmap using ggplot2
plt <- ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "white", vjust = 1) +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank())

ggsave("corr_plot.png", plt, width = 8, height = 6, dpi = 300)

# Drop direction_opp column
dataset <- dataset %>% select(-c(direction_opp))

# Combine toCoupon_GEQ15min and toCoupon_GEQ25min features
dataset <- dataset %>%
  mutate(drive_time = case_when(
    toCoupon_GEQ15min == 0 & toCoupon_GEQ25min == 0 ~ "Less than 15 min",
    toCoupon_GEQ15min == 1 & toCoupon_GEQ25min == 0 ~ "15-25 min",
    toCoupon_GEQ15min == 1 & toCoupon_GEQ25min == 1 ~ "More than 25 min",
    TRUE ~ "Other"
  ))

# Drop toCoupon_GEQ15min and toCoupon_GEQ25min
dataset <- dataset %>% select(-c(toCoupon_GEQ15min, toCoupon_GEQ25min))

# Change temperature into categorical values
dataset$temperature <- factor(dataset$temperature, labels = c("Low", "Medium", "High"))

# Change other numerical features to categorical features
numerical_columns <- sapply(dataset, is.numeric)

dataset[, numerical_columns] <- lapply(dataset[, numerical_columns], as.character)

# Bi-variate analysis - feature vs target

feature_names <- setdiff(colnames(dataset), "Y")

for (col in feature_names) {
  
  plt <- ggplot(dataset, aes(x = .data[[col]], fill = Y)) +
    geom_bar(position = "stack", stat = "count", width = 0.8) +
    labs(title = paste(col, "with target"), x = col, y = "count") +
    scale_fill_discrete(name = "Target (Y)",
                        labels = c("Reject", "Accept")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(1,1),  # Adjust the position (top-right corner in this case)
      legend.justification = c(1, 1)  # Justification of the legend
    )
  
  if(col != "occupation") {
    plt <- plt + 
      geom_text(stat = "count", aes(label = scales::percent(round(..prop.., 4)), group = Y),
                position = position_stack(vjust = 0.5),   # Adjust the position of the text
                size = 3)
  } else {
    plt <- plt + coord_flip()
  }
  
  ggsave(paste(col, "_target_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)
  
}

# Analysing the type of destination (CoffeHouse) and coupon.
plot_coupon_dest <- function(coupon_type, dest_type){
  
  plt_df <- dataset[dataset$coupon == coupon_type, ]
  
  plt <- ggplot(plt_df, aes(x = .data[[dest_type]], fill = Y)) +
    geom_bar(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = scales::percent(round(..count.. / sum(..count..), 3))), 
              stat = "count", position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
    scale_fill_discrete(name = "Y",
                        labels = c("Reject", "Accept")) +
    labs(title = paste("Comparing per month visit to", dest_type, "based on", coupon_type, "coupon"),
         x = paste("visits in a month"),
         y = "Count")
  
  ggsave(paste(coupon_type, "_", dest_type, "_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)

}

plot_coupon_dest("Coffee House", "CoffeeHouse")

plot_coupon_dest("Carry out & Take away", "CarryAway")


# Multivariate analysis
plot_cat_vs_cat <- function(feat1, feat2, df, rot_angle, size_val, round_val = 4) {
  
  df$feat1_feat2 <- paste(df[[feat1]], df[[feat2]] , sep = " & ")

  plt <- ggplot(df, aes(x = feat1_feat2, fill = Y)) +
    geom_bar(position = position_dodge(width = 0.9)) +
    geom_text(aes(label = scales::percent(round(..count.. / sum(..count..), round_val))), 
              stat = "count", position = position_dodge(width = 0.9), vjust = -0.5, size = size_val) +
    scale_fill_discrete(name = "Y",
                        labels = c("Reject", "Accept")) +
    labs(title = paste("Effect of", feat1, "and", feat2, "on target"),
         x = paste(feat1, "and", feat2, "combined"),
         y = "Count") +
    theme(
      axis.text.x = element_text(angle = rot_angle, hjust = 1)
    )
  
  ggsave(paste(feat1, "_", feat2, "_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)
  
}

df <- data.frame(dataset)

plot_cat_vs_cat("weather", "temperature", df, 45, 2.5)

plot_cat_vs_cat("passanger", "destination", df, 45, 2)

plot_cat_vs_cat("coupon", "time", df, 45, 2.5)

plot_cat_vs_cat("coupon", "expiration", df, 45, 2)

plot_more_cols <- function(feat1, feat2, feat3, df, rot_angle) {
  
  df$feat1_feat2_feat3 <- paste(df[[feat1]], df[[feat2]], df[[feat3]], sep = " & ")
  
  plt <- ggplot(df, aes(x = feat1_feat2_feat3, fill = Y)) +
    geom_bar(position = position_dodge(width = 0.9)) +
    scale_fill_discrete(name = "Y",
                        labels = c("Reject", "Accept")) +
    labs(title = paste("Effect of", feat1, "and", feat2, "and", feat3, "on target"),
         x = paste(feat1, "and", feat2, "and", feat3, "combined"),
         y = "Count") +
    theme(
      axis.text.x = element_text(angle = rot_angle, hjust = 1)
    )
  
  ggsave(paste(feat1, "_", feat2, "_", feat3,"_plot.png", sep = ""), plt, width = 8, height = 6, dpi = 300)

}

plot_more_cols("coupon", "expiration", "drive_time", df, 90)




# Plot Decision Tree

tree_model <- rpart(Y ~., data = dataset, method = "class")

png("decision_tree_plot.png", width = 800, height = 600, units = "px", res = 300)
rpart.plot(tree_model)
dev.off()


# Using random forest algorithm for finding feature importance
dataset$Y <- as.factor(dataset$Y)

rf_model <- randomForest(Y ~ ., data = dataset, ntree = 100)

# Get feature importance
importance_values <- importance(rf_model)

normalized_importance <- scale(importance_values, center = min(importance_values), scale = diff(range(importance_values)))


# Plotting feature importance
importance_df <- data.frame(
  Feature = rownames(normalized_importance),
  Importance = normalized_importance[, 1]
)

importance_df <- importance_df[order(importance_df$Importance, decreasing = FALSE), ]

importance_df$Feature <- factor(importance_df$Feature, levels = importance_df$Feature)

plt <- ggplot(importance_df, aes(x = Importance, y = Feature)) +
  geom_bar(stat = "identity", fill = "#DFB982") +
  labs(title = "Feature Importance using Random Forest",
       x = "Importance",
       y = "Features")


ggsave(paste("feat_imp_plot.png"), plt, width = 8, height = 6, dpi = 300)


