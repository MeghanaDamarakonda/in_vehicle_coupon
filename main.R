library(pacman)

pacman::p_load(rio, heatmaply, naniar, tidyverse, miscset, DescTools, reshape2)

dataset <- import("~/Documents/ADA Project/in-vehicle-coupon-recommendation.csv", na.strings = "")

dim(dataset) # 26 attributes, 12684 instances

head(dataset)

tail(dataset)

str(dataset)

column_types <- sapply(dataset, class)

# Gives the count of categorical and numerical columns in the dataset
type_counts <- table(column_types)

type_df <- data.frame(Type = c("Categorical", "Numerical"), Count = as.numeric(type_counts))

# Create a bar plot using ggplot2
plt <- ggplot(type_df, aes(x = Type, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Types of attributes", y = "Count")

ggsave("attribute_type_plot.png", plt, width = 8, height = 6, dpi = 300)

categorical_col_names <- names(column_types[column_types == "character"])

categorical_df <- dataset[categorical_col_names]

dim(categorical_df) #we have 18 categorical features

summary(dataset)

colSums(is.na(dataset)) # calculate the number of missing values in every column

plt <- vis_miss(dataset)

ggsave("missing_val_plot.png", plt, width = 8, height = 6, dpi = 300)


# drop car column from original dataset and categorical dataframe
dataset <- dataset %>% select(-c(car))
categorical_df <- categorical_df %>% select(-c(car))

dim(categorical_df)

# Start univariate analysis for categorical features

# Split dataframe into two parts based on unique categories >10 and <=10

cols_with_5_cats <- sapply(categorical_df, function(col) n_distinct(col, na.rm = TRUE) > 5)

col_names_with_5_cats <- names(cols_with_5_cats[cols_with_5_cats])

cat_df_with_5_cols <- categorical_df[col_names_with_5_cats]

cat_df_with_more_cols <- categorical_df %>% select(-c(col_names_with_5_cats))

for (col in names(cat_df_with_more_cols)) {
  plt <- ggplot(cat_df_with_more_cols, aes_string(x = col, fill = col)) +
    geom_bar(alpha = 1) +
    ggtitle(paste("Count Plot for", col)) +
    xlab(col) +
    ylab("Count") +
    theme_light()
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
      theme_light() + 
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

# Bi-variate analysis for categorical variables


#check for duplicates
distinct(dataset)

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


