library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

product_data <- read_csv("/Users/rcm/Documents/CapstoneProject2/ecommerce_product_dataset.csv")
head(product_data)

summary(product_data)

df_cleaned <- na.omit(product_data)
View(df_cleaned)

ggplot(df_cleaned, aes(x = Rating)) +
  geom_histogram(binwidth = 0.3, fill = 'blue', color = 'black') +
  ggtitle('Distribution of Ratings') +
  xlab('Rating') +
  ylab('Frequency')

ggplot(df_cleaned, aes(x = Price)) +
  geom_histogram(binwidth = 50, fill = 'green', color = 'black') +
  ggtitle('Distribution of Prices') +
  xlab('Price') +
  ylab('Frequency')

ggplot(df_cleaned, aes(x = factor(Rating), y = NumReviews)) +
  geom_boxplot(fill = 'orange', color = 'black', alpha = 1) +
  ggtitle('Boxplot of Number of Reviews by Product Rating') +
  xlab('Rating') +
  ylab('Number of Reviews') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

ggplot(df_cleaned, aes(x = factor(Rating), y = NumReviews)) +
  geom_violin(fill = 'skyblue', color = 'black', alpha = 1) +
  ggtitle('Violin Plot of Number of Reviews by Product Rating') +
  xlab('Rating') +
  ylab('Number of Reviews') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

ggplot(df_cleaned, aes(x = Category, y = Rating)) +
  geom_boxplot(fill = 'purple', color = 'black', alpha = 1) +
  ggtitle('Ratings by Category') +
  xlab('Category') +
  ylab('Rating') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df_cleaned, aes(x = Category, y = NumReviews)) +
  geom_boxplot(fill = 'red', color = 'black', alpha = 1) +
  ggtitle('Number of Reviews by Category') +
  xlab('Category') +
  ylab('Number of Reviews') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

df_cleaned$DateAdded <- as.Date(df_cleaned$DateAdded)
monthly_reviews <- df_cleaned %>%
  mutate(month = floor_date(DateAdded, 'month')) %>%
  group_by(month) %>%
  summarise(NumReviews = sum(NumReviews))

ggplot(monthly_reviews, aes(x = month, y = NumReviews)) +
  geom_line() +
  ggtitle('Number of Reviews Over Time') +
  xlab('Date') +
  ylab('Number of Reviews')

seasonal_reviews <- df_cleaned %>%
  mutate(month = month(DateAdded, label = TRUE)) %>%
  group_by(month) %>%
  summarise(NumReviews = sum(NumReviews))

ggplot(seasonal_reviews, aes(x = month, y = NumReviews)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  ggtitle('Seasonal Trends in Number of Reviews') +
  xlab('Month') +
  ylab('Number of Reviews')

monthly_prices <- df_cleaned %>%
  mutate(month = floor_date(DateAdded, 'month')) %>%
  group_by(month) %>%
  summarise(avg_price = mean(Price))

ggplot(monthly_prices, aes(x = month, y = avg_price)) +
  geom_line() +
  ggtitle('Average Price Over Time') +
  xlab('Date') +
  ylab('Average Price')

numeric_df <- df_cleaned[sapply(df_cleaned, is.numeric)]
head(numeric_df)
correlation_matrix <- cor(numeric_df, use = 'complete.obs')
print(correlation_matrix)
install.packages('reshape2')
library(reshape2)

correlation_melted <- melt(correlation_matrix)
correlation_melted <- correlation_melted[correlation_melted$Var1 != correlation_melted$Var2, ]

create_bar_plot <- function(variable, correlation_data) {
  variable_correlations <- correlation_data[correlation_data$Var1 == variable, ]
  ggplot(variable_correlations, aes(x = reorder(Var2, value), y = value)) +
    geom_bar(stat = 'identity', fill = 'blue') +
    coord_flip() +
    ggtitle(paste('Pearson Correlation Coefficients with', variable)) +
    xlab('Variables') +
    ylab('Pearson Correlation Coefficient') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

variables <- colnames(correlation_matrix)

for (variable in variables) {
  print(create_bar_plot(variable, correlation_melted))
}

perform_correlation_tests <- function(df_cleaned) {
  results <- data.frame(variable1 = character(),
                        variable2 = character(),
                        correlation = numeric(),
                        p_value = numeric(),
                        stringsAsFactors = FALSE)
  
  for(i in 1:(ncol(df_cleaned) - 1)) {
    for (j in (i + 1):ncol(df_cleaned)) {
      var1 <- colnames(df_cleaned)[i]
      var2 <- colnames(df_cleaned)[j]
      test_result <- cor.test(df_cleaned[[var1]], df_cleaned[[var2]], method = 'pearson')
      results <- rbind(results, data.frame(variable1 = var1,
                                           variable2 = var2,
                                           correlation = test_result$estimate,
                                           p_value = test_result$p.value,
                                           stringsAsFactors = FALSE))
    }
  }
  
  return(results)
}

correlation_test_results <- perform_correlation_tests(numeric_df)
print(correlation_test_results)
