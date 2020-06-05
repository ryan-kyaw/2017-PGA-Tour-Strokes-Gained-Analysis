# Using strokes gained data to predict players scores 

library(tidyverse)

df_2017 <- read.csv("2017.csv")

# Train/Test Split 

df_2017$id <- 1:nrow(df_2017)
df_train_2017 <- df_2017 %>% sample_frac(0.7)
df_test_2017 <- df_2017 %>% anti_join(df_train_2017)
df_train_2017$id <- NULL
df_test_2017$id <- NULL
write.csv(df_test_2017, "test.csv", row.names = FALSE)
rm(df_test_2017)

# Create the model using the Train data 

train_model <- lm(AVG_SCORE ~ SG_PUTTING_PER_ROUND + SG.OTT + SG.APR + SG.ARG,
                  data = df_train_2017)
summary(train_model)

# Load the test data and use the model to predict on the test data 

df_test_2017 <- read.csv("test.csv")
avg_score_actual <- df_test_2017$AVG_SCORE
avg_score_pred <- predict(train_model, df_test_2017)
pred_vs_actual_scores <- data.frame(cbind(actual = avg_score_actual, predicted = avg_score_pred))

# Calculate the mean absolute percentage error
MAPE_home <- mean(abs((pred_vs_actual_scores$predicted - pred_vs_actual_scores$actual))
                  /pred_vs_actual_scores$actual)
