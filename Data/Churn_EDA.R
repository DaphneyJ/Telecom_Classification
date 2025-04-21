


# Clear variables in memory
rm(list=ls())


# Load the leaps package
# Load required libraries
library(dplyr)
library(forcats)
library(leaps)
library(ggplot2)
library(corrplot)
library(MASS)
library(lars)
library(pls)
library(knitr)
library(car)
library(pastecs)
library(readxl)
library(vip)
library(tidyr)
library(gridExtra)
library(caret)
library(GGally)
library(reshape2)
library(xtable)
library(psych)
library(glmnet)
library(pROC)
library(randomForest)
library(gbm)
library(xgboost)
library(cluster)
library(factoextra)
library(reshape2)
library(scales)
library(ggbeeswarm)
library(patchwork) 


### Load dataset 
status <- read.csv("C:/R/Telco_customer_churn_status.csv")
demographics <- read.csv("C:/R/Telco_customer_churn_demographics.csv")
location <- read.csv("C:/R/Telco_customer_churn_location.csv")
services <- read.csv("C:/R/Telco_customer_churn_services.csv")
str(status)
str(demographics)
str(location )
str(services)


data <- status %>%
  left_join(demographics, by = "Customer.ID") %>%
  left_join(location, by = "Customer.ID") %>%
  left_join(services, by = "Customer.ID")

selected_vars <- c("Satisfaction.Score",
                   "Churn.Value",
                   "CLTV",
                   "Churn.Category",
                   "Churn.Reason",
                   "Gender",
                   "Age",
                   "Under.30",
                   "Senior.Citizen",
                   "Married",
                   "Dependents",
                   "Number.of.Dependents",
                   "Latitude",
                   "Longitude",
                   "Referred.a.Friend",
                   "Number.of.Referrals",
                   "Tenure.in.Months",
                   "Offer",
                   "Phone.Service",
                   "Avg.Monthly.Long.Distance.Charges",
                   "Multiple.Lines",
                   "Internet.Service",
                   "Internet.Type",
                   "Avg.Monthly.GB.Download",
                   "Online.Security",
                   "Online.Backup",
                   "Device.Protection.Plan",
                   "Premium.Tech.Support",
                   "Streaming.TV",
                   "Streaming.Movies",
                   "Streaming.Music",
                   "Unlimited.Data",
                   "Contract",
                   "Paperless.Billing",
                   "Payment.Method",
                   "Monthly.Charge",
                   "Total.Charges",
                   "Total.Refunds",
                   "Total.Extra.Data.Charges",
                   "Total.Long.Distance.Charges",
                   "Total.Revenue")

churndata <- data %>%
  dplyr::select(all_of(selected_vars)) %>%
  na.omit() %>%
  mutate(Churn = as.factor(Churn.Value)) %>%
  dplyr::select(-Churn.Value)

str(churndata)
head(churndata)
summary(churndata)

########
##EDA###
########

churndata$Churn <- as.numeric(as.character(churndata$Churn))

sd(churndata$Churn, na.rm = TRUE)  

### find numeric columns
numeric_cols <- churndata %>%
  select_if(is.numeric) %>%
  colnames()

print(numeric_cols)

### correlation matrix chart
cor_matrix <- cor(churndata[numeric_cols], use = "complete.obs")
print(cor_matrix)

### Correlation Scatter plot Matrix
pairs.panels(
  churndata[numeric_cols],  # Use base R bracket notation
  method = "pearson", 
  hist.col = "skyblue",  
  density = TRUE,
  ellipses = TRUE,   
  lm = TRUE,  
  cex.cor = 0.7,      
  digits = 7    
)

### correlation matrix plot
corrplot::corrplot(
  cor_matrix, 
  method = "pie",       
  order = "hclust",     
  type = "upper",      
  tl.col = "black",     
  tl.srt = 45,          
  addCoef.col = "black",
  number.digits = 5,
  tl.cex = 0.6,         
  number.cex = 0.3,     
  mar = c(2, 2, 2, 2)  
)


###Select top 10 important numeric variables
key_vars <- c("Satisfaction.Score", "Tenure.in.Months", "Monthly.Charge",
              "Number.of.Referrals", "Total.Extra.Data.Charges",
              "Avg.Monthly.GB.Download", "Total.Refunds", "CLTV",
              "Total.Charges", "Total.Long.Distance.Charges")

###key variables + Churn 
churn_corr_data <- churndata[, key_vars]
churn_corr_data$Churn <- as.numeric(as.character(churndata$Churn))

###key variable correlation matrix
cor_matrix <- cor(churn_corr_data, use = "complete.obs")

corrplot::corrplot(
  cor_matrix,
  method = "pie",
  order = "hclust",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.digits = 5,
  tl.cex = 0.6,
  number.cex = 0.3,
  mar = c(2, 2, 2, 2)
)


#### variable types
summary_types <- sapply(churndata, class)
table(summary_types)
categorical_vars <- names(churndata)[sapply(churndata, function(x) is.factor(x) || is.character(x))]
numeric_vars <- names(churndata)[sapply(churndata, is.numeric)]

print(categorical_vars)
print(numeric_vars)

numeric_vars <- sapply(churndata, is.numeric)

normality_results <- sapply(names(churndata)[numeric_vars], function(var) {
  x <- na.omit(churndata[[var]])
  if (length(x) >= 3) {
    x_sample <- if (length(x) > 5000) sample(x, 5000) else x
    return(shapiro.test(x_sample)$p.value)
  } else {
    return(NA)
  }
})

##### test for normality of variables (p < 0.05)
print(normality_results[normality_results < 0.05])

all_vars <- setdiff(names(churndata), "Churn")

# Run chi-square tests for categorical variables and t-tests for numeric variables
chi_square_results <- lapply(all_vars, function(var) {
  
  if (is.factor(churndata[[var]]) || is.character(churndata[[var]])) {
    table_data <- table(as.factor(churndata[[var]]), churndata$Churn)  
    
    if (all(table_data > 0)) {  
      test_result <- chisq.test(table_data)
      return(data.frame(Variable = var, P_Value = test_result$p.value))
    }
    
  } else {
    if (length(unique(churndata[[var]][churndata$Churn == 0])) > 1 && 
        length(unique(churndata[[var]][churndata$Churn == 1])) > 1) {
      test_result <- t.test(churndata[[var]] ~ churndata$Churn)
      return(data.frame(Variable = var, P_Value = test_result$p.value))
    }
  }
  
  return(NULL) 
})


chi_square_results <- Filter(Negate(is.null), chi_square_results)

if (length(chi_square_results) > 0) {
  chi_square_df <- do.call(rbind, chi_square_results)
}

significant_vars <- chi_square_df %>% arrange(P_Value) %>% filter(P_Value < 0.05)
  

significant_vars <- significant_vars %>% mutate(Variable = fct_reorder(Variable, P_Value))

print(significant_vars)

#### Create bar plot for significant variables
ggplot(significant_vars, aes(x = -log10(P_Value), y = Variable)) +
  geom_col(fill = "#00BFC4", width = 0.7) + 
  geom_vline(xintercept = -log10(0.05), linetype = "dashed", color = "red") +  
  labs(
    title = "Significant Predictors of Customer Churn",
    subtitle = "Chi-Square & T-Test Results (p < 0.05)",
    x = expression("-log"[10] * "(p-value)"),  
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50"),
    plot.caption = element_text(size = 8, color = "gray50")
  ) +
  annotate(
    "text", x = -log10(0.05) + 0.2, y = max(as.numeric(significant_vars$Variable)), 
    label = "p = 0.05 threshold", color = "red", hjust = 0, vjust = 0
  )



#####categorical variables bar plots

churndata$Churn <- as.factor(churndata$Churn)


categorical_vars <- c("Contract", "Internet.Type", "Dependents", "Internet.Service", 
                      "Payment.Method", "Paperless.Billing", "Online.Security", 
                      "Unlimited.Data", "Premium.Tech.Support", "Senior.Citizen", 
                      "Married", "Referred.a.Friend", "Online.Backup", 
                      "Device.Protection.Plan", "Streaming.TV", "Streaming.Movies", 
                      "Under.30", "Streaming.Music", "Multiple.Lines")
categorical_vars

significant_categorical_vars <- significant_vars %>%
  filter(Variable %in% categorical_vars) %>%
  pull(Variable) %>%
  as.character() 

print(significant_categorical_vars)
######################################

plot_bar_vs_churn <- function(var) {
  if (var %in% colnames(churndata)) {  
    ggplot(churndata, aes(x = as.character(.data[[var]]), fill = as.factor(Churn))) +  
      geom_bar(position = "fill", alpha = 0.8) +  
      geom_text(
        stat = "count", 
        aes(label = ..count..), 
        position = position_fill(vjust = 0.5), 
        size = 3
      ) +  
      labs(title = paste("Proportion of Churn by", var), x = "Proportion", y = var) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_discrete(name = "Churn") +  
      coord_flip() +  
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 7),  
        plot.title = element_text(size = 6),  
        legend.text = element_text(size = 6),  
        legend.title = element_text(size = 7),  
        legend.key.size = unit(0.3, "cm") 
      )  
  } else {
    message(paste("Skipping variable:", var, "- Column not found"))
    return(NULL)  
  }
}


plot_list <- lapply(significant_categorical_vars, plot_bar_vs_churn)

plot_list <- Filter(Negate(is.null), plot_list)


do.call(grid.arrange, c(plot_list, ncol = min(5, length(plot_list))))


####key varibles bar plots
subscription_vars <- c("Contract", "Online.Security", "Senior.Citizen", "Under.30", "Payment.Method", "Paperless.Billing")

sub_plot_list <- lapply(subscription_vars, plot_bar_vs_churn)
sub_plot_list <- Filter(Negate(is.null), sub_plot_list)
do.call(grid.arrange, c(sub_plot_list, ncol = 3))


#####numeric variables boxplots

str(churndata)


continuous_vars <- names(churndata)[sapply(churndata, is.numeric)]

print(continuous_vars)

churndata$Churn <- as.factor(churndata$Churn)

continuous_vars <- continuous_vars[continuous_vars %in% names(churndata)]

plot_box_vs_churn <- function(var) {
  ggplot(churndata, aes(x = Churn, y = .data[[var]], fill = Churn)) +
    geom_boxplot(outlier.color = "red", alpha = 0.7) +
    labs(title = paste("Boxplot of", var, "by Churn"), x = "Churn", y = var) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8),  
      axis.title.y = element_text(size = 8, angle = 90)  
    )
}



box_plots <- lapply(continuous_vars, plot_box_vs_churn)

do.call(grid.arrange, c(box_plots, ncol = 4))


### Key variables boxplots 
key_continuous_vars <- c("Satisfaction.Score", 
                         "CLTV",
                         "Tenure.in.Months", 
                         "Monthly.Charge", 
                         "Number.of.Referrals",
                         "Total.Charges",
                         "Total.Revenue",
                         "Avg.Monthly.GB.Download")

box_plots_key <- lapply(key_continuous_vars, plot_box_vs_churn)

do.call(grid.arrange, c(box_plots_key, ncol = 4)) 

####################################################
##Build multiple classification models and Tuning###
####################################################
churndata <- churndata %>% 
  dplyr::select(-Churn.Category, -Churn.Reason)

churndata <- churndata %>%
  dplyr::select(!c(Latitude, Longitude, Total.Revenue, Total.Charges, Total.Refunds))


X <- churndata %>% dplyr::select(-Churn)
y <- churndata$Churn


preproc <- preProcess(X, method = c("center", "scale", "YeoJohnson", "nzv"))
X_processed <- predict(preproc, X)

X_matrix <- model.matrix(~ . -1, data = X_processed)

# Split Train & test data
set.seed(123)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_matrix[train_index, ]
X_test <- X_matrix[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

#### Ridge Regression 

cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)
ridge_model <- glmnet(X_train, y_train, family = "binomial", 
                      alpha = 0, lambda = cv_ridge$lambda.min)

#### LASSO Regression 
cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
best_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(X_train, y_train, family = "binomial", 
                      alpha = 1, lambda = cv_lasso$lambda.min)


##### Create plots
ridge_plot <- vip(ridge_model, num_features = 20, geom = "point") + 
  ggtitle("Ridge Regression Importance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

lasso_plot <- vip(lasso_model, num_features = 20, geom = "point") + 
  ggtitle("LASSO Regression Importance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

combined_plot <- ridge_plot + lasso_plot  # Uses patchwork
print(combined_plot)

#### Coefficient tables
coef_df <- function(model) {
  coefs <- as.matrix(coef(model))
  data.frame(
    Feature = rownames(coefs),
    Coefficient = coefs[,1]
  ) %>% arrange(desc(abs(Coefficient)))
}

ridge_coeffs <- coef_df(ridge_model)
lasso_coeffs <- coef_df(lasso_model)

print(head(ridge_coeffs, 10))
print(head(lasso_coeffs, 10))

#### Improved model evaluation using AUC
library(pROC)


predict_and_evaluate <- function(model, type = "response") {
  probs <- as.numeric(predict(model, newx = X_test, type = type))
  y_factor <- factor(as.character(y_test), levels = c("0", "1"))
  predicted_labels <- factor(ifelse(probs > 0.5, "1", "0"), levels = c("0", "1"))
  roc_obj <- suppressMessages(roc(response = y_factor, predictor = probs, levels = c("0", "1")))
  list(
    AUC = auc(roc_obj),
    ConfusionMatrix = confusionMatrix(predicted_labels, y_factor, positive = "1")
  )
}

ridge_perf <- predict_and_evaluate(ridge_model)
lasso1_perf <- predict_and_evaluate(lasso_model)

cat("Ridge Regression AUC:", ridge_perf$AUC, "\n")
print(ridge_perf$ConfusionMatrix)

cat("LASSO Regression AUC:", lasso1_perf$AUC, "\n")
print(lasso1_perf$ConfusionMatrix)


str(churndata)

#### stepwise selection for Logistic Regression
set.seed(123)
churndata$Churn <- as.factor(churndata$Churn)
train_index <- createDataPartition(churndata$Churn, p = 0.8, list = FALSE)
train_data <- churndata[train_index, ]
test_data <- churndata[-train_index, ]

logit_model <- glm(Churn ~ ., data = train_data, family = binomial)

### Perform stepwise selection using AIC
stepwise_lr <- stepAIC(logit_model, direction = "both", trace = FALSE)
summary(stepwise_lr)


##### Random Forest

tune_grid <- expand.grid(mtry = c(2, 4, 6, 8))
rf_model <- train(Churn ~ ., data = train_data, method = "rf",
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = tune_grid, ntree = 500)
print(rf_model$bestTune)

# Assess model performance on test data
rf_predictions <- predict(rf_model, newdata = test_data)

# Compute confusion matrix
conf_matrix <- confusionMatrix(rf_predictions, test_data$Churn)
print(conf_matrix)

# Feature Importance
importance_df <- varImp(rf_model)
print(importance_df)

# Plot Feature Importance
plot(importance_df, main = "Random Forest Feature Importance")

rf_model <- randomForest(Churn ~ ., 
                         data = train_data,
                         ntree = 500,
                         importance = TRUE,
                         proximity = TRUE)
summary(rf_model)
# Define tuning grid with nearest integers
tune_grid1 <- expand.grid(mtry = c(4, 5))


rf_model_tuned <- train(Churn ~ ., 
                        data = train_data, 
                        method = "rf",
                        trControl = trainControl(method = "cv", number = 5), 
                        tuneGrid = tune_grid1, 
                        ntree = 500)

# Print best mtry
print(rf_model_tuned$bestTune)

# Get feature importance
rf_importance <- importance(rf_model, type = 1) %>%
  as.data.frame() %>%
  {data.frame(Feature = rownames(.), Importance = .$MeanDecreaseAccuracy)} %>%
  arrange(desc(Importance))

# Plot feature importance
ggplot(rf_importance[1:15, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest Feature Importance",
       x = "Features",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()


#### XGBoost
xgb_train <- model.matrix(Churn ~ . -1, data = train_data)
y_train <- as.numeric(as.factor(y_train)) - 1  

xgb_train_matrix <- xgb.DMatrix(data = as.matrix(xgb_train), label = y_train)

set.seed(123)
xgb_model <- xgboost(
  data = xgb_train_matrix,
  objective = "binary:logistic",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  verbose = 0
)

xgb_importance <- xgb.importance(feature_names = colnames(xgb_train), model = xgb_model) %>%
  as.data.frame() %>%
  rename(Importance = Gain) %>%
  arrange(desc(Importance))

# Plot feature importance
ggplot(xgb_importance[1:15, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "XGBoost Feature Importance",
       x = "Features",
       y = "Gain") +
  theme_minimal()



###########################
### models Key Varibles ###
###########################


# Ridge Regression Feature Importance Plot

ridge_importance <- vip::vi(ridge_model)


ridge_plot <- ggplot(ridge_importance[1:20, ], 
                     aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient from light blue to dark blue
  coord_flip() +
  ggtitle("Ridge Regression Feature Importance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

print(ridge_plot)

### LASSO Regression Feature Importance Plot 

lasso_importance <- vip::vi(lasso_model)

lasso_plot <- ggplot(lasso_importance[1:20, ], 
                     aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_col() +
  scale_fill_gradient(low = "lightpink", high = "red") +  # Gradient from light pink to red
  coord_flip() +
  ggtitle("LASSO Regression Feature Importance") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))


print(lasso_plot)

### Random Forest Feature Importance Plot 

rf_plot <- ggplot(rf_importance[1:15, ], 
                  aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_col() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +  # Gradient from light blue to dark blue
  coord_flip() +
  labs(title = "Random Forest Feature Importance",
       x = "Features",
       y = "Mean Decrease in Accuracy") +
  theme_minimal()



### XGBoost Feature Importance Plot
xgb_plot <- ggplot(xgb_importance[1:15, ], 
                   aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_col() +
  scale_fill_gradient(low = "yellow", high = "darkorange") +  # Gradient from yellow to dark orange
  coord_flip() +
  labs(title = "XGBoost Feature Importance",
       x = "Features",
       y = "Gain") +
  theme_minimal()

### Combine four plots
combined_plot <- (ridge_plot | lasso_plot) / (rf_plot | xgb_plot)
print(combined_plot)



