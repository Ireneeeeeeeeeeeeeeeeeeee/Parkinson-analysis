# Load data
data <- read.csv("tele_parkinsons.csv", header=TRUE)

# Check datatype of each attribute
str(data)

# Change datatype of attributes "subject." and "sex" to categorical
data$sex <- as.factor(data$sex)
data$subject. <- as.factor(data$subject.)

# Check datatype again to confirm the changes
str(data)

# Look at the summary of data
summary(data)

# Calculate the percentage of missing values for attribute "motor_UPDRS"
663/5875*100

# Calculate the percentage of missing values for attribute NHR"
399/5875*100

# Drop the attribute "motor_UPDRS"
df <- data[-5]

# Examine the distribution of attribute "NHR"
hist(df$NHR)

# Fill the missing values of attribute "NHR" with the median of "NHR"
median_NHR <- median(df$NHR, na.rm=T)
df$NHR <- ifelse(is.na(df$NHR), median_NHR, df$NHR)

# Check the summary of dataframe to ensure missing values are handled
summary(df)

# Use boxplot to identify the outliers for each numerical attribute
attributes_to_check <- names(df)[c(-1, -3)]
par(mfrow = c(2, 2))
for (attribute in attributes_to_check) {
  boxplot(df[, attribute], main=attribute)
}

# Remove the instances with values outside the lower and upper bound of boxplots
Q1 <- quantile(df$Jitter...)[[2]]
Q3 <- quantile(df$Jitter...)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Jitter... >= (Q1 - 1.5 * IQR)) & (df$Jitter... <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Jitter.Abs.)[[2]]
Q3 <- quantile(df$Jitter.Abs.)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Jitter.Abs. >= (Q1 - 1.5 * IQR)) & (df$Jitter.Abs. <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Jitter.RAP)[[2]]
Q3 <- quantile(df$Jitter.RAP)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Jitter.RAP >= (Q1 - 1.5 * IQR)) & (df$Jitter.RAP <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Jitter.PPQ5)[[2]]
Q3 <- quantile(df$Jitter.PPQ5)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Jitter.PPQ5 >= (Q1 - 1.5 * IQR)) & (df$Jitter.PPQ5 <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Jitter.DDP)[[2]]
Q3 <- quantile(df$Jitter.DDP)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Jitter.DDP >= (Q1 - 1.5 * IQR)) & (df$Jitter.DDP <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Shimmer)[[2]]
Q3 <- quantile(df$Shimmer)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Shimmer >= (Q1 - 1.5 * IQR)) & (df$Shimmer <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Shimmer.dB.)[[2]]
Q3 <- quantile(df$Shimmer.dB.)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Shimmer.dB. >= (Q1 - 1.5 * IQR)) & (df$Shimmer.dB. <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Shimmer.APQ3)[[2]]
Q3 <- quantile(df$Shimmer.APQ3)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Shimmer.APQ3 >= (Q1 - 1.5 * IQR)) & (df$Shimmer.APQ3 <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Shimmer.APQ5)[[2]]
Q3 <- quantile(df$Shimmer.APQ5)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Shimmer.APQ5 >= (Q1 - 1.5 * IQR)) & (df$Shimmer.APQ5 <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Shimmer.APQ11)[[2]]
Q3 <- quantile(df$Shimmer.APQ11)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Shimmer.APQ11 >= (Q1 - 1.5 * IQR)) & (df$Shimmer.APQ11 <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$Shimmer.DDA)[[2]]
Q3 <- quantile(df$Shimmer.DDA)[[4]]
IQR <- Q3 - Q1
df <- df[(df$Shimmer.DDA >= (Q1 - 1.5 * IQR)) & (df$Shimmer.DDA <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$NHR)[[2]]
Q3 <- quantile(df$NHR)[[4]]
IQR <- Q3 - Q1
df <- df[(df$NHR >= (Q1 - 1.5 * IQR)) & (df$NHR <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$HNR)[[2]]
Q3 <- quantile(df$HNR)[[4]]
IQR <- Q3 - Q1
df <- df[(df$HNR >= (Q1 - 1.5 * IQR)) & (df$HNR <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$RPDE)[[2]]
Q3 <- quantile(df$RPDE)[[4]]
IQR <- Q3 - Q1
df <- df[(df$RPDE >= (Q1 - 1.5 * IQR)) & (df$RPDE <= (Q3 + 1.5 * IQR)),]

Q1 <- quantile(df$PPE)[[2]]
Q3 <- quantile(df$PPE)[[4]]
IQR <- Q3 - Q1
df <- df[(df$PPE >= (Q1 - 1.5 * IQR)) & (df$PPE <= (Q3 + 1.5 * IQR)),]

# Remove the instances with negative "test_time" value
df <- df[df$test_time > 0,]

# Check the dimensions of dataframe after removing outliers
dim(df)

# Separate target and features from the dataframe
target <- df$total_UPDRS
features <- df[-5]

# Check which transformation method is suitbale for numerical features
par(mfrow = c(2, 2))
features_transform <- as.data.frame(features)
attributes_to_plot <- names(features_transform)[c(-1:-4)]
for (attribute in attributes_to_plot) {
  hist(features_transform[, attribute], main=paste("original", attribute), xlab="")
  hist(sqrt(features_transform[, attribute]), main=paste("square root", attribute), xlab="")
  hist(log(features_transform[, attribute]), main=paste("natural log", attribute), xlab="")
  hist(log10(features_transform[, attribute]), main=paste("log base 10", attribute), xlab="")
}

# Transform the right-skewed numerical features
to_sqrt <- names(features_transform)[c(5:16)]
for (attribute in to_sqrt) {
  features_transform[[attribute]] <- sqrt(features_transform[[attribute]])
}
features_transform$DFA <- log10(features_transform$DFA)

# Normalize the features in the range of 0 and 1
library(caret)
process <- preProcess(features_transform, method=c("range"))
features_scaled <- predict(process, features_transform)

# Check the summary of scaled features to confirm the normalization
summary(features_scaled)

# Check the summary of target
summary(target)

# Check the range, mean, standard deviation, median, IQR of numerical features before transformation
attributes_to_check <- names(features)[c(-1, -3)]
for (attribute in attributes_to_check) {
  range = max(features[, attribute]) - min(features[, attribute])
  mean = mean(features[, attribute])
  std = sd(features[, attribute])
  median = median(features[, attribute])
  IQR = IQR(features[, attribute])
  print(attribute)
  print(paste("Range:", range))
  print(paste("Mean:", mean))
  print(paste("Standard deviation:", std))
  print(paste("Median:", median))
  print(paste("IQR:", IQR))
  print("......................................")
}

# Check the distribution of numerical features after transformation
par(mfrow=c(2,2))
attributes_to_plot <- names(features_scaled)[c(-1, -3)]
for (attribute in attributes_to_plot) {
  hist(features_scaled[, attribute], main=attribute, xlab="")
}

# Check the frequency of each category for categorical features
attributes_to_check <- names(features_scaled)[3]
for (attribute in attributes_to_check) {
  categories <- summary(features_scaled[attribute])
  print(categories)
}

# Check the distribution of categorical features
par(mfrow=c(1,1))
sex_counts = c(68, 32)
colors = c("blue", "red")
pie(sex_counts, labels=paste0(sex_counts, "%"), main="Gender", col=colors, border=colors)
legend("topright", legend=c("Male", "Female"), fill=colors)

# Check the range, mean, standard deviation, median, IQR of target
range = max(target) - min(target)
mean = mean(target)
std = sd(target)
median = median(target)
IQR = IQR(target)
print("total_UPDRS")
print(paste("Range:", range))
print(paste("Mean:", mean))
print(paste("Standard deviation:", std))
print(paste("Median:", median))
print(paste("IQR:", IQR))

# Check the distribution of target
hist(target, main="total_UPDRS", xlab="")

# Check the correlation of numerical attributes after transformation
features_scaled$total_UPDRS <- target
cor(features_scaled[c(2, 5:21)])

# Determine the relationship betweeen numerical features and target after transformation (assume linear)
attributes_to_plot <- names(features_scaled)[c(2, 5:20)]
for (attribute in attributes_to_plot) {
  print(ggplot(features_scaled, aes_string(x=attribute, y="total_UPDRS")) + geom_point()
        + stat_smooth(method="lm") + labs(title=paste("total_UPDRS vs", attribute)))
}

# Determine the relationship betweeen numerical features and target after transformation (assume non-linear)
attributes_to_plot <- names(features_scaled)[c(2, 5:20)]
for (attribute in attributes_to_plot) {
  print(ggplot(features_scaled, aes_string(x=attribute, y="total_UPDRS")) + geom_point() + geom_smooth()
        + labs(title=paste("total_UPDRS vs", attribute)))
}

# Determine if the categorical features can be used to predict target
anova_sex <- aov(total_UPDRS ~ sex, data=features_scaled)
print("Anova for Sex")
summary(anova_sex)

# Determine the relationship of categorical features with target
boxplot(total_UPDRS ~ sex, data=features_scaled, main="Total UPDRS vs Sex")

# Selected features for modeling
features_selected <- features_scaled[c(2, 3, 5, 14, 16, 17, 18, 19, 20)]
# names(features_selected) <- c("age", "sex", "Jitter(%)", "Shimmer:APQ11", "NHR", "HNR", "RPDE", "DFA", "PPE")
head(features_selected)

# Summary of selected features
summary(features_selected)

# Targer for modeling
head(target)

# Summary of target
summary(target)

# Combine features and target
features_selected$total_UPDRS <- target
head(features_selected)

# Train test split
library(dplyr)
set.seed(123)
training.samples <- features_selected$total_UPDRS %>% createDataPartition(p=0.8, list=FALSE) 
train.data <- features_selected[training.samples,] 
test.data <- features_selected[-training.samples,]
train.control <- trainControl(method="cv", number=4)

# Check the cross-validation result of linear regression
set.seed(123)
lm <- train(total_UPDRS ~ ., data=train.data, method="lm", trControl=train.control, metric="Rsquared")
print(lm)

# Build linear regression model and check the summary
set.seed(123)
lm <- lm(total_UPDRS ~ ., data=train.data)
summary(lm)

# Diagnosis plot for linear regression
par(mfrow=c(2,2))
plot(lm)

# Check the multicolinearity of linear regression
library(car)
vif <- as.matrix(vif(lm))
as.matrix(vif[order(vif[,1], decreasing=T),])

# Check train and test errors of linear regression
train_predict_lm <- predict(lm, train.data)
train_errors_lm <- data.frame(model = "Linear Regression",
                              RMSE_train = RMSE(train_predict_lm, train.data$total_UPDRS),
                              R2_train = R2(train_predict_lm, train.data$total_UPDRS),
                              MAE_train = MAE(train_predict_lm, train.data$total_UPDRS))
test_predict_lm <- predict(lm, test.data)
test_errors_lm <- data.frame(RMSE_test = RMSE(test_predict_lm, test.data$total_UPDRS),
                             R2_test = R2(test_predict_lm, test.data$total_UPDRS),
                             MAE_test = MAE(test_predict_lm, test.data$total_UPDRS))
result_lm <- cbind(train_errors_lm, test_errors_lm)
result_lm

# Residual plot of linear regression
par(mfrow=c(1,1))
residuals <- test.data$total_UPDRS - test_predict_lm
plot(test.data$total_UPDRS, residuals, xlab='Observed', ylab='Residuals',
     main='Residual Plot of Linear Regression')
abline(0,0)

# Check the cross-validation result of regression tree and determine the best hyperparameters
set.seed(123)
library(rpart)
rtGrid <- expand.grid(cp=seq(0.0001, 0.002, 0.0002))
rt <- train(total_UPDRS ~ ., data=train.data, method="rpart", trControl=train.control, metric="Rsquared",
            tuneGrid=rtGrid, control=rpart.control(maxdepth=8))
print(rt)

# Plot hyperparamter tuning results of regression tree
plot(rt, metric="Rsquared", main="Hyperparameter Tuning for Regression Tree")

# Build regression tree model and check the summary
set.seed(123)
rt <- rpart(total_UPDRS ~ ., data=train.data, control=rpart.control(maxdepth=8, cp=7e-04))
summary(rt)

# Plot the full regression tree model
library(rpart.plot)
rpart.plot(rt, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)

# Check the feature importance of regression tree
feature_importance <- as.matrix(rt$variable.importance)
as.matrix(feature_importance[order(feature_importance[,1], decreasing=T),])

# Check train and test errors of regression tree
train_predict_rt <- predict(rt, train.data)
train_errors_rt <- data.frame(model = "Regression Tree",
                              RMSE_train = RMSE(train_predict_rt, train.data$total_UPDRS),
                              R2_train = R2(train_predict_rt, train.data$total_UPDRS),
                              MAE_train = MAE(train_predict_rt, train.data$total_UPDRS))
test_predict_rt <- predict(rt, test.data)
test_errors_rt <- data.frame(RMSE_test = RMSE(test_predict_rt, test.data$total_UPDRS),
                             R2_test = R2(test_predict_rt, test.data$total_UPDRS),
                             MAE_test = MAE(test_predict_rt, test.data$total_UPDRS))
result_rt <- cbind(train_errors_rt, test_errors_rt)
result_rt

# Residual plot of regression tree
residuals <- test.data$total_UPDRS - test_predict_rt
plot(test.data$total_UPDRS, residuals, xlab='Observed', ylab='Residuals',
     main='Residual Plot of Regression Tree')
abline(0,0)

# Check the cross-validation result of radial svm and determine the best hyperparameters
set.seed(123)
svm <- train(total_UPDRS ~ ., data=train.data, method="svmRadial", trControl=train.control, metric="Rsquared",
             tuneLength=10)
print(svm)

# Plot hyperparamter tuning results of radial svm
plot(svm, metric="Rsquared", main="Hyperparameter Tuning for Radial SVM")

# Build radial svm model and check the summary
set.seed(123)
library(kernlab)
svm <- ksvm(total_UPDRS ~ ., data=train.data, kernel="rbfdot", C=32, kpar=list(sigma=0.09639375))
print(svm)

# Check train and test errors of radial svm
train_predict_svm <- predict(svm, train.data)
train_errors_svm <- data.frame(model = "Radial SVM",
                               RMSE_train = RMSE(train_predict_svm, train.data$total_UPDRS),
                               R2_train = R2(train_predict_svm, train.data$total_UPDRS),
                               MAE_train = MAE(train_predict_svm, train.data$total_UPDRS))
test_predict_svm <- predict(svm, test.data)
test_errors_svm <- data.frame(RMSE_test = RMSE(test_predict_svm, test.data$total_UPDRS),
                              R2_test = R2(test_predict_svm, test.data$total_UPDRS),
                              MAE_test = MAE(test_predict_svm, test.data$total_UPDRS))
result_svm <- cbind(train_errors_svm, test_errors_svm)
result_svm

# Residual plot of radial svm
residuals <- test.data$total_UPDRS - test_predict_svm
plot(test.data$total_UPDRS, residuals, xlab='Observed', ylab='Residuals',
     main='Residual Plot of Radial SVM Regression')
abline(0,0)

# Check the cross-validation result of random forest and determine the best hyperparameters
set.seed(123)
rfr <- train(total_UPDRS ~ ., data=train.data, method="rf", trControl=train.control, metric="Rsquared",
             tuneLength=10)
print(rfr)

# Plot hyperparamter tuning results of random forest
plot(rfr, metric="Rsquared", main="Hyperparameter Tuning for Random Forest")

# Build random forest model and check the summary
set.seed(123)
library(randomForest)
rfr <- randomForest(total_UPDRS ~ ., data=train.data, mtry=8)
print(rfr)

# Visualize the impact of trees on the error
plot(rfr, main="Error vs Number of Tree")

# Check the feature importance of random forest
feature_importance_rfr <- rfr$importance
as.matrix(feature_importance_rfr[order(feature_importance_rfr[,1], decreasing=T),])

# Check train and test errors of random forest
train_predict_rfr <- predict(rfr, train.data)
train_errors_rfr <- data.frame(model = "Random Forest",
                               RMSE_train = RMSE(train_predict_rfr, train.data$total_UPDRS),
                               R2_train = R2(train_predict_rfr, train.data$total_UPDRS),
                               MAE_train = MAE(train_predict_rfr, train.data$total_UPDRS))
test_predict_rfr <- predict(rfr, test.data)
test_errors_rfr <- data.frame(RMSE_test = RMSE(test_predict_rfr, test.data$total_UPDRS),
                              R2_test = R2(test_predict_rfr, test.data$total_UPDRS),
                              MAE_test = MAE(test_predict_rfr, test.data$total_UPDRS))
result_rfr <- cbind(train_errors_rfr, test_errors_rfr)
result_rfr

# Residual plot of random forest
residuals <- test.data$total_UPDRS - test_predict_rfr
plot(test.data$total_UPDRS, residuals, xlab='Observed', ylab='Residuals',
     main='Residual Plot of Random Forest Regression')
abline(0,0)

# Combine all results
all_results <- rbind(result_lm, result_rt, result_svm, result_rfr)
all_results