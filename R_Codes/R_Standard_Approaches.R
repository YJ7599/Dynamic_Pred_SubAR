# Load necessary libraries
library(tidyr)
library(readxl)
library(dplyr)
library(caret)
library(ROCR)
library(pROC)
library(ciTools)
library(lme4)
library(boot)
library(PRROC)
library(rms)
library(rjags)
library(grid)
library(gridExtra)
library(zoo)
library(timeROC)

#####################################
# Data Upload & Data Transformation #
#####################################

# Define the file path for the Excel file
file_path <-  ".../Data"

# Read in the Excel file
ori_dat <- read_excel(file_path)

# Rename the specific column
colnames(ori_dat)[colnames(ori_dat) == "MOLPRNTX_V1.3"] = "TruGraf"

# Select and filter the relevant columns
dat <- ori_dat[, c("USUBJID", "TruGraf", "TRAC", "REJ_vs_NoREJ", "BGEVSNUM")]
dat <- dat %>% filter(BGEVSNUM %in% 1:12) %>% mutate(USUBJID = as.factor(USUBJID), BGEVSNUM = as.numeric(BGEVSNUM))

# Conversion from visit number to months 
dat$Month <- as.character(factor(dat$BGEVSNUM, levels = 2:12, labels = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24)))
dat$Month <- as.numeric(dat$Month)

# Filter and clean data for analysis
dat_12_subject <- dat[dat$BGEVSNUM == 12 & !is.na(dat$BGEVSNUM) & !is.na(dat$REJ_vs_NoREJ) & !is.na(dat$TRAC) & !is.na(dat$TruGraf), ]$USUBJID
dat_clean <- dat %>% filter(USUBJID %in% dat_12_subject & !is.na(USUBJID) & !is.na(BGEVSNUM))
dat_clean$two_year_rej <- rep(dat_clean[dat_clean$BGEVSNUM == 12, ]$REJ_vs_NoREJ, as.numeric(table(dat_clean$USUBJID)[table(dat_clean$USUBJID) != 0]))
dat_clean$two_year_rej <- ifelse(dat_clean$two_year_rej == "Rej", 1, 0)
dat_clean <- dat_clean[complete.cases(dat_clean[, c("TruGraf", "TRAC")]),]

# Scale and transform relevant columns
dat_whole <- dat_clean
dat_whole$TRAC <- (log10(dat_whole$TRAC) - min(log10(dat_whole$TRAC))) / (max(log10(dat_whole$TRAC)) - min(log10(dat_whole$TRAC)))
dat_whole$TruGraf <- 0.01 * dat_whole$TruGraf
dat_whole$Month_scaled <- dat_whole$Month * (1/24)

# Identify and handle duplicate entries
dup_dat_check <- dat_whole %>% group_by(USUBJID) %>% count(BGEVSNUM)
subject_dup <- as.character(dup_dat_check[dup_dat_check$n > 1,]$USUBJID)

indices_dup <- list()
j = 1

for (subject in unique(subject_dup)) {
  dup_dat <- dup_dat_check[(dup_dat_check$USUBJID == subject) & (dup_dat_check$n > 1), "BGEVSNUM"]
  
  subject_indices <- which((dat_whole$USUBJID == subject) & (dat_whole$BGEVSNUM %in% unlist(dup_dat)))
  
  if (length(subject_indices) == 2 & length(unique(dat_whole[subject_indices,]$BGEVSNUM[1:2])) == 1) {
    indices_dup[[j]] <- subject_indices
    j = j + 1
  } else if (length(subject_indices) == 3 & length(unique(dat_whole[subject_indices,]$BGEVSNUM[1:3])) == 1) {
    indices_dup[[j]] <- subject_indices
    j = j + 1
  } else if (length(subject_indices) == 4 & length(unique(dat_whole[subject_indices,]$BGEVSNUM[1:2])) == 1 & length(unique(dat_whole[subject_indices,]$BGEVSNUM[3:4])) == 1) {
    indices_dup[[j]] <- subject_indices[1:2]
    indices_dup[[j+1]] <- subject_indices[3:4]
    j = j + 2
  }
}

dup_vec <- list()

for (i in 1:length(indices_dup)) {
  dat_whole[indices_dup[[i]],]$TruGraf <- mean(dat_whole[indices_dup[[i]],]$TruGraf)
  dup_vec[[i]] <- indices_dup[[i]][-1]
}

dat_whole <- dat_whole[-unlist(dup_vec), ]

# Check for remaining duplicates and correct specific errors
dup_dat_check <- dat_whole %>% group_by(USUBJID) %>% count(BGEVSNUM)
subject_dup <- as.character(dup_dat_check[dup_dat_check$n > 1,]$USUBJID)

ind_error <- which(dat_whole$USUBJID == "CTOT0844010" & dat_whole$BGEVSNUM == 6)
dat_whole <- dat_whole[-ind_error, ]

dup_dat_check <- dat_whole %>% group_by(USUBJID) %>% count(BGEVSNUM)
subject_dup <- as.character(dup_dat_check[dup_dat_check$n > 1,]$USUBJID)

# Function to add missing rows for specific BGEVSNUM values
add_missing_rows_miss <- function(df, check_values) {
  all_rows <- expand.grid(USUBJID = unique(df$USUBJID), BGEVSNUM = check_values)
  merged_df <- merge(all_rows, df, by = c("USUBJID", "BGEVSNUM"), all.x = TRUE)
  
  merged_df$Month <- rep(c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), length(unique(df$USUBJID)))
  merged_df$Month_scaled <- merged_df$Month * (1/24)
  
  two_year_rej <- (df %>% group_by(USUBJID) %>% slice_head(n = 1))[, c("USUBJID", "two_year_rej")]
  merged_df <- merge(merged_df, two_year_rej, by = "USUBJID")
  merged_df <- merged_df %>% arrange(USUBJID, Month) %>% group_by(USUBJID) %>% ungroup()
  
  merged_df <- merged_df[, -which(colnames(merged_df) == "two_year_rej.x")]
  colnames(merged_df)[colnames(merged_df) == "two_year_rej.y"] = "two_year_rej"
  return(merged_df)
}

# Add missing rows if necessary and arrange data
if (length(unique(table(dat_whole$BGEVSNUM))) != 1) {
  check_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  
  dat_whole <- add_missing_rows_miss(dat_whole, check_values)
  dat_whole <- dat_whole %>% group_by(USUBJID) %>% arrange(USUBJID, BGEVSNUM)
}

##################################################
# Data split into Train/Test (30% for Test Data) #
##################################################

# Set seed for reproducibility
set.seed(3334)

# Sample test subjects with rejection
test_subjects_rej <- sample(
  unique(dat_whole$USUBJID[dat_whole$two_year_rej == 1]), 
  round(length(unique(dat_whole$USUBJID[dat_whole$two_year_rej == 1])) * 0.3)
)

# Sample test subjects without rejection
test_subjects_norej <- sample(
  unique(dat_whole$USUBJID[dat_whole$two_year_rej == 0]), 
  round(length(unique(dat_whole$USUBJID[dat_whole$two_year_rej == 0])) * 0.3)
)

# Combine test subjects
test_subjects <- c(test_subjects_rej, test_subjects_norej)

# Split the dataset into test and train datasets
test_dat <- subset(dat_whole, (USUBJID %in% test_subjects))
train_dat <- subset(dat_whole, !(USUBJID %in% test_subjects))

# Extract unique subject IDs for train and test datasets
train_subjects <- unique(train_dat$USUBJID)
test_subjects <- unique(test_dat$USUBJID)

#######################################################
# Parallel cross-sectional logistic regression models #
#######################################################

# Prepare training data for cross-sectional models
train_dat_cross_M <- data.frame(train_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TruGraf) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(train_dat$USUBJID))
train_dat_cross_M <- data.frame(train_dat_cross_M)
train_dat_wide_M <- reshape(data = train_dat_cross_M, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

train_dat_cross_T <- data.frame(train_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TRAC) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(train_dat$USUBJID))
train_dat_cross_T <- data.frame(train_dat_cross_T)
train_dat_wide_T <- reshape(data = train_dat_cross_T, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

# Prepare test data for cross-sectional models
test_dat_cross_M <- data.frame(test_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TruGraf) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(test_dat$USUBJID))
test_dat_cross_M <- data.frame(test_dat_cross_M)
test_dat_wide_M <- reshape(data = test_dat_cross_M, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

test_dat_cross_T <- data.frame(test_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TRAC) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(test_dat$USUBJID))
test_dat_cross_T <- data.frame(test_dat_cross_T)
test_dat_wide_T <- reshape(data = test_dat_cross_T, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

# Initialize lists to store predictions and confidence intervals
parallel_logit_pred <- list()
parallel_logit_ci <- list()

# Train and evaluate logistic regression models for each month
for (i in 2:12) {
  two_year_rej <- train_dat %>% group_by(USUBJID) %>% summarise(rej = two_year_rej[1])
  train_dat_sub <- cbind(train_dat_wide_M[, i], train_dat_wide_T[, i])
  
  two_year_rej_test <- test_dat %>% group_by(USUBJID) %>% summarise(rej = two_year_rej[1])
  test_dat_sub <- cbind(test_dat_wide_M[, i], test_dat_wide_T[, i])
  
  train_dat_combine <- cbind(two_year_rej[, 1], train_dat_sub, two_year_rej[, 2])
  test_dat_combine <- cbind(two_year_rej_test[, 1], test_dat_sub, two_year_rej_test[, 2])
  colnames(test_dat_combine) <- colnames(train_dat_combine)
  
  logit_model <- glm(rej ~ ., data = train_dat_combine[, 2:ncol(train_dat_combine)], family = binomial(link = "logit"))
  print(summary(logit_model))
  pred <- predict(logit_model, newdata = test_dat_combine[, 2:ncol(test_dat_combine) - 1], type = "response")
  pred_dat <- data.frame(USUBJID = as.character(test_dat_combine$USUBJID), pred = pred)
  pred_dat <- merge(pred_dat, data.frame(test_dat[test_dat$USUBJID %in% test_subjects,] %>% group_by(USUBJID) %>% slice(1))[, c("USUBJID", "two_year_rej")], by = "USUBJID")
  
  ci <- add_ci(test_dat_combine, logit_model)[, c(ncol(test_dat_combine) + 2, ncol(test_dat_combine) + 3)]
  ci <- cbind(as.character(test_dat_combine$USUBJID), pred, test_dat_combine$rej, ci)
  colnames(ci) <- c("USUBJID", "predicted_prob", "two_year_rej", "Lower", "Upper")
  
  parallel_logit_pred[[i]] <- pred_dat
  parallel_logit_ci[[i]] <- ci
}

# Initialize list and matrix to store evaluation metrics
parallel_logit_pred_list <- list()
parallel_logit_evaluation <- matrix(NA, nrow = length(parallel_logit_pred) - 1, ncol = 2)

j <- 1

# Evaluate predictions for each month
for (i in 2:12) {
  pred_dat <- parallel_logit_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))$auc)
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej))^2)
  
  parallel_logit_pred_list[[j]] <- pred_dat
  parallel_logit_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  
  j <- j + 1
}

# Name the prediction list and evaluation dataframe
names(parallel_logit_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
parallel_logit_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), parallel_logit_evaluation, row.names = names(parallel_logit_pred_list))
colnames(parallel_logit_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "brier_sc")

# Save the evaluation results to a CSV file
write.csv(parallel_logit_evaluation, ".../parallel_logit_evaluation.csv", row.names = FALSE)

#########################################################################
# Multiple logistic regression model using mean exposure across visits  #
#########################################################################

# Initialize lists to store predictions and confidence intervals
mean_logit_pred <- list()
mean_logit_ci <- list()

# Loop through visits to create and evaluate models
for (i in 2:12) {
  # Prepare training data
  two_year_rej <- train_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(rej = two_year_rej[1])
  
  train_dat_sub <- train_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(
      Mean_M = mean(TruGraf, na.rm = TRUE),
      Mean_T = mean(TRAC, na.rm = TRUE)
    ) %>%
    dplyr::select(Mean_M, Mean_T)
  
  train_dat_combine <- cbind(two_year_rej[, 1], train_dat_sub, two_year_rej[, 2])
  
  # Prepare test data
  two_year_rej_test <- test_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(rej = two_year_rej[1])
  
  test_dat_sub <- test_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(
      Mean_M = mean(TruGraf, na.rm = TRUE),
      Mean_T = mean(TRAC, na.rm = TRUE)
    ) %>%
    dplyr::select(Mean_M, Mean_T)
  
  test_dat_combine <- cbind(two_year_rej_test[, 1], test_dat_sub, two_year_rej_test[, 2])
  colnames(test_dat_combine) <- colnames(train_dat_combine)
  
  # Fit logistic regression model
  logit_model <- glm(rej ~ Mean_M + Mean_T, data = train_dat_combine, family = binomial(link = "logit"))
  pred <- predict(logit_model, newdata = test_dat_combine, type = "response")
  pred_dat <- data.frame(USUBJID = as.character(test_dat_combine$USUBJID), pred = pred, two_year_rej = test_dat_combine$rej)
  
  # Compute confidence intervals
  ci <- add_ci(test_dat_combine, logit_model)[, c(ncol(test_dat_combine) + 2, ncol(test_dat_combine) + 3)]
  ci <- cbind(as.character(test_dat_combine$USUBJID), pred, test_dat_combine$rej, ci)
  colnames(ci) <- c("USUBJID", "predicted_prob", "two_year_rej", "Lower", "Upper")
  
  mean_logit_pred[[i]] <- pred_dat
  mean_logit_ci[[i]] <- ci
}

# Initialize list and matrix to store evaluation metrics
mean_logit_pred_list <- list()
mean_logit_evaluation <- matrix(NA, nrow = length(mean_logit_pred) - 1, ncol = 3)

j <- 1

# Evaluate predictions for each visit
for (i in 2:12) {
  pred_dat <- mean_logit_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(auc(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))))
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  mean_logit_pred_list[[j]] <- pred_dat
  mean_logit_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  
  j <- j + 1
}

# Name the prediction list and evaluation dataframe
names(mean_logit_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
mean_logit_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), mean_logit_evaluation, row.names = names(mean_logit_pred_list))
colnames(mean_logit_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "brier_sc")

# Save the evaluation results to a CSV file
write.csv(mean_logit_evaluation, ".../mean_logit_evaluation.csv", row.names = FALSE)

###########################################################################
# Multiple logistic regression model using maximum exposure across visits #
###########################################################################

# Initialize lists to store predictions and confidence intervals
max_logit_pred <- list()
max_logit_ci <- list()

# Loop through visits to create and evaluate models
for (i in 2:12) {
  # Prepare training data
  two_year_rej <- train_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(rej = two_year_rej[1])
  
  train_dat_sub <- train_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(
      Max_M = ifelse(any(!is.na(TruGraf)), max(TruGraf, na.rm = TRUE), NA),
      Max_T = ifelse(any(!is.na(TRAC)), max(TRAC, na.rm = TRUE), NA)
    ) %>%
    dplyr::select(Max_M, Max_T)
  
  train_dat_combine <- cbind(two_year_rej[, 1], train_dat_sub, two_year_rej[, 2])
  
  # Prepare test data
  two_year_rej_test <- test_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(rej = two_year_rej[1])
  
  test_dat_sub <- test_dat %>%
    subset(BGEVSNUM %in% 2:i) %>%
    group_by(USUBJID) %>%
    summarise(
      Max_M = ifelse(any(!is.na(TruGraf)), max(TruGraf, na.rm = TRUE), NA),
      Max_T = ifelse(any(!is.na(TRAC)), max(TRAC, na.rm = TRUE), NA)
    ) %>%
    dplyr::select(Max_M, Max_T)
  
  test_dat_combine <- cbind(two_year_rej_test[, 1], test_dat_sub, two_year_rej_test[, 2])
  colnames(test_dat_combine) <- colnames(train_dat_combine)
  
  # Fit logistic regression model
  logit_model <- glm(rej ~ Max_M + Max_T, data = train_dat_combine, family = binomial(link = "logit"))
  pred <- predict(logit_model, newdata = test_dat_combine, type = "response")
  pred_dat <- data.frame(USUBJID = as.character(test_dat_combine$USUBJID), pred = pred, two_year_rej = test_dat_combine$rej)
  
  # Compute confidence intervals
  ci <- add_ci(test_dat_combine, logit_model)[, c(ncol(test_dat_combine) + 2, ncol(test_dat_combine) + 3)]
  ci <- cbind(as.character(test_dat_combine$USUBJID), pred, test_dat_combine$rej, ci)
  colnames(ci) <- c("USUBJID", "predicted_prob", "two_year_rej", "Lower", "Upper")
  
  max_logit_pred[[i]] <- pred_dat
  max_logit_ci[[i]] <- ci
}

# Initialize list and matrix to store evaluation metrics
max_logit_pred_list <- list()
max_logit_evaluation <- matrix(NA, nrow = length(max_logit_pred) - 1, ncol = 3)

j <- 1

# Evaluate predictions for each visit
for (i in 2:12) {
  pred_dat <- max_logit_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(auc(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))))
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  max_logit_pred_list[[j]] <- pred_dat
  max_logit_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  
  j <- j + 1
}

# Name the prediction list and evaluation dataframe
names(max_logit_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
max_logit_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), max_logit_evaluation, row.names = names(max_logit_pred_list))
colnames(max_logit_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "brier_sc")

# Save the evaluation results to a CSV file
write.csv(max_logit_evaluation, ".../max_logit_evaluation.csv", row.names = FALSE)

#########################################################
# Multiple logistic regression model with 4/3/4 windows #
#########################################################

# Prepare data for cross-sectional models
train_dat_cross_M <- data.frame(train_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TruGraf) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(train_dat$USUBJID))
train_dat_cross_M <- data.frame(train_dat_cross_M)
train_dat_wide_M <- reshape(data = train_dat_cross_M, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

train_dat_cross_T <- data.frame(train_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TRAC) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(train_dat$USUBJID))
train_dat_cross_T <- data.frame(train_dat_cross_T)
train_dat_wide_T <- reshape(data = train_dat_cross_T, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

test_dat_cross_M <- data.frame(test_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TruGraf) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(test_dat$USUBJID))
test_dat_cross_M <- data.frame(test_dat_cross_M)
test_dat_wide_M <- reshape(data = test_dat_cross_M, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

test_dat_cross_T <- data.frame(test_dat) %>%
  dplyr::select(USUBJID, BGEVSNUM, TRAC) %>%
  filter(BGEVSNUM %in% 2:12) %>%
  filter(USUBJID %in% unique(test_dat$USUBJID))
test_dat_cross_T <- data.frame(test_dat_cross_T)
test_dat_wide_T <- reshape(data = test_dat_cross_T, idvar = "USUBJID", timevar = "BGEVSNUM", direction = "wide")

# Compute windowed means for training and test data
train_window_M <- train_dat_wide_M %>%
  transmute(
    USUBJID = USUBJID,
    window.1.M = rowMeans(dplyr::select(., 2), na.rm = TRUE),
    window.2.M = rowMeans(dplyr::select(., 2:3), na.rm = TRUE),
    window.3.M = rowMeans(dplyr::select(., 2:4), na.rm = TRUE),
    window.4.M = rowMeans(dplyr::select(., 2:5), na.rm = TRUE),
    window.5.M = rowMeans(dplyr::select(., 6), na.rm = TRUE),
    window.6.M = rowMeans(dplyr::select(., 6:7), na.rm = TRUE),
    window.7.M = rowMeans(dplyr::select(., 6:8), na.rm = TRUE),
    window.8.M = rowMeans(dplyr::select(., 9), na.rm = TRUE),
    window.9.M = rowMeans(dplyr::select(., 9:10), na.rm = TRUE),
    window.10.M = rowMeans(dplyr::select(., 9:11), na.rm = TRUE),
    window.11.M = rowMeans(dplyr::select(., 9:12), na.rm = TRUE)
  ) %>%
  as.data.frame()

train_window_T <- train_dat_wide_T %>%
  transmute(
    USUBJID = USUBJID,
    window.1.T = rowMeans(dplyr::select(., 2), na.rm = TRUE),
    window.2.T = rowMeans(dplyr::select(., 2:3), na.rm = TRUE),
    window.3.T = rowMeans(dplyr::select(., 2:4), na.rm = TRUE),
    window.4.T = rowMeans(dplyr::select(., 2:5), na.rm = TRUE),
    window.5.T = rowMeans(dplyr::select(., 6), na.rm = TRUE),
    window.6.T = rowMeans(dplyr::select(., 6:7), na.rm = TRUE),
    window.7.T = rowMeans(dplyr::select(., 6:8), na.rm = TRUE),
    window.8.T = rowMeans(dplyr::select(., 9), na.rm = TRUE),
    window.9.T = rowMeans(dplyr::select(., 9:10), na.rm = TRUE),
    window.10.T = rowMeans(dplyr::select(., 9:11), na.rm = TRUE),
    window.11.T = rowMeans(dplyr::select(., 9:12), na.rm = TRUE)
  ) %>%
  as.data.frame()

test_window_M <- test_dat_wide_M %>%
  transmute(
    USUBJID = USUBJID,
    window.1.M = rowMeans(dplyr::select(., 2), na.rm = TRUE),
    window.2.M = rowMeans(dplyr::select(., 2:3), na.rm = TRUE),
    window.3.M = rowMeans(dplyr::select(., 2:4), na.rm = TRUE),
    window.4.M = rowMeans(dplyr::select(., 2:5), na.rm = TRUE),
    window.5.M = rowMeans(dplyr::select(., 6), na.rm = TRUE),
    window.6.M = rowMeans(dplyr::select(., 6:7), na.rm = TRUE),
    window.7.M = rowMeans(dplyr::select(., 6:8), na.rm = TRUE),
    window.8.M = rowMeans(dplyr::select(., 9), na.rm = TRUE),
    window.9.M = rowMeans(dplyr::select(., 9:10), na.rm = TRUE),
    window.10.M = rowMeans(dplyr::select(., 9:11), na.rm = TRUE),
    window.11.M = rowMeans(dplyr::select(., 9:12), na.rm = TRUE)
  ) %>%
  as.data.frame()

test_window_T <- test_dat_wide_T %>%
  transmute(
    USUBJID = USUBJID,
    window.1.T = rowMeans(dplyr::select(., 2), na.rm = TRUE),
    window.2.T = rowMeans(dplyr::select(., 2:3), na.rm = TRUE),
    window.3.T = rowMeans(dplyr::select(., 2:4), na.rm = TRUE),
    window.4.T = rowMeans(dplyr::select(., 2:5), na.rm = TRUE),
    window.5.T = rowMeans(dplyr::select(., 6), na.rm = TRUE),
    window.6.T = rowMeans(dplyr::select(., 6:7), na.rm = TRUE),
    window.7.T = rowMeans(dplyr::select(., 6:8), na.rm = TRUE),
    window.8.T = rowMeans(dplyr::select(., 9), na.rm = TRUE),
    window.9.T = rowMeans(dplyr::select(., 9:10), na.rm = TRUE),
    window.10.T = rowMeans(dplyr::select(., 9:11), na.rm = TRUE),
    window.11.T = rowMeans(dplyr::select(., 9:12), na.rm = TRUE)
  ) %>%
  as.data.frame()

# Initialize lists to store predictions and confidence intervals
window_logit_pred <- list()
window_logit_ci <- list()

# Loop through visits to create and evaluate models
for (i in 2:12) {
  # Prepare training and test data
  two_year_rej <- train_dat %>% group_by(USUBJID) %>% summarise(rej = two_year_rej[1])
  two_year_rej_test <- test_dat %>% group_by(USUBJID) %>% summarise(rej = two_year_rej[1])
  
  if (i < 6) {
    train_dat_sub <- cbind(train_window_M[, i], train_window_T[, i])
    test_dat_sub <- cbind(test_window_M[, i], test_window_T[, i])
  } else if ((5 < i) & (i < 9)) {
    train_dat_sub <- cbind(train_window_M[, c(i, 5)], train_window_T[, c(i, 5)])
    test_dat_sub <- cbind(test_window_M[, c(i, 5)], test_window_T[, c(i, 5)])
  } else {
    train_dat_sub <- cbind(train_window_M[, c(i, 5, 8)], train_window_T[, c(i, 5, 8)])
    test_dat_sub <- cbind(test_window_M[, c(i, 5, 8)], test_window_T[, c(i, 5, 8)])
  }
  
  train_dat_combine <- cbind(two_year_rej[, 1], train_dat_sub, two_year_rej[, 2])
  test_dat_combine <- cbind(two_year_rej_test[, 1], test_dat_sub, two_year_rej_test[, 2])
  colnames(test_dat_combine) <- colnames(train_dat_combine)
  
  # Fit logistic regression model
  logit_model <- glm(rej ~ ., data = train_dat_combine[, 2:ncol(train_dat_combine)], family = binomial(link = "logit"))
  pred_on_test <- predict(logit_model, newdata = test_dat_combine[, 2:ncol(test_dat_combine) - 1], type = "response")
  
  pred_dat <- data.frame(USUBJID = as.character(test_dat_combine$USUBJID), pred = pred_on_test, two_year_rej = test_dat_combine$rej)
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  # Compute confidence intervals
  ci <- add_ci(test_dat_combine, logit_model)[, c(ncol(test_dat_combine) + 2, ncol(test_dat_combine) + 3)]
  ci <- cbind(as.character(test_dat_combine$USUBJID), pred_on_test, test_dat_combine$rej, ci)
  colnames(ci) <- c("USUBJID", "predicted_prob", "two_year_rej", "Lower", "Upper")
  
  window_logit_pred[[i]] <- pred_dat
  window_logit_ci[[i]] <- ci
}

# Initialize list and matrix to store evaluation metrics
window_logit_pred_list <- list()
window_logit_evaluation <- matrix(NA, nrow = length(window_logit_pred) - 1, ncol = 3)

j <- 1

# Evaluate predictions for each visit
for (i in 2:12) {
  pred_dat <- window_logit_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(auc(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))))
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  window_logit_pred_list[[j]] <- pred_dat
  window_logit_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  
  j <- j + 1
}

# Name the prediction list and evaluation dataframe
names(window_logit_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
window_logit_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), window_logit_evaluation, row.names = names(window_logit_pred_list))
colnames(window_logit_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "brier_sc")

# Save the evaluation results to a CSV file
write.csv(window_logit_evaluation, ".../window_logit_evaluation.csv", row.names = FALSE)
