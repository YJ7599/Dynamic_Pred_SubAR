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

#########
# 2SMLE #
#########

# Historic Model Development
# Stage 1: A mixed model: Random intercept and slope
Stage.1.M = lmer(TruGraf ~ 1 + Month_scaled + (1 + Month_scaled | USUBJID), train_dat, REML = TRUE)
Stage.1.T = lmer(TRAC ~ 1 + Month_scaled + (1 + Month_scaled | USUBJID), train_dat, REML = TRUE)

# Extract confidence intervals and random effects for the models
CI.parameters.M = confint(profile(Stage.1.M))     # The confidence intervals of the fixed effects 
RE.M = ranef(Stage.1.M)[["USUBJID"]]              # The random effects 
RE.id.M = cbind(USUBJID = rownames(RE.M), RE.M)
SubAR.RE.M = merge(unique(train_dat[, c("USUBJID", "two_year_rej")]), RE.id.M, by = "USUBJID")
colnames(SubAR.RE.M)[3] <- "re.int.M"              # The random intercepts 
colnames(SubAR.RE.M)[4] <- "re.slope.M"           # The random slopes 

CI.parameters.T = confint(profile(Stage.1.T))     # The confidence intervals of the fixed effects 
RE.T = ranef(Stage.1.T)[["USUBJID"]]              # The random effects 
RE.id.T = cbind(USUBJID = rownames(RE.T), RE.T)
SubAR.RE.T = merge(unique(train_dat[, c("USUBJID", "two_year_rej")]), RE.id.T, by = "USUBJID")
colnames(SubAR.RE.T)[3] <- "re.int.T"              # The random intercepts 
colnames(SubAR.RE.T)[4] <- "re.slope.T"           # The random slopes 

# Combine random effects for both models
SubAR.RE <- cbind(SubAR.RE.M, SubAR.RE.T[, c(3, 4)])

# Stage 2: Logistic regression using the random effects and age as predictors

env <- globalenv()                                # Grab the global environment
env$dd <- datadist(SubAR.RE)                      # Assign the datadist to it

options(datadist = "dd")

M = lrm(two_year_rej ~ re.int.M + re.slope.M + re.int.T + re.slope.T, data = SubAR.RE)  

# Extract coefficients and variance components
Beta0.M = as.numeric(fixef(Stage.1.M)[1])
Beta1.M = as.numeric(fixef(Stage.1.M)[2])

Beta0.T = as.numeric(fixef(Stage.1.T)[1])
Beta1.T = as.numeric(fixef(Stage.1.T)[2])

vcov.T = as.data.frame(VarCorr(Stage.1.T))
d11.T = vcov.T$sdcor[1]
d22.T = vcov.T$sdcor[2]
d12.T = vcov.T$vcov[3]
Sigma.res.T = vcov.T$sdcor[4]

vcov.M = as.data.frame(VarCorr(Stage.1.M))
d11.M = vcov.M$sdcor[1]
d22.M = vcov.M$sdcor[2]
d12.M = vcov.M$vcov[3]
Sigma.res.M = vcov.M$sdcor[4] 

Alpha0 = as.numeric(coefficients(M)[1])
Alpha1 = as.numeric(coefficients(M)[2])
Alpha2 = as.numeric(coefficients(M)[3])
Alpha3 = as.numeric(coefficients(M)[4])
Alpha4 = as.numeric(coefficients(M)[5])

# Combine parameter estimates
param.est <- c(Beta0.M, Beta1.M, Beta0.T, Beta1.T, Sigma.res.M, d11.M, d12.M, d22.M, Sigma.res.T, d11.T, d12.T, d22.T, Alpha0, Alpha1, Alpha2, Alpha3, Alpha4)
names(param.est) <- c("Beta0_M", "Beta1_M", "Beta0_T", "Beta1_T", "Sigma_res_M", "d11_M", "d12_M", "d22_M", "Sigma_res_T", "d11_T", "d12_T", "d22_T", "Alpha0", "Alpha1", "Alpha2", "Alpha3", "Alpha4")

# Function for dynamic prediction using 2SMLE
dyn.pred.func <- function(dat, test.subjects, est.result, period){ 
  
  pred.vec <- c() 
  dat <- dat[dat$BGEVSNUM %in% (as.numeric(period)),]
  dat <- dat[dat$USUBJID %in% test.subjects,]
  
  for (subject in as.character(test.subjects)){
    
    new <- dat[dat$USUBJID == subject, ] 
    new <- new[complete.cases(new[, c("TruGraf", "TRAC", "BGEVSNUM", "two_year_rej")]),]
    
    if (nrow(new) != 0){
      D.new = new$Month_scaled
      M.new = new$TruGraf
      T.new = new$TRAC
      
      new.patient = data.frame(D.new, M.new, T.new) 
      new.patient$D.new <- as.numeric(new.patient$D.new)
      
      Xbeta_M = est.result[["Beta0_M"]] + est.result[["Beta1_M"]] * new.patient$D.new
      Xbeta_T = est.result[["Beta0_T"]] + est.result[["Beta1_T"]] * new.patient$D.new
      
      R_M = diag(length(new.patient$D.new))*(est.result[["Sigma_res_M"]]**2)  
      R_T = diag(length(new.patient$D.new))*(est.result[["Sigma_res_T"]]**2) 
      
      Z_M = matrix(c(rep(1,length(new.patient$D.new)), new.patient$D.new),
                   nrow=length(new.patient$D.new), ncol=2)  
      
      Z_T = matrix(c(rep(1,length(new.patient$D.new)), new.patient$D.new),
                   nrow=length(new.patient$D.new), ncol=2)
      
      kk_M = new.patient$M.new - Xbeta_M
      kk_T = new.patient$T.new - Xbeta_T
      
      G_M = matrix(c(est.result[["d11_M"]]**2, est.result[["d12_M"]], est.result[["d12_M"]], est.result[["d22_M"]]**2),nrow=2,ncol=2)
      G_T = matrix(c(est.result[["d11_T"]]**2, est.result[["d12_T"]], est.result[["d12_T"]], est.result[["d22_T"]]**2),nrow=2,ncol=2)
      
      SigmaMatrix_M = Z_M %*% G_M %*% t(Z_M) + R_M
      SigmaMatrix_T = Z_T %*% G_T %*% t(Z_T) + R_T
      
      Inversigma_M = solve(SigmaMatrix_M)
      Inversigma_T = solve(SigmaMatrix_T)
      
      REPredicted_M = G_M %*% t(Z_M) %*% Inversigma_M %*% kk_M
      REPredicted_T = G_T %*% t(Z_T) %*% Inversigma_T %*% kk_T
      
      RandomIntercept_M = REPredicted_M[1,1]
      RandmSlope_M = REPredicted_M[2,1]
      
      RandomIntercept_T = REPredicted_T[1,1]
      RandmSlope_T = REPredicted_T[2,1]
      
      lp = est.result[["Alpha0"]] + est.result[["Alpha1"]]*RandomIntercept_M + est.result[["Alpha2"]]*RandmSlope_M + est.result[["Alpha3"]]*RandomIntercept_T + est.result[["Alpha4"]]*RandmSlope_T 
      pred = inv.logit(lp)
      
      pred.vec <- c(pred.vec, pred)
    } 
    else {
      pred.vec <- c(pred.vec, NA)
    }
  }
  return(pred.vec)
}

# Perform predictions using 2SMLE
TWOSMLE_pred <- list()

for (i in 2:12){
  pred_vec <- dyn.pred.func(test_dat, test_subjects, param.est, 2:i)
  
  pred_dat <- data.frame(USUBJID = as.character(test_subjects), pred = pred_vec)
  pred_dat <- merge(pred_dat, data.frame(test_dat[test_dat$USUBJID %in% test_subjects,] %>% group_by(USUBJID) %>% slice(1))[,c("USUBJID", "two_year_rej")], by = "USUBJID")
  
  TWOSMLE_pred [[i]] <- pred_dat
}

# Initialize list and matrix to store evaluation metrics
TWOSMLE_pred_list <- list()
TWOSMLE_evaluation <- matrix(NA, nrow = length(TWOSMLE_pred) - 1, ncol = 3)

# Evaluate predictions for each visit
j <- 1
for (i in 2:12){
  pred_dat <- TWOSMLE_pred [[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))$auc)
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  TWOSMLE_pred_list [[j]] <- pred_dat
  
  TWOSMLE_evaluation [j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  j <- j + 1 
}

# Name the prediction list and evaluation dataframe
names(TWOSMLE_pred_list) <- paste("Month" , c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
TWOSMLE_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), TWOSMLE_evaluation, row.names = names(TWOSMLE_pred))
colnames(TWOSMLE_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "brier_sc")

# Save the evaluation results to a CSV file
write.csv(TWOSMLE_evaluation, ".../2SMLE_evaluation.csv", row.names = FALSE)

#########
# JMMLE #
#########

# Save the initial parameter estimates values and the train dataset to derive JMMLE parameter estimates in SAS 
# Transform the variance parameters to be squared values
jmmle.param.init <- param.est 
jmmle.param.init[c("Sigma_res_M", "d11_M", "d22_M", "Sigma_res_T", "d11_T", "d22_T")] <- jmmle.param.init[c("Sigma_res_M", "d11_M", "d22_M", "Sigma_res_T", "d11_T", "d22_T")]^2

# Create a dataframe for initial parameter estimates and save it as a CSV file
jmmle.param.init.df <- data.frame(Parameter = names(jmmle.param.init), Estimate = jmmle.param.init)
write.csv(jmmle.param.init.df,  ".../jmmle_param_est_init_whole.csv", row.names = FALSE)

# Prepare the training dataset and save it as a CSV file
train_dat <- data.frame(train_dat)
jmmle_train_dat <- train_dat[complete.cases(train_dat[, c("TRAC", "TruGraf")]),]
write.csv(jmmle_train_dat, ".../jmmle_real_whole.csv", row.names = FALSE)

# Load the parameter estimates derived from JMMLE in SAS
param.est.JMMLE <- read.csv(".../Table_Estimates_JMMLE.csv")
param.est.JMMLE <- param.est.JMMLE$Estimate[c(1, 2, 7, 8, 3, 4, 5, 6, 9, 10, 11, 12, 17, 18, 19, 20, 21 )]
names(param.est.JMMLE) <- c("Beta0_M", "Beta1_M", "Beta0_T", "Beta1_T", "Sigma_res_M", "d11_M", "d12_M", "d22_M", "Sigma_res_T", "d11_T", "d12_T", "d22_T", "Alpha0", "Alpha1", "Alpha2", "Alpha3", "Alpha4")

# Transform the variance parameters back to standard deviations
param.est.JMMLE[c("d11_M", "d22_M", "d11_T", "d22_T", "Sigma_res_M", "Sigma_res_T")] <- sqrt(param.est.JMMLE[c("d11_M", "d22_M", "d11_T", "d22_T", "Sigma_res_M", "Sigma_res_T")])

# Initialize a list to store JMMLE predictions
JMMLE_pred <- list()

# Loop through each month to make predictions
for (i in 2:12) {
  pred_vec <- dyn.pred.func(test_dat, test_subjects, param.est.JMMLE, 2:i)
  pred_dat <- data.frame(USUBJID = as.character(test_subjects), pred = pred_vec)
  pred_dat <- merge(pred_dat, data.frame(test_dat[test_dat$USUBJID %in% test_subjects,] %>% group_by(USUBJID) %>% slice(1))[,c("USUBJID", "two_year_rej")], by = "USUBJID")
  
  JMMLE_pred [[i]] <- pred_dat
}

# Initialize a list and matrix to store evaluation metrics
JMMLE_pred_list <- list()
JMMLE_evaluation <- matrix(NA, nrow = length(JMMLE_pred) - 1, ncol = 3)

# Evaluate predictions for each month
j <- 1
for (i in 2:12) {
  pred_dat <- JMMLE_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))$auc)
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  JMMLE_pred_list[[j]] <- pred_dat
  JMMLE_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  j <- j + 1 
}

# Name the prediction list and evaluation dataframe
names(JMMLE_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "") 
JMMLE_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), JMMLE_evaluation, row.names = names(JMMLE_pred))
colnames(JMMLE_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "brier_sc")

# Save the evaluation results to a CSV file
write.csv(JMMLE_evaluation, ".../JMMLE_evaluation.csv", row.names = FALSE)

#######
# 2SB #
#######

# Initialize parameters for the 2SB model
N <- length(unique(train_dat$USUBJID))
M <- as.numeric(table(train_dat$USUBJID)[names(table(train_dat$USUBJID)) %in% unique(train_dat$USUBJID)])
scaled_visit <- train_dat$Month_scaled
dat_train_bayes <- train_dat

# Create matrix for scaled visits
scaled_visitn_mat <- matrix(NA, nrow = N, ncol = 11)
for (i in 1:N) {
  scaled_visitn_mat[i, 1:M[i]] <- dat_train_bayes$Month_scaled[which(dat_train_bayes$USUBJID == unique(dat_train_bayes$USUBJID)[i])]
}

# Create matrix for TruGraf values
TruGraf_mat <- matrix(NA, nrow = N, ncol = 11)
for (i in 1:N) {
  TruGraf_mat[i, 1:M[i]] <- dat_train_bayes$TruGraf[which(dat_train_bayes$USUBJID == unique(dat_train_bayes$USUBJID)[i])]
}

# Create matrix for TRAC values
TRAC_mat <- matrix(NA, nrow = N, ncol = 11)
for (i in 1:N) {
  TRAC_mat[i, 1:M[i]] <- dat_train_bayes$TRAC[which(dat_train_bayes$USUBJID == unique(dat_train_bayes$USUBJID)[i])]
}

# Define the JAGS model for TruGraf
model_string.1 <- "model {
  for (i in 1:N) {
    ## random effects of MV normal---LME model ###
    b0[i, 1] <- 0
    b0[i, 2] <- 0
    b[i, 1:2] ~ dmnorm(b0[i, 1:2], ISigma[,]) ## MV normal

    for (j in 1:M[i]) {
      ## Response model ###
      TruGraf[i, j] ~ dnorm(beta[1] + b[i, 1] + (beta[2] + b[i, 2]) * scaled_visitn[i, j], tau)
    }
  }

  ## prior distribution of the parameters
  #(1) Coefficients
  for (k in 1:2) { beta[k] ~ dnorm(0, .01) }

  #(2) Precision parameters
  tau ~ dgamma(.01, .01)
  sigma.tau <- 1 / tau

  #(3) Variance-covariance matrix
  ISigma[1:2, 1:2] ~ dwish(R[,], 3)
  Sigma[1:2, 1:2] <- inverse(ISigma[,])

  R[1, 1] <- 1
  R[1, 2] <- 0
  R[2, 2] <- 1
  R[2, 1] <- 0
}"

# Define the JAGS model for TRAC
model_string.2 <- "model {
  for (i in 1:N) {
    ## random effects of MV normal---LME model ###
    b0[i, 1] <- 0
    b0[i, 2] <- 0
    b[i, 1:2] ~ dmnorm(b0[i, 1:2], ISigma[,]) ## MV normal

    for (j in 1:M[i]) {
      ## Response model ###
      TRAC[i, j] ~ dnorm(beta[1] + b[i, 1] + (beta[2] + b[i, 2]) * scaled_visitn[i, j], tau)
    }
  }

  ## prior distribution of the parameters
  #(1) Coefficients
  for (k in 1:2) { beta[k] ~ dnorm(0, .01) }

  #(2) Precision parameters
  tau ~ dgamma(.01, .01)
  sigma.tau <- 1 / tau

  #(3) Variance-covariance matrix
  ISigma[1:2, 1:2] ~ dwish(R[,], 3)
  Sigma[1:2, 1:2] <- inverse(ISigma[,])

  R[1, 1] <- 1
  R[1, 2] <- 0
  R[2, 2] <- 1
  R[2, 1] <- 0
}"

# Fit the TruGraf model using JAGS
model1 <- jags.model(textConnection(model_string.1), 
                     data = list(TruGraf = TruGraf_mat, N = N, M = M, scaled_visitn = scaled_visitn_mat),
                     n.chains = 3)
params_M <- c('b', 'beta', 'Sigma', 'sigma.tau')
samps.M <- coda.samples(model1, params_M, n.iter = 10000)

# Summarize the TruGraf model samples
summary.samps.M <- summary(window(samps.M, start = 1000))
summary.M <- as.data.frame(summary.samps.M$statistics)
n.row <- nrow(summary.M)
rand.M <- summary.M[-c(1, 2, 3, 4, n.row-2, n.row-1, n.row),] 

b1.M = rand.M$Mean[1:N]
b2.M = rand.M$Mean[(N+1):length(rand.M$Mean)]

# Extract the model fixed effects and variance covariance components for TruGraf
statistics.model.M <- as.data.frame(summary.samps.M$statistics)
params.model.M = statistics.model.M[c(n.row-2, n.row-1, 1, 4, 2, n.row), 'Mean']
params.model.M[c(3, 4, 6)] = sqrt(params.model.M[c(3, 4, 6)])

# Fit the TRAC model using JAGS
model2 <- jags.model(textConnection(model_string.2), 
                     data = list(TRAC = TRAC_mat, N = N, M = M, scaled_visitn = scaled_visitn_mat),
                     n.chains = 3)
params_T <- c('b', 'beta', 'Sigma', 'sigma.tau')
samps.T <- coda.samples(model2, params_T, n.iter = 10000)

# Summarize the TRAC model samples
summary.samps.T <- summary(window(samps.T, start = 1000))
summary.T <- as.data.frame(summary.samps.T$statistics)
rand.T <- summary.T[-c(1, 2, 3, 4, n.row-2, n.row-1, n.row),] 

b1.T = rand.T$Mean[1:N]
b2.T = rand.T$Mean[(N+1):length(rand.T$Mean)]

# Extract the model fixed effects and variance covariance components for TRAC
statistics.model.T <- as.data.frame(summary.samps.T$statistics)
params.model.T = statistics.model.T[c(n.row-2, n.row-1, 1, 4, 2, n.row), 'Mean']
params.model.T[c(3, 4, 6)] = sqrt(params.model.T[c(3, 4, 6)])

# Stage two: Logistic regression model for two-year rejection
model_2nd_string <- "model {
  for (i in 1:N) {
    ## Logistic regression model ###
    two_year_rej[i] ~ dbin(pi[i], 1)
    logit(pi[i]) <- a[1] + a[2] * b1.M[i] + a[3] * b2.M[i] + a[4] * b1.T[i] + a[5] * b2.T[i]
    lp[i] = logit(pi[i])
  }

  ## prior distribution of the parameters 
  for (ll in 1:5) { a[ll] ~ dnorm(0, .01) }
}"

two_year_rej <- unique(dat_train_bayes[, c("USUBJID", "two_year_rej")])$two_year_rej

# Fit the logistic regression model using JAGS
model3 <- jags.model(textConnection(model_2nd_string), 
                     data = list(two_year_rej = two_year_rej, N = N, b1.M = b1.M, b2.M = b2.M, b1.T = b1.T, b2.T = b2.T),
                     n.chains = 3)
params_a <- c('a')
samps_a <- coda.samples(model3, params_a, n.iter = 10000) 
summary.model.a <- summary(window(samps_a, start = 2000)) 
summary.a <- as.data.frame(summary.model.a$statistics)
params.model.a <- summary.a[c(1:5), 'Mean']

# The results of the 2SB: to be added to Table 1
params.model.2SB <- c(params.model.M, params.model.T, params.model.a)
TWOSB.param.est <- params.model.2SB[c(1, 2, 7, 8, 6, 3, 5, 4, 12, 9, 11, 10, 13, 14, 15, 16, 17)]
names(TWOSB.param.est) <- c("Beta0_M", "Beta1_M", "Beta0_T", "Beta1_T", "Sigma_res_M", "d11_M", "d12_M", "d22_M", "Sigma_res_T", "d11_T", "d12_T", "d22_T", "Alpha0", "Alpha1", "Alpha2", "Alpha3", "Alpha4")

# Initialize list to store 2SB predictions
TWOSB_pred <- list()

# Loop through each month to make predictions
for (i in 2:12) {
  pred_vec <- dyn.pred.func(test_dat, test_subjects, TWOSB.param.est, 2:i)
  pred_dat <- data.frame(USUBJID = as.character(test_subjects), pred = pred_vec)
  pred_dat <- merge(pred_dat, data.frame(test_dat[test_dat$USUBJID %in% test_subjects,] %>% group_by(USUBJID) %>% slice(1))[,c("USUBJID", "two_year_rej")], by = "USUBJID")
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  TWOSB_pred[[i]] <- pred_dat
}

# Initialize list and matrix to store evaluation metrics
TWOSB_pred_list <- list() 
TWOSB_evaluation <- matrix(NA, nrow = length(TWOSB_pred) - 1, ncol = 3)

# Evaluate predictions for each month
j <- 1
for (i in 2:12) {
  pred_dat <- TWOSB_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(auc(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))))
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  TWOSB_pred_list[[j]] <- pred_dat
  TWOSB_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  j <- j + 1 
}

# Name the prediction list and evaluation dataframe
names(TWOSB_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
TWOSB_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), TWOSB_evaluation, row.names = names(TWOSB_pred))
colnames(TWOSB_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "MSE")

# Save the evaluation results to a CSV file
write.csv(TWOSB_evaluation, ".../2SB_evaluation.csv", row.names = FALSE)

#######
# JMB #
#######

# Define the JAGS model for joint modeling of TruGraf and TRAC
Model_string.JMB = "model {
  for (i in 1:N) {
    ## random effects
    b0[i, 1] = 0
    b0[i, 2] = 0
    b0[i, 3] = 0
    b0[i, 4] = 0

    b[i, 1:4] ~ dmnorm(b0[i, 1:4], ISigma[,]) ## MV normal

    for (j in 1:M[i]) {
      ## Response model ###
      TruGraf[i, j] ~ dnorm(betaM[1] + b[i, 1] + (betaM[2] + b[i, 2]) * scaled_visitn[i, j], tauM)
      TRAC[i, j] ~ dnorm(betaT[1] + b[i, 3] + (betaT[2] + b[i, 4]) * scaled_visitn[i, j], tauT)
    }

    ## Logistic regression model ###
    two_year_rej[i] ~ dbin(pi[i], 1)
    logit(pi[i]) <- a[1] + a[2] * b[i, 1] + a[3] * b[i, 2] + a[4] * b[i, 3] + a[5] * b[i, 4]
    lp[i] = logit(pi[i])
  }

  ## prior distribution of the parameters
  #(1) Coefficients
  for (k in 1:2) { betaM[k] ~ dnorm(0, .01) }
  for (k in 1:2) { betaT[k] ~ dnorm(0, .01) }
  for (ll in 1:5) { a[ll] ~ dnorm(0, .01) }

  #(2) Precision parameters
  tauM ~ dgamma(.01, .01)
  tauT ~ dgamma(.01, .01)
  sigma.tau.M = 1 / tauM
  sigma.tau.T = 1 / tauT

  #(3) Variance-covariance matrix
  ISigma[1:4, 1:4] ~ dwish(R[,], 10)
  Sigma[1:4, 1:4] <- inverse(ISigma[,])

  R[1, 1] <- 1
  R[1, 2] <- 0
  R[1, 3] <- 0
  R[1, 4] <- 0
  R[2, 1] <- 0 
  R[2, 2] <- 1
  R[2, 3] <- 0
  R[2, 4] <- 0
  R[3, 1] <- 0
  R[3, 2] <- 0
  R[3, 3] <- 1
  R[3, 4] <- 0
  R[4, 1] <- 0
  R[4, 2] <- 0
  R[4, 3] <- 0
  R[4, 4] <- 1
}"

# Fit the JMB model using JAGS
Model.JMB <- jags.model(textConnection(Model_string.JMB), 
                        data = list(two_year_rej = two_year_rej, TruGraf = TruGraf_mat, TRAC = TRAC_mat, N = N, M = M, scaled_visitn = scaled_visitn_mat),
                        n.chains = 3)
update(Model.JMB, 10000, progress.bar = "none") 

# Generate posterior samples for the JMB model
params <- c('betaM', 'betaT', 'a', 'Sigma', 'sigma.tau.M', 'sigma.tau.T')
samps <- coda.samples(Model.JMB, params, n.iter = 10000) 
summary.model.JMB <- summary(window(samps))  # the posterior means of the joint model coefficients 

# Extract and summarize the JMB model parameters
summary.JMB <- as.data.frame(summary.model.JMB$statistics)
params.model.JMB <- summary.JMB[c(22, 23, 24, 25, 26, 1, 2, 6, 27, 11, 12, 16, 17, 18, 19, 20, 21), "Mean"]
params.model.JMB[c(5, 6, 8, 9, 10, 12)] <- sqrt(params.model.JMB[c(5, 6, 8, 9, 10, 12)])
JMB.param.est <- params.model.JMB 

names(JMB.param.est) <- c("Beta0_M", "Beta1_M", "Beta0_T", "Beta1_T", "Sigma_res_M", "d11_M", "d12_M", "d22_M", "Sigma_res_T", "d11_T", "d12_T", "d22_T", "Alpha0", "Alpha1", "Alpha2", "Alpha3", "Alpha4")

# Initialize list to store JMB predictions
JMB_pred <- list()

# Loop through each month to make predictions
for (i in 2:12) {
  pred_vec <- dyn.pred.func(test_dat, test_subjects, JMB.param.est, 2:i)
  pred_dat <- data.frame(USUBJID = as.character(test_subjects), pred = pred_vec)
  pred_dat <- merge(pred_dat, data.frame(test_dat[test_dat$USUBJID %in% test_subjects,] %>% group_by(USUBJID) %>% slice(1))[,c("USUBJID", "two_year_rej")], by = "USUBJID")
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  JMB_pred[[i]] <- pred_dat
}

# Initialize list and matrix to store evaluation metrics
JMB_pred_list <- list() 
JMB_evaluation <- matrix(NA, nrow = length(JMB_pred) - 1, ncol = 3)

# Evaluate predictions for each month
j <- 1
for (i in 2:12) {
  pred_dat <- JMB_pred[[i]]
  pred_dat <- pred_dat[complete.cases(pred_dat),]
  
  auc_roc <- as.numeric(auc(roc(pred_dat$two_year_rej, as.numeric(pred_dat$pred))))
  auc_pr <- pr.curve(scores.class0 = as.numeric(pred_dat$pred), weights.class0 = pred_dat$two_year_rej, curve = TRUE)
  brier_sc <- mean((as.numeric(pred_dat$pred) - as.numeric(pred_dat$two_year_rej)) ^ 2)
  
  JMB_pred_list[[j]] <- pred_dat
  JMB_evaluation[j, ] <- c(auc_roc, auc_pr$auc.integral, brier_sc)
  j <- j + 1 
}

# Name the prediction list and evaluation dataframe
names(JMB_pred_list) <- paste("Month", c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), sep = "")
JMB_evaluation <- data.frame(Month = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24), JMB_evaluation, row.names = names(JMB_pred))
colnames(JMB_evaluation) <- c("Month", "AUC_ROC", "AUC_PR", "MSE")

# Save the evaluation results to a CSV file
write.csv(JMB_evaluation, ".../JMB_evaluation.csv", row.names = FALSE)
