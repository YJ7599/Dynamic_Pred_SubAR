# Load necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)

###################################
# Data Load & Data Transformation #
###################################

# Define the file path for the Excel file
file_path <-  ".../Data"

# Read in the Excel file
ori_dat <- read_excel(file_path)

# Rename the specific column
colnames(ori_dat)[colnames(ori_dat) == "MOLPRNTX_V1.3"] <- "TruGraf"

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
  colnames(merged_df)[colnames(merged_df) == "two_year_rej.y"] <- "two_year_rej"
  return(merged_df)
}

# Add missing rows if necessary and arrange data
if (length(unique(table(dat_whole$BGEVSNUM))) != 1) {
  check_values <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  
  dat_whole <- add_missing_rows_miss(dat_whole, check_values)
  dat_whole <- dat_whole %>% group_by(USUBJID) %>% arrange(USUBJID, BGEVSNUM)
}

#################################################################################
# Spaghetti Plots for Each Biomarker Across Months After Kidney Transplantation #
#################################################################################

# Subset data for TruGraf and TRAC biomarkers
dat_bio1 <- subset(dat_whole, !is.na(TruGraf))
dat_bio2 <- subset(dat_whole, !is.na(TRAC))

set.seed(1)

# Sample subjects for TruGraf biomarker
dat_bio1_whole <- dat_bio1 %>% group_by(USUBJID) %>% filter(all(c(2, 12) %in% BGEVSNUM))
sample.subjects.rej.1 <- sample(unique(dat_bio1_whole[dat_bio1_whole$two_year_rej == 1,]$USUBJID), 20)
sample.subjects.norej.1 <- sample(unique(dat_bio1_whole[dat_bio1_whole$two_year_rej == 0,]$USUBJID), 20)
sample.subjects.1 <- c(sample.subjects.rej.1, sample.subjects.norej.1)

# Sample subjects for TRAC biomarker
dat_bio2_whole <- dat_bio2 %>% group_by(USUBJID) %>% filter(all(c(2, 12) %in% BGEVSNUM))
sample.subjects.rej.2 <- sample(unique(dat_bio2_whole[dat_bio2_whole$two_year_rej == 1,]$USUBJID), 20)
sample.subjects.norej.2 <- sample(unique(dat_bio2_whole[dat_bio2_whole$two_year_rej == 0,]$USUBJID), 20)
sample.subjects.2 <- c(sample.subjects.rej.2, sample.subjects.norej.2)

# Plot for TruGraf biomarker
ggplot(data = dat_bio1, 
       aes(x = Month, y = TruGraf, group = USUBJID)) + 
  geom_line(color = "grey", alpha = 0.4) + 
  geom_line(data = subset(dat_bio1, (USUBJID %in% sample.subjects.1)), alpha = 0.6, aes(color = as.factor(two_year_rej))) + 
  geom_point(data = subset(dat_bio1, ((USUBJID %in% sample.subjects.1) & (BGEVSNUM == 12))), alpha = 0.7, aes(color = as.factor(two_year_rej))) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12.5)) + 
  labs(title = NULL, color = "two year subAR", x = "Months After KT", y = "TruGraf") + 
  geom_smooth(aes(group = as.factor(two_year_rej), color = as.factor(two_year_rej)),  method = "loess") + 
  scale_color_manual(values = c("green", "purple")) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

# Plot for TRAC biomarker
ggplot(data = dat_bio2, 
       aes(x = Month, y = TRAC, group = USUBJID)) + 
  geom_line(color = "grey", alpha = 0.4) + 
  geom_line(data = subset(dat_bio2, (USUBJID %in% sample.subjects.2)), alpha = 0.6, aes(color = as.factor(two_year_rej))) + 
  geom_point(data = subset(dat_bio2, ((USUBJID %in% sample.subjects.2) & (BGEVSNUM == 12))), alpha = 0.7, aes(color = as.factor(two_year_rej))) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12.5)) + 
  labs(title = NULL, color = "two year subAR", x = "Months After KT", y = "log (donor-derived cfDNA)") + 
  geom_smooth(aes(group = as.factor(two_year_rej), color = as.factor(two_year_rej)),  method = "loess") + 
  scale_color_manual(values = c("green", "purple")) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))
