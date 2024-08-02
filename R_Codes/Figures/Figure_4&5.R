library(gridExtra)

######################
## Calibration Plot ##
######################

calibration_parallel <- list() 
calibration_mean <- list() 
calibration_max <- list() 
calibration_window <- list() 

calibration_twosmle <- list() 
calibration_jmmle <- list() 
calibration_twosb <- list() 
calibration_jmb <- list() 

for (i in 1:nrow(parallel_logit_evaluation)){
  dat_parallel <- parallel_logit_pred_list[[i]]
  dat_mean <- mean_logit_pred_list[[i]]
  dat_max <- max_logit_pred_list[[i]]
  dat_window <- window_logit_pred_list[[i]]
  
  dat_twosmle <- TWOSMLE_pred_list[[i]]
  dat_jmmle <- JMMLE_pred_list[[i]]
  dat_twosb <- TWOSB_pred_list[[i]]
  dat_jmb <- JMB_pred_list[[i]]
  
  dat_parallel <- dat_parallel %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_parallel$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_parallel <- dat_parallel %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  dat_mean <- dat_mean %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_mean$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_mean <- dat_mean %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    )
  
  dat_max <- dat_max %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_max$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_max <- dat_max %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    )
  
  dat_window <- dat_window %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_window$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_window <- dat_window %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  dat_twosmle <- dat_twosmle %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_twosmle$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_twosmle <- dat_twosmle %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  dat_jmmle <- dat_jmmle %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_jmmle$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_jmmle <- dat_jmmle %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    )
  
  dat_twosb <- dat_twosb %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_twosb$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_twosb <- dat_twosb %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    ) 
  dat_jmb <- dat_jmb %>%
    mutate(pred_bin = cut(pred, breaks = quantile(dat_jmb$pred, probs = seq(0, 1, length.out = 6)), include.lowest = TRUE, labels = FALSE))
  
  cal_jmb <- dat_jmb %>%
    group_by(pred_bin) %>%
    dplyr::summarize(
      midpoint = mean(pred, na.rm = TRUE),
      Percent= mean(as.numeric(two_year_rej) == 1, na.rm = TRUE),
      .groups = 'drop'
    ) 
  
  Month <- c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24)[i]
  
  calibration_parallel[[i]] <- ggplot(cal_parallel, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "royalblue") +  
    geom_point(color = "royalblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_mean[[i]] <- ggplot(cal_mean, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "#8A3324") +  
    geom_point(color = "#8A3324") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_max[[i]] <- ggplot(cal_max, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "#d11141") +  
    geom_point(color = "#d11141") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_window[[i]] <- ggplot(cal_window, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "#FFC425") +  
    geom_point(color = "#FFC425") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_twosmle[[i]] <- ggplot(cal_twosmle, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "skyblue") +  
    geom_point(color = "skyblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) + 
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_jmmle[[i]] <- ggplot(cal_jmmle, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "lightcoral") +  
    geom_point(color = "lightcoral") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_twosb[[i]] <- ggplot(cal_twosb, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "#0FB859") +  
    geom_point(color = "#0FB859") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
  
  calibration_jmb[[i]] <- ggplot(cal_jmb, aes(x = midpoint, y = Percent)) +
    geom_smooth(method = "loess", se = FALSE, color = "purple") +  
    geom_point(color = "purple") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Probability", title = paste0(paste("Month " , Month, sep = ""))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    theme_bw() + 
    theme_minimal() + 
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.line = element_line(colour = "black"), 
      plot.title = element_text(size = 13)
    )
}

grid.arrange(grobs = calibration_parallel, ncol = 4)
grid.arrange(grobs = calibration_mean, ncol = 4)
grid.arrange(grobs = calibration_max, ncol = 4)
grid.arrange(grobs = calibration_window, ncol = 4)
grid.arrange(grobs = calibration_twosmle, ncol = 4)
grid.arrange(grobs = calibration_jmmle, ncol = 4)
grid.arrange(grobs = calibration_twosb, ncol = 4)
grid.arrange(grobs = calibration_jmb, ncol = 4)
