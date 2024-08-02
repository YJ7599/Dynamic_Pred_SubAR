##################
## Data Upload ##
##################

# Load the predictive performance evaluation results which is the result of 'R_Standard_Approaches.R' & 'R_Advanced_Approaches.R' 
parallel_evaluation <- read.csv(".../parallel_logit_evaluation.csv")
mean_logit_evaluation <- read.csv(".../mean_logit_evaluation.csv")
max_logit_evaluation <- read.csv(".../max_logit_evaluation.csv") 
window_logit_evaluation <- read.csv(".../window_logit_evaluation.csv") 
TWOSMLE_evaluation <- read.csv(".../2SMLE_evaluation.csv")
JMMLE_evaluation <- read.csv(".../JMMLE_evaluation.csv")
TWOSB_evaluation <- read.csv(".../2SB_evaluation.csv")
JMB_evaluation <- read.csv(".../JMB_evaluation.csv")

#######################################
## Dynamic AUC (ROC) Trajectory Plot ##
#######################################

parallel_auc <- round(mean(parallel_evaluation$AUC_ROC), 2) 
mean_auc <- round(mean(mean_logit_evaluation$AUC_ROC), 2) 
max_auc <- round(mean(max_logit_evaluation$AUC_ROC), 2) 
window_auc <- round(mean(window_logit_evaluation$AUC_ROC), 2) 
TWOSMLE_auc <- round(mean(TWOSMLE_evaluation$AUC_ROC), 2) 
JMMLE_auc <- round(mean(JMMLE_evaluation$AUC_ROC), 2) 
TWOSB_auc <- round(mean(TWOSB_evaluation$AUC_ROC), 2)
JMB_auc <- round(mean(JMB_evaluation$AUC_ROC), 2)

plot_parallel <- ggplot(parallel_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "royalblue", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(-0.7, 1.8, 2, -0.7, -0.7, 1.8, -0.7, -0.7, 1.8, 1.8, -0.7),  na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") +  
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = parallel_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1.)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_mean <- ggplot(mean_logit_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "#8A3324", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(-0.7, 1.8, -0.7, 1.8, -0.7,rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") +  
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = mean_logit_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_max <- ggplot(max_logit_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "#d11141", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(1.8, -0.7, 1.8, -0.7, 1.8, rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") +
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = max_logit_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1.3)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_window <- ggplot(window_logit_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "#E6AC20", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(1.8, -0.7, 1.8, -0.7, -0.7, -0.7, -0.7, 1.8, -0.7, -0.7, -0.7), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") +
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = window_logit_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_TWOSMLE <- ggplot(TWOSMLE_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "skyblue", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(1.8, -0.7, 1.8, -0.7, rep(-0.7, 7)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") + 
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = TWOSMLE_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_JMMLE <- ggplot(JMMLE_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "lightcoral", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(1.8, 1.8, 1.8, -0.7, rep(-0.7, 7)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") +
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = JMMLE_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_TWOSB <- ggplot(TWOSB_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "#0FB859", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(-0.7, -0.7, 1.8, -0.7, rep(-0.7, 3), 1.8, rep(-0.7, 3)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") + 
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = TWOSB_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_JMB <- ggplot(JMB_evaluation) + geom_line(aes(x = Month, y = AUC_ROC), color = "purple", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = AUC_ROC), size = 2) + geom_text(aes(x = Month, y = AUC_ROC, label = round(AUC_ROC, 2)), size = 3, vjust = c(1.8, -0.7, 1.8, -0.7, rep(-0.7, 7)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "AUC (ROC)") + 
  coord_cartesian(ylim = c(0.3, 1)) + 
  geom_hline(yintercept = JMB_evaluation$AUC_ROC[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_parallel <- plot_parallel + annotation_custom(
  grob = textGrob(
    label = c(
      paste("Parallel cross-sectional logistic regression models (Overall AUC:", parallel_auc, ")")
    ),
    x = 0.05, 
    y = 0.975,
    just = "left", 
    gp = gpar(col = "royalblue", cex = 0.85, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_mean <- plot_mean + annotation_custom(
  grob = textGrob(
    label = c(
      paste("Multiple logistic regression model using mean exposure (Overall AUC:", mean_auc, ")")
    ),
    x = 0.05, 
    y = 0.975,
    just = "left", 
    gp = gpar(col = "#8A3324", cex = 0.85, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_max <- plot_max + annotation_custom(
  grob = textGrob(
    label = c(
      paste("Multiple logistic regression model using max exposure (Overall AUC:", max_auc, ")")
    ),
    x = 0.05, 
    y = 0.975,
    just = "left", 
    gp = gpar(col = "#d11141", cex = 0.85, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_window <- plot_window + annotation_custom(
  grob = textGrob(
    label = c(
      paste("Multiple logistic regression model with 4/3/4 windows (Overall AUC:", window_auc, ")")
    ),
    x = 0.05,
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "#E6AC20", cex = 0.85, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_TWOSMLE <- plot_TWOSMLE + annotation_custom(
  grob = textGrob(
    label = c(
      paste("2SMLE (Overall AUC:", TWOSMLE_auc, ")")
    ),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "skyblue", cex = 0.85, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_JMMLE <- plot_JMMLE + annotation_custom(
  grob = textGrob(
    label = c(
      paste("JMMLE (Overall AUC:", JMMLE_auc, ")")
    ),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "lightcoral", cex = 0.85, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_TWOSB <- plot_TWOSB + annotation_custom(
  grob = textGrob(
    label = c(
      paste("2SB (Overall AUC:", TWOSB_auc, ")")
    ),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "#0FB859", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_JMB <- plot_JMB + annotation_custom(
  grob = textGrob(
    label = c(
      paste("JMB (Overall AUC:", JMB_auc, ")")
    ),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "purple", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

grid.arrange(plot_parallel, plot_mean, plot_max, plot_window, ncol = 2)
grid.arrange(plot_TWOSMLE, plot_JMMLE, plot_TWOSB, plot_JMB, ncol = 2)

whole_compare_auc <- cbind(parallel_evaluation$Month, parallel_evaluation$AUC_ROC, mean_logit_evaluation$AUC_ROC, max_logit_evaluation$AUC_ROC, window_logit_evaluation$AUC_ROC, TWOSMLE_evaluation$AUC_ROC, JMMLE_evaluation$AUC_ROC, TWOSB_evaluation$AUC_ROC, JMB_evaluation$AUC_ROC)
colnames(whole_compare_auc) <- c("Month", "Parallel", "Mean", "Max", "Window", "TWOSMLE","JMMLE", "TWOSB", "JMB")
whole_compare_auc <- data.frame(whole_compare_auc)
auc_score_long <- pivot_longer(whole_compare_auc, cols = -Month, names_to = "Method", values_to = "AUC")

method_colors <- c(
  "Parallel" = "royalblue",
  "Mean" = "#8A3324",
  "Max" = "#d11141",
  "Window" = "#E6AC20",
  "TWOSMLE" = "skyblue",
  "JMMLE" = "lightcoral",
  "TWOSB" = "#0FB859",
  "JMB" = "purple"
)

# Define specific linetypes for each method
method_linetypes <- c(
  "Parallel" = "dotted",
  "Mean" = "dotted",
  "Max" = "dotted",
  "Window" = "dotted",
  "TWOSMLE" = "solid",
  "JMMLE" = "solid",
  "TWOSB" = "solid",
  "JMB" = "solid"
)

# Define the order of methods in the legend
method_order <- c("Parallel", "Mean", "Max", "Window", "TWOSMLE", "JMMLE", "TWOSB", "JMB")

# Create the line plot
auc_compare_vis <- ggplot(auc_score_long, aes(x = Month, y = AUC, color = Method, linetype = Method)) +
  geom_line(size = 1) +  # Adjust line width if needed
  scale_color_manual(values = method_colors, breaks = method_order) +
  scale_linetype_manual(values = method_linetypes, breaks = method_order) +
  theme_minimal() +
  labs(title = "AUC (ROC) Over Time",
       x = "Month",
       y = "AUC (ROC)") +
  theme_bw() +
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(size = 13),
    legend.position = "none"  # This line removes the legend
  ) +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

auc_compare_vis + annotation_custom(
  grob = textGrob(
    label = c(
      paste("Parallel cross-sectional logistic regression models (Overall AUC:", parallel_auc, ")"),
      paste("Multiple logistic regression model using mean exposure (Overall AUC:", mean_auc, ")"),
      paste("Multiple logistic regression model using max exposure (Overall AUC:", max_auc, ")"),
      paste("Multiple logistic regression model with 4/3/4 windows (Overall AUC:", window_auc, ")"), 
      paste("2SMLE (Overall AUC:", TWOSMLE_auc, ")"),
      paste("JMMLE (Overall AUC:", JMMLE_auc, ")"),
      paste("2SB (Overall AUC:", TWOSB_auc, ")"),
      paste("JMB (Overall AUC:", JMB_auc, ")")
    ),
    x = rep(0.65, 4), 
    y = c(0.4, 0.35, 0.3, 0.25, 0.2, 0.1, 0.15, 0.05),
    just = "left", 
    gp = gpar(col = c("royalblue", "#8A3324", "#d11141", "#E6AC20", "skyblue", "lightcoral", "#0FB859", "purple"), cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)