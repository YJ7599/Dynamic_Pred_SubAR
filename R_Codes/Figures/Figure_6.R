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

###########################################
## Dynamic Brier Score Trajectory Plot ##
###########################################

parallel_brier <- round(mean(parallel_evaluation$brier_sc), 2) 
mean_brier <- round(mean(mean_logit_evaluation$brier_sc), 2) 
max_brier <- round(mean(max_logit_evaluation$brier_sc), 2) 
window_brier <- round(mean(window_logit_evaluation$brier_sc), 2) 
TWOSMLE_brier <- round(mean(TWOSMLE_evaluation$brier_sc), 2) 
JMMLE_brier <- round(mean(JMMLE_evaluation$brier_sc), 2) 
TWOSB_brier <- round(mean(TWOSB_evaluation$brier_sc), 2)
JMB_brier <- round(mean(JMB_evaluation$brier_sc), 2)

plot_parallel <- ggplot(parallel_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "royalblue", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(1.8, 1.8, -0.7, 2, 2, -0.7, 2, rep(-0.7, 4)),  na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = parallel_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_mean <- ggplot(mean_logit_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "#8A3324", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(1.8, 1.8, -0.7, 1.8, rep(-0.7, 7)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = mean_logit_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_max <- ggplot(max_logit_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "#d11141", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(-0.7, -0.7, 1.8, -0.7, 1.8, rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = max_logit_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_window <- ggplot(window_logit_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "#E6AC20", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(1.8, 1.8, -0.7, 1.8, 1.8, rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = window_logit_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_TWOSMLE <- ggplot(TWOSMLE_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "skyblue", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(1.8, 1.8, 1.8, 1.8, 1.8, rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = TWOSMLE_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_JMMLE <- ggplot(JMMLE_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "lightcoral", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(1.8, -0.7, -0.7, 1.8, 1.8, rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = JMMLE_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_TWOSB <- ggplot(TWOSB_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "#0FB859", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(-0.7, 1.8, -0.7, 1.8, 1.8, -0.7, -0.7, 1.8, rep(-0.7, 3)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = TWOSB_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_JMB <- ggplot(JMB_evaluation) + geom_line(aes(x = Month, y = brier_sc), color = "purple", lwd = 1, lty = 1) + 
  geom_point(aes(x = Month, y = brier_sc), size = 2) + geom_text(aes(x = Month, y = brier_sc, label = round(brier_sc, 2)), size = 3, vjust = c(1.8, -0.7, -0.7, 1.8, 1.8, rep(-0.7, 6)), na.rm = TRUE) + 
  labs(title = NULL, x = "Months After KT", y = "Brier Score") + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  geom_hline(yintercept = JMB_evaluation$brier_sc[1], linetype = "dashed") + 
  geom_vline(xintercept = 24, linetype = "dashed") + 
  theme_bw() + theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(size = 13)) + ylim(c(0, 1)) + scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 9, 12, 15, 18, 21, 24))

plot_parallel <- plot_parallel + annotation_custom(
  grob = textGrob(
    label = paste("Parallel cross-sectional logistic regression models (Overall brier:", parallel_brier, ")"), 
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "royalblue", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_mean <- plot_mean + annotation_custom(
  grob = textGrob(
    label = paste("Multiple logistic regression model using mean exposure (Overall brier:", mean_brier, ")"),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "#8A3324", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_max <- plot_max + annotation_custom(
  grob = textGrob(
    label = paste("Multiple logistic regression model using max exposure (Overall brier:", max_brier, ")"),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "#d11141", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_window <- plot_window + annotation_custom(
  grob = textGrob(
    label = paste("Multiple logistic regression model with 4/3/4 windows (Overall brier:", window_brier, ")"), 
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "#E6AC20", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_TWOSMLE <- plot_TWOSMLE + annotation_custom(
  grob = textGrob(
    label = paste("2SMLE (Overall brier:", TWOSMLE_brier, ")"), 
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "skyblue", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_JMMLE <- plot_JMMLE + annotation_custom(
  grob = textGrob(
    label = paste("JMMLE (Overall brier:", JMMLE_brier, ")"),
    x = 0.05, 
    y = 0.975, 
    just = "left",  
    gp = gpar(col = "lightcoral", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_TWOSB <- plot_TWOSB + annotation_custom(
  grob = textGrob(
    label = paste("2SB (Overall brier:", TWOSB_brier, ")"),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "#0FB859", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

plot_JMB <- plot_JMB + annotation_custom(
  grob = textGrob(
    label = paste("JMB (Overall brier:", JMB_brier, ")"),
    x = 0.05, 
    y = 0.975, 
    just = "left", 
    gp = gpar(col = "purple", cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

grid.arrange(plot_parallel, plot_mean, plot_max, plot_window, ncol = 2)
grid.arrange(plot_TWOSMLE, plot_JMMLE, plot_TWOSB, plot_JMB, ncol = 2)

whole_compare_brier_sc <- cbind(parallel_evaluation$Month, parallel_evaluation$brier_sc, mean_logit_evaluation$brier_sc, max_logit_evaluation$brier_sc, window_logit_evaluation$brier_sc, TWOSMLE_evaluation$brier_sc, JMMLE_evaluation$brier_sc, TWOSB_evaluation$brier_sc, JMB_evaluation$brier_sc)
colnames(whole_compare_brier_sc) <- c("Month", "Parallel", "Mean", "Max", "Window", "TWOSMLE", "JMMLE", "TWOSB", "JMB")
whole_compare_brier_sc <- data.frame(whole_compare_brier_sc) 
brier_score_long <- pivot_longer(whole_compare_brier_sc, cols = -Month, names_to = "Method", values_to = "brier_sc")

brier_compare_vis <- ggplot(brier_score_long, aes(x = Month, y = brier_sc, color = Method, linetype = Method)) +
  geom_line(size = 1) +  # Adjust line width if needed
  scale_color_manual(values = method_colors, breaks = method_order) +
  scale_linetype_manual(values = method_linetypes, breaks = method_order) +
  theme_minimal() +
  labs(title = "Brier Score Over Time",
       x = "Month",
       y = "Brier Score") +
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

brier_compare_vis + annotation_custom(
  grob = textGrob(
    label = c(
      paste("Parallel cross-sectional logistic regression models (Overall Brier Score:", parallel_brier, ")"),
      paste("Multiple logistic regression model using mean exposure (Overall Brier Score:", mean_brier, ")"),
      paste("Multiple logistic regression model using max exposure (Overall Brier Score:", max_brier, ")"),
      paste("Multiple logistic regression model with 4/3/4 windows (Overall Brier Score:", window_brier, ")"), 
      paste("2SMLE (Overall Brier Score:", TWOSMLE_brier, ")"),
      paste("JMMLE (Overall Brier Score:", JMMLE_brier, ")"),
      paste("2SB (Overall Brier Score:", TWOSB_brier, ")"),
      paste("JMB (Overall Brier Score:", JMB_brier, ")")
    ),
    x = rep(0.05, 4), 
    y = c(0.4, 0.35, 0.3, 0.25, 0.2, 0.1, 0.15, 0.05),
    just = "left", 
    gp = gpar(col = c("royalblue", "#8A3324", "#d11141", "#E6AC20", "skyblue", "lightcoral", "#0FB859", "purple"), cex = 0.7, fontface = "bold")
  ),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)
