#################################################################################
## Plots for Individual Trajectories of Predicted Probabilities and Biomarkers ##
#################################################################################

concat.dat <- list()

pred.lists <- list(
  parallel_logit_pred_list, 
  mean_logit_pred_list,
  max_logit_pred_list,
  window_logit_pred_list, 
  TWOSMLE_pred_list,
  JMMLE_pred_list, 
  TWOSB_pred_list,
  JMB_pred_list)

for (i in 1:length(pred.lists)){
  for (j in seq_along(pred.lists[[i]])){
    month <- gsub("Month", "", names(pred.lists[[i]]))
    pred.lists[[i]][[j]]$Month <- as.numeric(month[j])
  }
}

for (i in seq_along(parallel_logit_pred_list)) {
  month <- gsub("Month", "", names(parallel_logit_pred_list))
  parallel_logit_pred_list[[i]]$source <- as.numeric(month[i]) 
}

methods <- c("Parallel", "Mean", "Max", "Window", "2SMLE", "JMMLE", "2SB", "JMB")

for (i in seq_along(pred.lists)) {
  concat.dat[[i]] <- bind_rows(pred.lists[[i]])
  concat.dat[[i]][, 5] <- methods[i]
  
  colnames(concat.dat[[i]]) <- c("USUBJID", "predicted_prob", "two_year_rej", "Month", "Method")
}

compare_method_list <- concat.dat

compare_method_dyn_pred <- function(compare_method_list, id){
  graph_compare_methods <- list() 
  
  dat_graph <- test_dat[test_dat$BGEVSNUM %in% 2:12 & as.character(test_dat$USUBJID) == id,]
  
  compare_method_dat <- do.call(rbind, lapply(compare_method_list, function(df) df[as.character(df$USUBJID) == id,]))
  compare_method_dat$Method <- factor(compare_method_dat$Method, levels = c("Parallel", "Mean", "Max", "Window", "2SMLE", "JMMLE", "2SB", "JMB"))
  
  unique_Month <- as.numeric(unique(compare_method_dat$Month))
  sorted_Month <- sort(unique_Month)
  compare_method_dat$Month <- factor(compare_method_dat$Month, levels = sorted_Month)
  
  category_colors <- c(
    "Parallel" = "royalblue",
    "Mean" = "#8A3324",
    "Max" = "#d11141",
    "Window" = "#FFC425",
    "2SMLE" = "skyblue",
    "JMMLE" = "lightcoral",
    "2SB" = "#0FB859",
    "JMB" = "purple"
  )
  
  method_linetypes <- c(
    "Parallel" = "dotted",
    "Mean" = "dotted",
    "Max" = "dotted",
    "Window" = "dotted",
    "2SMLE" = "solid",
    "JMMLE" = "solid",
    "2SB" = "solid",
    "JMB" = "solid"
  )
  
  method_order <- c("Parallel", "Mean", "Max", "Window", "2SMLE", "JMMLE", "2SB", "JMB")
  
  segments_lower <- dat_graph[which(!complete.cases(dat_graph[, "TruGraf"])) - 1, ][complete.cases(dat_graph[which(!complete.cases(dat_graph[, "TruGraf"])) - 1, ]$TruGraf),]
  segments_upper <- dat_graph[which(!complete.cases(dat_graph[, "TruGraf"])) + 1, ][complete.cases(dat_graph[which(!complete.cases(dat_graph[, "TruGraf"])) + 1, ]$TruGraf),]
  
  segments_dat <- cbind(segments_lower[, c("TruGraf", "TRAC", "Month")], segments_upper[, c("TruGraf", "TRAC", "Month")])
  colnames(segments_dat) <- c("TG.L", "TR.L", "Month.L",  "TG.U", "TR.U", "Month.U")
  
  graph_compare_methods[[1]] <- ggplot(data = dat_graph, aes(x = reorder(Month, Month), y = TruGraf, group = "USUBJID")) +
    geom_line(aes(color = "red")) +
    geom_segment(data = segments_dat, aes(x = reorder(Month.L, Month.L), xend = reorder(Month.U, Month.U), y = TG.L, yend = TG.U), color = "red", linetype = "dotted") +
    geom_point(size=2, shape=20) +
    theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + xlab("Month") + ylab ("TruGraf")
  
  graph_compare_methods[[2]] <- ggplot(data = dat_graph, aes(x = reorder(Month, Month), y = TRAC, group = "USUBJID")) +
    geom_line(aes(color = "red")) +
    geom_segment(data = segments_dat, aes(x = reorder(Month.L, Month.L), xend = reorder(Month.U, Month.U), y = TR.L, yend = TR.U), color = "red", linetype = "dotted") +
    geom_point(size=2, shape=20) +
    theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + xlab("Month") + ylab ("log(donor-derived cfDNA)")
  
  graph_compare_methods[[3]] <- ggplot(compare_method_dat, aes(x = reorder(Month, Month), y = as.numeric(predicted_prob), group = Method,  color = Method, linetype = Method)) +
    geom_line(size = 1) + 
    geom_point() + 
    scale_color_manual(values = category_colors) +
    scale_linetype_manual(values = method_linetypes, breaks = method_order) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.line = element_line(colour = "black")) + xlab("Method") + 
    labs(title = NULL, 
         x = "Month", 
         y = "Predicted Probability of SubAR") + 
    theme(legend.text = element_text(size = 6), 
          legend.key.size = unit(0.5, "lines")) + xlab("Month") 
  
  layout <- rbind(c(1, 1), c(2, 3))
  
  grid.arrange(graph_compare_methods[[3]], graph_compare_methods[[1]], graph_compare_methods[[2]], layout_matrix = layout)
}

unique(concat.dat[[1]]$USUBJID)
rej_subject <- concat.dat[[1]][concat.dat[[1]]$Month == 24 & concat.dat[[1]]$two_year_rej == 1, ]$USUBJID
no_rej_subject <- concat.dat[[1]][concat.dat[[1]]$Month == 24 & concat.dat[[1]]$two_year_rej == 0, ]$USUBJID

compare_method_dyn_pred(concat.dat, rej_subject[1]) 
compare_method_dyn_pred(concat.dat, no_rej_subject[1]) 