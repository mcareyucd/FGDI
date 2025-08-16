# --- Load required data and functions ---
data(Amutee_Data)

# --- Assign group labels and compute FGDI scores ---
ID <- c(rep("Case", 18), rep("Control", 42))
FGDI <- FGDI(A_Data, ID, 0.99)

# --- Extract relevant metadata ---
Side <- as.factor(A_data_info$Amputation.side)
KLevel <- A_data_info$K.Level
AS <- A_data_info$Amputation.side
ind_L <- which(Side == "Left")
ind_R <- which(Side == "Right")

# --- Amputated leg analysis ---
data_amputated <- data.frame(
  sFGDI = c(FGDI$SFGDIL[ind_L], FGDI$SFGDIR[ind_R]),
  Severity = as.factor(c(KLevel[ind_L], KLevel[ind_R])),
  AS = as.factor(c(AS[ind_L], AS[ind_R]))
)

# Perform one-sided Wilcoxon test
wilcox.test(sFGDI ~ Severity, data = data_amputated, alternative = "greater")

# --- Plot: sFGDI on Amputated Leg ---
library(ggpubr)
p1 <- ggline(data_amputated, x = "Severity", y = "sFGDI",
             add = c("mean_se", "dotplot"),
             ylab = "sFGDI on the Amputated Leg", xlab = "K-Level") +
  scale_color_grey(start = 0.8, end = 0.2)

# --- Non-amputated leg analysis ---
data_non_amputated <- data.frame(
  sFGDI = c(FGDI$SFGDIL[ind_R], FGDI$SFGDIR[ind_L]),
  Severity = as.factor(c(KLevel[ind_R], KLevel[ind_L])),
  AS = as.factor(c(AS[ind_R], AS[ind_L]))
)

# Perform one-sided Wilcoxon test
wilcox.test(sFGDI ~ Severity, data = data_non_amputated, alternative = "greater")

# --- Plot: sFGDI on Non-Amputated Leg ---
p2 <- ggline(data_non_amputated, x = "Severity", y = "sFGDI",
             add = c("mean_se", "dotplot"),
             ylab = "sFGDI on the non-Amputated Leg", xlab = "K-Level") +
  scale_color_grey(start = 0.8, end = 0.2)

# --- Display Amputated vs. Non-Amputated Comparison ---
library(gridExtra)
grid.arrange(p1, p2, ncol = 2, nrow = 1)


# --- Plot all joint angles ---
titles <- c(
  "LHS Pelvis tilt", "LHS Pelvis obliquity", "LHS Pelvis rotation",
  "LHS Hip flexion/extension", "LHS Hip add/abduction", "LHS Hip rotation",
  "LHS Knee flexion/extension", "LHS Ankle dorsi/plantarflexion", "LHS Foot int/external rotation",
  "RHS Hip flexion/extension", "RHS Hip add/abduction", "RHS Hip rotation",
  "RHS Knee flexion/extension", "RHS Ankle dorsi/plantarflexion", "RHS Foot int/external rotation"
)

joint_indices <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 17, 18)
plots <- mapply(
  FUN = plot_gait_comparison,
  joint_index = joint_indices,
  title = titles,
  MoreArgs = list(FGDI = FGDI, Data = A_Data, ID = ID, combined=FALSE),
  SIMPLIFY = FALSE
)

# Arrange all plots in a grid
grid.arrange(grobs = plots, ncol = 3, nrow = 5, heights = rep(4, 5))
