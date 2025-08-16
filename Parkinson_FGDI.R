# ================================
# Load Data and Supporting Scripts
# ================================
data(Parkinson_Data)

# ================================
# Compute FGDI Scores
# ================================

ID <- c(rep("Case", 21), rep("Control", 42))
FGDI <- FGDI(P_Data, ID, 0.99)

# =======================================
# Visualise FGDI Component Scores in a MAP
# =======================================

loc <- c("Pel tilt", "Hip flex", "Hip flex", "Knee flex", "Knee flex",
         "Ank dors", "Ank dors", "Pel obl", "Hip abd", "Hip abd",
         "Pel rot", "Hip rot", "Hip rot", "Foot rot", "Foot rot")

side <- c("Both", "Left", "Right", "Left", "Right", "Left", "Right",
          "Both", "Left", "Right", "Both", "Left", "Right", "Left", "Right")

ind <- c(1, 4, 13, 7, 16, 8, 14, 2, 5, 11, 3, 6, 15, 9, 18)
ind_max <- which.max(abs(FGDI$zFGDI))

Scores <- data.frame(
  y = round(FGDI$zFGDIU[ind_max, ind]),
  loc = loc,
  side = side
)

library(forcats)
library(ggplot2)

Scores %>%
  mutate(loc = fct_relevel(loc, 
                           "Pel tilt", "Hip flex", "Knee flex",
                           "Ank dors", "Pel obl", "Hip abd",
                           "Pel rot", "Hip rot", "Foot rot")) %>%
  ggplot(aes(x = loc, y = y, fill = side)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(y)), vjust = -0.5, size = 3.5, position = position_dodge(width = 1)) +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme_minimal() +
  labs(x = "Kinematic Variable", y = "sFGDI for a Subject with PD")

# ================================
# FGDI vs Clinical Severity Scales
# ================================

# Extract severity scales from clinical data
Scale  <- as.numeric(P_data_info$OFF...Hoehn...Yahr[1:26])
Scale1 <- as.numeric(P_data_info$OFF...UPDRS.II[1:26])
Scale2 <- as.numeric(P_data_info$OFF...UPDRS.III[1:26])
Scale3 <- as.factor(P_data_info$FoG.group[1:26])

# Subject indices used for plotting
ind_I <- c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 24)

# ================================
# FGDI vs Hoehn & Yahr
# ================================

# Combine data into data frame for Kruskal-Wallis test
data <- data.frame(
  sFGDI = FGDI$zFGDI[1:21],
  Severity = Scale[ind_I]
)

kruskal.test(sFGDI ~ Severity, data = data)

q1 <- qplot(Severity, sFGDI, data = data, main = "Combined Approach", xlab = "Hoehn & Yahr") +
  theme(plot.title = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 9))

# ================================
# FGDI vs Freezing of gait
# ================================

# Freezing of gait group analysis
data <- data.frame(
  sFGDI = FGDI$zFGDI[1:21],
  Severity = Scale3[ind_I]
)

res <- wilcox.test(sFGDI ~ Severity, data = data, exact = FALSE, alternative = "greater")
print(res)

q1a <- qplot(Severity, sFGDI, data = data, main = "Combined Approach", xlab = "Freezing of gait") +
  theme(plot.title = element_text(size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 9))

# ================================
# FGDI vs MDS-UPDRS Part II
# ================================

# Linear regression with MDS-UPDRS Part II
data <- data.frame(
  sFGDI = FGDI$zFGDI[1:21],
  Severity = Scale1[ind_I]
)

ggplotRegression <- function(fit) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "grey", lty = 2) +
    theme(plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 9)) +
    labs(title = paste("Slope =", signif(fit$coef[[2]], 3),
                       "P =", signif(summary(fit)$coef[2, 4], 3)),
         x = "MDS-UPDRS Part II", y = "sFGDI")
}

q2 <- ggplotRegression(lm(sFGDI ~ Severity, data = data))

# Linear regression with MDS-UPDRS Part III
data <- data.frame(
  sFGDI = FGDI$zFGDI[1:21],
  Severity = Scale2[ind_I]
)

q3 <- ggplotRegression(lm(sFGDI ~ Severity, data = data))

# Arrange the clinical comparison plots
library(gridExtra)
grid.arrange(q1, q1a, q2, q3, ncol = 4, nrow = 1)

# ================================
# Plot FGDI Curves for All Joints
# ================================

titles <- c(
  "LHS Pelvis tilt", "LHS Pelvis obliquity", "LHS Pelvis rotation",
  "LHS Hip flexion/extension", "LHS Hip add/abduction", "LHS Hip rotation",
  "LHS Knee flexion/extension", "LHS Ankle dorsi/plantarflexion", "LHS Foot int/external rotation",
  "RHS Hip flexion/extension", "RHS Hip add/abduction", "RHS Hip rotation",
  "RHS Knee flexion/extension", "RHS Ankle dorsi/plantarflexion", "RHS Foot int/external rotation"
)

joint_indices <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 17, 18)

# Generate plots using the plot_gait_comparison() function
plots <- mapply(
  FUN = plot_gait_comparison,
  joint_index = joint_indices,
  title = titles,
  MoreArgs = list(FGDI = FGDI, Data = P_Data, ID = ID, combined = TRUE),
  SIMPLIFY = FALSE
)

# Display all joint plots in a grid
grid.arrange(grobs = plots, ncol = 3, nrow = 5, heights = rep(4, 5))
