#' Plot Gait Curves with FGDI Overlay
#'
#' Plots gait curves comparing healthy controls to individuals with highest deviation.
#'
#' @param joint_index Index of the joint angle to plot.
#' @param title Title for the plot.
#' @param FGDI Output list from FGDI() function.
#' @param Data List of matrices containing the gait data.
#' @param ID Subject group labels.
#' @param combined Logical indicating if you want to see the combined FGDI results (TRUE). This approach yields a measure
#' of severity by collectively considering both legs, and displays the maximum gait abnormality. If FALSE
#' the max gait pathology is displayed for each leg individually left in green and right in blue. 
#'
#' @return A ggplot object.
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer 
#' @importFrom magrittr %>% 
#' @importFrom stats sd
#' @importFrom ggplot2 ggplot aes geom_line ggtitle xlab ylab theme theme_bw scale_color_manual
#' @name plot_gait_comparison

plot_gait_comparison <- function(joint_index, title, FGDI, Data, ID, combined) {
  # Indices for control and key subjects
  ind_H <- which(ID == "Control")
  
  if(combined==TRUE){
  ind <- which.max(FGDI$zFGDI)  
  }else{
  ind_SL <- which.max(FGDI$SFGDIL)
  ind_SR <- which.max(FGDI$SFGDIR)
  }
  # Sequence for x-axis
  xvals <- seq(0, ncol(Data[[joint_index]])-1, 1)
  
  # Healthy matrix: rows = subjects, cols = timepoints (51)
  healthy_matrix <- as.matrix(Data[[joint_index]][ind_H, ])
  colnames(healthy_matrix) <- xvals
  healthy_df <- as.data.frame(healthy_matrix)
  healthy_df$Subject <- paste0("S", seq_len(nrow(healthy_df)))
  
  # Long format for healthy group
  healthy_long <- pivot_longer(healthy_df, 
                               cols = -Subject, 
                               names_to = "Time", 
                               values_to = "Angle") %>%
    mutate(Time = as.numeric(Time))
  
  
  if(combined==TRUE){
    # Overlay: healthy mean, worst LHS, worst RHS
    df_overlay <- data.frame(
      Time = rep(xvals, 2),
      Angle = c(
        colMeans(healthy_matrix),
        as.numeric(Data[[joint_index]][ind, ])
      ),
      Group = factor(rep(c("Healthy Mean", "Max"), each = length(xvals)))
    )
  }else{
  # Overlay: healthy mean, worst LHS, worst RHS
  df_overlay <- data.frame(
    Time = rep(xvals, 3),
    Angle = c(
      colMeans(healthy_matrix),
      as.numeric(Data[[joint_index]][ind_SL, ]),
      as.numeric(Data[[joint_index]][ind_SR, ])
    ),
    Group = factor(rep(c("Healthy Mean", "MAX LHS", "MAX RHS"), each = length(xvals)))
  )
  }
  
  # Build the plot
  p <-ggplot(data = healthy_long) +  
    geom_line(data = healthy_long, aes(x = Time, y = Angle, group = Subject),
              color = "lightgrey", linewidth = 0.5) +
    geom_line(data = df_overlay, aes(x = Time, y = Angle, group = Group, color = Group),
              linewidth = 1) +
    ggtitle(title) +
    xlab("% of Gait cycle") +
    ylab("Angle") +
    theme_bw() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "green", "blue"))
  
  return(p)
}