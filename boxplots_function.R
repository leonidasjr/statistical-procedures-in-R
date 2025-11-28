## BOXPLOTS ####

#' @title Generate a Grid of Customized Boxplots
#'
#' @description This function generates a series of boxplots for specified
#' numeric variables against a factor variable, allowing for customized
#' axis labels, factor levels, and colors. The plots are arranged in a grid.
#'
#' @param data A data frame containing the data to plot.
#' @param factor_var A character string specifying the name of the factor
#'        (grouping) variable in the data frame.
#' @param plot_vars A character vector of the names of the numeric variables
#'        to be plotted on the y-axis.
#' @param y_axis_labels A character vector of the desired display names
#'        for the y-axis of each corresponding variable in \code{plot_vars}.
#' @param factor_levels An optional character vector specifying the desired
#'        order of the factor levels. If NULL, current order is used.
#' @param factor_labels An optional character vector specifying the labels
#'        to display for the factor levels in the plot. Must match the order
#'        of \code{factor_levels} if provided.
#' @param custom_colors A named character vector for fill colors (e.g.,
#'        c("Label1" = "red", "Label2" = "blue")). Names must match the
#'        \code{factor_labels}.
#' @param n_row The desired number of rows in the final plot grid.
#' @param n_col The desired number of columns in the final plot grid.
#'
#' @return A \code{gtable} object (from \code{gridExtra::grid.arrange})
#'         containing the arranged boxplots.
#'
#' @examples
#' # significant_vars <- c("VarA", "VarB", "VarC")
#' # renamed_vars <- c("Variable A", "Variable B", "Variable C")
#' # factor_levels <- c("Control", "Treatment1", "Treatment2")
#' # factor_labels <- c("Ctrl\nGrp", "Treat\n1", "Treat\n2")
#' # custom_colors <- c("Ctrl\nGrp" = "#E41A1C", "Treat\n1" = "#377EB8", "Treat\n2" = "#4DAF4A")
#'
#' # plot_grid <- generate_boxplots(
#' #   data = my_data,
#' #   factor_var = "Group",
#' #   plot_vars = significant_vars,
#' #   y_axis_labels = renamed_vars,
#' #   factor_levels = factor_levels,
#' #   factor_labels = factor_labels,
#' #   custom_colors = custom_colors,
#' #   n_row = 1,
#' #   n_col = 3
#' # )
#' # ggsave("my_boxplots.png", plot_grid, width = 15, height = 5)
generate_boxplots <- function(data, factor_var, plot_vars, y_axis_labels,
                              factor_levels = NULL, factor_labels = NULL,
                              custom_colors = NULL, n_row = 2, n_col = 5) {
  
  if (length(plot_vars) != length(y_axis_labels)) {
    stop("Length of 'plot_vars' must match length of 'y_axis_labels'.")
  }
  
  plot_data <- data # Create a copy for manipulation
  
  # 1. Rename and reorder the factor variable
  if (!is.null(factor_levels) && !is.null(factor_labels)) {
    plot_data[[factor_var]] <- factor(plot_data[[factor_var]],
                                      levels = factor_levels,
                                      labels = factor_labels)
  }
  
  # 2. Define colors based on the final factor levels/labels
  if (is.null(custom_colors)) {
    # If no custom colors, use ggplot default color scheme
    color_map <- NULL
  } else {
    # Use custom colors
    color_map <- custom_colors
    if (!all(names(custom_colors) %in% levels(plot_data[[factor_var]]))) {
      warning("Not all names in 'custom_colors' match the final factor labels/levels.")
    }
  }
  
  # 3. Create a list to store plots
  plot_list <- list()
  
  # 4. Generate plots
  for (i in seq_along(plot_vars)) {
    var <- plot_vars[i]
    y_label <- y_axis_labels[i]
    panel_letter <- LETTERS[i] # A, B, C, ...
    
    # Get y-axis range to position the panel letter
    # Use quantile to avoid outlier stretching the annotation far away from plot area
    y_range <- range(plot_data[[var]], na.rm = TRUE)
    # y_max <- max(y_range[2], quantile(plot_data[[var]], 0.95, na.rm=TRUE)) * 1.05
    y_max <- y_range[2] + (y_range[2] - y_range[1]) * 0.05 # Place slightly above max
    
    p <- ggplot(plot_data, aes(x = !!sym(factor_var), y = !!sym(var), fill = !!sym(factor_var))) +
      geom_boxplot(color = "black") +
      
      # Use scale_fill_manual if custom_colors were provided
      if (!is.null(color_map)) {
        scale_fill_manual(values = color_map)
      } else {
        # If no custom colors, ggplot will use default
        scale_fill_discrete()
      } +
      
      # Annotation position: x=Inf aligns to the far right of the plot area, using geom_text
      geom_text(aes(x = Inf, y = y_max, label = panel_letter),
                size = 6, fontface = "bold", hjust = 1.2, inherit.aes = FALSE,
                data = data.frame(y_max = y_max, panel_letter = panel_letter)) +
      
      theme_classic() +
      labs(x = NULL, y = y_label) +
      theme(
        plot.title = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 14),
        legend.position = "none"
      )
    
    plot_list[[i]] <- p
  }
  
  # 5. Arrange plots in a grid
  final_plot <- grid.arrange(grobs = plot_list, nrow = n_row, ncol = n_col)
  
  return(final_plot)
}

# --- Example of use (User must customize the following lines) ---

# # ASSUMPTIONS:
# # - 'your_data' is your loaded dataset.
# # - 'YourFactorVariable' is your factor column name (e.g., "LANGUAGE").

# # 1. Define the variables for plotting
# variables_to_plot <- c("f1", "f2", "f3", "f4", "f1norm",
#                        "f1sd", "f2sd", "f3sd", "f4sd", "d1f2")

# # 2. Define the display names for the y-axis
# y_labels_for_plot <- c("f1 (Hz)", "f2 (Hz)", "f3 (Hz)", "f4 (Hz)", "f1 (z-score)",
#                        "f1 st.dev.", "f2 st.dev.", "f3 st.dev.", "f4 st.dev.", "∂f2 (Hz/s)")

# # 3. Define the factor levels and labels
# # Original levels in your data
# original_levels <- c("EL1", "EL2", "PL1")
# # Labels you want to appear on the x-axis
# display_labels <- c("English\nL1", "English\nL2", "Portuguese\nL1")

# # 4. Define custom fill colors (Names must match 'display_labels')
# custom_fill_colors <- c("English\nL1" = "#E41A1C", # Red
#                         "English\nL2" = "#377EB8", # Blue
#                         "Portuguese\nL1" = "#4DAF4A") # Green

# # 5. Generate and arrange the boxplots
# # final_plots <- generate_boxplots(
# #   data = your_data,
# #   factor_var = "YourFactorVariable", # e.g., "LANGUAGE"
# #   plot_vars = variables_to_plot,
# #   y_axis_labels = y_labels_for_plot,
# #   factor_levels = original_levels,
# #   factor_labels = display_labels,
# #   custom_colors = custom_fill_colors,
# #   n_row = 2,
# #   n_col = 5
# # )

# # 6. Save the final grid plot (optional)
# # ggsave("my_final_boxplots.png", final_plots, width = 15, height = 8)