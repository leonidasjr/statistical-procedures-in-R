# Load necessary libraries (tidyverse for data wrangling)
library(tidyverse)
library(rstatix) # Optional, but useful for calculating effect size (rank-biserial correlation)

#' @title Perform the Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
#'
#' @description Compares the distributions of two independent groups using the
#' non-parametric Mann-Whitney U test. It also calculates the rank-biserial
#' correlation (r) as a measure of effect size.
#'
#' @param data A data frame containing the data to analyze.
#' @param dependent_var A character string specifying the numeric dependent variable.
#' @param grouping_var A character string specifying the two-level grouping variable.
#' @param alpha The significance level (default is 0.05).
#' @param output_path An optional file path to save the console output.
#'
#' @return A list containing the test result object and the effect size (r).
#'
#' @examples
#' # results_mw <- perform_mann_whitney_u(
#' #   data = my_non_normal_data,
#' #   dependent_var = "Score",
#' #   grouping_var = "TreatmentGroup"
#' # )
perform_mann_whitney_u <- function(data, dependent_var, grouping_var, alpha = 0.05, output_path = NULL) {
  
  # 1. Setup and Input Validation
  if (!dependent_var %in% names(data)) {
    stop(paste("Dependent variable '", dependent_var, "' not found in the data frame.", sep = ""))
  }
  if (!grouping_var %in% names(data)) {
    stop(paste("Grouping variable '", grouping_var, "' not found in the data frame.", sep = ""))
  }
  
  group_levels <- unique(data[[grouping_var]])
  if (length(group_levels) != 2) {
    stop(paste("Grouping variable '", grouping_var, "' must have exactly two unique levels. Found:", length(group_levels)))
  }
  
  # Ensure grouping variable is treated as a factor
  data[[grouping_var]] <- as.factor(data[[grouping_var]])
  
  formula_str <- paste(dependent_var, "~", grouping_var)
  
  if (!is.null(output_path)) {
    sink(output_path)
    cat("--- Mann-Whitney U Test (Wilcoxon Rank-Sum) Results ---\n\n")
  }
  
  cat("Formula:", formula_str, "\n")
  cat("Groups being compared:", paste(group_levels, collapse = " vs "), "\n\n")
  
  # 2. Perform the Mann-Whitney U Test
  cat("Performing Test...\n")
  tryCatch({
    # The default wilcox.test() uses the W statistic (Wilcoxon Rank-Sum)
    # which is equivalent to the Mann-Whitney U test.
    # We use exact = FALSE for continuity correction when sample size is large, which is robust.
    test_result <- wilcox.test(as.formula(formula_str), data = data, exact = FALSE, correct = TRUE)
  }, error = function(e) {
    cat("\nERROR: Failed to run the Mann-Whitney U test.\n")
    cat("Original error message:", conditionMessage(e), "\n")
    if (!is.null(output_path)) {
      sink()
    }
    return(NULL)
  })
  
  if (is.null(test_result)) return(NULL)
  
  # 3. Calculate Effect Size (Rank-Biserial Correlation)
  # This uses the rstatix package for simple calculation
  effect_size_r <- data %>% wilcox_effsize(as.formula(formula_str))
  
  # 4. Print Results
  cat("\n--- Mann-Whitney U Test Statistical Output ---\n")
  print(test_result)
  
  p_value <- test_result$p.value
  
  cat("\n--- Effect Size (Rank-Biserial Correlation, r) ---\n")
  r_value <- effect_size_r$effsize[1]
  cat(paste("r =", round(r_value, 3), " (Magnitude: ", effect_size_r$magnitude[1], ") \n", sep=""))
  
  cat('\n--- Conclusion ---\n')
  
  # Interpret the effect size magnitude
  effect_interpretation <- case_when(
    abs(r_value) < 0.3 ~ "small",
    abs(r_value) < 0.5 ~ "moderate",
    TRUE ~ "large"
  )
  
  if (p_value < alpha) {
    cat(paste("The difference between the two groups is **statistically significant** (p = ", round(p_value, 4), ").\n", sep=""))
    cat(paste("There is a **", effect_interpretation, "** effect size (r = ", round(r_value, 3), ").\n", sep=""))
  } else {
    cat(paste("The difference between the two groups is **not statistically significant** (p = ", round(p_value, 4), ").\n", sep=""))
    cat(paste("The effect size is **", effect_interpretation, "** (r = ", round(r_value, 3), "), but not statistically significant.\n", sep=""))
  }
  
  # 5. Descriptive Statistics (Medians and IQR)
  cat('\n--- Descriptive Statistics ---\n')
  descriptives <- data %>%
    group_by(!!sym(grouping_var)) %>%
    summarise(
      N = n(),
      Median = median(!!sym(dependent_var), na.rm = TRUE),
      IQR = IQR(!!sym(dependent_var), na.rm = TRUE),
      .groups = 'drop'
    )
  print(descriptives)
  cat("Reported values are Median and Interquartile Range (IQR).\n")
  
  # Close the sink if it was opened
  if (!is.null(output_path)) {
    sink()
    cat(paste("\nOutput saved to:", output_path, "\n"))
  }
  
  # Return the results
  return(list(
    test_result = test_result,
    effect_size = effect_size_r,
    descriptives = descriptives
  ))
}

# --- Example of use (User must customize the following lines) ---

# # ASSUMPTIONS:
# # - 'my_data' is your loaded dataset.
# # - "NonNormalOutcome" is the numeric dependent variable.
# # - "Condition" is the two-level grouping factor (e.g., "A" and "B").

# # 1. Define variables
# outcome_variable <- "NonNormalOutcome"
# grouping_factor <- "Condition"

# # 2. Define output path
# mw_output_path <- "PATH/TO/SAVE/mann_whitney_u_results.txt"

# # 3. Run the analysis
# # mw_results <- perform_mann_whitney_u(
# #   data = my_data,
# #   dependent_var = outcome_variable,
# #   grouping_var = grouping_factor,
# #   output_path = mw_output_path
# # )