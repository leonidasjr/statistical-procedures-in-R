# Load necessary libraries
# plots and some stats
library(tidyverse)
library(gridExtra)
library(rlang)

# Dunn's test
library(FSA)

## Kruskal-Wallis test & pairwise Dunn's test ####

#' @title Perform Kruskal-Wallis Test and Pairwise Dunn's Test
#'
#' @description This function iterates through all numeric variables in a dataset
#' and performs a Kruskal-Wallis test against a specified factor variable.
#' For variables where the Kruskal-Wallis p-value is < 0.05, it then performs
#' a pairwise Dunn's post-hoc test with Bonferroni correction.
#'
#' @param data A data frame containing the data to analyze.
#' @param factor_var A character string specifying the name of the factor
#'        (grouping) variable in the data frame.
#' @param output_path An optional character string specifying the file path
#'        where the console output should be saved. If NULL (default),
#'        output is printed only to the console.
#'
#' @return A list where names are the significant numeric variables (p < 0.05)
#'         and values are their corresponding Kruskal-Wallis p-values.
#'
#' @examples
#' # Assuming 'my_data' is your dataset and 'Group' is your factor variable
#' # significant_results <- perform_kruskal_dunn(my_data, factor_var = "Group")
perform_kruskal_dunn <- function(data, factor_var, output_path = NULL) {
  
  # Check if the factor variable exists
  if (!factor_var %in% names(data)) {
    stop(paste("Factor variable '", factor_var, "' not found in the data frame.", sep = ""))
  }
  
  # Optionally redirect output to a file
  if (!is.null(output_path)) {
    sink(output_path)
    cat("--- Kruskal-Wallis and Dunn's Test Results ---\n\n")
  }
  
  # Extract numeric variables (excluding the factor variable)
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  numeric_vars <- setdiff(numeric_vars, factor_var)
  
  # Initialize an empty list to store significant variables
  significant_vars <- list()
  
  # Loop through numeric variables
  for (var in numeric_vars) {
    formula_str <- paste(var, "~", factor_var)
    
    # Kruskal-Wallis test
    model <- kruskal.test(as.formula(formula_str), data = data)
    p_value <- model$p.value
    
    # Compute effect size (Eta-squared)
    # H = model$statistic, n = number of non-missing observations
    n <- sum(!is.na(data[[var]]) & !is.na(data[[factor_var]]))
    eta_squared <- as.numeric(model$statistic) / (n - 1)
    
    if (p_value < 0.05) {
      significant_vars[[var]] <- p_value
      
      # Print Kruskal-Wallis test summary and effect size (eta-squared)
      cat("Variable:", var, "\n")
      print(model)
      cat("Effect size (Eta-squared):", round(eta_squared, 4), "\n\n")
      
      # Dunn's test for pairwise comparisons
      dunn_results <- dunnTest(as.formula(formula_str), data = data, method = "bonferroni")
      cat("Dunn's test for pairwise comparisons of", var, " (Bonferroni-adjusted):\n")
      print(dunn_results)
      cat("\n")
      
      # Print unadjusted p-values < 0.05 (for specific reporting needs)
      significant_comparisons <- dunn_results$res[dunn_results$res$P.unadj < 0.05, ]
      if (nrow(significant_comparisons) > 0) {
        cat("Significant pairwise comparisons for", var, "with P.unadj < 0.05:\n")
        print(significant_comparisons)
        cat("\n")
      }
      
      cat('=========================\n')
    }
  }
  
  # Close the sink if it was opened
  if (!is.null(output_path)) {
    sink()
    cat(paste("\nOutput saved to:", output_path, "\n"))
  }
  
  # Return the list of significant variables
  return(significant_vars)
}

# --- Example of use (User must customize the following lines) ---

# # 1. LOAD YOUR DATASET: Replace 'PATH/TO/YOUR/FILE.txt' and 'your_data'
# your_data <- read.delim("PATH/TO/YOUR/FILE.txt", stringsAsFactors = TRUE)

# # 2. SPECIFY YOUR FACTOR VARIABLE: Replace 'YourFactorVariable'
# factor_variable_name <- "YourFactorVariable" # e.g., "Group", "Condition", "LANGUAGE"

# # 3. RUN THE ANALYSIS: Replace 'PATH/TO/SAVE/RESULTS.txt' or set to NULL
# # Save the output to a file and get the list of significant variables
# significant_vars_list <- perform_kruskal_dunn(
#   data = your_data,
#   factor_var = factor_variable_name,
#   output_path = "PATH/TO/SAVE/RESULTS.txt"
# )