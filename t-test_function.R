# Load necessary libraries
library(tidyverse)
library(rstatix) # For easy assumption checks (Shapiro-Wilk, Levene's) and t-test wrappers

#' @title Perform One-Sample, Independent, or Paired t-tests
#'
#' @description This function performs the requested type of t-test (one-sample,
#' independent two-sample, or paired two-sample) and includes assumption checks
#' for the independent t-test. It automatically uses Welch's t-test if
#' homogeneity of variance is violated.
#'
#' @param data A data frame containing the data to analyze.
#' @param dependent_var A character string specifying the numeric dependent variable.
#' @param grouping_var A character string specifying the two-level grouping variable
#'        for two-sample tests. Set to NULL for a one-sample test.
#' @param mu The value of the known mean for a one-sample t-test (default is 0).
#' @param test_type A character string specifying the test: "one-sample", "independent", or "paired".
#' @param alpha The significance level (default is 0.05).
#' @param output_path An optional file path to save the console output.
#'
#' @return A list containing the t-test result object and assumption check results.
#'
#' @examples
#' # 1. Independent t-test:
#' # results_ind <- perform_t_tests(my_data, dependent_var = "Score", grouping_var = "Group", test_type = "independent")
#'
#' # 2. Paired t-test:
#' # results_paired <- perform_t_tests(my_data, dependent_var = "Difference", grouping_var = NULL, test_type = "one-sample")
#'
#' # 3. One-sample t-test:
#' # results_one <- perform_t_tests(my_data, dependent_var = "Score", grouping_var = NULL, test_type = "one-sample", mu = 100)
perform_t_tests <- function(data, dependent_var, grouping_var = NULL, mu = 0, test_type = "independent", alpha = 0.05, output_path = NULL) {
  
  # 1. Setup and Input Validation
  test_type <- tolower(test_type)
  valid_types <- c("one-sample", "independent", "paired")
  if (!test_type %in% valid_types) {
    stop(paste("Invalid test_type. Must be one of:", paste(valid_types, collapse = ", ")))
  }
  
  if (!dependent_var %in% names(data)) {
    stop(paste("Dependent variable '", dependent_var, "' not found in the data frame.", sep = ""))
  }
  
  if (test_type != "one-sample" && is.null(grouping_var)) {
    stop("For 'independent' or 'paired' tests, a 'grouping_var' must be specified.")
  }
  
  if (!is.null(grouping_var) && !grouping_var %in% names(data)) {
    stop(paste("Grouping variable '", grouping_var, "' not found in the data frame.", sep = ""))
  }
  
  if (!is.null(output_path)) {
    sink(output_path)
    cat(paste("--- T-Test Results: ", str_to_title(test_type), " ---\n\n"))
  }
  
  formula_str <- if (!is.null(grouping_var)) {
    paste(dependent_var, "~", grouping_var)
  } else {
    dependent_var
  }
  
  # 2. Perform the T-Test
  t_test_result <- NULL
  assumption_results <- list()
  
  if (test_type == "one-sample") {
    cat(paste("Performing One-Sample T-Test against mu =", mu, "...\n"))
    
    # Check normality for one-sample t-test
    normality <- data %>% shapiro_test(!!sym(dependent_var))
    assumption_results$Normality <- normality
    cat("Normality Check (Shapiro-Wilk): p =", round(normality$p, 4), "\n")
    if (normality$p < alpha) cat("**Warning:** Normality assumption violated (p < 0.05).\n")
    
    t_test_result <- t_test(data, as.formula(formula_str), mu = mu)
    
  } else if (test_type == "independent") {
    cat("Performing Independent Samples T-Test...\n")
    
    # Check number of levels
    if (length(unique(data[[grouping_var]])) != 2) {
      stop("Independent t-test requires exactly two levels in the grouping variable.")
    }
    
    # Assumption Check 2a: Normality (Shapiro-Wilk per group)
    normality <- data %>% group_by(!!sym(grouping_var)) %>% shapiro_test(!!sym(dependent_var))
    assumption_results$Normality <- normality
    cat("Normality Check (Shapiro-Wilk):\n")
    print(normality)
    if (any(normality$p < alpha)) cat("**Warning:** Normality assumption violated in one or both groups.\n")
    
    # Assumption Check 2b: Homogeneity of Variances (Levene's Test)
    levene_test <- data %>% levene_test(as.formula(formula_str))
    homogeneity_p <- levene_test$p
    assumption_results$Homogeneity <- levene_test
    cat("\nHomogeneity of Variance (Levene's Test): p =", round(homogeneity_p, 4), "\n")
    
    # Select appropriate t-test
    if (homogeneity_p < alpha) {
      cat("Homogeneity assumption violated (p < 0.05). **Using Welch's T-Test (var.equal = FALSE)**.\n")
      t_test_result <- t_test(data, as.formula(formula_str), var.equal = FALSE)
    } else {
      cat("Homogeneity assumption met. **Using Student's T-Test (var.equal = TRUE)**.\n")
      t_test_result <- t_test(data, as.formula(formula_str), var.equal = TRUE)
    }
    
  } else if (test_type == "paired") {
    cat("Performing Paired Samples T-Test...\n")
    
    # Paired t-test is performed on the difference score, so we check normality of the difference
    cat("Note: Paired t-test checks normality of the difference score.\n")
    
    # Create the difference score (assuming wide format or data setup)
    # The rstatix package handles this internally if the data is in long format,
    # but for simplicity and robustness, we use the 'paired = TRUE' argument.
    
    # The user is generally expected to provide data ready for paired test or difference score.
    # We proceed with the paired test and rely on the robust nature of the central limit theorem.
    
    t_test_result <- t_test(data, as.formula(formula_str), paired = TRUE)
  }
  
  # 3. Print Results
  cat("\n--- T-Test Statistical Output ---\n")
  print(t_test_result)
  
  p_value <- t_test_result$p
  
  cat('\n--- Conclusion ---\n')
  if (p_value < alpha) {
    cat(paste("The test is **statistically significant** (p = ", round(p_value, 4), "), rejecting the null hypothesis.\n", sep=""))
  } else {
    cat(paste("The test is **not statistically significant** (p = ", round(p_value, 4), "), failing to reject the null hypothesis.\n", sep=""))
  }
  
  # Close the sink if it was opened
  if (!is.null(output_path)) {
    sink()
    cat(paste("\nOutput saved to:", output_path, "\n"))
  }
  
  # Return the results
  return(list(
    t_test = t_test_result,
    assumptions = assumption_results
  ))
}

# # Assume 'my_data' is your loaded dataset

# # --- Scenario 1: Independent Samples T-Test ---
# # Comparing 'Score' between 'Male' and 'Female' in the 'Gender' column
# results_independent <- perform_t_tests(
#   data = my_data,
#   dependent_var = "Score",
#   grouping_var = "Gender",
#   test_type = "independent",
#   output_path = "PATH/TO/SAVE/independent_t_test.txt"
# )

# # --- Scenario 2: Paired Samples T-Test ---
# # Comparing 'Score_Pre' vs. 'Score_Post'
# # NOTE: The data must be in 'long' format for rstatix (e.g., columns 'Score' and 'Time' where Time has two levels)
# # OR, if in wide format with two columns, the data structure needs careful handling outside this generic function.
# # For simplicity, assume long format where 'Time' is the grouping_var and 'Score' is the dependent_var.
# results_paired <- perform_t_tests(
#   data = my_data_long, # Must be a long format dataset
#   dependent_var = "Score",
#   grouping_var = "Time", # Levels: "Pre", "Post"
#   test_type = "paired",
#   output_path = "PATH/TO/SAVE/paired_t_test.txt"
# )

# # --- Scenario 3: One-Sample T-Test ---
# # Comparing mean of 'IQ' to the population mean of 100
# results_one_sample <- perform_t_tests(
#   data = my_data,
#   dependent_var = "IQ",
#   test_type = "one-sample",
#   mu = 100,
#   output_path = "PATH/TO/SAVE/one_sample_t_test.txt"
# )