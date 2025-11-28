# Load necessary libraries
library(tidyverse)
library(rstatix) # For Levene's test and convenient ANOVA/Tukey wrappers

#' @title Perform ANOVA and Post-hoc Tukey HSD for Multiple Variables
#'
#' @description This function iterates through a list of numeric variables,
#' performs an ANOVA against a specified factor variable, checks assumptions,
#' and conducts a post-hoc Tukey HSD test if the ANOVA is significant.
#' It uses Welch's ANOVA if the homogeneity of variance assumption is violated.
#'
#' @param data A data frame containing the data to analyze.
#' @param factor_var A character string specifying the name of the factor
#'        (grouping) variable in the data frame.
#' @param numeric_vars A character vector of the names of the numeric
#'        (dependent) variables to test.
#' @param alpha The significance level (default is 0.05).
#' @param output_path An optional file path to save the console output.
#'
#' @return A list where names are the significant numeric variables (ANOVA p < alpha)
#'         and values are their corresponding ANOVA p-values.
#'
#' @examples
#' # significant_anova_results <- perform_anova_tukey(
#' #   data = my_data,
#' #   factor_var = "Group",
#' #   numeric_vars = c("VarA", "VarB", "VarC")
#' # )
perform_anova_tukey <- function(data, factor_var, numeric_vars, alpha = 0.05, output_path = NULL) {
  
  # Check if all variables exist
  if (!all(c(factor_var, numeric_vars) %in% names(data))) {
    missing_vars <- setdiff(c(factor_var, numeric_vars), names(data))
    stop(paste("The following variables are missing from the data frame:", paste(missing_vars, collapse = ", ")))
  }
  
  # Convert factor variable to factor in case it's not already
  data[[factor_var]] <- as.factor(data[[factor_var]])
  
  # Optionally redirect output
  if (!is.null(output_path)) {
    sink(output_path)
    cat("--- ANOVA and Tukey HSD Results ---\n\n")
  }
  
  significant_vars <- list()
  
  for (var in numeric_vars) {
    cat('==================================================================\n')
    cat('Dependent Variable:', var, '\n')
    cat('==================================================================\n')
    
    # 1. Assumption Check: Homogeneity of Variances (Levene's Test)
    levene_test <- levene_test(data, as.formula(paste(var, "~", factor_var)))
    homogeneity_p <- levene_test$p
    
    cat("Levene's Test (Homogeneity of Variances): p =", round(homogeneity_p, 4), "\n")
    
    # 2. Perform ANOVA
    if (homogeneity_p >= alpha) {
      cat("Assumption met: Using standard One-Way ANOVA.\n")
      model <- anova_test(data, as.formula(paste(var, "~", factor_var)))
      anova_type <- "Standard ANOVA"
      anova_p <- model$p
    } else {
      cat("Assumption violated: Using **Welch's One-Way ANOVA** (more robust to unequal variances).\n")
      model <- welch_anova_test(data, as.formula(paste(var, "~", factor_var)))
      anova_type <- "Welch's ANOVA"
      anova_p <- model$p
    }
    
    # Print ANOVA results
    cat(anova_type, "Results:\n")
    print(model)
    cat("\n")
    
    # Check for significance
    if (anova_p < alpha) {
      significant_vars[[var]] <- anova_p
      cat('--- SIGNIFICANT ANOVA (p <', alpha, ') ---\n')
      
      # 3. Post-hoc Test
      if (anova_type == "Standard ANOVA") {
        # Perform Tukey HSD only for standard ANOVA
        cat("Performing Tukey HSD Post-hoc Test:\n")
        posthoc <- data %>% tukey_hsd(as.formula(paste(var, "~", factor_var)))
      } else {
        # Perform Games-Howell for Welch's ANOVA (more appropriate when variances are unequal)
        cat("Performing Games-Howell Post-hoc Test (recommended for Welch's ANOVA):\n")
        posthoc <- data %>% games_howell_test(as.formula(paste(var, "~", factor_var)))
      }
      
      print(posthoc)
      cat("\nSignificant Pairwise Comparisons (p <", alpha, "):\n")
      # Filter and print significant pairs
      significant_pairs <- posthoc %>% filter(p.adj < alpha)
      if (nrow(significant_pairs) > 0) {
        print(significant_pairs)
      } else {
        cat("No pairwise comparisons were significant after adjustment.\n")
      }
    } else {
      cat('--- NOT SIGNIFICANT ---\n')
    }
    
    # 4. Optional: Check Normality of Residuals
    # Note: Normality check is often less critical for large samples (Central Limit Theorem)
    # and for balanced designs, but still good to check.
    # We use a base R aov() model to extract residuals
    resid_model <- aov(as.formula(paste(var, "~", factor_var)), data = data)
    normality_test <- shapiro.test(residuals(resid_model))
    cat("\nShapiro-Wilk Test on Residuals (Normality): p =", round(normality_test$p.value, 4), "\n")
    if (normality_test$p.value < alpha) {
      cat("**Warning:** Normality of residuals assumption may be violated (p <", alpha, ").\n")
    } else {
      cat("Assumption met: Residuals appear normally distributed.\n")
    }
    cat('\n')
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

# # ASSUMPTIONS:
# # - 'your_data' is your loaded dataset.
# # - The previous script's 'significant_results' (from Kruskal-Dunn) is not used here.
# #   This function takes a specific list of numeric variables for ANOVA.

# # 1. Define the variables for testing (these must be numeric columns)
# anova_vars <- c("Var_A", "Var_B", "Var_C")

# # 2. Define your factor variable (grouping column)
# factor_variable_name <- "Group"

# # 3. Define the output path
# anova_output_path <- "PATH/TO/SAVE/ANOVA_RESULTS.txt"

# # 4. Run the ANOVA analysis
# # anova_significant_results <- perform_anova_tukey(
# #   data = your_data,
# #   factor_var = factor_variable_name,
# #   numeric_vars = anova_vars,
# #   output_path = anova_output_path
# # )