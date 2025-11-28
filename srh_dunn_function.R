# Load necessary libraries
library(tidyverse)
library(rcompanion) # For the Scheirer-Ray-Hare test
library(FSA)         # For Dunn's post-hoc test

#' @title Perform Scheirer-Ray-Hare (SRH) Non-Parametric 2-Way Factorial Test
#'
#' @description This function performs the Scheirer-Ray-Hare test, a non-parametric
#' alternative to 2-Way ANOVA, followed by Dunn's test for significant main effects
#' or interaction, where appropriate.
#'
#' @param data A data frame containing the data to analyze.
#' @param factor_var1 A character string specifying the name of the first factor variable.
#' @param factor_var2 A character string specifying the name of the second factor variable.
#' @param numeric_vars A character vector of the names of the numeric (dependent) variables to test.
#' @param alpha The significance level (default is 0.05).
#' @param output_path An optional file path to save the console output.
#'
#' @return A list of lists, where the top level keys are the significant numeric variables
#'         and the inner list contains the results (p-values) for significant effects.
#'
#' @examples
#' # results_srh <- perform_srh_test(
#' #   data = my_data,
#' #   factor_var1 = "FactorA",
#' #   factor_var2 = "FactorB",
#' #   numeric_vars = c("NonNormalScore")
#' # )
perform_srh_test <- function(data, factor_var1, factor_var2, numeric_vars, alpha = 0.05, output_path = NULL) {
  
  # Check if all variables exist
  required_vars <- c(factor_var1, factor_var2, numeric_vars)
  if (!all(required_vars %in% names(data))) {
    missing_vars <- setdiff(required_vars, names(data))
    stop(paste("The following variables are missing from the data frame:", paste(missing_vars, collapse = ", ")))
  }
  
  # Convert factor variables to factor
  data[[factor_var1]] <- as.factor(data[[factor_var1]])
  data[[factor_var2]] <- as.factor(data[[factor_var2]])
  
  # Define the full formula string for the SRH test
  formula_str <- as.formula(paste("~", factor_var1, "*", factor_var2))
  
  # Optionally redirect output
  if (!is.null(output_path)) {
    sink(output_path)
    cat("--- Scheirer-Ray-Hare Test Results ---\n\n")
  }
  
  all_significant_results <- list()
  
  for (var in numeric_vars) {
    cat('==================================================================\n')
    cat('Dependent Variable:', var, '\n')
    cat('==================================================================\n')
    
    # 1. Perform Scheirer-Ray-Hare Test
    # SRH is sensitive to unbalanced designs; it uses Type II SS by default.
    srh_model <- scheirerRayHare(
      as.formula(paste(var, formula_str)),
      data = data,
      type = 2 # Type II Sum of Squares (standard for unbalanced data)
    )
    
    # Print SRH results
    cat("Scheirer-Ray-Hare Test Results (Non-parametric 2-Way):\n")
    print(srh_model)
    cat("\n")
    
    # Extract effects and p-values
    srh_results <- srh_model$p.value
    interaction_effect <- paste(factor_var1, ":", factor_var2, sep="")
    
    # 2. Post-hoc Analysis for Significant Effects
    significant_effects <- names(srh_results[srh_results < alpha])
    
    if (length(significant_effects) > 0) {
      cat('--- SIGNIFICANT EFFECTS (p <', alpha, ') ---\n')
      
      var_significant_results <- list()
      
      # 2a. Interaction Effect Priority
      if (interaction_effect %in% significant_effects) {
        cat("Interaction Effect is SIGNIFICANT. Post-hoc on interaction groups recommended.\n")
        var_significant_results[[interaction_effect]] <- srh_results[interaction_effect]
        
        # Combine factors into a single interaction group variable for post-hoc Dunn's test
        data$InteractionGroup <- interaction(data[[factor_var1]], data[[factor_var2]], sep = " & ")
        
        cat("Performing Dunn's Post-hoc Test on all Interaction Groups:\n")
        
        # Post-hoc Dunn's Test
        dunn_results <- dunnTest(
          as.formula(paste(var, "~ InteractionGroup")),
          data = data,
          method = "bonferroni"
        )
        
        print(dunn_results$res)
        
        cat("\nSignificant Pairwise Comparisons (Bonferroni-adjusted p <", alpha, "):\n")
        significant_pairs <- dunn_results$res[dunn_results$res$P.adj < alpha, ]
        if (nrow(significant_pairs) > 0) {
          print(significant_pairs)
        } else {
          cat("No pairwise comparisons were significant after adjustment.\n")
        }
        
      } else {
        # 2b. Main Effects (if interaction is NOT significant)
        cat("Interaction Effect is NOT significant. Examining Main Effects...\n")
        
        for (effect in c(factor_var1, factor_var2)) {
          if (effect %in% significant_effects) {
            cat("\nMain Effect of", effect, "is SIGNIFICANT.\n")
            var_significant_results[[effect]] <- srh_results[effect]
            
            # Perform post-hoc Dunn's test on the main effect factor
            cat("Performing Dunn's Post-hoc Test on Main Effect of", effect, ":\n")
            
            dunn_results <- dunnTest(
              as.formula(paste(var, "~", effect)),
              data = data,
              method = "bonferroni"
            )
            
            print(dunn_results$res)
            
            cat("\nSignificant Pairwise Comparisons (Bonferroni-adjusted p <", alpha, "):\n")
            significant_pairs <- dunn_results$res[dunn_results$res$P.adj < alpha, ]
            if (nrow(significant_pairs) > 0) {
              print(significant_pairs)
            } else {
              cat("No pairwise comparisons were significant after adjustment.\n")
            }
          }
        }
      }
      all_significant_results[[var]] <- var_significant_results
    } else {
      cat('--- NO SIGNIFICANT EFFECTS ---\n')
    }
    cat('\n')
  }
  
  # Close the sink if it was opened
  if (!is.null(output_path)) {
    sink()
    cat(paste("\nOutput saved to:", output_path, "\n"))
  }
  
  # Return the list of significant variables and their effects
  return(all_significant_results)
}