# Load necessary libraries
library(tidyverse)
library(rstatix) # For Levene's test, ANOVA, and post-hoc tests

#' @title Perform 2-Way ANOVA and Post-hoc Tests
#'
#' @description This function performs a 2-Way ANOVA to test the main effects
#' of two independent factors and their interaction on a list of numeric variables.
#' It includes checks for homogeneity of variances and proceeds with appropriate
#' post-hoc tests (Tukey HSD or Games-Howell) for significant effects.
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
#' # results_2way_anova <- perform_2way_anova(
#' #   data = my_data,
#' #   factor_var1 = "Gender",
#' #   factor_var2 = "Treatment",
#' #   numeric_vars = c("ScoreA", "ScoreB")
#' # )
perform_2way_anova <- function(data, factor_var1, factor_var2, numeric_vars, alpha = 0.05, output_path = NULL) {
  
  # Check if all variables exist
  required_vars <- c(factor_var1, factor_var2, numeric_vars)
  if (!all(required_vars %in% names(data))) {
    missing_vars <- setdiff(required_vars, names(data))
    stop(paste("The following variables are missing from the data frame:", paste(missing_vars, collapse = ", ")))
  }
  
  # Convert factor variables to factor
  data[[factor_var1]] <- as.factor(data[[factor_var1]])
  data[[factor_var2]] <- as.factor(data[[factor_var2]])
  
  # Define the full formula string for the 2-Way ANOVA
  formula_str <- paste("~", factor_var1, "*", factor_var2) # Factor1 + Factor2 + Factor1:Factor2
  
  # Optionally redirect output
  if (!is.null(output_path)) {
    sink(output_path)
    cat("--- 2-Way ANOVA Results ---\n\n")
  }
  
  all_significant_results <- list()
  
  for (var in numeric_vars) {
    cat('==================================================================\n')
    cat('Dependent Variable:', var, '\n')
    cat('==================================================================\n')
    
    # 1. Assumption Check: Homogeneity of Variances (Levene's Test)
    # The grouping variable for Levene's test is the interaction term
    interaction_group <- paste(factor_var1, ":", factor_var2)
    levene_formula <- as.formula(paste(var, "~", interaction_group))
    
    levene_test <- levene_test(data, levene_formula)
    homogeneity_p <- levene_test$p
    
    cat("Levene's Test (Homogeneity of Variances based on interaction groups): p =", round(homogeneity_p, 4), "\n")
    
    # 2. Perform 2-Way ANOVA
    if (homogeneity_p >= alpha) {
      cat("Assumption met: Using standard 2-Way ANOVA (Type II Sum of Squares).\n")
      # Note: We use Type II SS, which is standard when interaction is not significant,
      # but rstatix's anova_test handles the general case well.
      model <- data %>%
        anova_test(as.formula(paste(var, formula_str)))
      anova_type <- "Standard ANOVA"
    } else {
      cat("Assumption violated: **Standard ANOVA results should be interpreted cautiously**. \n")
      cat("Note: No automatic Welch's correction exists for the interaction term in 2-way ANOVA. Proceeding with standard ANOVA, but results for main effects may be unreliable.\n")
      model <- data %>%
        anova_test(as.formula(paste(var, formula_str)))
      anova_type <- "Standard ANOVA (Heteroscedasticity Warning)"
    }
    
    # Print ANOVA results
    cat(anova_type, "Results:\n")
    print(model)
    cat("\n")
    
    # 3. Post-hoc Analysis for Significant Effects
    significant_effects <- model %>% filter(p < alpha)
    
    if (nrow(significant_effects) > 0) {
      cat('--- SIGNIFICANT EFFECTS (p <', alpha, ') ---\n')
      
      var_significant_results <- list()
      
      # 3a. Interaction Effect
      interaction_row <- significant_effects %>% filter(Effect == interaction_group)
      if (nrow(interaction_row) > 0) {
        cat("Interaction Effect is SIGNIFICANT. Main effects should be ignored.\n")
        
        var_significant_results[[interaction_group]] <- interaction_row$p
        
        # Post-hoc for interaction: Simple Main Effects
        cat("Performing Simple Main Effects Post-hoc:\n")
        # Effect of Factor 1 at each level of Factor 2
        posthoc_f1 <- data %>%
          group_by(!!sym(factor_var2)) %>%
          anova_test(as.formula(paste(var, "~", factor_var1))) %>%
          add_significance()
        
        cat("\nEffect of", factor_var1, "at each level of", factor_var2, ":\n")
        print(posthoc_f1)
        
        # Effect of Factor 2 at each level of Factor 1
        posthoc_f2 <- data %>%
          group_by(!!sym(factor_var1)) %>%
          anova_test(as.formula(paste(var, "~", factor_var2))) %>%
          add_significance()
        
        cat("\nEffect of", factor_var2, "at each level of", factor_var1, ":\n")
        print(posthoc_f2)
        
      } else {
        # 3b. Main Effects (if interaction is NOT significant)
        cat("Interaction Effect is NOT significant. Examining Main Effects...\n")
        
        for (effect in c(factor_var1, factor_var2)) {
          main_effect_row <- significant_effects %>% filter(Effect == effect)
          
          if (nrow(main_effect_row) > 0) {
            cat("\nMain Effect of", effect, "is SIGNIFICANT.\n")
            var_significant_results[[effect]] <- main_effect_row$p
            
            # Perform pairwise comparison (Tukey or Games-Howell)
            cat("Performing Tukey HSD Post-hoc on Main Effect of", effect, ":\n")
            
            # Recreate formula for post-hoc on marginal means
            posthoc_formula <- as.formula(paste(var, "~", effect))
            
            posthoc_result <- data %>%
              tukey_hsd(posthoc_formula) %>%
              add_significance()
            
            print(posthoc_result)
            
            cat("\nSignificant Pairwise Comparisons (p <", alpha, "):\n")
            significant_pairs <- posthoc_result %>% filter(p.adj < alpha)
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
    
    # 4. Normality Check (Residuals from the full model)
    resid_model <- aov(as.formula(paste(var, formula_str)), data = data)
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
  
  # Return the list of significant variables and their effects
  return(all_significant_results)
}