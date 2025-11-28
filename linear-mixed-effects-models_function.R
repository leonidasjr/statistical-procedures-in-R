# Load necessary libraries
library(tidyverse)
library(lme4)     # For the lmer() function
library(lmerTest) # For getting p-values and ANOVA results for fixed effects
library(performance) # For model assumption checks and R-squared

#' @title Perform Linear Mixed-Effects Model (LMM) Analysis
#'
#' @description This function fits a Linear Mixed-Effects Model, tests the
#' significance of fixed effects using Satterthwaite's approximation (via lmerTest),
#' and checks the model assumptions.
#'
#' @param data A data frame containing the data to analyze.
#' @param dependent_var A character string specifying the name of the numeric
#'        dependent (outcome) variable.
#' @param fixed_effects A character vector of the independent variables (IVs)
#'        to be included as fixed effects (e.g., c("FactorA", "CovariateX")).
#'        Interaction terms should be specified with a colon or asterisk (e.g., "FactorA:FactorB" or "FactorA*FactorB").
#' @param random_intercepts A character vector of variables that represent
#'        grouping factors for the random effects (e.g., c("SubjectID", "ItemID")).
#' @param alpha The significance level for reporting (default is 0.05).
#' @param output_path An optional file path to save the console output.
#'
#' @return A list containing the fitted LMM object and the ANOVA table for fixed effects.
#'
#' @examples
#' # LMM_results <- fit_lmm(
#' #   data = my_data,
#' #   dependent_var = "ReactionTime",
#' #   fixed_effects = c("Condition", "Age", "Condition:Age"),
#' #   random_intercepts = c("SubjectID", "TrialType")
#' # )
fit_lmm <- function(data, dependent_var, fixed_effects, random_intercepts, alpha = 0.05, output_path = NULL) {
  
  # 1. Formula Construction
  fixed_part <- paste(fixed_effects, collapse = " + ")
  random_part <- paste("(", 1, " | ", random_intercepts, ")", collapse = " + ")
  
  lmer_formula <- as.formula(paste(dependent_var, "~", fixed_part, "+", random_part))
  
  cat("LMM Formula:", deparse(lmer_formula), "\n\n")
  
  # 2. Optionally redirect output
  if (!is.null(output_path)) {
    sink(output_path)
    cat("--- Linear Mixed-Effects Model Results ---\n\n")
  }
  
  # 3. Fit the LMM
  cat("Fitting Model...\n")
  tryCatch({
    # Use lmerTest::lmer for p-values via Satterthwaite approximation
    model <- lmer(lmer_formula, data = data, REML = TRUE)
  }, error = function(e) {
    cat("\nERROR: Failed to fit the LMM. Check model complexity, data structure, and convergence issues.\n")
    cat("Original error message:", conditionMessage(e), "\n")
    if (!is.null(output_path)) {
      sink()
    }
    return(NULL)
  })
  
  if (is.null(model)) return(NULL)
  
  # 4. Model Summary and ANOVA
  cat("\n--- Model Summary (Fixed and Random Effects) ---\n")
  print(summary(model))
  
  cat("\n--- Type III ANOVA for Fixed Effects (using Satterthwaite's method) ---\n")
  # Use the standard anova() function from lmerTest to get Type III SS
  fixed_effects_anova <- anova(model, type = 3)
  print(fixed_effects_anova)
  
  cat("\n--- R-Squared and ICC ---\n")
  # Marginal R2 (Variance explained by fixed effects) and Conditional R2 (Fixed + Random)
  r_squared <- r2(model)
  print(r_squared)
  
  # Intraclass Correlation Coefficient (ICC) for random intercepts
  icc_values <- icc(model)
  cat("\nIntraclass Correlation Coefficient (ICC):\n")
  print(icc_values)
  
  # 5. Assumption Checks (via performance package)
  cat("\n--- LMM Assumption Checks ---\n")
  
  # Check 5a: Linearity/Homoscedasticity (Residuals vs. Fitted)
  check_resids <- check_model(model, check = c("linearity", "homogeneity"), verbose = FALSE)
  cat("Check Linearity/Homoscedasticity: Please inspect the plot for flat, horizontal trends.\n")
  # 
  
  [Image of check_model linearity plot]
  
  
  # Check 5b: Normality of Residuals
  normality_result <- check_normality(model, verbose = FALSE)
  cat("\nCheck Normality of Residuals (Shapiro-Wilk):\n")
  print(normality_result)
  
  # Check 5c: Multicollinearity
  vif_result <- check_collinearity(model)
  cat("\nCheck Multicollinearity (VIF):\n")
  print(vif_result)
  
  if (any(vif_result$VIF > 5)) {
    cat("**Warning:** Some VIF values are > 5, suggesting potential multicollinearity.\n")
  }
  
  # 6. Conclusion
  cat('\n=========================================\n')
  cat("Significant fixed effects (p <", alpha, "):\n")
  
  significant_effects <- fixed_effects_anova %>%
    filter(`Pr(>F)` < alpha) %>%
    select(Effect = "Pr(>F)")
  
  if (nrow(significant_effects) > 0) {
    print(significant_effects)
  } else {
    cat("No significant fixed effects found at p <", alpha, ".\n")
  }
  cat('\n')
  
  # Close the sink if it was opened
  if (!is.null(output_path)) {
    sink()
    cat(paste("\nOutput saved to:", output_path, "\n"))
  }
  
  # Return the model and results
  return(list(
    model = model,
    anova_table = fixed_effects_anova,
    r_squared = r_squared
  ))
}

# --- Example of use (User must customize the following lines) ---

# # ASSUMPTIONS:
# # - 'my_data' is your loaded dataset.
# # - "Outcome" is a numeric column.
# # - "Group" and "Time" are factors or numeric covariates.
# # - "Subject" is the random grouping variable.

# # 1. Define variables
# outcome_variable <- "Your_Dependent_Variable"
# fixed_vars <- c("Group", "Time", "Group:Time") # Include interaction
# random_vars <- c("Subject_ID")

# # 2. Define output path
# lmm_output_path <- "PATH/TO/SAVE/LMM_RESULTS.txt"

# # 3. Run the LMM analysis
# # lmm_results <- fit_lmm(
# #   data = my_data,
# #   dependent_var = outcome_variable,
# #   fixed_effects = fixed_vars,
# #   random_intercepts = random_vars,
# #   output_path = lmm_output_path
# # )