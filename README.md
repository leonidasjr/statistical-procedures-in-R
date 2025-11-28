# statistical-procedures-in-R
This repository provides a collection of reusable R functions designed to facilitate reproducible and robust automated-based statistical analysis commonly required in 
Phonetic Sciences, Linguistics, and related and scientific areas in general.
All functions are built to be highly reproducible by accepting data and variable names as parameters, allowing any user to easily apply them to their own datasets without modifying the core code.

Statistical Functions Included

1. Non-Parametric Comparison (Two Groups)

Function Name:	perform_mann_whitney_u

Test Performed:	Mann-Whitney U Test (Wilcoxon Rank-Sum)

Audience Context: Essential for comparing a measurement (e.g., F1 values, articulation rate) between two independent groups (e.g., control vs. patient, L1 vs. L2 speakers) when the data is not normally distributed or the group sizes are unequal.

2. Non-Parametric Comparison (Multiple Groups)

Function Name: perform_kruskal_dunn

Test Performed: Kruskal-Wallis Test with Dunn's Post-hoc

Audience Context: Used to compare a measurement across three or more independent groups (e.g., three language groups, four severity levels) when the data violates parametric assumptions (ANOVA).

#PostHoc test

Function Name: perform_srh_test

Test Performed: Scheirer-Ray-Hare (SRH) Test

Audience Context: The non-parametric equivalent of the Two-Way ANOVA. Used to test the main effects and interaction of two independent factors (e.g., Language Group and Gender) on a measurement when assumptions are violated.

3. . Parametric Comparison (Means)

Function Name: perform_t_testsTest

Performed: T-Tests (One-Sample, Independent, Paired)

Audience Context: Used for comparing means. The function handles three types: 1) One-Sample (compare mean to a constant $\mu$), 2) Independent (compare two independent groups, with automatic Welch's correction for unequal variances), and 3) Paired (compare related samples, e.g., pre- vs. post-train scores).

Function Name: perform_anova_tukey

Test Performed: One-Way ANOVA with Tukey HSD Post-hoc

Audience Context: Used to compare the means of three or more independent groups when data is approximately normal. The function checks for homogeneity of variances and switches to Welch's ANOVA and Games-Howell post-hoc if the assumption is violated.

Function Name: perform_2way_anova

Test Performed: Two-Way ANOVA

Audience Context: Used to test the main effects of two independent factors (e.g., vowel type and speaking style) and their interaction on a dependent variable. The function prioritizes the interaction term in its analysis.

4. Advanced Modeling (Non-Independent Data)

Function Name: fit_lmm

Test Performed: Linear Mixed-Effects Model (LMM)

Audience Context: Crucial for modeling data where observations are not independent (e.g., repeated measures, multiple words from the same speaker, multiple measurements from the same recording session). It allows for the specification of fixed effects (IVs) and random intercepts (e.g., Subject ID, Item ID) to properly account for data structure.

5. Plotting Function

Function Name: generate_boxplots

Test Performed: Customizable Boxplot Grid Generation

Audience Context: Creates a visually clear, publication-ready grid of boxplots for multiple variables against a single factor. It allows extensive customization of labels, colors, and grid layout for easy visualization of group comparisons.
