#create_statistics_tables.R

library(flextable) # format table for Word 
library(officer) # handle Word doc operations
library(car) # ANOVA

create_word_tables <- function(data, 
                             transformed_var = "bc_RRL",
                             group_var,
                             doc_path) {
  
  # Get statistical results
  formula <- as.formula(paste(transformed_var, "~", group_var))
  anova_result <- aov(formula, data = data)
  anova_summary <- summary(anova_result)[[1]]
  tukey_result <- TukeyHSD(anova_result)
  
  # Create Word document
  doc <- read_docx()
  
  # Format ANOVA table
  anova_df <- data.frame(
    Source = rownames(anova_summary),
    Df = anova_summary$"Df",
    `Sum Sq` = round(anova_summary$"Sum Sq", 3),
    `Mean Sq` = round(anova_summary$"Mean Sq", 3),
    `F value` = round(anova_summary$"F value", 3),
    `P value` = format.pval(anova_summary$"Pr(>F)", digits = 3)
  )
  
  # Format Tukey table
  tukey_df <- as.data.frame(tukey_result[[1]]) %>%
    round(3) %>%
    mutate(
      Comparison = rownames(.),
      `P value` = format.pval(`p adj`, digits = 3)
    ) %>%
    dplyr::select(Comparison, diff, lwr, upr, `P value`)
  
  # Create ANOVA flextable
  anova_title <- paste("Table SX: Analysis of variance (ANOVA) results comparing R/(R+L) across", 
                      ifelse(group_var == "Biome", "forest biomes", "plant functional types"),
                      "using box-cox transformed data.")
  
  doc <- doc %>% 
    body_add_par(anova_title) %>%
    body_add_flextable(
      flextable(anova_df) %>%
        autofit() %>%
        theme_box()
    ) %>%
    body_add_par("\n") # Add space between tables
  
  # Create Tukey flextable
  tukey_title <- paste("Table SY: Post-hoc Tukey's HSD test results showing pairwise comparisons between R/(R+L) across",
                      ifelse(group_var == "Biome", "forest biomes", "plant types"),
                      "using box-cox transformed data.")
  
  doc <- doc %>%
    body_add_par(tukey_title) %>%
    body_add_flextable(
      flextable(tukey_df) %>%
        set_header_labels(
          diff = "Difference",
          lwr = "Lower CI",
          upr = "Upper CI"
        ) %>%
        autofit() %>%
        theme_box()
    )
  
  # Save the Word document
  print(doc, target = doc_path)
}
