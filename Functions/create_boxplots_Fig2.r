
library(gghalves)       # For half-violin/half-box plots
library(viridis)        # For viridis color scales
library(multcompView)   # For letter display from multiple comparisons
library(patchwork)      # For combining plots
library(ggbeeswarm)     # for bee swarm plots

# colors for biomes 
colors_biome <- viridis(3, end = .8) # dark purple endpoint 0, less yellow upper end 0.9


# Define text sizes centrally
text_sizes <- list(
  plot_title = 10,
  axis_title = 9,
  axis_text = 10,
  annotation = 3.5,  # Size for annotated text 
  header = 4,        # Size for section headers
  vjustanova = 2
)

# For Fig 2a: differences in R/(R+L) by biome
create_biome_plot_letters <- function(data,
                                    transformed_var = "bc_RRL",
                                    raw_var = "RRL",
                                    group_var = "Biome") {
  # Run ANOVA and Tukey HSD on transformed data
  formula <- as.formula(paste(transformed_var, "~", group_var))
  anova_result <- aov(formula, data = data)
  anova_p <- format.pval(summary(anova_result)[[1]]$`Pr(>F)`[1], digits = 3)
  
  # Get letters from Tukey results
  tukey_result <- TukeyHSD(anova_result)
  letters_result <- multcompLetters4(anova_result, tukey_result)[[group_var]]
  
  # Create letter position dataframe
  letter_df <- data.frame(
    group = names(letters_result$Letters),
    letter = unname(letters_result$Letters)
  ) %>%
    mutate(
      # First get the unique letters in order of appearance from left to right
      new_letter = match(letter, unique(letter[match(group, levels(data[[group_var]]))])),
      # Convert numbers back to letters
      letter = letters[new_letter],
      y = tapply(data[[raw_var]], data[[group_var]], max)[as.character(group)] + 0.05,
      group = factor(group, levels = levels(data[[group_var]]))
    )
  
  # Create plot
  plot <- ggplot(data, aes(x = .data[[group_var]], y = .data[[raw_var]])) +
    geom_half_point(aes(color = .data[[group_var]]),
                    transformation = position_quasirandom(width = 0.1),
                    side = "l", size = 0.5, alpha = 0.3) +
    geom_half_boxplot(aes(fill = .data[[group_var]]), side = "r", alpha = .6) +
    geom_text(data = letter_df,
              aes(x = group, y = y, label = letter),
              size = 4) +
    scale_fill_viridis_d(option = "viridis", end = 0.8) +
    scale_color_viridis_d(option = "viridis", end = 0.8) +
    guides(color = "none", fill = "none") +
    labs(x = "", y = "R/(R+L)") + 
    annotate("text", x = Inf, y = Inf,
             label = paste("ANOVA p =", anova_p),
             hjust = 1.1, vjust = text_sizes$vjustanova, size = text_sizes$annotation) +
    theme_bw()+
    theme(
     axis.text.x = element_text(size = text_sizes$axis_text)) + 
    ylim(0, 0.65)
  
  return(plot)
}

# For Figure 2b: differences in R/(R+L) by biome & dominant leaf morphology ("leaftype")
create_pft_plot_letters <- function(data,
                                  transformed_var = "bc_RRL",
                                  raw_var = "RRL",
                                  group_var = "biome_leaftype") {
  # Run ANOVA and Tukey HSD on transformed data
  formula <- as.formula(paste(transformed_var, "~", group_var))
  anova_result <- aov(formula, data = data)
  anova_p <- format.pval(summary(anova_result)[[1]]$`Pr(>F)`[1], digits = 3)
  
  # Get letters from Tukey results
  tukey_result <- TukeyHSD(anova_result)
  letters_result <- multcompLetters4(anova_result, tukey_result)
  
  # Get unique letters and create mapping
  original_letters <- unique(unlist(strsplit(letters_result[[group_var]]$Letters, "")))
  new_letters <- rev(original_letters)  # Reverse the letter order
  letter_map <- setNames(new_letters, original_letters)
  
  # Create letter position dataframe
  letter_df <- data.frame(
    group = names(letters_result[[group_var]]$Letters),
    letter = letters_result[[group_var]]$Letters
  ) %>%
    mutate(
      # Transform each letter, preserving multiple-letter assignments
      letter = sapply(letter, function(x) {
        chars <- strsplit(x, "")[[1]]
        paste(letter_map[chars], collapse = "")
      }),
      group = factor(group, levels = levels(data[[group_var]])),
      y = vapply(as.character(group), function(g) {
        max(data[data[[group_var]] == g, raw_var], na.rm = TRUE) + 0.05
      }, numeric(1))
    )
  # Create plot
  plot <- ggplot(data, aes(x = .data[[group_var]], y = .data[[raw_var]])) +
    geom_half_point(aes(color = .data[[group_var]]),
                    transformation = position_quasirandom(width = 0.1),
                    side = "l", size = 0.5, alpha = 0.3) +
    geom_half_boxplot(aes(fill = .data[[group_var]]), side = "r", alpha = .6) +
    geom_text(data = letter_df,
              aes(x = group, y = y, label = letter),
              size = 4) +
    scale_fill_viridis_d(option = "viridis", end = 0.8) +
    scale_color_viridis_d(option = "viridis", end = 0.8) +
    scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
    guides(color = "none", fill = "none") +
    labs(x = "", y = "R/(R+L)") +
    annotate("text", x = Inf, y = Inf,
             label = paste("ANOVA p =", anova_p),
             hjust = 1.1, vjust = text_sizes$vjustanova, 
             size = text_sizes$annotation) +
    theme_bw() +
    theme(axis.text.x = element_text(size = text_sizes$axis_text)) +
    ylim(0, 0.65)
  
  return(plot)
}


# For Fig 2c: R/(R+L) by biome x age group 
create_biome_age_plot <- function(data, col_biome = "Biome", col_age = "Forest_age_group",
                                  colors_biome = colors_biome) {
  ggplot(df, aes(x = .data[[col_age]],
                 y = RRL,
                 fill = .data[[col_biome]]
                 )
                 ) +
  geom_half_point(aes(color = .data[[col_biome]]),
                      transformation = position_quasirandom(width = 0.1),
                      side = "l", size = 0.5, alpha = 0.3) +
      geom_half_boxplot(aes(fill = .data[[col_biome]]), side = "r", alpha = .6) +
    scale_fill_manual(values = colors_biome) +
    scale_color_manual(values = colors_biome) +
    theme_bw() +
    labs(y = "R/(R+L)",
         x = "",
         fill = "Forest Biome") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          legend.position = "none"
           ) +
    facet_wrap(as.formula(paste("~", col_biome)),  strip.position = "top") +
    theme(
          strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 12))
}
