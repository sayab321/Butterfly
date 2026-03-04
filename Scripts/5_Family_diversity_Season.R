####################
#Family #season
library(readxl)
library(ggplot2)
library(patchwork)

# Load data
df <- read_excel("~/Desktop/DataAnalysis/5_Family_diversity_season.xlsx")

# Setup factors and custom color mapping
df$Season <- factor(df$Season, 
                    levels = c("Pre-monsoon", "Monsoon", "Post-monsoon", "Winter"))

season_colors <- c(
  "Pre-monsoon"  = "#4477AA", 
  "Monsoon"      = "#EE6677", 
  "Post-monsoon" = "#228833", 
  "Winter"       = "#CCBB44")

# Define theme (12pt Serif, Black text)
common_style <- list(
  geom_col(position = position_dodge(preserve = "single"), color = "black", linewidth = 0.2),
  scale_fill_manual(values = season_colors, name = NULL),
  theme_minimal(base_family = "serif", base_size = 12),
  theme(
    # Ensure all text components are explicitly Black and 12pt
    text = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 14),
    
    # Formatting
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")))

# Create Plots
p1 <- ggplot(df, aes(x = Family, y = Richness, fill = Season)) +
  common_style +
  labs(title = "(a)", y = "Species richness", x = NULL)

p2 <- ggplot(df, aes(x = Family, y = Abundance, fill = Season)) +
  common_style +
  labs(title = "(b)", y = "Species abundance", x = NULL)

# Combine plot
combined_plot <- (p1 + p2) + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(
    legend.position = "bottom",
    text = element_text(family = "serif", color = "black", size = 12),
    legend.text = element_text(size = 12))

print(combined_plot)
