###############
#NMDS analysis
###############
#libraries
library(tidyverse); library(vegan); library(ggpubr); library(ggthemes); library(ggplot2); library(patchwork); library(reshape2); library(readxl)

#data
#df <- read_excel("~/Desktop/DataAnalysis/3_1_NMDS_season_banpale.xlsx")
#df <- read_excel("~/Desktop/DataAnalysis/3_2_NMDS_season_bhadrakali.xlsx")
#df <- read_excel("~/Desktop/DataAnalysis/3_3_NMDS_season_shantiban.xlsx")
df <- read_excel("~/Desktop/DataAnalysis/3_Combined_NMDS_Seasons_Data.xlsx")

str(df)

# Perform NMDS
nmds <- metaMDS(df[, -1], k = 2, trymax = 100)  # Exclude the first column (Season)

# Check the result
summary(nmds)

# Calculate Bray-Curtis dissimilarity
df.dist <- vegdist(df[, -1], method = "bray")

# ANOSIM
ppp <- anosim(df.dist, df$Season, permutations = 999, distance = "bray", strata = NULL, parallel = getOption("mc.cores"))

# Plot NMDS
p1 <- scores(nmds)$sites %>%
  cbind(df) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = 0.2, color = Season)) +
  stat_chull(geom = "polygon", aes(group = Season, color = Season, fill = Season), alpha = 0.3) +
  labs(title = "(a)") +
  theme_bw() + 
  theme(
    text = element_text(family = "serif", size = 12),
    plot.title = element_text(size = 12), 
    legend.position = "right",        
    legend.box = "vertical",          
    legend.title = element_blank(),    
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12, angle = 90),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c("#4477AA", "#EE6677", "#228833", "#CCBB44")) +
  scale_colour_manual(values = c("#4477AA", "#EE6677", "#228833", "#CCBB44")) +
  # Configure labels to stack vertically on the right
  guides(
    color = guide_legend(ncol = 1, order = 1), 
    fill = guide_legend(ncol = 1, order = 1),
    size = guide_legend(ncol = 1, order = 2) 
  ) +
  # Statistics annotations
  annotate("text", x = -0.78, y = 1.2, label = paste0("Stress = ", format(nmds$stress, digits = 2)), hjust = 0, size = 4) +
  annotate("text", x = -0.78, y = 1.01, label = paste("R =", round(ppp$statistic, 3)), color = "black", hjust = 0, size = 4) +
  annotate("text", x = -0.78, y = 0.85, label = paste("p =", round(ppp$signif, 3)), color = "black", hjust = 0, size = 4)

print(p1)

###########SIMPER
# SIMPER analysis
simper_result <- simper(df[, -1], group = df$Season, permutations = 999)

# View summary
summary(simper_result)

# Remove Season column for dissimilarity calculation
df.dist <- vegdist(df[, -1], method = "bray")

# Compute average Bray-Curtis dissimilarity between groups (seasons)
group_distances <- with(df, betadisper(df.dist, Season))  # Dispersion within groups

# Overall ANOVA to check if distances differ significantly
anova(group_distances)

#Compute group centroids
centroids <- aggregate(df[, -1], by = list(Season = df$Season), FUN = mean)

# Compute Bray–Curtis between season centroids
season_dissim <- vegdist(centroids[, -1], method = "bray")

# View as matrix
as.matrix(season_dissim)

#change this from above matrix
season_matrix <- matrix(
  c(0.0000000, 0.3181156, 0.4535584, 0.5638491,
    0.3181156, 0.0000000, 0.4513823, 0.4577523,
    0.4535584, 0.4513823, 0.0000000, 0.6051311,
    0.5638491, 0.4577523, 0.6051311, 0.0000000),
  nrow = 4, byrow = TRUE)

#Add row and column names
season_labels <- c("Winter", "Pre-monsoon", "Monsoon", "Post-monsoon")
rownames(season_matrix) <- season_labels
colnames(season_matrix) <- season_labels

#Convert to long format for ggplot
season_dissim_long <- melt(season_matrix, varnames = c("Season_A", "Season_B"), value.name = "Dissimilarity")

#heatmap
p2 <- ggplot(season_dissim_long, aes(Season_A, Season_B, fill = Dissimilarity)) +
  geom_tile(color = "white") +
  # Use a sequential orange palette that doesn't overlap with panel (a) colors
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(0, 0.7), 
                       name = "Bray–Curtis\nDissimilarity") +
  # Change text to black for better contrast against light orange tiles
  geom_text(aes(label = sprintf("%.2f", Dissimilarity)), 
            color = "black", size = 12 / .pt, family = "serif") + 
  theme_minimal(base_size = 12) +
  labs(title = "(b)", x = NULL, y = NULL) +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    panel.grid = element_blank())

print(p2)

# Combine vertically
combined_plot <- (p1 / p2) + 
  plot_layout(heights = c(1.2, 1)) # Giving p1 slightly more height for its legend

# Show
print(combined_plot)


#######commbining data
#######combined
#df1 <- read_excel("~/Desktop/NMDS_season_banpale1.xlsx")
#df2 <- read_excel("~/Desktop/NMDS_season_bhadrakali1.xlsx")
#df3 <- read_excel("~/Desktop/NMDS_season_shantiban1.xlsx")

#Combine datasets and convert to long format
#combined_long <- bind_rows(df1, df2, df3) %>%
 # pivot_longer(cols = -Season, names_to = "Species", values_to = "Count")

# Sum counts by Season and Species
# This handles both common species (sums them) and unique species (keeps them)
#final_long <- combined_long %>%
 # group_by(Season, Species) %>%
#  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = 'drop')

#Reshape back to wide format (like your original image)
# Missing values for unique species in specific datasets will be filled with 0
#final_df <- final_long %>%
 # pivot_wider(names_from = Species, values_from = Total_Count, values_fill = 0)
