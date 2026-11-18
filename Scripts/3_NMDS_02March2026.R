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
nmds <- metaMDS(df[, -1], k = 2, trymax = 100)  # exclude the first column (Season)

#result
summary(nmds)

# calculate Bray-Curtis dissimilarity
df.dist <- vegdist(df[, -1], method = "bray")

# ANOSIM
ppp <- anosim(df.dist, df$Season, permutations = 999, distance = "bray", strata = NULL, parallel = getOption("mc.cores"))

# plot NMDS
p1 <- scores(nmds)$sites %>%
  cbind(df) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = 0.2, color = Season)) +
  stat_chull(geom = "polygon", aes(group = Season, color = Season, fill = Season), alpha = 0.3) +
  labs(title = "(a)") +
  theme_bw() + theme(text = element_text(family = "serif", size = 12),
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
    panel.grid = element_blank()) +
  scale_fill_manual(values = c("#4477AA", "#EE6677", "#228833", "#CCBB44")) +
  scale_colour_manual(values = c("#4477AA", "#EE6677", "#228833", "#CCBB44")) +
  guides(color = guide_legend(ncol = 1, order = 1), 
    fill = guide_legend(ncol = 1, order = 1),
    size = guide_legend(ncol = 1, order = 2)) +
  # statistics annotations
  annotate("text", x = -0.78, y = 1.2, label = paste0("Stress = ", format(nmds$stress, digits = 2)), hjust = 0, size = 4) +
  annotate("text", x = -0.78, y = 1.01, label = paste("R =", round(ppp$statistic, 3)), color = "black", hjust = 0, size = 4) +
  annotate("text", x = -0.78, y = 0.85, label = paste("p =", round(ppp$signif, 3)), color = "black", hjust = 0, size = 4)

print(p1)

###########
# SIMPER analysis
simper_result <- simper(df[, -1], group = df$Season, permutations = 999)

# summary
summary(simper_result)
simper_summary <- summary(simper_result)

#extract top 5 from the summary data frames
extract_top5_summary <- function(comparison_name) {df <- as.data.frame(simper_summary[[comparison_name]])
  # Add the contrast name and species (row names) as columns
  df$Contrast <- comparison_name
  df$Species <- rownames(df)
  # Reorder
  df_top5 <- df %>% select(Contrast, Species, everything()) %>%
    head(5)
  return(df_top5)}

# Combine all contrasts
all_contrasts <- names(simper_summary)
final_simper_table <- bind_rows(lapply(all_contrasts, extract_top5_summary))

# save
print(final_simper_table)
#write.csv(final_simper_table, "~/Desktop/DataAnalysis/Simper_top5_species.csv", row.names = FALSE)

###Next###
# Remove season column for dissimilarity
df.dist <- vegdist(df[, -1], method = "bray")

# compute average Bray-Curtis dissimilarity between groups (seasons)
group_distances <- with(df, betadisper(df.dist, Season))  # dispersion within groups

# overall ANOVA to check if distances differ significantly
anova(group_distances)

#compute group centroids
centroids <- aggregate(df[, -1], by = list(Season = df$Season), FUN = mean)

# compute Bray–Curtis between season centroids
season_dissim <- vegdist(centroids[, -1], method = "bray")

# View
as.matrix(season_dissim)

#change this from above matrix
season_matrix <- matrix(
  c(0.0000000, 0.3181156, 0.4535584, 0.5638491,
    0.3181156, 0.0000000, 0.4513823, 0.4577523,
    0.4535584, 0.4513823, 0.0000000, 0.6051311,
    0.5638491, 0.4577523, 0.6051311, 0.0000000),
  nrow = 4, byrow = TRUE)

#add row and column names
season_labels <- c("Winter", "Pre-monsoon", "Monsoon", "Post-monsoon")
rownames(season_matrix) <- season_labels
colnames(season_matrix) <- season_labels

#convert for ggplot
season_dissim_long <- melt(season_matrix, varnames = c("Season_A", "Season_B"), value.name = "Dissimilarity")

#heatmap
p2 <- ggplot(season_dissim_long, aes(Season_A, Season_B, fill = Dissimilarity)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(0, 0.7), name = "Bray–Curtis\nDissimilarity") +
  geom_text(aes(label = sprintf("%.2f", Dissimilarity)), color = "black", size = 12 / .pt, family = "serif") + 
  theme_minimal(base_size = 12) + labs(title = "(b)", x = NULL, y = NULL) +
  theme(text = element_text(family = "serif"), plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1, size = 12), axis.text.y = element_text(size = 11),
    legend.title = element_text(size = 11), legend.text = element_text(size = 11), panel.grid = element_blank())

print(p2)

# Combine
combined_plot <- (p1 / p2) + 
  plot_layout(heights = c(1.2, 1))
print(combined_plot)