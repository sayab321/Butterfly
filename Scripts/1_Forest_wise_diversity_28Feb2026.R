##########################################################
#Forest wise butterfly diversity
##########################################################
#libraries
#install.packages("iNEXT"); install.packages("ggthemes")
library(iNEXT); library(ggthemes); library(ggplot2); library(readxl); library(patchwork); library(UpSetR)

#load data
forest_data <- read_excel("~/Desktop/DataAnalysis/1_Forest_wise_diversity.xlsx")

# Save the data to a CSV in the same folder
#write.csv(forest_data, "~/Desktop/DataAnalysis/1_Forest_wise_diversity.csv", row.names = FALSE)

# Extract the species column and convert it to a character vector
forest_names <- as.character(forest_data$Species)
# Extract the counts for each season
counts_species<- forest_data [, c("Banpale", "Bhadrakali", "Shanti Ban Batika")]
# Create a matrix with species counts
data_matrix <- as.matrix(counts_species)
# Check for missing values and handle them if necessary
if (any(is.na(data_matrix))) {data_matrix <- na.omit(data_matrix)}
#Assign species names as row names
rownames(data_matrix) <- forest_data$Species

set.seed(123)
# Run iNEXT function #species richness
forest_output <- iNEXT(data_matrix, q = 0, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

#forest_output is the output from iNEXT
gf <- ggiNEXT(forest_output, type = 1)

# Color-blind friendly hex codes
cb_palette <- c("#CC79A7", "#009E73", "#56B4E9")

gf <- gf + theme_few() + xlim(c(0,2500)) +
  theme(plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
    axis.title.x = element_text(size = 12, family = "serif"),
    axis.title.y = element_text(size = 12, family = "serif"), 
    axis.text.x = element_text(size = 12, family = "serif"), 
    axis.text.y = element_text(size = 12, family = "serif"),
    legend.text = element_text(size = 12, family = "serif"),
    legend.title = element_text(size = 12, family = "serif") # If there is a legend title
  ) + labs(x = "Number of individuals", y = "Species richness", title = "(a)") + 
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +   theme(legend.position = "none")
print(gf)

# forest_output the output from iNEXT
gf2 <- ggiNEXT(forest_output, type = 3)

gf2 <- gf2 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
  lgend.text = element_text(size = 9, family = "serif"),
) + labs(x = "Sample coverage", y = "Species richness", title = "(b)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +theme(
    legend.position = c(0.21, 0.56),
    legend.background = element_blank())
print(gf2)

#Shannon diversity #q=1
forest_output1 <- iNEXT(data_matrix, q = 1, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
gf3 <- ggiNEXT(forest_output1, type = 3)  # Shannon diversity plot

gf3 <- gf3 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "exp(Shannon)", title = "(c)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) + 
  theme(legend.position = "none")
print(gf3)

#Simpson diversity
# Run iNEXT function for Simpson diversity (q = 2)
forest_output2 <- iNEXT(data_matrix, q = 2, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
gf4 <- ggiNEXT(forest_output2, type = 3)  # Simpson diversity plot

gf4 <- gf4 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "1/Simpson", title = "(d)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  theme(legend.position = "none")
print(gf4)

# Combine the three plots in a single row
combined_plot <- gf+ gf2 + gf3 + gf4 + plot_layout(ncol = 2, nrow = 2)

# Print the combined plot
print(combined_plot)

############################################################
#Upset plot
# prepare data
binary_data <- as.data.frame(forest_data)
forest_cols <- c("Banpale", "Bhadrakali", "Shanti Ban Batika")
binary_data[forest_cols] <- lapply(binary_data[forest_cols], function(x) ifelse(x > 0, 1, 0))

# Set the global font family to serif
par(family = "serif")

# plot
upset(binary_data, sets = forest_cols, keep.order = TRUE,               
      sets.bar.color = c("#CC79A7", "#009E73", "#56B4E9"),
      mainbar.y.label = "Shared species intersections",   
      sets.x.label = "Species count", main.bar.color = "grey20", 
      order.by = "freq", empty.intersections = "on",
      text.scale = c(1.5, 1.2, 1.5, 1.2, 1.5, 1.2))