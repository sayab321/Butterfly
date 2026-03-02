###################
#Seasonal diversity
###################
library(iNEXT); library(ggthemes); library(ggplot2); library(readxl); library(patchwork); library(tidyverse); library(UpSetR); library(writexl)

#forest wise 
#data <- read_excel("~/Desktop/DataAnalysis/2_1_Banpale_season.xlsx") #banpale
#data <- read_excel("~/Desktop/DataAnalysis/2_2_Bhadrakali_season.xlsx") #bhadrakali
#data <- read_excel("~/Desktop/DataAnalysis/2_3_Shanti_Ban_season.xlsx") #shantiban

# Extract the species column and convert it to a character vector
species_names <- as.character(data$Species)

# Extract the counts for each season
counts_seasons <- data[, c("Pre-monsoon", "Monsoon", "Post-monsoon", "Winter")]

# Create a matrix with species counts
data_matrix <- as.matrix(counts_seasons)

# Check for missing values and handle them if necessary
if (any(is.na(data_matrix))) {
  data_matrix <- na.omit(data_matrix)}

set.seed(123)

# Run iNEXT function
inext_season <- iNEXT(data_matrix, q = 0, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# output from iNEXT
g1 <- ggiNEXT(inext_season, type = 1)

cb_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44")
#cb_palette <- c("#648FFF", "#DC267F", "#FE6100", "#FFB000")

#xlim(c(0,500) for Bhadrakali and Shantiban
g1 <- g1 + theme_few() + xlim(c(0,1000)) +
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
print(g1)

g2 <- ggiNEXT(inext_season, type = 3)

g2 <- g2 + theme_few() + theme(
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
print(g2)

#Shannon diversity #q=1
inext_season1 <- iNEXT(data_matrix, q = 1, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g3 <- ggiNEXT(inext_season1, type = 3)  # Shannon diversity plot

g3 <- g3 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "exp(Shannon)", title = "(c)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) + 
  theme(legend.position = "none")
print(g3)

#Simpson diversity
# Run iNEXT function for Simpson diversity (q = 2)
inext_season2 <- iNEXT(data_matrix, q = 2, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g4 <- ggiNEXT(inext_season2, type = 3)  # Simpson diversity plot

g4 <- g4 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "1/Simpson", title = "(d)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  theme(legend.position = "none")

# Print or display the plots
print(g4)

# Combine the three plots in a single row
combined_plot <- g1+ g2 + g3 + g4 + plot_layout(ncol = 2, nrow = 2)

# Print the combined plot
print(combined_plot)

#########################################################
#combined seasonal diversity #combine three forests
###########################################################

#load data
banpale <- read_excel("~/Desktop/DataAnalysis/Banpale_season.xlsx")
bhandrakali <- read_excel("~/Desktop/DataAnalysis/Bhadrakali_season.xlsx")
shanti <- read_excel("~/Desktop/DataAnalysis/Shanti_Ban_season.xlsx")

# Combine the three datasets
combined_data <- bind_rows(banpale, bhandrakali, shanti) %>%
  group_by(Species) %>% summarise(across(c("Pre-monsoon", "Monsoon", "Post-monsoon", "Winter"), 
                   ~ sum(.x, na.rm = TRUE))) %>% ungroup()


#write_xlsx(combined_data, "~/Desktop/DataAnalysis/2_Combined_Season_data.xlsx")

# Extract the species column and convert it to a character vector
species_names <- as.character(combined_data$Species)

# Extract the counts for each season
counts_seasons <- combined_data[, c("Pre-monsoon", "Monsoon", "Post-monsoon", "Winter")]

# Create a matrix with species counts
data_matrix <- as.matrix(counts_seasons)

# Check for missing values and handle them if necessary
if (any(is.na(data_matrix))) {
  data_matrix <- na.omit(data_matrix)}

set.seed(123)

# Run iNEXT function
combined_season <- iNEXT(data_matrix, q = 0, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Assuming banpale_season is the output from iNEXT
g1 <- ggiNEXT(combined_season, type = 1)

cb_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44")

g1 <- g1 + theme_few() + xlim(c(0,2000)) +
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
print(g1)

g2 <- ggiNEXT(combined_season, type = 3)

g2 <- g2 + theme_few() + theme(
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
print(g2)

#Shannon diversity #q=1
combined_season1 <- iNEXT(data_matrix, q = 1, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g3 <- ggiNEXT(combined_season1, type = 3)  # Shannon diversity plot

g3 <- g3 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "exp(Shannon)", title = "(c)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) + 
  theme(legend.position = "none")
print(g3)

#Simpson diversity
# Run iNEXT function for Simpson diversity (q = 2)
combined_season2 <- iNEXT(data_matrix, q = 2, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g4 <- ggiNEXT(combined_season2, type = 3)  # Simpson diversity plot

g4 <- g4 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "1/Simpson", title = "(d)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  theme(legend.position = "none")
print(g4)

# Combine the three plots in a single row
combined_plot <- g1+ g2 + g3 + g4 + plot_layout(ncol = 2, nrow = 2)

# Print the combined plot
print(combined_plot)

############################
#####UpsetPlot
##########################

# prepare data
binary_data <- as.data.frame(combined_data)
forest_cols <- c("Pre-monsoon", "Monsoon", "Post-monsoon", "Winter")
binary_data[forest_cols] <- lapply(binary_data[forest_cols], function(x) ifelse(x > 0, 1, 0))

# Set the global font family to serif
par(family = "serif")

# plot
upset(binary_data, sets = forest_cols, keep.order = TRUE,               
      sets.bar.color = c("#4477AA", "#EE6677", "#228833", "#CCBB44"),
      mainbar.y.label = "Shared species intersections",   
      sets.x.label = "Species count",   
      main.bar.color = "grey20", order.by = "freq", empty.intersections = "on",
      text.scale = c(1.5, 1.2, 1.5, 1.2, 1.5, 1.2))
