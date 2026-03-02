##############
###evenness
##################
# libraries
library(vegan); library(readxl)

# load data
#data <- read_excel("~/Desktop/DataAnalysis/Forest_wise_diversity.xlsx")
#data <- read_excel("~/Desktop/DataAnalysis/2_1_Banpale_season.xlsx") #banpale
#data <- read_excel("~/Desktop/DataAnalysis/2_2_Bhadrakali_season.xlsx") #bhadrakali
#data <- read_excel("~/Desktop/DataAnalysis/2_3_Shanti_Ban_season.xlsx") #shantiban
data <- read_excel("~/Desktop/DataAnalysis/2_Combined_Season_data.xlsx") #shantiban

# Remove species column for analysis
abundance_only <- data[, -1]
rownames(abundance_only) <- data$Species

# Function to calculate evenness
calculate_evenness <- function(abundances) {
  H <- diversity(abundances, index = "shannon")
  S <- sum(abundances > 0)
  if (S > 1) {
    J <- H / log(S)
  } else {
    J <- NA  # Evenness undefined if only one species
  } 
  return(J)}

# Apply evenness calculation to each forest
evenness_results <- apply(abundance_only, 2, calculate_evenness)

# Print results
evenness_results
