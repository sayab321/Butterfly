##########################################################
#indicator species analysis
##########################################################
#load libraries
#install.packages("indicspecies")
library(indicspecies); library(readxl); library(indicspecies); library(ggplot2); library(dplyr); library(forcats)

# data
df <- read_excel("~/Desktop/DataAnalysis/3_Combined_NMDS_Seasons_Data.xlsx")
str(df)

season <- df$Season
species_data <- df[, -1]  # exclude the season column

# IndVal analysis with 999 permutations
indval_result <- multipatt(species_data, season, func = "IndVal.g", control = how(nperm = 999))

# summary
summary(indval_result, indvalcomp = TRUE)

# extract the significant species only
significant_species <- indval_result$sign

# Indicator value summary data
indval_df <- data.frame(
  Species = c("italic('A. emolus')", "italic('Z. karsandra')", "italic('T. limniace')", 
    "italic('C. nerissa')", "italic('T. augias')", "italic('P. helenus')", 
    "italic('Z. amasa')", "italic('U. dilecta')", "italic('L. kansa')", 
    "italic('A. ranga')", "italic('I. salsala')", "italic('C. thyodamas')", 
    "italic('A. lyncida')", "italic('C. pyranthe')", "italic('J. alecto')", 
    "italic('J. bochus')", "italic('A. opalina')", "italic('E. mulciber')", 
    "italic('E. core')", "italic('P. aglea')", "italic('H. bolina')"),
  Group = c("Pre-monsoon", "Pre-monsoon", "Pre-monsoon", 
    "Pre-monsoon", "Pre-monsoon", "Pre-monsoon", 
    "Pre-monsoon", "Pre-monsoon", "Winter", 
    "Winter", "Monsoon + Post-monsoon", "Monsoon + Post-monsoon", 
    "Monsoon + Pre-monsoon", "Monsoon + Pre-monsoon", "Post-monsoon + Pre-monsoon", 
    "Post-monsoon + Pre-monsoon", "Post-monsoon + Pre-monsoon", "Post-monsoon + Pre-monsoon", 
    "Monsoon + Post-monsoon + Pre-monsoon", "Monsoon + Post-monsoon + Pre-monsoon", "Monsoon + Post-monsoon + Pre-monsoon"),
  Stat = c(1.000, 0.979, 0.950, 0.943, 0.933, 0.907, 0.877, 0.866, 
    1.000, 1.000, 0.913, 0.913, 1.000, 1.000, 0.973, 0.907, 
    0.894, 0.857, 1.000, 0.989, 0.943),
  p_value = c(0.012, 0.015, 0.009, 0.018, 0.021, 0.014, 0.025, 0.047, 
    0.012, 0.012, 0.021, 0.029, 0.005, 0.005, 0.006, 0.036, 
    0.050, 0.038, 0.012, 0.018, 0.044))

# arrange and reorder
indval_df <- indval_df %>% arrange(desc(Stat)) %>% mutate(Species = fct_reorder(Species, Stat))

# Plot
ggplot(indval_df, aes(x = Species, y = Stat, fill = Group)) + geom_col(width = 0.7) +
  geom_text(aes(label = round(Stat, 2)), hjust = -0.3, size = 4, family = "serif", color = "black") +
  coord_flip() + scale_x_discrete(labels = function(x) parse(text = x)) +
  labs(x = "Species", y = "Indicator value (IndVal)", fill = "Season / Season combinations") +
  scale_fill_brewer(palette = "Set3") + theme_minimal(base_family = "serif", base_size = 14) +
  theme(text = element_text(color = "black"), axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold", color = "black"),
        plot.subtitle = element_text(size = 12, color = "black"))

#save table
final_table <- indval_df
final_table$Group[duplicated(final_table$Group)] <- ""

# rename columns
colnames(final_table) <- c("Species", "Season/Combination", "Indicator value", "p")
# reorder columns
final_table <- final_table[, c("Season/Combination", "Species", "Indicator value", "p")]

# save
save_path <- "~/Desktop/DataAnalysis/Indicator_Species_Summary.xlsx"
write_xlsx(final_table, path = save_path, format_headers = TRUE)