install.packages("bipartite")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("magrittr") 
install.packages("dplyr") 

library(here)
library(tidyverse)
library(readxl)
library(openxlsx)
library(bipartite)
library(magrittr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)

setwd("~/Documents/Bombus project/Datos/networks/networks")

df = read.csv(here("batch2_bees_vs_plants_4feb.csv"))

df = df %>% group_by(Bumblebee_species, Plant_species) %>% summarize(N = n()) 

net_df = df %>% pivot_wider(names_from = "Bumblebee_species", values_from = "N", values_fill = 0)

write.csv(net_df, "final_all_batch2_4feb.csv")

bmat1=as.matrix(read.csv("allbees_vs_plants_7_oct_2024_2.csv", header=T, row.names=1))
bmat2=as.matrix(read.csv("final_all_batch2_4feb.csv", header=T, row.names=1))

bmat1_t = t(bmat1)
bmat2_t = t(bmat2)

#Calculate H2'

H2fun(bmat, H2_integer = TRUE)

#Calculate d'

specieslevel(bmat, level = "higher", index = "species specificity")

# Function to compute Shannon entropy (H')
shannon_entropy <- function(interactions) {
  proportions <- interactions / sum(interactions)  # Calculate interaction proportions
  proportions <- proportions[proportions > 0]  # Remove zero values to avoid log(0)
  H <- -sum(proportions * log2(proportions))  # Apply Shannon formula
  return(H)
}

# Apply to each bee (row)
H_values <- apply(bmat2_t, 1, shannon_entropy)

# Print results
H_values

H_network1 <- apply(bmat1_t, 1, shannon_entropy)
H_network2 <- apply(bmat2_t, 1, shannon_entropy)

# Perform a statistical test (e.g., Wilcoxon test)
wilcox.test(H_network1, H_network2)

boxplot(H_network1, H_network2, names=c("Network- all bees", "Network- batch2"),
        main="Comparison of Shannon Entropy (H')", ylab="Shannon Entropy (H')")

# Create a data frame for ggplot
data_plot <- data.frame(
  Shannon_Entropy = c(H_network1, H_network2),  # Combine both datasets
  Network = rep(c("Network all bees", "Network batch-2"), times = c(length(H_network1), length(H_network2)))  # Label each value
)

# Violin plot with customized colors
ggplot(data_plot, aes(x = Network, y = Shannon_Entropy, fill = Network)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Violin plot with transparency
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Add a boxplot inside
  theme_minimal() +  # Clean theme
  labs(title = "Comparison of Shannon Entropy (H')",
       x = "Network",
       y = "Shannon Entropy (H')") +
  scale_fill_manual(values = c("Network all bees" = "steelblue", "Network batch-2" = "tomato"))  # Custom colors

#Wilcoxon Signed-Rank Test (Paired Comparison)

# Define the values from your table
bee_species <- c("Bombus pascuorum", "Bombus terrestris", "Bombus lapidarius", 
                 "Bombus pratorum", "Bombus lucorum", "Bombus hortorum", "Bombus hypnorum")

# Specificity index (d') for both networks
specificity_all <- c(0.22, 0.22, 0.27, 0.29, 0.36, 0.29, 0.28)  # Full network
specificity_batch2 <- c(0.26, 0.22, 0.30, 0.34, 0.41, 0.48, 0.38)  # Batch 2

# Shannon entropy (H') for both networks
H_all <- c(4.40, 4.39, 3.81, 3.80, 3.46, 3.63, 3.57)  # Full network
H_batch2 <- c(3.64, 3.86, 3.17, 3.17, 3.02, 2.40, 2.64)  # Batch 2

# Wilcoxon test for d' (Specificity Index)
wilcox_d <- wilcox.test(specificity_all, specificity_batch2, paired = TRUE)
print("Wilcoxon Test for Specificity Index (d'):")
print(wilcox_d)

# Wilcoxon test for H' (Shannon Entropy)
wilcox_H <- wilcox.test(H_all, H_batch2, paired = TRUE)
print("Wilcoxon Test for Shannon Entropy (H'):")
print(wilcox_H)

#Scatterplot: Relationship Between Specificity Index (d') and Shannon Entropy (H')

# Create a data frame
data_plot <- data.frame(
  Species = rep(bee_species, 2),  # Duplicate species names for both networks
  Specificity_Index = c(specificity_all, specificity_batch2),
  Shannon_Entropy = c(H_all, H_batch2),
  Network = rep(c("All Bees Network", "Batch 2"), each = length(bee_species))
)

# Scatterplot with color differentiation
ggplot(data_plot, aes(x = Specificity_Index, y = Shannon_Entropy, color = Network)) +
  geom_point(size = 4) +  # Plot points
  geom_text(aes(label = Species), vjust = -1, size = 3) +  # Add bee species labels
  theme_minimal() +  # Clean theme
  labs(title = "Trade-off Between Specialisation (d') and Shannon Entropy (H')",
       x = "Specificity Index (d')",
       y = "Shannon Entropy (H')") +
  scale_color_manual(values = c("All Bees Network" = "steelblue", "Batch 2" = "tomato"))  # Custom colors

# Get species-level metrics for bees
bee_metrics <- specieslevel(bmat1, level = "higher")

# Print dependencies
bee_metrics$dependence

# Find the strongest plant interaction per bee
strongest_links <- apply(bmat2, 1, function(x) names(x)[which.max(x)])

# Print strongest links
strongest_links

write.csv(strongest_links, "strongest_links_batch2.csv")

networklevel(bmat1, index = "interaction strength")

networklevel(bmat1, index = "links per species")

grouplevel(bmat1, level="both", index=c("mean number of links", "weighted + 
                                       cluster coefficient", 
                                       "effective partners", "niche overlap"), 
           dist="bray")
