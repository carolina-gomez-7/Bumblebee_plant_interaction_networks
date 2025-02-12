# Install required packages (Only needed the first time)
install.packages("bipartite")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("magrittr")
install.packages("dplyr") 
install.packages("ggplot2")
install.packages("igraph")

# Load necessary libraries
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

# Set working directory
setwd("~/Documents/Bombus project/Datos/networks/networks")

# Load interaction data
df = read.csv(here("bombus_vs_plants.csv"))

# Summarize interaction data to count occurrences
df = df %>% group_by(Bumblebee_species, Plant_species) %>% summarize(N = n())

# Convert data to a matrix format (wide format)
net_df = df %>% pivot_wider(names_from = "Bumblebee_species", values_from = "N", values_fill = 0)

# Save formatted interaction matrix
write.csv(net_df, "allbees_vs_plants_7_oct_2024_2.csv")

# Read interaction matrices for all bees, females, and males
bmat = as.matrix(read.csv("allbees_vs_plants_7_oct_2024_2.csv", header=T, row.names=1))
bmat_females = as.matrix(read.csv("all_females_vs_plants_7_oct_2024.csv", header=T, row.names=1))
bmat_males = as.matrix(read.csv("all_males_vs_plants_7_oct_2024.csv", header=T, row.names=1))
bmat_batch2 = as.matrix(read.csv("final_all_batch2_4feb.csv", header=T, row.names=1))
# ===================== Plot Bipartite Network ===================== #
par(font = 3)
plotweb(bmat, col.high = "orange2", bor.col.interaction = "NA", col.interaction = "snow4", 
        col.low = "gold2", text.rot = 90, labsize = 2, low.spacing = 0.1, 
        high.spacing = 1, ybig = 1, low.lab.dis = 0.02, high.lab.dis = 0.02, 
        low.y = 1, high.y = 1.5)

# ===================== Calculate Observed H2' Index ===================== #
H2_obs1 <- H2fun(bmat, H2_integer = TRUE)[1]
H2_obs2 <- H2fun(bmat_females, H2_integer = TRUE)[1]
H2_obs3 <- H2fun(bmat_males, H2_integer = TRUE)[1]
H2_obs4 <- H2fun(bmat_batch2, H2_integer = TRUE)[1]

# ===================== Generate Null Models ===================== #
set.seed(123)  # Ensure reproducibility
null_models1 <- nullmodel(web = bmat, N = 1000, method = 'r2d')
null_models2 <- nullmodel(web = bmat_females, N = 1000, method = 'r2d')
null_models3 <- nullmodel(web = bmat_males, N = 1000, method = 'r2d')
null_models4 <- nullmodel(web = bmat_batch2, N = 1000, method = 'r2d')

#Generate null model with a different method
null_models1 <- nullmodel(web = bmat, N = 1000, method = 'swap.web')
null_models2 <- nullmodel(web = bmat_females, N = 1000, method = 'swap.web')
null_models3 <- nullmodel(web = bmat_males, N = 1000, method = 'swap.web')
null_models4 <- nullmodel(web = bmat_batch2, N = 1000, method = 'swap.web')

# Calculate H2' index for each null model
H2_nulls1 <- sapply(null_models1, function(x) H2fun(x, H2_integer = TRUE))
H2_nulls2 <- sapply(null_models2, function(x) H2fun(x, H2_integer = TRUE))
H2_nulls3 <- sapply(null_models3, function(x) H2fun(x, H2_integer = TRUE))
H2_nulls4 <- sapply(null_models4, function(x) H2fun(x, H2_integer = TRUE))

# Calculate p-values for observed H2'
p_value1 <- mean(H2_nulls1 >= H2_obs1)
p_value2 <- mean(H2_nulls2 >= H2_obs2)
p_value3 <- mean(H2_nulls3 >= H2_obs3)
p_value4 <- mean(H2_nulls4 >= H2_obs4)

# ===================== Plot H2' Null Model Comparisons ===================== #
plot(density(H2_nulls1), xlim = c(0, 1), lwd = 2, main = "H2' Index Null Model Comparison - All bees", xlab = "H2' Index")
abline(v = H2_obs1, col = "red", lwd = 2)
legend("topright", legend = paste("Observed H2' =", round(H2_obs1, 3), "\nP-value =", round(p_value1, 3)), col = "red", lwd = 2)

plot(density(H2_nulls2), xlim = c(0, 1), lwd = 2, main = "H2' Index Null Model Comparison - Females", xlab = "H2' Index")
abline(v = H2_obs2, col = "red", lwd = 2)
legend("topright", legend = paste("Observed H2' =", round(H2_obs2, 3), "\nP-value =", round(p_value2, 3)), col = "red", lwd = 2)

plot(density(H2_nulls3), xlim = c(0, 1), lwd = 2, main = "H2' Index Null Model Comparison - Males", xlab = "H2' Index")
abline(v = H2_obs3, col = "red", lwd = 2)
legend("topright", legend = paste("Observed H2' =", round(H2_obs3, 3), "\nP-value =", round(p_value3, 3)), col = "red", lwd = 2)

plot(density(H2_nulls4), xlim = c(0, 1), lwd = 2, main = "H2' Index Null Model Comparison - all bees", xlab = "H2' Index")

# Add observed H2' as a red vertical line
abline(v = H2_obs4, col = "red", lwd = 2)

# Adjust legend position to avoid overlap
legend("topright", 
       legend = paste("Observed H2' =", round(H2_obs4, 3), "\nP-value =", round(p_value4, 3)), 
       col = "red", lwd = 2, 
       bty = "n", # Removes the box around the legend
       inset = c(0.008, 0.08), # Moves the legend slightly inward
       xjust = 1, yjust = 1) # Adjust alignment to fit better


# ===================== Niche Overlap Analysis ===================== #
# Function to calculate niche overlap using Pianka's index
calc_niche_overlap <- function(network) {
  nicheoverlap(network, method = "pianka")
}

# Calculate observed niche overlap
niche_obs1 <- calc_niche_overlap(bmat)
niche_obs2 <- calc_niche_overlap(bmat_females)
niche_obs3 <- calc_niche_overlap(bmat_males)

# Generate null models for niche overlap
null_niche1 <- sapply(null_models1, calc_niche_overlap)
null_niche2 <- sapply(null_models2, calc_niche_overlap)
null_niche3 <- sapply(null_models3, calc_niche_overlap)

# Calculate p-values for niche overlap
p_niche1 <- mean(null_niche1 >= niche_obs1)
p_niche2 <- mean(null_niche2 >= niche_obs2)
p_niche3 <- mean(null_niche3 >= niche_obs3)

# ===================== Plot Niche Overlap Null Model Comparisons ===================== #
plot(density(null_niche1), xlim = c(0, 1), lwd = 2, main = "Niche Overlap Null Model - All bees", xlab = "Niche Overlap")
abline(v = niche_obs1, col = "red", lwd = 2)
legend("topright", legend = paste("Observed =", round(niche_obs1, 3), "\nP-value =", round(p_niche1, 3)), col = "red", lwd = 2)

plot(density(null_niche2), xlim = c(0, 1), lwd = 2, main = "Niche Overlap Null Model - Females", xlab = "Niche Overlap")
abline(v = niche_obs2, col = "red", lwd = 2)
legend("topright", legend = paste("Observed =", round(niche_obs2, 3), "\nP-value =", round(p_niche2, 3)), col = "red", lwd = 2)

plot(density(null_niche3), xlim = c(0, 1), lwd = 2, main = "Niche Overlap Null Model - Males", xlab = "Niche Overlap")
abline(v = niche_obs3, col = "red", lwd = 2)
legend("topright", legend = paste("Observed =", round(niche_obs3, 3), "\nP-value =", round(p_niche3, 3)), col = "red", lwd = 2)
