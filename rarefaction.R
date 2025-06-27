# =============================
# 1. Load Required Libraries
# =============================
install.packages("vegan")
library(vegan)

# =============================
# 2. Load the Interaction Matrix
# =============================
# This matrix should have plants as rows and bumblebees as columns
bmat <- as.matrix(read.csv("allbees_vs_plants_7_oct_2024_2.csv", header = TRUE, row.names = 1))

# =============================
# 3. Transpose the Matrix
# =============================
# vegan::rarefy expects samples (bees) in rows and species (plants) in columns
bmat_t <- t(bmat)

# =============================
# 4. Apply Rarefaction
# =============================
# Find the minimum number of visits across all bumblebee species
min_visits <- min(rowSums(bmat_t))

# Perform rarefaction to standardize sampling effort
rarefied <- rarefy(bmat_t, sample = min_visits)

# =============================
# 5. View and Plot Results
# =============================
# Print the expected number of plant species visited (rarefied richness)
print(rarefied)

# Plot as barplot
barplot(rarefied,
        las = 2,
        col = "steelblue",
        main = paste("Rarefied Plant Richness (Sample Size =", min_visits, ")"),
        ylab = "Expected Number of Plant Species")



#option 2-may18------------------

# =============================
# 1. Load Required Libraries
# =============================
install.packages("vegan")
install.packages("boot")
library(vegan)
library(boot)

# =============================
# 2. Load the Interaction Matrix
# =============================
bmat <- as.matrix(read.csv("allbees_vs_plants_7_oct_2024_2.csv", header = TRUE, row.names = 1))

# Transpose: rows = bumblebee species, columns = plant species
bmat_t <- t(bmat)

# =============================
# 3. Calculate Unrarefied Richness (Observed)
# =============================
observed_richness <- rowSums(bmat_t > 0)

# =============================
# 4. Rarefaction
# =============================
min_visits <- min(rowSums(bmat_t))
rarefied_richness <- rarefy(bmat_t, sample = min_visits)

# =============================
# 5. Bootstrapped Confidence Intervals for Rarefied Richness
# =============================

bootstrap_rarefy <- function(data_row, sample_size, reps = 200) {
  boot_means <- numeric(reps)
  data_row <- rep(colnames(bmat_t), times = data_row)
  for (i in 1:reps) {
    sample_i <- sample(data_row, sample_size, replace = FALSE)
    boot_means[i] <- length(unique(sample_i))
  }
  return(quantile(boot_means, probs = c(0.025, 0.975)))
}

conf_ints <- t(apply(bmat_t, 1, function(x) bootstrap_rarefy(x, min_visits)))
colnames(conf_ints) <- c("Lower", "Upper")

# =============================
# 6. Combine All into One Data Frame
# =============================
bee_species <- rownames(bmat_t)
summary_df <- data.frame(
  Species = bee_species,
  Observed = observed_richness,
  Rarefied = as.numeric(rarefied_richness),
  Lower_CI = conf_ints[, "Lower"],
  Upper_CI = conf_ints[, "Upper"]
)

write_csv(summary_df, "rarefaction_results.csv")
# =============================
# 7. Plot Comparison
# =============================
library(ggplot2)

ggplot(summary_df, aes(x = reorder(Species, -Rarefied))) +
  geom_bar(aes(y = Observed), stat = "identity", fill = "grey80", width = 0.6, alpha = 0.7) +
  geom_bar(aes(y = Rarefied), stat = "identity", fill = "steelblue", width = 0.4) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "black") +
  theme_minimal() +
  labs(
    title = paste("Observed vs. Rarefied Plant Richness (Sample Size =", min_visits, ")"),
    x = "Bumblebee Species",
    y = "Number of Plant Species Visited"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


