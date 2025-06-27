# ========================
# 1. Load Required Packages
# ========================

# Load libraries for data manipulation and plotting
library(reshape2)
library(ggplot2)
library(viridis)
library(tidyverse)
library(paletteer)

# ========================
# 2. Load Interaction Matrix
# ========================

# Load the interaction matrix: rows = plants, columns = bumblebee species
bmat <- as.matrix(read.csv("allbees_vs_plants_7_oct_2024_2.csv", row.names = 1))

# ========================
# 3. Define Log-Linear Mass-Action Preference Function
# ========================

mass_action_preference <- function(mat) {
  # Convert matrix to long format for modeling
  long <- melt(mat)
  names(long) <- c("Plant", "Bee", "Observed")
  long <- long %>% filter(Observed > 0)
  long$log_obs <- log(long$Observed)
  
  # Keep plant and bee factors in original order
  long$Plant <- factor(long$Plant, levels = rownames(mat))
  long$Bee   <- factor(long$Bee, levels = colnames(mat))
  
  # Fit log-linear model: log(observed) ~ plant + bee
  model <- lm(log_obs ~ Plant + Bee, data = long)
  coefs <- coef(model)
  intercept <- coefs[1]
  
  # Extract plant and bee effects
  plant_eff <- setNames(rep(0, nrow(mat)), rownames(mat))
  bee_eff   <- setNames(rep(0, ncol(mat)), colnames(mat))
  
  for (name in names(coefs)) {
    if (grepl("Plant", name)) {
      plant <- sub("Plant", "", name)
      plant_eff[plant] <- coefs[name]
    } else if (grepl("Bee", name)) {
      bee <- sub("Bee", "", name)
      bee_eff[bee] <- coefs[name]
    }
  }
  
  # Back-transform to effective abundances
  plant_abund <- exp(intercept + plant_eff)
  bee_abund   <- exp(bee_eff)
  
  # Calculate preference = observed / expected
  preference <- matrix(0, nrow = nrow(mat), ncol = ncol(mat),
                       dimnames = list(rownames(mat), colnames(mat)))
  for (i in rownames(mat)) {
    for (j in colnames(mat)) {
      expected <- plant_abund[i] * bee_abund[j]
      preference[i, j] <- ifelse(expected > 0, mat[i, j] / expected, 0)
    }
  }
  
  return(preference)
}

# ========================
# 4. Compute Preference Matrix
# ========================
pref_matrix <- mass_action_preference(bmat)

# ========================
# 5. Prepare Data for Plotting
# ========================
pref_long <- melt(pref_matrix, varnames = c("Plant", "Bee"), value.name = "Preference")

# ========================
# 6. Define Custom Plant Order (Optional but improves readability)
# ========================
plant_species_order <- c("Calystegia silvatica", "Centranthus ruber", "Phacelia tanacetifolia",
                         "Echium pininana", "Ligustrum vulgare", "Veronica albicans",
                         "Veronica spicata", "Rhinanthus minor", "Odontites sp",
                         "Buddleja davidii", "Stachys palustris", "Ballota nigra",
                         "Lavandula angustifolia", "Prunella vulgaris", "Nepeta racemosa",
                         "Salvia rosmarinus", "Salvia yangii", "Campanula latifolia",
                         "Arctium minus", "Cirsium arvense", "Cirsium vulgare",
                         "Centaurea nigra", "Centaurea cyanus", "Lapsana communis",
                         "Jacobea vulgaris", "Hydrangea paniculata", "Heracleum sphondylium",
                         "Impatiens glandulifera", "Primula bulleyana", "Rhododendron ponticum",
                         "Rhododendron sp1", "Rhododendron sp2", "Rhododendron van",
                         "Cornus sanguinea", "Iris sp", "Hypericum perforatum",
                         "Cotoneaster × suecicus", "Linum usitatissimum", "Filipendula ulmaria",
                         "Rubus caesius", "Rubus fruticosus agg.", "Lotus corniculatus",
                         "Trifolium repens", "Trifolium pratense", "Lathyrus pratensis",
                         "Vicia cracca", "Vicia sepium", "Cardamine pratensis",
                         "Sinapis alba", "Raphanus sativus", "Geranium pratense",
                         "Silene dioica", "Ranunculus acris", "Papaver rhoeas",
                         "Tilia sp", "Epilobium angustifolium", "Symphoricarpos albus",
                         "Cistus creticus", "Cistus sp", "Cistus salvifolius")

# Reverse the plant order for better display
plant_species_order <- rev(plant_species_order)
pref_long$Plant <- factor(pref_long$Plant, levels = plant_species_order)

# ========================
# 7. Plot Preference Heatmap
# ========================
ggplot(pref_long, aes(x = Bee, y = Plant, fill = Preference)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Preference", option = "inferno") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  scale_y_discrete(position = "right") +
  labs(
    title = "Mass-Action Preference Heatmap",
    x = "Bumblebee Species",
    y = "Plant Species"
  )
