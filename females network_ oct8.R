install.packages("patchwork")
library(here)
library(tidyverse)
library(ggtext)
library(forcats)
library(ggpubr)
library(gridGraphics)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(cowplot)
library(magrittr)
library(dplyr)
library(tidyr)

bmat = read_csv("all_females_vs_plants_7_oct_2024.csv")

plant_species_order <- c("Lotus corniculatus", "Centaurea nigra", "Epilobium angustifolium", "Lavandula angustifolia", "Trifolium repens",
                         "Impatiens glandulifera", "Symphoricarpos albus", "Ballota nigra", "Geranium pratense", "Vicia cracca", "Raphanus sativus",
                         "Cirsium arvense", "Ligustrum vulgare", "Cirsium vulgare", "Hypericum perforatum", "Jacobea vulgaris", "Rhinanthus minor",
                         "Tilia sp", "Calystegia silvatica", "Heracleum sphondylium", "Hydrangea paniculata", "Primula bulleyana", "Rubus fruticosus agg.",
                         "Trifolium pratense", "Cornus sanguinea", "Cotoneaster × suecicus", "Cyanus segetum", "Echium pininana", "Stachys palustris",
                         "Buddleja davidii", "Cistus salvifolius", "Filipendula ulmaria", "Lapsana communis", "Lathyrus pratensis", "Linum usitatissimum",
                         "Nepeta racemosa", "Papaver rhoeas", "Rhododendron sp1", "Rhododendron ponticum", "Salvia rosmarinus", "Salvia yangii",
                         "Silene dioica", "Campanula latifolia", "Cardamine pratensis", "Centranthus ruber", "Cistus sp", "Iris sp", "Odontites sp",
                         "Phacelia tanacetifolia", "Prunella vulgaris", "Ranunculus acris", "Rubus caesius", "Sinapis alba", "Vicia sepium")


plant_species_order <- c("Vicia sepium", "Sinapis alba", "Rubus caesius", "Ranunculus acris", "Prunella vulgaris", "Phacelia tanacetifolia", 
                         "Odontites sp", "Iris sp", "Cistus sp", "Centranthus ruber", "Cardamine pratensis", "Campanula latifolia", 
                         "Silene dioica", "Salvia yangii", "Salvia rosmarinus", "Rhododendron ponticum", "Rhododendron sp1", 
                         "Papaver rhoeas", "Nepeta racemosa", "Linum usitatissimum", "Lathyrus pratensis", "Lapsana communis", 
                         "Filipendula ulmaria", "Cistus salvifolius", "Buddleja davidii", "Stachys palustris", "Echium pininana", 
                         "Cyanus segetum", "Cotoneaster × suecicus", "Cornus sanguinea", "Trifolium pratense", "Rubus fruticosus agg.", 
                         "Primula bulleyana", "Hydrangea paniculata", "Heracleum sphondylium", "Calystegia silvatica", "Tilia sp", 
                         "Rhinanthus minor", "Jacobea vulgaris", "Hypericum perforatum", "Cirsium vulgare", "Ligustrum vulgare", 
                         "Cirsium arvense", "Raphanus sativus", "Vicia cracca", "Geranium pratense", "Ballota nigra", "Symphoricarpos albus", 
                         "Impatiens glandulifera", "Trifolium repens", "Lavandula angustifolia", "Epilobium angustifolium", "Centaurea nigra", 
                         "Lotus corniculatus")

species_order <- c("B_pascuorum", "B_terrestris", "B_lapidarius", "B_pratorum", "B_lucorum", "B_hortorum", "B_hypnorum")

species_order <- c("B_hypnorum", "B_hortorum", "B_lucorum", "B_pratorum", "B_lapidarius", "B_terrestris", "B_pascuorum")

blong = bmat %>% pivot_longer(starts_with("B_"), names_to = "Species", values_to = "Weight") %>% 
  # mutate(across(where(is.character), as.factor)) %>% 
  group_by(Plant_species) %>%
  mutate(num_obs = sum(Weight),
         num_spec = nlevels(as.factor(Species[Weight != 0]))) %>%
  arrange(desc(num_spec), desc(num_obs)) %>% ungroup() %>% 
  mutate(Plant_species = factor(Plant_species, levels = plant_species_order)) %>% 
  mutate(Species = factor(Species, levels = species_order)) %>% 
  mutate(across(where(is.factor), as.numeric, .names = "l_{.col}"))

bstat = bmat %>% rowwise() %>% mutate(bla  = across(where(is.numeric), function(x){if_else(x > 0, 1, 0)}),
                                      num_spec = sum(c_across(starts_with("bla"))),
                                      num_obs  = rowSums(across(starts_with("B_")))) %>% 
  select(Plant_species, num_spec, num_obs, everything(), -starts_with("bla")) %>% 
  arrange(desc(num_spec), desc(num_obs)) %>% ungroup()


# Data manipulation
blong_selection = blong %>%
  filter(Weight != 0) %>%
  droplevels() %>%
  mutate(Plant_species = fct_relevel(Plant_species, plant_species_order)) %>%
  mutate(Species = fct_relevel(Species, species_order)) %>%
  mutate(
    across(where(is.factor), as.numeric, .names = "l_{.col}"),
    x_plant = (l_Plant_species - 1) * (max(l_Species) - 1) / (max(l_Plant_species) - 1) * 3,
    x_spec = (l_Species - 1) * 3
  )

mplant = max(blong_selection$l_Plant_species) - 1
mspec = max(blong_selection$l_Species) - 1

mid_labels = (mplant - 1) * mspec / mplant / 2

plant_labels = blong_selection %>%
  group_by(Plant_species, l_Plant_species) %>%
  summarise(x = min(x_plant), y = -.1)
spec_labels = blong_selection %>%
  group_by(Species) %>%
  summarise(x = min(x_spec), y = 1.1)

# Define the color vectors according to the unique labels
plant_labels_unique <- unique(plant_labels$Plant_species)
species_labels_unique <- unique(spec_labels$Species)

# Create color vectors
plant_colors <- setNames(rep("chartreuse4", length(plant_labels_unique)), plant_labels_unique)
species_colors <- setNames(c("#F94144", "#F3722C", "#F8961E", "#F9C74F", "#90BE6D", "#43AA8B", "#577590"), species_labels_unique)

species_colors <- setNames(c("#577590", "#43AA8B", "#90BE6D", "#F9C74F", "#F8961E", "#F3722C", "#F94144"), species_labels_unique)

# Combine both color vectors
combined_colors <- c(plant_colors, species_colors)

# Assuming blong_selection, mid_labels, spec_labels, and plant_labels are already defined

p1 <- ggplot(blong_selection) +  
  geom_segment(aes(x = x_plant, y = 0, xend = x_spec, yend = 0.8, size = Weight, color = Species), lineend = "round") +
  geom_tile(aes(x = x_plant, y = -0.052, width = 0.06 + Weight/100, height = 0.1, fill = Plant_species)) +  # Adjust width and height for plant nodes
  geom_tile(aes(x = x_spec, y = 0.84, width = 0.06 + Weight/15, height = 0.1, fill = Species)) +  # Adjust width and height for species nodes
  scale_size_continuous(range = c(0.25, 3)) +  # Adjust thickness range for lines
  scale_color_manual(values = species_colors) +  # Define specific colors for species
  scale_fill_manual(values = combined_colors) +  # Assign specific colors for plants and species
  theme_void(base_size = 15) +
  # guides(size = "none") +
  theme(legend.position = "none") +
  theme(legend.direction = "horizontal") +
  annotate("text", x = mid_labels, y = -0.7, label = "Plants", fontface = 2, size = 7) +
  annotate("text", x = mid_labels, y = 1.3, label = "Bombus Species", fontface = 2, size = 7) +
  annotate("text", x = spec_labels$x, y = spec_labels$y - .05, label = spec_labels$Species, fontface = 3) +
  annotate("text", x = plant_labels$x, y = plant_labels$y - .009, label = plant_labels$Plant_species, size = 3.5, angle = 90, hjust = 1, fontface = 3)

print(p1)


#--------------Make plot vertical-----------------------

p1 <- ggplot(blong_selection) +  
  # Swap the x and y axes for vertical layout
  geom_segment(aes(y = x_plant, x = 0, yend = x_spec, xend = 0.8, size = Weight, color = Species), lineend = "round") +
  
  # Adjust the positions of plant and species tiles accordingly
  geom_tile(aes(y = x_plant, x = -0.052, height = 0.06 + Weight/100, width = 0.1, fill = Plant_species)) +  # Plant nodes on the right
  geom_tile(aes(y = x_spec, x = 0.84, height = 0.06 + Weight/15, width = 0.1, fill = Species)) +  # Species nodes on the left
  
  # Adjust size and color scales
  scale_size_continuous(range = c(0.25, 3)) +  # Adjust thickness range for lines
  scale_color_manual(values = species_colors) +  # Define specific colors for species
  scale_fill_manual(values = combined_colors) +  # Assign specific colors for plants and species
  
  # Apply theme settings
  theme_void(base_size = 15) +
  theme(legend.position = "none") +
  theme(legend.direction = "horizontal") +
  
  # Annotations for "Plants" and "Bombus Species" to match vertical orientation
  annotate("text", y = mid_labels, x = 1.3, label = "Bombus species", fontface = 2, size = 7) +
  annotate("text", y = mid_labels, x = -0.7, label = "Plants", fontface = 2, size = 7) +
  
  # Adjust species labels (left side)
  annotate("text", y = spec_labels$x, x = spec_labels$y - .05, label = spec_labels$Species, fontface = 3) +
  
  # Adjust plant labels (right side)
  annotate("text", y = plant_labels$x, x = plant_labels$y - .009, label = plant_labels$Plant_species, size = 3.5, angle = 0, hjust = 1, fontface = 3)

print(p1)
