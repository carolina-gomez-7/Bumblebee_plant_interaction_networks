# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read data from CSV file
# Replace 'your_file.csv' with the path to your actual CSV file
data <- read.csv("plant_family_visits_23_oct.csv")

# Make sure the columns are properly named and formatted
# The CSV should have columns: PlantFamily, BumblebeeSpecies, and Visits

# Create a summary table to calculate the percentage of visits
data_summary <- data %>%
  group_by(Plant_family) %>%
  mutate(Percentage = Visits / sum(Visits) * 100)


# Define a custom color palette for each bumblebee species
species_colors <- c(
  "Bombus pascuorum" = "#F94144",  
  "Bombus terrestris" = "#F3722C",  
  "Bombus lapidarius"   = "#F8961E", 
  "Bombus pratorum"  = "#F9C74F",
  "Bombus lucorum"  = "#90BE6D",
  "Bombus hortorum" = "#43AA8B")
  # "Bombus hypnorum" = "#577590",
  # "Bombus barbutellus" = "#3f37c9",
  # "Bombus rupestris" = "#560bad",
  # "Bombus vestalis" = "#b5179e")

# Custom order for bumblebee species
bumblebee_order <- c(
  "Bombus pascuorum",  
  "Bombus terrestris",  
  "Bombus lapidarius",   
  "Bombus pratorum",  
  "Bombus lucorum",  
  "Bombus hortorum")
  # "Bombus hypnorum", 
  # "Bombus barbutellus", 
  # "Bombus rupestris", 
  # "Bombus vestalis"


reversed_bumblebees <- rev(bumblebee_order)

Plant_family_order <- c("Apiaceae", "Asteraceae", "Balsaminaceae", "Boraginaceae",
                        "Brassicaceae", "Campanulaceae", "Caprifoliaceae", "Caryophyllaceae",
                        "Cistaceae", "Convolvulaceae", "Cornaceae", "Ericaceae", "Fabaceae",
                        "Geraniaceae", "Hydrangeaceae", "Hypericaceae", "Iridaceae", "Lamiaceae",
                        "Linaceae", "Oleaceae", "Onagraceae", "Orobanchaceae", "Papaveraceae",
                        "Plantaginaceae", "Primulaceae", "Ranunculaceae", "Rosaceae", 
                        "Scrophulariaceae", "Tiliaceae")

reversed_plant_families <- rev(Plant_family_order)

print(reversed_plant_families)

# Alphabetical order for plant families
# Assuming 'Plant_family' is a character column, convert it to a factor
data_summary$Plant_family <- factor(data_summary$Plant_family, levels = reversed_plant_families)

# Ensure the Bumblebee species are in the desired order using factor()
data_summary$Bumblebee_species <- factor(data_summary$Bumblebee_species, levels = reversed_bumblebees)

# Plot the 100% stacked bar chart with horizontal bars
ggplot(data_summary, aes(x = Bumblebee_species, y = Percentage, fill = Plant_family)) +
  geom_bar(stat = "identity", position = "fill") + # 'fill' ensures 100% stacking
  coord_flip() + # Horizontal bars
  labs(
    title = "Percentage of Bumblebee Species Visits to Plant Families",
    x = "Bumblebee species",
    y = "Percentage of Visits",
    fill = "Plant family"
  ) +
  scale_y_continuous(labels = scales::percent) + # Show percentage on y-axis
  theme_minimal() +
  
  # Position the legend at the bottom
  theme(legend.position = "bottom", 
        legend.title = element_text(face = "bold"), # Optionally make the legend title bold
        legend.text = element_text(size = 10))  # Adjust legend text size if needed
  # 
  # # Use scale_fill_manual() to assign specific colors to each species
  # scale_fill_manual(values = species_colors)


#-----------option2 -----------------

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Read the CSV file where rows are plant families and columns are bumblebee species
# Replace 'your_matrix_data.csv' with your actual file path
data_matrix <- read.csv("percentage_visits_plantfamilies_23_oct.csv", row.names = 1, check.names = FALSE)

# Check the first few rows of the matrix to ensure it's loaded correctly
head(data_matrix)

# Reshape the matrix from wide to long format using melt() from reshape2
data_long <- melt(data_matrix, id.vars = "PlantFamily")

# Rename the columns for better readability
colnames(data_long) <- c("PlantFamily", "BumblebeeSpecies", "Percentage")

# Plot the 100% stacked bar chart with horizontal bars (since percentages are already given)
ggplot(data_long, aes(x = PlantFamily, y = Percentage, fill = BumblebeeSpecies)) +
  geom_bar(stat = "identity", position = "fill") + # 'fill' ensures 100% stacking
  coord_flip() + # Horizontal bars
  labs(
    title = "Percentage of Bumblebee Species Visits to Plant Families",
    x = "Plant Family",
    y = "Percentage of Visits",
    fill = "Bumblebee Species"
  ) +
  scale_y_continuous(labels = scales::percent) + # Show percentage on y-axis
  theme_minimal()
