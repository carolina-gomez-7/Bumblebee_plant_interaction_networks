# Load necessary libraries
library(ggplot2)
library(readxl)

# Read the Excel file
data <- read_excel("bumblebees_vs_sex-10feb.xlsx", sheet = "Sheet1")

# Convert to long format for ggplot
data_long <- reshape2::melt(data, id.vars = "Bumblebee_species", 
                            variable.name = "Sex", 
                            value.name = "Abundance")

# Sort species by total abundance (most abundant on top)
data$Total <- data$Female + data$Male
sorted_species <- data$Bumblebee_species[order(-data$Total)]

data_long$Bumblebee_species <- factor(data_long$Bumblebee_species, levels = rev(sorted_species))

# Create the bar plot
ggplot(data_long, aes(x = Bumblebee_species, y = Abundance, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Horizontal bars
  scale_fill_manual(values = c("Female" = "gold2", "Male" = "sienna3")) +
  labs(x = "Bumblebee Species", y = "Abundance", fill = "Sex") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, face = "italic", hjust = 1.3),  # Bring species names closer to bars
    panel.grid.major = element_blank(),  # Remove grid
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "grey36"),  # Draw x-axis line
    # axis.line.y = element_line(color = "grey36", size = 1) # Draw y-axis line
  ) +
  geom_text(aes(label = Abundance), position = position_dodge(width = 0.9), hjust = -0.2)  # Add labels at end of bars
