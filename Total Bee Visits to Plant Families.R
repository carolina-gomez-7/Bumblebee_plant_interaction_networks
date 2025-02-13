# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load data (replace 'data.csv' with your actual file)
# Assumes the dataset has columns: 'PlantFamily' and 'BeeVisits'
Plant_family_visits <- read.csv("plant_family_total_visits.csv")

# Summarize total visits per plant family
summary_data <- data %>%
  group_by(PlantFamily) %>%
  summarize(TotalVisits = sum(BeeVisits, na.rm = TRUE))

# Create the bar plot
ggplot(Plant_family_visits, aes(x = reorder(Plant_family, -Total_visits), y = Total_visits)) +
  geom_bar(stat = "identity", fill = "olivedrab") +
  theme_minimal() +
  labs(
    title = "Total Bee Visits to Plant Families",
    x = "Plant Family",
    y = "Total Number of Visits"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.00001, size = 14, margin = margin(t = 0.5)),
    axis.text.y = element_text(size = 14),  # Increase y-axis number size
    axis.title.x = element_text(size = 16, margin = margin(t = 50)),
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18, hjust = 0.5), 
        panel.grid = element_blank()  # Removes grid lines
        )
