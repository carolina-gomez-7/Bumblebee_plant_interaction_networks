
# Load necessary library
library(ggplot2)

# Example data creation (replace with actual data frames)
# female_visitation and male_visitation are matrices with flower species as rows
# Columns represent bumblebee species; here we aggregate across bee species per flower

# Adjust the file paths as necessary
females_visitation <- read.csv("all_females_vs_plants_5_NOV_2024.csv", row.names = 1)
males_visitation <- read.csv("all_males_vs_plants_5_NOV_2024.csv", row.names = 1)

# Ensure matrices are in the correct format
females_visitation <- as.matrix(females_visitation)
males_visitation <- as.matrix(males_visitation)

# # Replace with your actual data frames/matrices
# female_visitation <- matrix(sample(1:10, 60, replace = TRUE), nrow = 30, ncol = 2)
# male_visitation <- matrix(sample(1:10, 60, replace = TRUE), nrow = 30, ncol = 2)

# Calculate total visits (rows: flower species, columns: bee species)
total_females <- rowSums(females_visitation)
total_males <- rowSums(males_visitation)

# Calculate sex ratio and total visits per flower species
flower_species <- rownames(females_visitation)  # Replace if you have species names
total_visits <- total_females + total_males
sex_ratio <- total_males / total_females

# Combine into a data frame and filter for flower species with more than 1 visit
data <- data.frame(
  Species = flower_species,
  Total_Visits = total_visits,
  Sex_Ratio = sex_ratio
)
data <- data[data$Total_Visits > 1, ]

# Calculate overall sex ratio
overall_male_female_ratio <- sum(total_males) / sum(total_females)


# Calculate expected range for sex ratio under null hypothesis (random variation)
# Here we create a smooth shaded area for the 2.5th and 97.5th percentiles
data$Expected_Upper <- qbinom(0.975, data$Total_Visits, prob = overall_male_female_ratio) / data$Total_Visits
data$Expected_Lower <- qbinom(0.025, data$Total_Visits, prob = overall_male_female_ratio) / data$Total_Visits

# Load necessary libraries
library(ggplot2)
library(ggrepel)  # For avoiding text overlap

# Plotting with ggplot2
plot <- ggplot(data, aes(x = Total_Visits, y = Sex_Ratio)) +
  geom_point(color = "chartreuse4", size = 3) +  # Scatter plot for flower species
  geom_ribbon(aes(ymin = Expected_Lower, ymax = Expected_Upper), fill = "lightgrey", alpha = 0.5) +
  geom_text_repel(aes(label = Species), size = 4, max.overlaps = Inf, segment.color = NA) +  # No connecting lines
  geom_hline(yintercept = overall_male_female_ratio, linetype = "dashed", color = "red") +  # Add sex ratio line
  labs(
    title = "Sex Ratio (M:F) of Flower-Visiting Bees Across Flower Species",
    x = "Total Bees Collected from Flower Species",
    y = "Male:Female Ratio"
  ) +
  theme_minimal()

# Show the plot
print(plot)