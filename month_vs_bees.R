install.packages("ggplot2")  # Install ggplot2 if not installed
library(ggplot2)  # Load the ggplot2 library


# Read data from a CSV file
# Replace "bumblebee_data.csv" with the actual path to your CSV file
data <- read.csv("bees_vs_month.csv")

# Ensure that the "Month" column is treated as a factor and ordered correctly
data$Month <- factor(data$Month, 
                     levels = c("Jun_22", "Jul_22", "Aug_22", "Jun_23","Jul_23", "Aug_23"))

# Plot: 7 histograms (one for each species), separating male and female counts by bars
ggplot(data, aes(x = Month, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' separates male/female bars
  facet_wrap(~ Species, ncol = 2) +  # Facet by Species, creating a separate plot for each species
  labs(x = "Month", y = "Number of Bumblebees", title = "Bumblebee Counts by Species, Month, and Sex") +
  theme_minimal() +  # Apply a minimal theme for clarity
  scale_fill_manual(values = c("Females" = "gold2", "Males" = "sienna3")) +  # Custom colors for female/male
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#option 1
ggplot(data, aes(x = Month, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +  # Adjust width for spacing
  facet_wrap(~ Species, ncol = 2) +  # Facet by Species, creating a separate plot for each species
  labs(x = "Month", y = "Number of Bumblebees", title = "Bumblebee Counts by Species, Month, and Sex") +
  theme_minimal() +  # Apply a minimal theme for clarity
  scale_fill_manual(values = c("Females" = "gold2", "Males" = "sienna3")) +  # Custom colors for female/male
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

unique(data$Sex)

#option 2

ggplot(data, aes(x = Month, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' separates male/female bars
  facet_wrap(~ Species, ncol = 2, labeller = labeller(Species = label_parsed)) +  # Italicize species names
  labs(x = "Month", y = "Number of Bumblebees", title = "Bumblebee Counts by Species, Month, and Sex") +
  theme_minimal() +  # Apply a minimal theme for clarity
  scale_fill_manual(values = c("Females" = "gold2", "Males" = "sienna3")) +  # Custom colors for female/male
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis font size
    axis.text.y = element_text(size = 12),  # Increase y-axis font size
    axis.title.x = element_text(size = 14),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14),  # Increase y-axis label font size
    strip.text = element_text(face = "italic", size = 14),  # Italicize and increase species label font size
    plot.title = element_text(size = 16, face = "bold")  # Increase title font size
  )

#option3

library(ggplot2)

# Assuming the month levels are in chronological order
data$Month <- factor(data$Month, levels = unique(data$Month))

ggplot(data, aes(x = Month, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' separates male/female bars
  facet_wrap(~ Species, ncol = 2, labeller = labeller(Species = label_parsed)) +  # Italicize species names
  labs(x = "Month", y = "Number of Bumblebees", title = "Bumblebee Counts by Species, Month, and Sex") +
  theme_minimal() +  # Apply a minimal theme for clarity
  scale_fill_manual(values = c("Females" = "gold2", "Males" = "sienna3")) +  # Custom colors for female/male
  geom_vline(xintercept = which(levels(data$Month) == "Aug_22") + 0.5, linetype = "dashed", color = "black", size = 1) +  # Add line after Aug_22
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis font size
    axis.text.y = element_text(size = 12),  # Increase y-axis font size
    axis.title.x = element_text(size = 14),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14),  # Increase y-axis label font size
    strip.text = element_text(face = "italic", size = 14),  # Italicize and increase species label font size
    plot.title = element_text(size = 16, face = "bold"),  # Increase title font size
    panel.spacing = unit(1.5, "cm")  # Increase space between charts
  )



