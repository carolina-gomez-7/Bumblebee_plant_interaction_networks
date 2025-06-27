# Load necessary libraries
library(ggplot2)
library(readr)
library(reshape2)
library(gridExtra)

# Read in the data
df <- read_csv("bees_vs_month.csv")

# Ensure correct ordering of categorical variables
df$Month <- factor(df$Month, levels = c("Jun_22", "Jul_22", "Aug_22", "Jun_23", "Jul_23", "Aug_23"))
df$Sex <- as.factor(df$Sex)
df$Species <- as.factor(df$Species)

# Create faceted bar plots (1 per species) showing male and female abundance per month
plot <- ggplot(df, aes(x = Month, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Species, ncol = 2) +
  labs(
    title = "Monthly Bumblebee Abundance by Species and Sex",
    x = "Month",
    y = "Abundance"
  ) +
  scale_fill_manual(values = c("Females" = "#e19f3d", "Males" = "#13213c")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic", size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

# Print the plot
print(plot)


# Two-way ANOVA: test for overall differences in abundance by sex, month, and interaction
anova_model <- aov(Count ~ Sex * Month, data = df)
summary(anova_model)

# Check assumptions
# Normality of residuals
shapiro.test(residuals(anova_model))
qqnorm(residuals(anova_model)); qqline(residuals(anova_model))

# Homogeneity of variances
library(car)
leveneTest(Count ~ Sex * Month, data = df)

# Visualize with boxplot
ggplot(df, aes(x = Month, y = Count, fill = Sex)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Females" = "#e19f3d", "Males" = "#13213c")) +
  theme_minimal() +
  labs(title = "Bee Count by Month and Sex")

# Analyze differences within each species individually
species_list <- unique(df$Species)
plot_list <- list()

for (sp in species_list) {
  df_sp <- subset(df, Species == sp)
  
  # Only proceed if at least 2 levels for both factors
  if (length(unique(df_sp$Month)) > 1 && length(unique(df_sp$Sex)) > 1) {
    model_sp <- aov(Count ~ Sex * Month, data = df_sp)
    cat("\nANOVA results for:", sp, "\n")
    print(summary(model_sp))
    
    # Plot for this species
    p <- ggplot(df_sp, aes(x = Month, y = Count, fill = Sex)) +
      geom_boxplot() +
      geom_jitter(position = position_jitterdodge(jitter.width = 0.2), size = 1.5) +
      labs(title = paste("Counts for", sp), x = "Month", y = "Count") +
      scale_fill_manual(values = c("Females" = "gold2", "Males" = "sienna3")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plot_list[[sp]] <- p
  }
}

# Display plots in a grid
grid.arrange(grobs = plot_list, ncol = 2)
