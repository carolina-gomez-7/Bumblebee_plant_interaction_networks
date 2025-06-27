#Female & Male species-specificity index

# Load necessary libraries
install.packages("ggpubr")  # Install ggpubr (only needed once)
library(ggpubr)  # Load ggpubr for stat_compare_means()
library(readxl)   # For reading Excel files
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping
library(ggplot2)  # Visualization

# Load the dataset (update file path accordingly)
file_path <- "females_males_specificity_index.xlsx"
data <- read_excel(file_path)

# View first few rows to check structure
head(data)

# ---- Step 1: Reshape Data to Long Format ----
data_long <- data %>%
  pivot_longer(cols = c(Females_d, Males_d), 
               names_to = "Sex", 
               values_to = "Specificity_d") %>%
  mutate(Sex = ifelse(Sex == "Females_d", "Female", "Male"))

# View reshaped data
head(data_long)

# ---- Step 2: Check Normality ----
shapiro_test <- shapiro.test(data_long$Specificity_d)

# Print normality test result
print("Shapiro-Wilk Normality Test:")
print(shapiro_test)

# ---- Step 3: Perform Independent t-test or Wilcoxon Test ----
if (shapiro_test$p.value > 0.05) {
  # If normal: Perform Independent t-test
  test_result <- t.test(Specificity_d ~ Sex, data = data_long, var.equal = TRUE)
  test_type <- "Independent t-test"
} else {
  # If not normal: Perform Wilcoxon Rank-Sum Test (Mann-Whitney U)
  test_result <- wilcox.test(Specificity_d ~ Sex, data = data_long)
  test_type <- "Wilcoxon Rank-Sum Test"
}

# Print test results
print(paste("Using:", test_type))
print(test_result)

# ---- Step 4: Visualization ----
ggplot(data_long, aes(x = Sex, y = Specificity_d, fill = Sex)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Comparison of Specificity Index (d') Between Sexes",
       x = "Bumblebee Sex",
       y = "Specificity Index (d')") +
  theme_minimal()

#plot option 2----

# Extract p-value
p_value <- test_result$p.value
p_label <- ifelse(p_value < 0.001, "p < 0.001", paste("p =", round(p_value, 3)))

# Print test results
print(paste("Using:", test_type))
print(test_result)

# ---- Step 3: Boxplot with P-Value Annotation ----
ggplot(data_long, aes(x = Sex, y = Specificity_d, fill = Sex)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jittered points
  scale_fill_manual(values = c("Female" = "gold2", "Male" = "sienna3")) +  # Custom colors
  labs(title = "Comparison of Specificity Index (d') Between Sexes",
       x = "Bumblebee Sex",
       y = "Specificity Index (d')") +
  theme_minimal(base_size = 16) +  # Increase font size
  theme(legend.position = "none",  # Hide legend (since labels are clear)
        axis.text = element_text(size = 14),  # Adjust axis text size
        axis.title = element_text(size = 18), # Adjust axis title size
        plot.title = element_text(size = 20, face = "bold")) +  # Make title larger and bold
  stat_compare_means(method = ifelse(shapiro_test$p.value > 0.05, "t.test", "wilcox.test"),
                     label.x = 1.5, label.y = max(data_long$Specificity_d) + 0.05, 
                     size = 6)  # Adds p-value above the boxplot



#------specifity indices---scatter plot--may24-------------------

# Load required libraries
library(ggplot2)

# Create a data frame with d' values for each sex and species
df <- data.frame(
  Species = c("Bombus pascuorum", "Bombus terrestris", "Bombus lapidarius",
              "Bombus pratorum", "Bombus lucorum", "Bombus hortorum", "Bombus hypnorum"),
  Female_d = c(0.37, 0.39, 0.34, 0.50, 0.40, 0.72, 0.51),
  Male_d = c(0.39, 0.29, 0.50, 0.63, 0.36, 0.64, 0.61)
)

# Optional: sort by female d' for nicer plotting order
df$Species <- factor(df$Species, levels = df$Species[order(df$Female_d)])

# Melt the data into long format for ggplot (if you prefer grouped barplot)
library(tidyr)
df_long <- pivot_longer(df, cols = c(Female_d, Male_d), names_to = "Sex", values_to = "d_prime")

# Scatter plot comparing male vs. female d' per species
ggplot(df, aes(x = Female_d, y = Male_d, label = Species)) +
  geom_point(size = 3, color = "darkorange") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(hjust = -0.1, vjust = 0, size = 3) +
  labs(
    title = "Comparison of Specialization Index (d') Between Male and Female Bumblebees",
    x = "Female d′",
    y = "Male d′"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  # Adjust max if needed
  theme_minimal(base_size = 12)


