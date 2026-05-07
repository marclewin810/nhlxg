# ==========================================
# FINAL PROJECT: NHL Expected Goals (xG) Model
# Author: Marc Lewin
# ==========================================

# --------------------------------------------------------------------------------------------
# STEP 1: Install and Load Required Packages
# --------------------------------------------------------------------------------------------
install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("danmorse314/hockeyR")
install.packages("sportyR")
install.packages("hexbin")
install.packages("gridExtra")
install.packages("pROC")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")


# Load the libraries into current session
library(tidyverse) # This includes ggplot2 for visuals and dplyr for data cleaning
library(hockeyR)   # The package that contains the NHL data
library(sportyR)   # Drawing the NHL rink background
library(gridExtra) # For making a neat, professional table
library(pROC)      # ROC curve  
library(rpart)     # Decision Tree
library(rpart.plot) # Plotting the Decision Tree
library(randomForest) # Random Forest

# --------------------------------------------------------------------------------------------
# STEP 2: Pull the Play-by-Play Data
# --------------------------------------------------------------------------------------------
options(download.file.method = "libcurl")
options(timeout = 300)
# We are pulling 5 seasons of data. 
# Note: In hockeyR, the year represents the season's ending year (e.g., 2025 = 2024-25 season).
# Define the specific seasons we want
target_seasons <- c(2019, 2020, 2022, 2023, 2024)

# Create an empty list to store each season's data
pbp_list <- list()

# Loop through our specific years
for (year in target_seasons) {
  print(paste("Downloading and formatting season:", year)) 
  
  # Download a single season
  temp_data <- load_pbp(year)
  
  # Convert every column to character so they stack perfectly
  temp_data <- temp_data %>%
    mutate(across(everything(), as.character))
  
  pbp_list[[as.character(year)]] <- temp_data
}

# Stack all 5 datasets into one massive dataframe
raw_pbp_data <- bind_rows(pbp_list)

# Now that they are stacked, we convert the columns we need for our EDA back into numbers (numerics). 
clean_pbp_data <- raw_pbp_data %>%
  mutate(
    x_fixed = as.numeric(x_fixed),
    y_fixed = as.numeric(y_fixed),
    shot_distance = as.numeric(shot_distance),
    shot_angle = as.numeric(shot_angle),
    period = as.numeric(period),
    period_seconds_remaining = as.numeric(period_seconds_remaining)
  )

# Check the dimensions to ensure it worked!
dim(clean_pbp_data)

# Free up space by removing the raw dataset since we already have the clean dataset
rm(raw_pbp_data)
gc()

# --------------------------------------------------------------------------------------------
# STEP 3: Filter Data and Reduce Memory Footprint
# --------------------------------------------------------------------------------------------
# We are filtering for unblocked shots and keeping only the columns needed  for the xG models and EDA

model_data <- clean_pbp_data %>%
  # Filter for unblocked shots (Goals, Saves, and Misses)
  filter(event_type %in% c("SHOT", "GOAL", "MISSED_SHOT")) %>%
  
  # Exclude empty net goals to prevent model distortion
  filter(is.na(empty_net) | empty_net == "FALSE") %>%
  
  # Select ONLY the essential columns to free up RAM
  select(
    season, 
    game_date, 
    period, 
    period_seconds_remaining,
    event_type, 
    event_team_abbr, 
    event_player_1_name, 
    strength_state,      
    x_fixed, 
    y_fixed, 
    shot_distance, 
    shot_angle, 
    secondary_type       # Shot type (Wrist, Slap, etc.)
  ) %>%
  
  # Create our binary target variable: 1 for Goal, 0 for Not a Goal
  mutate(is_goal = ifelse(event_type == "GOAL", 1, 0))

# Check the new dimensions
print("New model_data dimensions:")
dim(model_data)

# Delete the massive dataset since we now have model data and run garbage collection.
rm(clean_pbp_data)
gc()

# --------------------------------------------------------------------------------------
# STEP 4: EDA 1 - Spatial Shot Density Map
# --------------------------------------------------------------------------------------
# We will use 'sportyR' to draw a mathematically accurate NHL rink.
# We then "fold" the ice so all shots appear in one offensive zone.

# Prepare the data: Fold the ice
# An NHL rink's X-axis goes from -100 to 100. The right-side offensive zone is from roughly 
# X=25 to X=100. Taking the absolute value forces all shots there.
density_data <- model_data %>%
  mutate(
    x_fold = abs(x_fixed),
    y_fold = y_fixed
  )

# Build the Spatial Shot Density Plot
# geom_hockey() acts as our base ggplot layer. "ozone" zooms in on the offensive zone.
eda1_plot <- geom_hockey("nhl", display_range = "ozone") +
  # Add the hex-bins for shot density
  geom_hex(
    data = density_data,
    aes(x = x_fold, y = y_fold),
    alpha = 0.8,               # Slight transparency to see the rink lines underneath
    binwidth = c(3, 3)          # Size of the hexagons (3 feet by 3 feet)
  ) +
  # Use a professional color scale
  scale_fill_viridis_c(
    option = "inferno", 
    direction = -1, 
    name = "Shot Volume"
  ) +
  # Active title and subtitle
  labs(
    title = "NHL shots are heavily concentrated in the high-danger slot",
    subtitle = "Unblocked shots from 2018-2024 (Excluding 2020-21)",
    caption = "Data: hockeyR | Map: sportyR"
  ) +
  # Clean up clutter
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30"),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm") 
  )

# Display the plot
print(eda1_plot)
# Save the plot as a high-quality PNG image
ggsave("eda1_shot_density.png", plot = eda1_plot, width = 10, height = 8.2, dpi = 300)

# -------------------------------------------------------------------------------------
# STEP 5: EDA 2 - Shot Type Efficiency
# -------------------------------------------------------------------------------------

# Prepare and CLEAN the data
efficiency_data <- model_data %>%
  filter(!is.na(secondary_type)) %>%
  mutate(
    temp_type = str_to_lower(secondary_type), 
    clean_shot_type = case_when(
      str_detect(temp_type, "snap") ~ "Snap Shot",
      str_detect(temp_type, "wrist") ~ "Wrist Shot",
      str_detect(temp_type, "slap") ~ "Slap Shot",
      str_detect(temp_type, "wrap") ~ "Wrap-Around",
      str_detect(temp_type, "tip") ~ "Tip-In",
      str_detect(temp_type, "deflect") ~ "Deflected",
      str_detect(temp_type, "backhand") ~ "Backhand",
      str_detect(temp_type, "between") ~ "Between Legs",
      str_detect(temp_type, "bat") ~ "Batted",
      str_detect(temp_type, "poke") ~ "Poke",
      str_detect(temp_type, "penalty") ~ "Penalty Shot",
      TRUE ~ str_to_title(temp_type) 
    )
  ) %>%
  
  # Remove Penalty Shots because they skew in-game xG models
  filter(clean_shot_type != "Penalty Shot") %>%
  
  group_by(clean_shot_type) %>%
  summarize(
    total_shots = n(),
    goals = sum(is_goal),
    shooting_pct = goals / total_shots,
    .groups = 'drop'
  ) %>%
  filter(total_shots > 50) 

# Build the Bar Chart
eda2_plot <- ggplot(
  data = efficiency_data, 
  aes(x = reorder(clean_shot_type, shooting_pct), y = shooting_pct, fill = shooting_pct)
) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = scales::percent(shooting_pct, accuracy = 0.1)), 
    hjust = -0.2, 
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Batted pucks and redirections are the most dangerous in-game shots",
    subtitle = "Average shooting percentage by unblocked shot type (2018-24, excluding 2020-21)",
    x = NULL, 
    y = "Shooting Percentage",
    caption = "Data: hockeyR"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 20)),
    legend.position = "none", 
    panel.grid.major.y = element_blank(), 
    axis.text.y = element_text(size = 11, face = "bold")
  )

# Display the plot
print(eda2_plot)
# Save the plot
ggsave("eda2_shot_efficiency.png", plot = eda2_plot, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------------------------
# STEP 5.5: EDA 2 - Shot Type Efficiency RAW NUMBERS TABLE
# -------------------------------------------------------------------------------------
# VIEW TOTAL COUNTS OF EACH SHOT TYPE TO SEE DIFFERENCE IN SAMPLE SIZES
# View the raw shot counts, sorted from most to least
efficiency_data %>%
  select(clean_shot_type, total_shots, goals) %>%
  arrange(desc(total_shots)) %>%
  print()

# Create a visual table of the raw numbers
# Format the data to look like a professional table
table_data <- efficiency_data %>%
  select(clean_shot_type, total_shots, goals) %>%
  arrange(desc(total_shots)) %>%
  # Rename the columns so they look like clean headers, not code variables
  rename(
    `Shot Type` = clean_shot_type,
    `Total Shots` = total_shots,
    `Goals` = goals
  )

# Convert the dataframe into a visual graphic
table_visual <- tableGrob(table_data, rows = NULL) 

# Save the table as a mini image
ggsave("eda2_raw_numbers_table.png", plot = table_visual, width = 4, height = 5, dpi = 300)

# ------------------------------------------
# STEP 6: EDA 3 - The Distance Distribution
# ------------------------------------------

# Prepare the data: Filter out any rows missing a distance measurement
distance_data <- model_data %>%
  filter(!is.na(shot_distance))

# Build the Density Plot
eda3_plot <- ggplot(data = distance_data, aes(x = shot_distance)) +
  
  # geom_density creates a smooth curve showing where shots are concentrated
  geom_density(
    fill = "#21908C",  
    color = "#114a48", 
    alpha = 0.7
  ) +
  
  # Add a vertical dashed line to show the median shot distance
  geom_vline(
    aes(xintercept = median(shot_distance)),
    color = "black",
    linetype = "dashed",
    linewidth = 1 
  ) +
  
  # Label the median line with the exact calculated number
  annotate(
    "text",
    x = median(distance_data$shot_distance) + 2, 
    y = 0.025, 
    label = paste("Median:", round(median(distance_data$shot_distance), 1), "ft"),
    hjust = 0,
    fontface = "bold",
    size = 5
  ) +
  
  # Format the X-axis: 10-foot increments up to 100 feet (the max length of the offensive zone)
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  
  # Active title and clean labels
  labs(
    title = "Shot frequency drops dramatically beyond the faceoff circles (40 feet)",
    subtitle = "Density distribution of unblocked shot distances (2018-24, excluding 2020-21)",
    x = "Distance from Net (Feet)",
    y = "Density (Concentration of Shots)",
    caption = "Data: hockeyR"
  ) +
  
  # Clutter-free theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(), 
    axis.text = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold")
  )

# Display the plot
print(eda3_plot)
# Save the plot
ggsave("eda3_shot_distance.png", plot = eda3_plot, width = 10, height = 6, dpi = 300)

# ----------------------------------------------------------------------------------------------
# STEP 7: EDA 4 - Goals vs. Non-Goals Distance
# ----------------------------------------------------------------------------------------------

# Prepare the data: Create a clean text column for the legend
eda4_data <- distance_data %>%
  mutate(shot_result = ifelse(is_goal == 1, "Goal", "No Goal"))

# Calculate the exact medians for both groups to draw our reference lines
median_lines <- eda4_data %>%
  group_by(shot_result) %>%
  summarize(median_dist = median(shot_distance), .groups = 'drop')

# Extract the numbers to use in our subtitle
goal_med <- median_lines$median_dist[median_lines$shot_result == "Goal"]
nogoal_med <- median_lines$median_dist[median_lines$shot_result == "No Goal"]

# Build the Overlaid Density Plot
eda4_plot <- ggplot(data = eda4_data, aes(x = shot_distance, fill = shot_result, color = shot_result)) +
  
  # geom_density with 60% opacity so we can see where the shapes overlap
  geom_density(alpha = 0.6) +
  
  # Add dashed vertical lines for the medians
  geom_vline(
    data = median_lines, 
    aes(xintercept = median_dist, color = shot_result),
    linetype = "dashed", 
    linewidth = 1
  ) +
  
  # Set professional colors: A deep blue for saves/misses, and a bright gold/orange for goals
  scale_fill_manual(values = c("Goal" = "#E69F00", "No Goal" = "#2C3E50")) +
  scale_color_manual(values = c("Goal" = "#B37A00", "No Goal" = "#1A252F")) +
  
  # Format X-axis like the last plot
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  
  # Active title and highly specific subtitle using our dynamic calculations
  labs(
    title = "Shot proximity dictates success: Goals occur significantly closer to the net",
    subtitle = paste0("The median goal is scored from ", goal_med, " feet, while the median non-goal is taken from ", nogoal_med, " feet."),
    x = "Distance from Net (Feet)",
    y = "Density (Concentration of Events)",
    fill = "Shot Result",
    color = "Shot Result",
    caption = "Data: hockeyR"
  ) +
  
  # Clean theme formatting
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b = 20)),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold")
  )

# Display the plot
print(eda4_plot)
# Save the plot
ggsave("eda4_goals_vs_nogoals.png", plot = eda4_plot, width = 10, height = 6, dpi = 300)

# --------------------------------------------------------------------------------------
# STEP 8 : EDA 5 - Shot Angle Efficiency
# --------------------------------------------------------------------------------------

# Prepare the data: Group shots into buckets
angle_data <- model_data %>%
  filter(!is.na(shot_angle)) %>%

  mutate(
    angle_bin = cut(
      shot_angle, 
      breaks = c(seq(0, 90, by = 10), 180), 
      include.lowest = TRUE, 
      right = FALSE,
      labels = c("0-10°", "10-20°", "20-30°", "30-40°", "40-50°", "50-60°", "60-70°", "70-80°", "80-90°", "> 90°")
    )
  ) %>%
  
  filter(!is.na(angle_bin)) %>% 
  group_by(angle_bin) %>%
  summarize(
    total_shots = n(),
    goals = sum(is_goal),
    shooting_pct = goals / total_shots,
    .groups = 'drop'
  ) %>%
  filter(total_shots > 50) 

# Build the Bar Chart
eda5_plot <- ggplot(data = angle_data, aes(x = angle_bin, y = shooting_pct, fill = shooting_pct)) +
  geom_col() +
  
  geom_text(
    aes(label = scales::percent(shooting_pct, accuracy = 0.1)), 
    vjust = -0.5, 
    size = 4,
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(option = "mako", direction = -1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +

  labs(
    title = "Shooting percentage naturally fades as shots move away from dead center",
    subtitle = "Conversion rate by shot angle (2018-24, excluding 2020-21)",
    x = "Shot Angle (0° is dead center, 90° is the goal line, >90° is behind the net)",
    y = "Shooting Percentage",
    caption = "Data: hockeyR"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 20)),
    legend.position = "none", 
    panel.grid.major.x = element_blank(), 
    axis.text = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold")
  )

# Display the plot
print(eda5_plot)
# Save the final EDA plot
ggsave("eda5_shot_angle.png", plot = eda5_plot, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------------------------
# STEP 8.5: EDA 5 - Shot Angle Efficiency RAW NUMBERS TABLE
# -------------------------------------------------------------------------------------
# Format the angle data to look like a professional table
table_data_eda5 <- angle_data %>%
  select(angle_bin, total_shots, goals) %>%
  # Rename the columns for clean presentation
  rename(
    `Shot Angle` = angle_bin,
    `Total Shots` = total_shots,
    `Goals` = goals
  )

# Convert the dataframe into a visual graphic (tableGrob)
# rows = NULL prevents it from adding row numbers to the left side
table_visual_eda5 <- tableGrob(table_data_eda5, rows = NULL) 

# Save the table as a mini image
# Width of 4 and height of 5 keeps it in a clean, vertical column format
ggsave("eda5_raw_numbers_table.png", plot = table_visual_eda5, width = 4, height = 5, dpi = 300)




# --------------------------------------------------------------------------------------------
# STEP 9: Final Feature Engineering & Train/Test Split (Final Data Preperation) --------------
# --------------------------------------------------------------------------------------------

# 1. Finalize the Modeling Dataset
ml_data <- model_data %>%
  filter(!is.na(shot_distance) & !is.na(shot_angle) & !is.na(secondary_type)) %>%
  
  mutate(
    temp_type = str_to_lower(secondary_type),
    clean_shot_type = case_when(
      str_detect(temp_type, "snap") ~ "Snap Shot",
      str_detect(temp_type, "wrist") ~ "Wrist Shot",
      str_detect(temp_type, "slap") ~ "Slap Shot",
      str_detect(temp_type, "wrap") ~ "Wrap-Around",
      str_detect(temp_type, "tip") ~ "Tip-In",
      str_detect(temp_type, "deflect") ~ "Deflected",
      str_detect(temp_type, "backhand") ~ "Backhand",
      str_detect(temp_type, "between") ~ "Between Legs",
      str_detect(temp_type, "bat") ~ "Batted",
      str_detect(temp_type, "poke") ~ "Poke",
      TRUE ~ str_to_title(temp_type)
    )
  ) %>%
  
  filter(clean_shot_type != "Penalty Shot") %>%
  
  mutate(
    clean_shot_type = as.factor(clean_shot_type),
    is_goal = as.factor(is_goal) 
  )

# 2. Chronological Train/Test Split
# The NHL API formats the 2023-24 season as "20232024"
train_data <- ml_data %>% filter(season != "20232024")test_data  <- ml_data %>% filter(season == "20232024")

# 3. Verify the Split
print("--- Machine Learning Data Split ---")
print(paste("Training Set Rows (2019-2023):", nrow(train_data)))
print(paste("Testing Set Rows (2024):", nrow(test_data)))

# --------------------------------------------------------------------------------------------
# STEP 10: Model 1 - Baseline Logistic Regression
# --------------------------------------------------------------------------------------------

# 1. Train the Baseline Model on the 2019-2023 data
# We are asking the model to predict 'is_goal' based on Distance, Angle, and Shot Type.
print("Training Baseline Logistic Regression...")

baseline_log_model <- glm(
  is_goal ~ shot_distance + shot_angle + clean_shot_type,
  data = train_data,
  family = "binomial" # This tells R it is a 0 or 1 classification problem
)

# 2. View the Statistical Significance of the Features
# This will output a table. Look at the far right column for '***' 
# The stars mean a feature is highly statistically significant in predicting a goal.
summary(baseline_log_model)

# --------------------------------------------------------------------------------------------
# STEP 10 (Part 2): Evaluate Baseline Logistic Regression
# --------------------------------------------------------------------------------------------
print("Testing model on 2024 unseen data...")

# 1. Generate probabilities (Expected Goals / xG) for the 2024 test data
test_data <- test_data %>%
  mutate(
    # predict() spits out the raw probability (xG)
    predicted_xg = predict(baseline_log_model, newdata = test_data, type = "response"),
    
    # Classification requires a hard cutoff. 
    # If the xG is greater than 0.5 (50%), the model formally predicts a "1" (Goal).
    predicted_class = factor(ifelse(predicted_xg > 0.5, 1, 0), levels = c("0", "1"))
  )

# 2. Build the Confusion Matrix
conf_matrix <- table(Predicted = test_data$predicted_class, Actual = test_data$is_goal)
print("--- Confusion Matrix ---")
print(conf_matrix)

# 3. Calculate Evaluation Metrics (Accuracy, Precision, Recall, F-Measure)
# Extract the four quadrants of the matrix
TN <- conf_matrix[1,1] # True Negatives (Predicted 0, Actual 0)
FP <- conf_matrix[2,1] # False Positives (Predicted 1, Actual 0)
FN <- conf_matrix[1,2] # False Negatives (Predicted 0, Actual 1)
TP <- conf_matrix[2,2] # True Positives (Predicted 1, Actual 1)

accuracy <- (TP + TN) / sum(conf_matrix)
precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP)) # Prevent dividing by zero error
recall <- TP / (TP + FN)
f_measure <- ifelse((precision + recall) == 0, 0, 2 * ((precision * recall) / (precision + recall)))

# 4. Print the final metrics for the report
print("--- Baseline Model Performance Metrics ---")
print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F-Measure:", round(f_measure, 4)))

# --------------------------------------------------------------------------------------------
# STEP 10 (Part 3): Finding the Optimal Threshold & AUC
# --------------------------------------------------------------------------------------------
print("Calculating ROC Curve and Optimal Threshold...")

# 1. Create the ROC Curve object
# This tests every possible probability threshold automatically
roc_curve <- roc(
  response = test_data$is_goal, 
  predictor = test_data$predicted_xg,
  levels = c("0", "1")
)

# 2. Extract the exact AUC (Area Under the Curve)
# AUC is the ultimate grade for a classification model. 
# 0.50 is a random coin flip. 1.00 is perfect prediction.
baseline_auc <- auc(roc_curve)
print(paste("Baseline Model AUC:", round(baseline_auc, 4)))

# 3. Ask the package to calculate the mathematically "best" threshold
# It does this by finding the perfect balance between True Positives and False Positives
optimal_stats <- coords(
  roc_curve, 
  "best", 
  best.method = "youden", 
  ret = c("threshold", "accuracy", "specificity", "sensitivity") # sensitivity = recall
)

print("--- Mathematically Optimal Threshold ---")
print(optimal_stats)

# ROC CURVE PLOT ------------------------------------------------------------------
print("Generating ROC Curve Plot...")

# Use ggroc to plot the ROC curve object we created earlier
roc_plot <- ggroc(roc_curve, color = "#21908C", linewidth = 1.2) +
  
  # Add a diagonal dashed line representing a 50/50 coin flip (random guessing)
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey50", linewidth = 1) +
  
  # Add labels and title
  labs(
    title = "Logistic Regression Model Performance",
    subtitle = paste("Area Under the Curve (AUC):", round(baseline_auc, 4)),
    x = "Specificity (True Negative Rate)",
    y = "Sensitivity (True Positive Rate/Recall)",
    caption = "Data: hockeyR | Model: Logistic Regression"
  ) +
  
  # Clean, professional theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b = 20)),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(roc_plot)

# Save the plot for the final presentation and report
ggsave("model1_roc_curve.png", plot = roc_plot, width = 8, height = 6, dpi = 300)

# ROC CURVE PLOT COMPARISON -------------------------------------------------------------
print("Plotting the ROC curve with threshold points...")

# 1. Extract the exact X/Y coordinates for both thresholds on our existing curve
point_50 <- coords(roc_curve, x = 0.5, input = "threshold", ret = c("specificity", "sensitivity"))
point_opt <- coords(roc_curve, x = "best", best.method = "youden", ret = c("specificity", "sensitivity"))

# 2. Build the comparison plot
roc_comparison_plot <- ggroc(roc_curve, color = "#21908C", linewidth = 1.2) +
  
  # The random guessing line
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey50", linewidth = 1) +
  
  # Add a RED dot for the 50% threshold (The "fake" 93% accuracy spot)
  geom_point(aes(x = point_50$specificity, y = point_50$sensitivity), color = "red", size = 5) +
  annotate(
    "text", 
    x = point_50$specificity - 0.08, 
    y = point_50$sensitivity + 0.05,
    label = "50% Threshold\n(0 Recall)", 
    color = "red", 
    fontface = "bold"
  ) +
  
  # Add a GOLD dot for the 8.3% optimal threshold
  geom_point(aes(x = point_opt$specificity, y = point_opt$sensitivity), color = "#E69F00", size = 5) +
  annotate(
    "text", 
    x = point_opt$specificity + 0.08, 
    y = point_opt$sensitivity - 0.05,
    label = "8.3% Threshold\n(78% Recall)", 
    color = "#B37A00", 
    fontface = "bold"
  ) +
  
  # Labels and styling
  labs(
    title = "Why lower the threshold? Finding the analytical sweet spot",
    subtitle = "A 50% cutoff misses every goal. The 8.3% cutoff captures 78% of them.",
    x = "Specificity (True Negative Rate)",
    y = "Sensitivity (True Positive Rate/Recall)",
    caption = "Data: hockeyR | Model: Logistic Regression"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 20)),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(roc_comparison_plot)

# Save the plot
ggsave("model1_roc_threshold_comparison.png", plot = roc_comparison_plot, width = 9, height = 6, dpi = 300)
       
# --------------------------------------------------------------------------------------------
# STEP 10 (Part 4): Final Baseline Evaluation (Optimal Threshold)
# --------------------------------------------------------------------------------------------
print("Evaluating Baseline Model with Optimal Threshold...")

# 1. Use the exact threshold the model calculated (approx 0.0834)
best_threshold <- optimal_stats[1, "threshold"]

test_data <- test_data %>%
  mutate(
    # Re-classify the shots using our new, scientifically backed threshold
    final_predicted_class = factor(ifelse(predicted_xg > best_threshold, 1, 0), levels = c("0", "1"))
  )

# 2. Build the Final Confusion Matrix
final_conf_matrix <- table(Predicted = test_data$final_predicted_class, Actual = test_data$is_goal)
print("--- Final Optimal Confusion Matrix ---")
print(final_conf_matrix)

# 3. Calculate Final Evaluation Metrics
TN <- final_conf_matrix[1,1] 
FP <- final_conf_matrix[2,1] 
FN <- final_conf_matrix[1,2] 
TP <- final_conf_matrix[2,2] 

final_accuracy <- (TP + TN) / sum(final_conf_matrix)
final_precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP)) 
final_recall <- TP / (TP + FN)
final_f_measure <- ifelse((final_precision + final_recall) == 0, 0, 2 * ((final_precision * final_recall) / (final_precision + final_recall)))

# 4. Print the final metrics for your written report
print("--- Final Baseline Metrics (Logistic Regression) ---")
print(paste("Threshold Used:", round(best_threshold, 4)))
print(paste("Accuracy:", round(final_accuracy, 4)))
print(paste("Precision:", round(final_precision, 4)))
print(paste("Recall:", round(final_recall, 4)))
print(paste("F-Measure:", round(final_f_measure, 4)))

# --------------------------------------------------------------------------------------------
# STEP 11 (Part 1): Model 2 - Decision Tree (Basic Fit & Visualize)
# --------------------------------------------------------------------------------------------
print("Training the initial Decision Tree with adjusted priors...")

# 1. Fit the Decision Tree model
# We add parms = list(prior = c(0.5, 0.5)) to force the tree to treat Goals and No Goals 
# with equal importance, completely neutralizing the 91/9 class imbalance.
basic_tree_model <- rpart(
  is_goal ~ shot_distance + shot_angle + clean_shot_type,
  data = train_data,
  method = "class",
  parms = list(prior = c(0.5, 0.5)), 
  control = rpart.control(cp = 0.001) 
)

print("Plotting the tree...")

# Stretch the canvas to 2800x2000 to pull branches apart and fix overlaps
png("model2_basic_decision_tree.png", width = 2800, height = 2000, res = 200)

rpart.plot(
  basic_tree_model, 
  type = 5,           
  extra = 104,        
  box.palette = c("#2C3E50", "#E69F00"), 
  main = "Initial Decision Tree",  
  shadow.col = "gray",
  nn = TRUE,
  tweak = 1.1,       # Tightened text slightly to prevent horizontal bleed
  faclen = 4         # Keep the abbreviations
)

dev.off() 

# Display it in your RStudio viewer (It will look cramped here, trust the saved PNG file!)
rpart.plot(
  basic_tree_model, 
  type = 5, extra = 104, box.palette = c("#2C3E50", "#E69F00"), 
  main = "Initial Decision Tree", tweak = 1.1, faclen = 4
)


# --------------------------------------------------------------------------------------------
# STEP 11 (Part 2): Decision Tree Cross-Validation
# --------------------------------------------------------------------------------------------
print("Plotting Cross-Validation Error Rates...")

# Plot the complexity parameter (cp) table
# This shows us the error rate for different tree sizes
png("model2_cv_plot.png", width = 800, height = 600, res = 150)
plotcp(basic_tree_model)
dev.off()

# Show it in the viewer
plotcp(basic_tree_model)

# --------------------------------------------------------------------------------------------
# STEP 11 (Part 3): Pruning the Decision Tree
# --------------------------------------------------------------------------------------------
print("Pruning the tree to prevent overfitting...")

# We prune the tree using the optimal complexity parameter (cp) for a 5-split tree
optimal_cp <- 0.0026

pruned_tree_model <- prune(basic_tree_model, cp = optimal_cp)

# Plot the final, pruned tree to see the difference
png("model2_pruned_decision_tree.png", width = 2000, height = 1200, res = 200)

rpart.plot(
  pruned_tree_model, 
  type = 5,           
  extra = 104,        
  box.palette = c("#2C3E50", "#E69F00"), 
  main = "Final Pruned Decision Tree",  
  shadow.col = "gray",
  nn = TRUE,
  tweak = 1.2,       
  faclen = 4         
)

dev.off() 

# Display it in your RStudio viewer 
rpart.plot(
  pruned_tree_model, 
  type = 5, extra = 104, box.palette = c("#2C3E50", "#E69F00"), 
  main = "Final Pruned Decision Tree", tweak = 1.2, faclen = 4
)

# --------------------------------------------------------------------------------------------
# STEP 11 (Part 4): Evaluate the Pruned Decision Tree
# --------------------------------------------------------------------------------------------
print("Testing the Pruned Decision Tree on 2024 unseen data...")

# 1. Generate probabilities for the 2024 test data
# Note: predict() for a tree returns a matrix with two columns (Prob of 0, Prob of 1). 
# We use [,"1"] to grab just the probability of a goal.
test_data <- test_data %>%
  mutate(
    tree_predicted_xg = predict(pruned_tree_model, newdata = test_data, type = "prob")[,"1"]
  )

# 2. Create the ROC Curve and calculate AUC
tree_roc_curve <- roc(
  response = test_data$is_goal, 
  predictor = test_data$tree_predicted_xg,
  levels = c("0", "1")
)

tree_auc <- auc(tree_roc_curve)
print(paste("Decision Tree AUC:", round(tree_auc, 4)))

# 3. Find the Mathematically Optimal Threshold
tree_optimal_stats <- coords(
  tree_roc_curve, 
  "best", 
  best.method = "youden", 
  ret = c("threshold", "accuracy", "specificity", "sensitivity")
)

tree_best_threshold <- tree_optimal_stats[1, "threshold"]
print(paste("Optimal Tree Threshold:", round(tree_best_threshold, 4)))

# 4. Apply the Threshold and Build Confusion Matrix
test_data <- test_data %>%
  mutate(
    tree_predicted_class = factor(ifelse(tree_predicted_xg > tree_best_threshold, 1, 0), levels = c("0", "1"))
  )

tree_conf_matrix <- table(Predicted = test_data$tree_predicted_class, Actual = test_data$is_goal)
tree_conf_matrix

# 5. Calculate Final Metrics
TN_tree <- tree_conf_matrix[1,1] 
FP_tree <- tree_conf_matrix[2,1] 
FN_tree <- tree_conf_matrix[1,2] 
TP_tree <- tree_conf_matrix[2,2] 

tree_accuracy <- (TP_tree + TN_tree) / sum(tree_conf_matrix)
tree_precision <- ifelse((TP_tree + FP_tree) == 0, 0, TP_tree / (TP_tree + FP_tree)) 
tree_recall <- TP_tree / (TP_tree + FN_tree)
tree_f_measure <- ifelse((tree_precision + tree_recall) == 0, 0, 2 * ((tree_precision * tree_recall) / (tree_precision + tree_recall)))

# 6. Print the Final Report Metrics
print("--- Final Model 2 Metrics (Decision Tree) ---")
print(paste("Accuracy:", round(tree_accuracy, 4)))
print(paste("Precision:", round(tree_precision, 4)))
print(paste("Recall:", round(tree_recall, 4)))
print(paste("F-Measure:", round(tree_f_measure, 4)))

# --------------------------------------------------------------------------------------------
# STEP 12 (Part 1): Model 3 - Random Forest (Training)
# --------------------------------------------------------------------------------------------
print("Preparing data for the Random Forest...")

# 1. Balanced Downsampling setup
# Count exactly how many actual goals exist in the training data
num_goals <- sum(train_data$is_goal == "1")

print(paste("Building 100 trees using a balanced mix of", num_goals, "Goals and", num_goals, "Saves per tree..."))
print("This might take 1 to 3 minutes. Grab some water and let the laptop work!")

# 2. Train the Random Forest
set.seed(313) # Sets a random seed so your results never randomly change
rf_model <- randomForest(
  is_goal ~ shot_distance + shot_angle + clean_shot_type,
  data = train_data,
  ntree = 100, # 100 trees is plenty for hockey data without overloading your RAM
  sampsize = c("0" = num_goals, "1" = num_goals), # Forces the 50/50 perspective for every tree
  importance = TRUE # Tells the model to strictly rank which features were the most useful
)

# 3. View the Initial Output
print("--- Random Forest Training Complete ---")
print(rf_model)

# --------------------------------------------------------------------------------------------
# STEP 12 (Part 2): Model 3 - Random Forest (Feature Importance)
# --------------------------------------------------------------------------------------------
print("Calculating which variables the Random Forest relied on most...")

# Save the plot directly as a high-quality PNG
png("model3_rf_importance.png", width = 1200, height = 800, res = 150)

# Generate the Variable Importance Plot
varImpPlot(
  rf_model, 
  main = "Random Forest: Which features drive NHL goals?",
  col = "#21908C", # Teal color to match your EDA theme
  pch = 19,        # Uses solid dots instead of empty circles
  cex = 1.2        # Makes the dots and text a little larger
)

dev.off()

# Display it in your RStudio viewer
varImpPlot(
  rf_model, 
  main = "Random Forest Feature Importance", 
  col = "#21908C", 
  pch = 19, 
  cex = 1.2
)

# --------------------------------------------------------------------------------------------
# STEP 12 (Part 3): Evaluate the Random Forest
# --------------------------------------------------------------------------------------------
print("Testing the Random Forest on 2024 unseen data...")

# 1. Generate probabilities for the 2024 test data
# Like the tree, predict() returns a matrix. We grab the "1" column for goal probability.
test_data <- test_data %>%
  mutate(
    rf_predicted_xg = predict(rf_model, newdata = test_data, type = "prob")[,"1"]
  )

# 2. Create the ROC Curve and calculate AUC
rf_roc_curve <- roc(
  response = test_data$is_goal, 
  predictor = test_data$rf_predicted_xg,
  levels = c("0", "1")
)

rf_auc <- auc(rf_roc_curve)
print(paste("Random Forest AUC:", round(rf_auc, 4)))

# 3. Find the Mathematically Optimal Threshold
rf_optimal_stats <- coords(
  rf_roc_curve, 
  "best", 
  best.method = "youden", 
  ret = c("threshold", "accuracy", "specificity", "sensitivity")
)

rf_best_threshold <- rf_optimal_stats[1, "threshold"]
print(paste("Optimal Forest Threshold:", round(rf_best_threshold, 4)))

# 4. Apply the Threshold and Build Confusion Matrix
test_data <- test_data %>%
  mutate(
    rf_predicted_class = factor(ifelse(rf_predicted_xg > rf_best_threshold, 1, 0), levels = c("0", "1"))
  )

rf_conf_matrix <- table(Predicted = test_data$rf_predicted_class, Actual = test_data$is_goal)

# 5. Calculate Final Metrics
TN_rf <- rf_conf_matrix[1,1] 
FP_rf <- rf_conf_matrix[2,1] 
FN_rf <- rf_conf_matrix[1,2] 
TP_rf <- rf_conf_matrix[2,2] 

rf_accuracy <- (TP_rf + TN_rf) / sum(rf_conf_matrix)
rf_precision <- ifelse((TP_rf + FP_rf) == 0, 0, TP_rf / (TP_rf + FP_rf)) 
rf_recall <- TP_rf / (TP_rf + FN_rf)
rf_f_measure <- ifelse((rf_precision + rf_recall) == 0, 0, 2 * ((rf_precision * rf_recall) / (rf_precision + rf_recall)))

# 6. Print the Final Report Metrics
print("--- Final Model 3 Metrics (Random Forest) ---")
print(paste("Accuracy:", round(rf_accuracy, 4)))
print(paste("Precision:", round(rf_precision, 4)))
print(paste("Recall:", round(rf_recall, 4)))
print(paste("F-Measure:", round(rf_f_measure, 4)))

## DONE!! --------------------------------------------------------------------------------