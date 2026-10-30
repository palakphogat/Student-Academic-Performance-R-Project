# ==============================================================================
# STUDENT RECOMMENDATION AND ANALYSIS MINI PROJECT
# ==============================================================================

# 1. Load Core Libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(cluster)
library(factoextra)

# 2. Fetch Data Directly from Web Link
data_url <- "https://raw.githubusercontent.com/guipsamora/pandas_exercises/master/04_Apply/Students_Alcohol_Consumption/student-mat.csv"

cat("Fetching dataset from online source...\n")
student_data <- read.csv(data_url, sep = ",")

# Display First Few Records & Summary
cat("\n--- DATASET OVERVIEW ---\n")
print(head(student_data))

# 3. Data Cleaning & Feature Selection
df_clean <- student_data %>%
  select(studytime, absences, failures, freetime, goout, G1, G2, G3)

cat("\nSummary Statistics:\n")
print(summary(df_clean))

# 4. Exploratory Data Analysis (EDA) & Visualizations

# Graph 1: Study Time vs Final Grade (G3)
ggplot(df_clean, aes(x = factor(studytime), y = G3, fill = factor(studytime))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs")) +
  labs(title = "Effect of Weekly Study Time on Final Grade",
       x = "Weekly Study Time Category",
       y = "Final Grade (0-20)",
       fill = "Study Time Class") +
  theme_minimal()

# Graph 2: Absences vs Final Grade
ggplot(df_clean, aes(x = absences, y = G3)) +
  geom_point(color = "#2b5c8f", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
  labs(title = "Impact of Absences on Final Performance",
       x = "Number of School Absences",
       y = "Final Grade (0-20)") +
  theme_minimal()

# Graph 3: Correlation Matrix
cor_matrix <- cor(df_clean)
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", 
         title = "Feature Correlation Matrix", mar = c(0,0,1,0))

# 5. Machine Learning - Predictive Modeling
set.seed(123)
n_rows <- nrow(df_clean)
train_indices <- sample(1:n_rows, size = 0.8 * n_rows)

train_data <- df_clean[train_indices, ]
test_data  <- df_clean[-train_indices, ]

# Linear Regression Model predicting final score G3
model <- lm(G3 ~ studytime + absences + failures + G1 + G2, data = train_data)

cat("\n--- MODEL PERFORMANCE SUMMARY ---\n")
print(summary(model))

# Predictions on test data
predictions <- predict(model, newdata = test_data)
rmse_val <- sqrt(mean((test_data$G3 - predictions)^2))
cat("\nModel Root Mean Squared Error (RMSE):", round(rmse_val, 2), "\n")

# 6. Student Segmentation / Behavioral Clustering
set.seed(123)
scaled_data <- scale(df_clean %>% select(studytime, absences, failures, G3))

# Perform K-Means Clustering into 3 Student Clusters
kmeans_res <- kmeans(scaled_data, centers = 3, nstart = 25)
df_clean$Cluster <- as.factor(kmeans_res$cluster)

cat("\n--- STUDENT CLUSTER PROFILES ---\n")
cluster_summary <- df_clean %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Study_Time = mean(studytime),
    Avg_Absences   = mean(absences),
    Avg_Failures   = mean(failures),
    Avg_Final_Grade = mean(G3),
    Count          = n()
  )
print(cluster_summary)

# Plot Clusters
fviz_cluster(kmeans_res, data = scaled_data,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Student Behavioral Clusters")

# 7. Automated Recommendation Engine Rule
recommend_student <- function(study_hours, absences_cnt, past_failures) {
  cat("\n======================================================\n")
  cat("AUTOMATED STUDENT INTERVENTION & RECOMMENDATION\n")
  cat("======================================================\n")
  
  if (past_failures > 0 || study_hours < 2) {
    cat("Status: HIGH RISK STUDENT\n")
    cat("Recommendation: Assign to academic mentoring, require at least 5 hours/week of guided study sessions.\n")
  } else if (absences_cnt > 10) {
    cat("Status: ATTENDANCE RISK\n")
    cat("Recommendation: Attendance tracking review required; issue warning letter.\n")
  } else {
    cat("Status: ON TRACK / GOOD STANDING\n")
    cat("Recommendation: Eligible for advanced honors coursework and peer tutoring roles.\n")
  }
}

# Example Recommendation Query
recommend_student(study_hours = 1, absences_cnt = 12, past_failures = 1)