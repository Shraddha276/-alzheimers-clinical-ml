# Load required library
library(readr)

# Load clinical data (not included in repo)
clinical_data <- read_csv("data/fact_sheet.csv")

# Select relevant features
alz_data <- clinical_data[, c("Age", "Educ", "MMSE", "CDR")]

# Remove missing values
alz_data <- na.omit(alz_data)

# Create binary diagnosis label
alz_data$Diagnosis <- ifelse(alz_data$CDR > 0, 1, 0)

# Train-test split
set.seed(42)
index <- sample(1:nrow(alz_data), 0.7 * nrow(alz_data))
train <- alz_data[index, ]
test  <- alz_data[-index, ]

# Train logistic regression model
model <- glm(Diagnosis ~ Age + Educ + MMSE,
             data = train,
             family = "binomial")

# Predict on test set
pred_prob <- predict(model, test, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Accuracy
accuracy <- mean(pred_class == test$Diagnosis)
print(accuracy)
