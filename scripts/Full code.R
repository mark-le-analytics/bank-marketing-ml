# Load required libraries
packages <- c("dplyr", "ggplot2", "tidyr", "corrplot", "readr","kableExtra","reshape2","tidyverse","caret","recipes","janitor","themis",
              "imputeTS","gridExtra")
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]

if (length(to_install)) install.packages(to_install)
lapply(packages, library, character.only = TRUE)

# Load both datasets
bank1 <- read.csv("bank-additional.csv", sep = ";")
bank2 <- read.csv("bank.csv", sep = ";")

# Backups
bank1_original <- bank1
bank2_original <- bank2

# Overview
cat("\n=== Dataset 1 Overview (bank-additional.csv) ===\n")
cat("Rows:", nrow(bank1), "\nColumns:", ncol(bank1), "\n")
str(bank1)

cat("\n=== Dataset 2 Overview (bank.csv) ===\n")
cat("Rows:", nrow(bank2), "\nColumns:", ncol(bank2), "\n")
str(bank2)

# Incomplete Data Checks
cat("\n=== 'unknown' Values in Dataset 1 ===\n")
sapply(bank1, function(x) sum(x == "unknown", na.rm = TRUE))

cat("\n=== 'unknown' Values in Dataset 2 ===\n")
sapply(bank2, function(x) sum(x == "unknown", na.rm = TRUE))

# Replace 'unknown' with NA in character fields
bank1 <- bank1 %>%
  mutate(across(where(is.character), ~na_if(.x, "unknown")))

bank2 <- bank2 %>%
  mutate(across(where(is.character), ~na_if(.x, "unknown")))

# Inaccurate Data Checks
cat("\n=== Inaccurate Data in Dataset 1 ===\n")
cat("Non-positive duration values:", sum(bank1$duration <= 0, na.rm = TRUE), "\n")
cat("Suspicious age values (<18 or >100):", 
    sum(bank1$age < 18 | bank1$age > 100, na.rm = TRUE), "\n")

duplicate_count1 <- sum(duplicated(bank1))
cat("Number of duplicate rows in Dataset 1:", duplicate_count1, "\n")
head(bank1[duplicated(bank1), ])

cat("\n=== Inaccurate Data in Dataset 2 ===\n")
cat("Non-positive duration values:", sum(bank2$duration <= 0, na.rm = TRUE), "\n")
cat("Suspicious age values (<18 or >100):", 
    sum(bank2$age < 18 | bank2$age > 100, na.rm = TRUE), "\n")

duplicate_count2 <- sum(duplicated(bank2))
cat("Number of duplicate rows in Dataset 2:", duplicate_count2, "\n")
head(bank2[duplicated(bank2), ])

# Updated Summaries
cat("\n=== Dataset 1 Summary ===\n")
summary(bank1)

cat("\n=== Dataset 2 Summary ===\n")
summary(bank2)


# ====================================
# I. Data Exploration
# ====================================


# ====================================
# 1. Chi Square Test of Independence
# ====================================

# Create AgeGroup
bank_clean <- bank1 %>%
  mutate(
    AgeGroup = cut(
      age,
      breaks = c(0, 20, 30, 40, 50, 60, 70, Inf),
      labels = c("20 and Below", "21–30", "31–40", 
                 "41–50", "51–60", "61–70", "70+"),
      right = TRUE,
      include.lowest = TRUE
    ),
    y = ifelse(y == "yes", 1, 0),
    Outcome = ifelse(y == 1, "Yes", "No")
  )

# Variables to test against target (y)
vars <- c("AgeGroup", "job", "marital", "education", "default",
          "housing", "loan", "contact", "month", "day_of_week", "poutcome")

# Create results table
chi_result <- data.frame(
  Variable = character(),
  Chi_Square = numeric(),
  DF = integer(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Run Chi-Square tests
for (v in vars) {
  tbl <- table(bank_clean[[v]], bank_clean$y)
  test <- chisq.test(tbl)
  
  # Append results
  chi_result <- rbind(
    chi_result,
    data.frame(
      Variable = v,
      Chi_Square = as.numeric(test$statistic),
      DF = as.integer(test$parameter),
      P_Value = test$p.value
    )
  )
}

# View results
print(chi_result)

# ====================================
# 2. Age analysis
# ====================================

# Ensure y is numeric
bank_clean$y <- as.integer(bank_clean$y)

# View dataset
View(bank_clean)

# =========================
# Conversion Rate
# =========================
total_conversions <- sum(bank_clean$y)
total_rows <- nrow(bank_clean)
conversion_rate <- total_conversions / total_rows * 100

total_conversions
total_rows
conversion_rate

# =========================
# Age Distribution
# =========================
age_dist <- bank_clean %>%
  group_by(AgeGroup, Outcome) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(age_dist, aes(x = AgeGroup, y = Count, fill = Outcome)) +
  geom_col(position = "stack", width = 0.5) +
  labs(
    title = "Age Distribution by Subscription",
    x = "Age Group",
    y = "Count",
    fill = "Subscription"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  scale_fill_manual(
    values = c("No" = "#de425b", "Yes" = "#67ac75")
  )

# =========================
# Age Conversion Rate
# =========================
age_con <- bank_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  )

print(age_con)

# Convert AgeGroup to character (optional for plotting order control)
age_con$AgeGroup <- as.character(age_con$AgeGroup)

# =========================
# Visualisation
# =========================
ggplot(age_con, aes(x = AgeGroup, y = ConversionRate)) +
  geom_col(width = 0.5, fill = "darkgreen") +
  labs(
    title = "Conversion Rate by Age Group",
    x = "Age Group",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# 3. Job Distribution
# =========================
library(forcats)

ggplot(bank_clean, aes(x = fct_infreq(job), fill = Outcome)) +
  geom_bar(width = 0.5, position = "stack") +
  labs(
    title = "Job Distribution by Subscription",
    x = "Job",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

# =========================
# Job Conversion Rate
# =========================
job_con <- bank_clean %>%
  group_by(job) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(job_con)

# Reorder factor for plotting
job_con$job <- factor(
  job_con$job,
  levels = job_con$job[order(-job_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(job_con, aes(x = job, y = ConversionRate)) +
  geom_col(width = 0.5, fill = "darkgreen") +
  labs(
    title = "Conversion Rate by Job",
    x = "Job",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )



# =========================
# 4. Marital Status Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(marital), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Marital Status Distribution by Subscription",
    x = "Marital Status",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# Marital Status Conversion
# =========================
marital_con <- bank_clean %>%
  group_by(marital) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(marital_con)

# Reorder factor
marital_con$marital <- factor(
  marital_con$marital,
  levels = marital_con$marital[order(-marital_con$ConversionRate)]
)

# Visualisation
ggplot(marital_con, aes(x = marital, y = ConversionRate, fill = marital)) +
  geom_col(width = 0.5) +
  labs(
    title = "Conversion Rate by Marital Status",
    x = "Marital Status",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  guides(fill = "none")

# =========================
# Age & Marital Status Conversion
# =========================
age_marital_con <- bank_clean %>%
  group_by(AgeGroup, marital) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  )

print(age_marital_con)

# Optional: reorder AgeGroup (kept simple to avoid breaking logical order)
age_marital_con$AgeGroup <- as.character(age_marital_con$AgeGroup)

# Visualisation
ggplot(age_marital_con, aes(x = AgeGroup, y = ConversionRate, fill = marital)) +
  geom_col(position = "stack", width = 0.7) +
  labs(
    title = "Conversion Rate by Age Group and Marital Status",
    x = "Age Group",
    y = "Conversion Rate (%)",
    fill = "Marital Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# =========================
# 5. Education Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(education), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Education Distribution by Subscription",
    x = "Education",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

# =========================
# Education Conversion Rate
# =========================
education_con <- bank_clean %>%
  group_by(education) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(education_con)

# Reorder factor for plotting
education_con$education <- factor(
  education_con$education,
  levels = education_con$education[order(-education_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(education_con, aes(x = education, y = ConversionRate)) +
  geom_col(width = 0.5, fill = "darkgreen") +
  labs(
    title = "Conversion Rate by Education",
    x = "Education",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )


# =========================
# Default Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(default), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Default Distribution by Subscription",
    x = "Default",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# 6. Default Conversion Rate
# =========================
default_con <- bank_clean %>%
  group_by(default) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(default_con)

# Reorder factor for plotting
default_con$default <- factor(
  default_con$default,
  levels = default_con$default[order(-default_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(default_con, aes(x = default, y = ConversionRate, fill = default)) +
  geom_col(width = 0.5) +
  labs(
    title = "Conversion Rate by Default Status",
    x = "Has Defaulted Credit?",
    y = "Conversion Rate (%)"
  ) +
  scale_fill_manual(
    values = c("yes" = "#de425b", "no" = "#67ac75"),
    labels = c("yes" = "Yes", "no" = "No")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  ) +
  guides(fill = "none")



# =========================
# 7. Housing Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(housing), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Housing Distribution by Subscription",
    x = "Housing",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# 7. Housing Conversion Rate
# =========================
housing_con <- bank_clean %>%
  group_by(housing) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(housing_con)

# Reorder factor for plotting
housing_con$housing <- factor(
  housing_con$housing,
  levels = housing_con$housing[order(-housing_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(housing_con, aes(x = housing, y = ConversionRate, fill = housing)) +
  geom_col(width = 0.5) +
  labs(
    title = "Conversion Rate by Housing Loan Status",
    x = "Has Housing Loan?",
    y = "Conversion Rate (%)"
  ) +
  scale_fill_manual(
    values = c("yes" = "#67ac75", "no" = "#de425b"),
    labels = c("yes" = "Yes", "no" = "No")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  ) +
  guides(fill = "none")


# =========================
# Loan Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(loan), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Loan Distribution by Subscription",
    x = "Loan",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# Loan Conversion Rate
# =========================
loan_con <- bank_clean %>%
  group_by(loan) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(loan_con)

# Reorder factor for plotting
loan_con$loan <- factor(
  loan_con$loan,
  levels = loan_con$loan[order(-loan_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(loan_con, aes(x = loan, y = ConversionRate, fill = loan)) +
  geom_col(width = 0.5) +
  labs(
    title = "Conversion Rate by Personal Loan",
    x = "Has Personal Loan?",
    y = "Conversion Rate (%)"
  ) +
  scale_fill_manual(
    values = c("yes" = "#67ac75", "no" = "#de425b"),
    labels = c("yes" = "Yes", "no" = "No")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  ) +
  guides(fill = "none")


# =========================
# 8. Contact Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(contact), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Contact Distribution by Subscription",
    x = "Contact",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# Contact Conversion Rate
# =========================
contact_con <- bank_clean %>%
  group_by(contact) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(contact_con)

# Reorder factor for plotting
contact_con$contact <- factor(
  contact_con$contact,
  levels = contact_con$contact[order(-contact_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(contact_con, aes(x = contact, y = ConversionRate, fill = contact)) +
  geom_col(width = 0.5) +
  labs(
    title = "Conversion Rate by Contact Method",
    x = "Contact Method",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  ) +
  guides(fill = "none")



# =========================
# 9. Month Distribution
# =========================

# Ensure correct month order
bank_clean$month <- factor(
  bank_clean$month,
  levels = c("jan", "feb", "mar", "apr", "may", "jun",
             "jul", "aug", "sep", "oct", "nov", "dec")
)

ggplot(bank_clean, aes(x = month, fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Month Distribution by Subscription",
    x = "Month",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# Month Conversion Rate
# =========================
month_con <- bank_clean %>%
  group_by(month) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  )

print(month_con)

# =========================
# Visualisation
# =========================
ggplot(month_con, aes(x = month, y = ConversionRate)) +
  geom_col(width = 0.5, fill = "darkgreen") +
  labs(
    title = "Conversion Rates by Last Contact Month",
    x = "Month",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10)
  )



# =========================
# 10. Day Distribution
# =========================

# Ensure correct weekday order
bank_clean$day_of_week <- factor(
  bank_clean$day_of_week,
  levels = c("mon", "tue", "wed", "thu", "fri")
)

ggplot(bank_clean, aes(x = day_of_week, fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Day Distribution by Subscription",
    x = "Day",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 10)
  )

# =========================
# Day Conversion Rate
# =========================
day_con <- bank_clean %>%
  group_by(day_of_week) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  )

print(day_con)

# =========================
# Visualisation
# =========================
ggplot(day_con, aes(x = day_of_week, y = ConversionRate)) +
  geom_col(width = 0.5, fill = "darkgreen") +
  labs(
    title = "Conversion Rate by Last Contact Day of Week",
    x = "Day of Week",
    y = "Conversion Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10)
  )


# =========================
# 11. Poutcome Distribution
# =========================
ggplot(bank_clean, aes(x = fct_infreq(poutcome), fill = Outcome)) +
  geom_bar(position = "stack") +
  labs(
    title = "Poutcome Distribution by Subscription",
    x = "Poutcome",
    y = "Count",
    fill = "Subscription"
  ) +
  scale_fill_manual(values = c("Yes" = "#67ac75", "No" = "#de425b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18)
  )

# =========================
# Poutcome Conversion Rate
# =========================
poutcome_con <- bank_clean %>%
  group_by(poutcome) %>%
  summarise(
    TotalCount = n(),
    NumberConversions = sum(y),
    .groups = "drop"
  ) %>%
  mutate(
    ConversionRate = NumberConversions / TotalCount * 100
  ) %>%
  arrange(desc(ConversionRate))

print(poutcome_con)

# Reorder factor for plotting
poutcome_con$poutcome <- factor(
  poutcome_con$poutcome,
  levels = poutcome_con$poutcome[order(-poutcome_con$ConversionRate)]
)

# =========================
# Visualisation
# =========================
ggplot(poutcome_con, aes(x = poutcome, y = ConversionRate, fill = poutcome)) +
  geom_col(width = 0.5) +
  labs(
    title = "Conversion Rate by Previous Campaign Outcome",
    x = "Previous Campaign Outcome",
    y = "Conversion Rate (%)"
  ) +
  scale_fill_manual(
    values = c("success" = "#67ac75", "failure" = "#de425b", "nonexistent" = "#999999"),
    labels = c("success" = "Success", "failure" = "Failure", "nonexistent" = "No Previous Contact")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  ) +
  guides(fill = "none")





# =========================
# 12. Covariance and Correlation
# =========================
bank <- read.csv("bank-additional.csv", sep = ";") %>%
  na.omit() %>%
  distinct() %>%
  select(-duration)

# Select numeric variables
numerical_bank <- bank[, sapply(bank, is.numeric)]

# =========================
# Covariance Matrix
# =========================
cov_matrix <- cov(numerical_bank)

cov_df <- as.data.frame(cov_matrix)
cov_df$Variable <- rownames(cov_df)
cov_df <- cov_df[, c(ncol(cov_df), 1:(ncol(cov_df) - 1))]

# Display table
cov_df %>%
  kable("html", caption = "Covariance Matrix of Numerical Features") %>%
  kable_styling(full_width = TRUE, position = "center", font_size = 14,
                bootstrap_options = c("striped", "hover"))

# =========================
# Correlation Matrix Heatmap
# =========================
cor_matrix <- cor(numerical_bank)
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) +
  ggtitle("Correlation of Numerical Features")

# =========================
# Add Binary Target
# =========================
bank$y_num <- ifelse(bank$y == "yes", 1, 0)

# =========================
# Correlation with Target
# =========================
cor_with_y <- cor(bank[, sapply(bank, is.numeric)], bank$y_num)

cor_df <- as.data.frame(cor_with_y)
colnames(cor_df) <- "Correlation_with_y"
cor_df$Feature <- rownames(cor_df)

# Plot
ggplot(cor_df, aes(x = reorder(Feature, Correlation_with_y), 
                   y = Correlation_with_y, 
                   fill = Correlation_with_y)) +
  geom_col() +
  geom_text(aes(label = round(Correlation_with_y, 2)),
            vjust = ifelse(cor_df$Correlation_with_y < 0, 1.5, -0.5),
            color = "black") +
  theme_minimal() +
  labs(
    title = "Correlation of Numerical Features with Target (y)",
    x = "Features",
    y = "Correlation with y"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# =========================
# II. Data preparation for modelling
# =========================

# =========================
# 1. LOAD DATA
# =========================
bank <- read.csv("bank-additional.csv", sep = ";")

# =========================
# 2. REMOVE DUPLICATES
# =========================
cat("Duplicate rows:", sum(duplicated(bank)), "\n")
bank <- bank[!duplicated(bank), ]

# =========================
# 3. DROP VARIABLES
# =========================
# Note: Be careful dropping too many variables (see comments below)
bank$duration <- NULL   # Known only after outcome
bank$loan <- NULL
bank$housing <- NULL
bank$default <- NULL
bank$day_of_week <- NULL

# =========================
# 4. HANDLE "unknown" AS NA
# =========================
cols_unknown <- c("job", "marital", "education")

cat("\nStep 4: 'unknown' counts BEFORE recoding:\n")
sapply(bank[cols_unknown], function(x) sum(x == "unknown"))

# Replace "unknown" with NA
bank <- bank %>%
  mutate(across(all_of(cols_unknown), ~na_if(as.character(.x), "unknown")))

# =========================
# 5. IMPUTE MISSING VALUES (Mode for categorical)
# =========================
impute_mode <- function(x) {
  mode_val <- names(sort(table(x), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_val
  return(factor(x))  # keep as factor
}

bank[cols_unknown] <- lapply(bank[cols_unknown], impute_mode)

# =========================
# CHECK RESULTS
# =========================
cat("\nMissing values AFTER imputation:\n")
sapply(bank[cols_unknown], function(x) sum(is.na(x)))

cat("\nValue distribution AFTER imputation:\n")
lapply(bank[cols_unknown], table)

# =========================
# 6. FEATURE ENGINEERING
# =========================

cat("\nStep 6: 'pdays' value counts BEFORE transformation:\n")
print(table(bank$pdays))

bank <- bank %>%
  mutate(
    pdays_cat = ifelse(pdays == 999, "never", as.character(pdays)),
    pdays_cat = factor(pdays_cat)
  ) %>%
  select(-pdays)

cat("\nStep 6: 'pdays_cat' value counts AFTER transformation:\n")
print(table(bank$pdays_cat))

cat("\nIs 'pdays' column removed?:", !("pdays" %in% names(bank)), "\n")

# =========================
# 7. STRATIFIED SAMPLING
# =========================
set.seed(1304)

index <- createDataPartition(bank$y, p = 0.7, list = FALSE)
train_data <- bank[index, ]
test_data  <- bank[-index, ]

# =========================
# 8. RECIPE PREPROCESSING
# =========================

# Base recipe
base_recipe <- function(data) {
  recipe(y ~ ., data = data) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
}

# =========================
# 8.1 Balanced + Scaled (ANN, kNN, SVM)
# =========================
rec_scaled_balanced <- base_recipe(train_data) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_upsample(y)

prep_scaled_balanced <- prep(rec_scaled_balanced, training = train_data)

train_scaled_balanced <- juice(prep_scaled_balanced)
test_scaled_balanced  <- bake(prep_scaled_balanced, new_data = test_data)

# =========================
# 8.2 Balanced + Unscaled (DT, RF)
# =========================
rec_unscaled_balanced <- base_recipe(train_data) %>%
  step_upsample(y)

prep_unscaled_balanced <- prep(rec_unscaled_balanced, training = train_data)

train_unscaled_balanced <- juice(prep_unscaled_balanced)
test_unscaled_balanced  <- bake(prep_unscaled_balanced, new_data = test_data)

# =========================
# 8.3 Imbalanced + Scaled (baseline ANN, SVM, kNN)
# =========================
rec_scaled_imbalanced <- base_recipe(train_data) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

prep_scaled_imbalanced <- prep(rec_scaled_imbalanced, training = train_data)

train_scaled_imbalanced <- juice(prep_scaled_imbalanced)
test_scaled_imbalanced  <- bake(prep_scaled_imbalanced, new_data = test_data)

# =========================
# 8.4 Imbalanced + Unscaled (baseline DT, RF)
# =========================
rec_unscaled_imbalanced <- base_recipe(train_data)

prep_unscaled_imbalanced <- prep(rec_unscaled_imbalanced, training = train_data)

train_unscaled_imbalanced <- juice(prep_unscaled_imbalanced)
test_unscaled_imbalanced  <- bake(prep_unscaled_imbalanced, new_data = test_data)




# =========================
# III. Model Building
# =========================

library(rpart)
library(rpart.plot)
library(caret)

# =========================
# 1. Decision Tree (Balanced Dataset)
# =========================

# Train model
dt_bal <- rpart(y ~ ., data = train_unscaled_balanced, method = "class")

# Print model
print(dt_bal)

# =========================
# Visualise Tree
# =========================
rpart.plot(
  dt_bal,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree: Bank Marketing (Balanced Dataset)"
)

# Summary
summary(dt_bal)

# =========================
# Predictions
# =========================
dt_bal_predictions <- predict(
  dt_bal,
  test_unscaled_balanced,
  type = "class"
)

# =========================
# Comparison Table
# =========================
dt_bal_comparison <- data.frame(
  Actual = test_unscaled_balanced$y,
  Predicted = dt_bal_predictions
)

# Misclassified rows
dt_bal_misclassified <- dt_bal_comparison[
  dt_bal_comparison$Actual != dt_bal_comparison$Predicted, 
]

cat("Number of misclassified rows:", nrow(dt_bal_misclassified), "\n")

# =========================
# Evaluation
# =========================
dt_bal_comparison$Actual <- factor(dt_bal_comparison$Actual, levels = c("no", "yes"))
dt_bal_comparison$Predicted <- factor(dt_bal_comparison$Predicted, levels = c("no", "yes"))

dt_bal_cm <- confusionMatrix(
  dt_bal_comparison$Predicted,
  dt_bal_comparison$Actual,
  positive = "yes"
)

print(dt_bal_cm)

# =========================
# Extract Metrics
# =========================
dt_bal_accuracy  <- dt_bal_cm$overall["Accuracy"]
dt_bal_precision <- dt_bal_cm$byClass["Pos Pred Value"]
dt_bal_recall    <- dt_bal_cm$byClass["Sensitivity"]

dt_bal_f1 <- 2 * as.numeric(dt_bal_precision) * as.numeric(dt_bal_recall) /
  (as.numeric(dt_bal_precision) + as.numeric(dt_bal_recall))

# =========================
# Store Results
# =========================
dt_bal_metrics <- data.frame(
  Model = "Decision Tree (Balanced)",
  Accuracy = round(as.numeric(dt_bal_accuracy), 4),
  Precision = round(as.numeric(dt_bal_precision), 4),
  Recall = round(as.numeric(dt_bal_recall), 4),
  F1_Score = round(dt_bal_f1, 4)
)

print(dt_bal_metrics)


# =========================
# Decision Tree (Imbalanced Dataset)
# =========================

# Train model
dt_imb <- rpart(y ~ ., data = train_unscaled_imbalanced, method = "class")

# Print model
print(dt_imb)

# =========================
# Visualise Tree
# =========================
rpart.plot(
  dt_imb,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree: Bank Marketing (Imbalanced Dataset)"
)

# Summary
summary(dt_imb)

# =========================
# Predictions
# =========================
dt_imb_predictions <- predict(
  dt_imb,
  test_unscaled_imbalanced,
  type = "class"
)

# =========================
# Comparison Table
# =========================
dt_imb_comparison <- data.frame(
  Actual = test_unscaled_imbalanced$y,
  Predicted = dt_imb_predictions
)

# Misclassified rows
dt_imb_misclassified <- dt_imb_comparison[
  dt_imb_comparison$Actual != dt_imb_comparison$Predicted,
]

cat("Number of misclassified rows:", nrow(dt_imb_misclassified), "\n")

# =========================
# Evaluation
# =========================
dt_imb_comparison$Actual <- factor(dt_imb_comparison$Actual, levels = c("no", "yes"))
dt_imb_comparison$Predicted <- factor(dt_imb_comparison$Predicted, levels = c("no", "yes"))

dt_imb_cm <- confusionMatrix(
  dt_imb_comparison$Predicted,
  dt_imb_comparison$Actual,
  positive = "yes"
)

print(dt_imb_cm)

# =========================
# Extract Metrics
# =========================
dt_imb_accuracy  <- dt_imb_cm$overall["Accuracy"]
dt_imb_precision <- dt_imb_cm$byClass["Pos Pred Value"]
dt_imb_recall    <- dt_imb_cm$byClass["Sensitivity"]

dt_imb_f1 <- 2 * as.numeric(dt_imb_precision) * as.numeric(dt_imb_recall) /
  (as.numeric(dt_imb_precision) + as.numeric(dt_imb_recall))

# =========================
# Store Results
# =========================
dt_imb_metrics <- data.frame(
  Model = "Decision Tree (Imbalanced)",
  Accuracy = round(as.numeric(dt_imb_accuracy), 4),
  Precision = round(as.numeric(dt_imb_precision), 4),
  Recall = round(as.numeric(dt_imb_recall), 4),
  F1_Score = round(dt_imb_f1, 4)
)

print(dt_imb_metrics)

# =========================
# Cross Validation (5-Fold) - Decision Tree (Balanced)
# =========================

set.seed(123)

folds <- createFolds(train_unscaled_balanced$y, k = 5)
cv_bal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  # Split into train / validation
  cv_bal_train <- train_unscaled_balanced[-folds[[i]], ]
  cv_bal_test  <- train_unscaled_balanced[folds[[i]], ]
  
  # Train model
  cv_bal_model <- rpart(y ~ ., data = cv_bal_train, method = "class")
  
  # Predict
  cv_bal_predictions <- predict(cv_bal_model, newdata = cv_bal_test, type = "class")
  
  # Ensure factor levels match
  cv_bal_test$y <- factor(cv_bal_test$y, levels = c("no", "yes"))
  cv_bal_predictions <- factor(cv_bal_predictions, levels = c("no", "yes"))
  
  # Confusion matrix
  cv_bal_cm <- confusionMatrix(
    cv_bal_predictions,
    cv_bal_test$y,
    positive = "yes"
  )
  
  # Extract metrics
  acc  <- cv_bal_cm$overall["Accuracy"]
  prec <- cv_bal_cm$byClass["Pos Pred Value"]
  rec  <- cv_bal_cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  # Store results
  cv_bal_results[i, "F1"] <- f1
  cv_bal_results[i, "Accuracy"] <- acc
}

# =========================
# Summary Statistics
# =========================
mean_f1_bal <- mean(cv_bal_results$F1)
sd_f1_bal   <- sd(cv_bal_results$F1)

mean_acc_bal <- mean(cv_bal_results$Accuracy)
sd_acc_bal   <- sd(cv_bal_results$Accuracy)

cat("Balanced DT - Mean F1:", round(mean_f1_bal, 4),
    "| Std Dev:", round(sd_f1_bal, 4), "\n")

cat("Balanced DT - Mean Accuracy:", round(mean_acc_bal, 4),
    "| Std Dev:", round(sd_acc_bal, 4), "\n")

print(cv_bal_results)

# =========================
# Cross Validation (5-Fold) - Decision Tree (Imbalanced)
# =========================

set.seed(123)

folds_imbal <- createFolds(train_unscaled_imbalanced$y, k = 5)
cv_imb_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  # Split into train / validation
  cv_imb_train <- train_unscaled_imbalanced[-folds_imbal[[i]], ]
  cv_imb_test  <- train_unscaled_imbalanced[folds_imbal[[i]], ]
  
  # Train model
  cv_imb_model <- rpart(y ~ ., data = cv_imb_train, method = "class")
  
  # Predict
  cv_imb_predictions <- predict(
    cv_imb_model,
    newdata = cv_imb_test,
    type = "class"
  )
  
  # Ensure consistent factor levels
  cv_imb_test$y <- factor(cv_imb_test$y, levels = c("no", "yes"))
  cv_imb_predictions <- factor(cv_imb_predictions, levels = c("no", "yes"))
  
  # Confusion matrix
  cv_imb_cm <- confusionMatrix(
    cv_imb_predictions,
    cv_imb_test$y,
    positive = "yes"
  )
  
  # Extract metrics (correct names)
  acc  <- cv_imb_cm$overall["Accuracy"]
  prec <- cv_imb_cm$byClass["Pos Pred Value"]
  rec  <- cv_imb_cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  # Store results
  cv_imb_results[i, "Accuracy"] <- acc
  cv_imb_results[i, "F1"] <- f1
}

# =========================
# Summary Statistics
# =========================
mean_f1_imb <- mean(cv_imb_results$F1)
sd_f1_imb   <- sd(cv_imb_results$F1)

mean_acc_imb <- mean(cv_imb_results$Accuracy)
sd_acc_imb   <- sd(cv_imb_results$Accuracy)

cat("Imbalanced DT - Mean F1:", round(mean_f1_imb, 4),
    "| Std Dev:", round(sd_f1_imb, 4), "\n")

cat("Imbalanced DT - Mean Accuracy:", round(mean_acc_imb, 4),
    "| Std Dev:", round(sd_acc_imb, 4), "\n")

print(cv_imb_results)


# =========================
# Function: Confusion Matrix Heatmap
# =========================
plot_confusion <- function(cm, title) {
  
  df <- as.data.frame(cm$table)
  colnames(df) <- c("Prediction", "Reference", "Freq")
  
  ggplot(df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5, color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    labs(
      title = title,
      x = "Predicted",
      y = "Actual"
    )
}

# =========================
# Create Plots
# =========================
p_dt_bal <- plot_confusion(dt_bal_cm, "Decision Tree (Balanced)")
p_dt_imb <- plot_confusion(dt_imb_cm, "Decision Tree (Imbalanced)")

# =========================
# Display
# =========================
print(p_dt_bal)
print(p_dt_imb)

# Side-by-side comparison
grid.arrange(p_dt_bal, p_dt_imb, ncol = 2)


# =========================
# 2. RANDOM FOREST 
# =========================



library(randomForest)
library(caret)
library(ggplot2)

# =========================
# RANDOM FOREST - BALANCED
# =========================
rf_bal <- randomForest(
  y ~ ., 
  data = train_unscaled_balanced,
  ntree = 500
)

pred_bal <- predict(rf_bal, newdata = test_unscaled_balanced)

conf_bal <- confusionMatrix(
  pred_bal,
  test_unscaled_balanced$y,
  positive = "yes"
)

print(conf_bal)

# =========================
# RANDOM FOREST - IMBALANCED
# =========================
rf_imbal <- randomForest(
  y ~ ., 
  data = train_unscaled_imbalanced,
  ntree = 500
)

pred_imbal <- predict(rf_imbal, newdata = test_unscaled_imbalanced)

conf_imbal <- confusionMatrix(
  pred_imbal,
  test_unscaled_imbalanced$y,
  positive = "yes"
)

print(conf_imbal)

# =========================
# Convert Confusion Matrix → DataFrame
# =========================
cm_bal_df <- as.data.frame(conf_bal$table)
colnames(cm_bal_df) <- c("Prediction", "Reference", "Freq")

cm_imbal_df <- as.data.frame(conf_imbal$table)
colnames(cm_imbal_df) <- c("Prediction", "Reference", "Freq")

# =========================
# Plot Function
# =========================
plot_cm <- function(df, title) {
  ggplot(df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    labs(title = title, x = "Predicted", y = "Actual")
}

# =========================
# Plot Results
# =========================
p_rf_bal <- plot_cm(cm_bal_df, "Random Forest (Balanced)")
p_rf_imbal <- plot_cm(cm_imbal_df, "Random Forest (Imbalanced)")

print(p_rf_bal)
print(p_rf_imbal)


# =========================
# Plot Imbalanced Confusion Matrix
# =========================
plot_imbal_rf <- ggplot(cm_imbal_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(
    title = "Random Forest (Imbalanced)",
    x = "Predicted",
    y = "Actual"
  )

# =========================
# Side-by-side comparison
# =========================
grid.arrange(p_rf_bal, plot_imbal_rf, ncol = 2)

# =========================
# Summary Metrics (Balanced vs Imbalanced RF)
# =========================

# Extract correct metric names from confusionMatrix
precision_bal <- conf_bal$byClass["Pos Pred Value"]
recall_bal    <- conf_bal$byClass["Sensitivity"]

precision_imbal <- conf_imbal$byClass["Pos Pred Value"]
recall_imbal    <- conf_imbal$byClass["Sensitivity"]

f1_bal <- 2 * as.numeric(precision_bal) * as.numeric(recall_bal) /
  (as.numeric(precision_bal) + as.numeric(recall_bal))

f1_imbal <- 2 * as.numeric(precision_imbal) * as.numeric(recall_imbal) /
  (as.numeric(precision_imbal) + as.numeric(recall_imbal))

# Create summary table
cv_results <- data.frame(
  Dataset = c("Balanced", "Imbalanced"),
  
  Accuracy = c(
    conf_bal$overall["Accuracy"],
    conf_imbal$overall["Accuracy"]
  ),
  
  Precision = c(
    precision_bal,
    precision_imbal
  ),
  
  Recall = c(
    recall_bal,
    recall_imbal
  ),
  
  F1_Score = c(
    f1_bal,
    f1_imbal
  )
)

print(cv_results)


# =========================
# Cross Validation (5-Fold) - Random Forest (Balanced)
# =========================

set.seed(123)

folds <- createFolds(train_unscaled_balanced$y, k = 5)

cv_results_bal <- data.frame(
  Fold = integer(),
  Accuracy = numeric(),
  F1_Score = numeric()
)

for (i in 1:5) {
  
  # Split into train / validation
  train_fold <- train_unscaled_balanced[-folds[[i]], ]
  test_fold  <- train_unscaled_balanced[folds[[i]], ]
  
  # Train model
  rf_model <- randomForest(y ~ ., data = train_fold, ntree = 500)
  
  # Predict
  preds <- predict(rf_model, newdata = test_fold)
  
  # Ensure consistent factor levels
  test_fold$y <- factor(test_fold$y, levels = c("no", "yes"))
  preds <- factor(preds, levels = c("no", "yes"))
  
  # Evaluate
  cm <- confusionMatrix(preds, test_fold$y, positive = "yes")
  
  # Extract correct metrics
  acc <- cm$overall["Accuracy"]
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(precision) * as.numeric(recall) /
    (as.numeric(precision) + as.numeric(recall))
  
  # Store results
  cv_results_bal <- rbind(
    cv_results_bal,
    data.frame(
      Fold = i,
      Accuracy = as.numeric(acc),
      F1_Score = as.numeric(f1)
    )
  )
  
  cat("Fold", i,
      "- Accuracy:", round(acc, 4),
      "| F1 Score:", round(f1, 4), "\n")
}

# =========================
# Summary
# =========================
cv_results_bal <- rbind(
  cv_results_bal,
  data.frame(
    Fold = "Mean",
    Accuracy = mean(cv_results_bal$Accuracy),
    F1_Score = mean(cv_results_bal$F1_Score)
  ),
  data.frame(
    Fold = "SD",
    Accuracy = sd(cv_results_bal$Accuracy),
    F1_Score = sd(cv_results_bal$F1_Score)
  )
)

print(cv_results_bal)


# =========================
# Cross Validation (5-Fold) - Random Forest (Imbalanced)
# =========================

set.seed(123)

folds_imbal <- createFolds(train_unscaled_imbalanced$y, k = 5)

cv_results_imbal <- data.frame(
  Fold = integer(),
  Accuracy = numeric(),
  F1_Score = numeric()
)

for (i in 1:5) {
  
  # Split into train / validation
  train_fold <- train_unscaled_imbalanced[-folds_imbal[[i]], ]
  test_fold  <- train_unscaled_imbalanced[folds_imbal[[i]], ]
  
  # Train model
  rf_model <- randomForest(y ~ ., data = train_fold, ntree = 500)
  
  # Predict
  preds <- predict(rf_model, newdata = test_fold)
  
  # Ensure consistent factor levels
  test_fold$y <- factor(test_fold$y, levels = c("no", "yes"))
  preds <- factor(preds, levels = c("no", "yes"))
  
  # Evaluate
  cm <- confusionMatrix(preds, test_fold$y, positive = "yes")
  
  # Extract correct metrics
  acc <- cm$overall["Accuracy"]
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(precision) * as.numeric(recall) /
    (as.numeric(precision) + as.numeric(recall))
  
  # Store results
  cv_results_imbal <- rbind(
    cv_results_imbal,
    data.frame(
      Fold = i,
      Accuracy = as.numeric(acc),
      F1_Score = as.numeric(f1)
    )
  )
  
  cat("Fold", i,
      "(Imbalanced) - Accuracy:", round(acc, 4),
      "| F1 Score:", round(f1, 4), "\n")
}

# =========================
# Summary
# =========================
cv_results_imbal <- rbind(
  cv_results_imbal,
  data.frame(
    Fold = "Mean",
    Accuracy = mean(cv_results_imbal$Accuracy),
    F1_Score = mean(cv_results_imbal$F1_Score)
  ),
  data.frame(
    Fold = "SD",
    Accuracy = sd(cv_results_imbal$Accuracy),
    F1_Score = sd(cv_results_imbal$F1_Score)
  )
)

print(cv_results_imbal)



# =========================
# 3. Support Vector Machine
# =========================


# =========================
# Imbalanced SVM Preprocessing
# =========================
rec_imbal <- base_recipe(train_data) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  prep(training = train_data, retain = TRUE)

train_imbal <- juice(rec_imbal)
test_imbal  <- bake(rec_imbal, new_data = test_data)

# =========================
# Train SVM (caret)
# =========================
set.seed(42)

svm_imbal <- train(
  y ~ .,
  data = train_imbal,
  method = "svmLinear",
  trControl = cv_ctrl_imbal,
  metric = "F1",
  tuneLength = 5
)

print(svm_imbal)

# =========================
# Final Evaluation
# =========================
svm_final_imbal <- svm(
  y ~ ., 
  data = train_imbal,
  kernel = "linear"
)

pred_imbal_test <- predict(svm_final_imbal, newdata = test_imbal)

conf_imbal <- confusionMatrix(
  pred_imbal_test,
  test_imbal$y,
  positive = "yes"
)

print(conf_imbal)

# =========================
# Cross Validation (Manual)
# =========================
set.seed(123)

folds <- createFolds(train_imbal$y, k = 5)
cv_imbal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- train_imbal[-folds[[i]], ]
  test_fold  <- train_imbal[folds[[i]], ]
  
  model <- svm(
    y ~ ., 
    data = train_fold,
    kernel = "linear",
    cost = 1
  )
  
  preds <- predict(model, newdata = test_fold)
  
  # Fix factor levels
  test_fold$y <- factor(test_fold$y, levels = c("no", "yes"))
  preds <- factor(preds, levels = c("no", "yes"))
  
  cm <- confusionMatrix(preds, test_fold$y, positive = "yes")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_imbal_results[i, "F1"] <- f1
  cv_imbal_results[i, "Accuracy"] <- acc
}

# =========================
# Summary
# =========================
mean_f1  <- mean(cv_imbal_results$F1)
sd_f1    <- sd(cv_imbal_results$F1)

mean_acc <- mean(cv_imbal_results$Accuracy)
sd_acc   <- sd(cv_imbal_results$Accuracy)

cat("Imbalanced SVM - Mean F1:", round(mean_f1, 4),
    "| Std Dev:", round(sd_f1, 4), "\n")

cat("Imbalanced SVM - Mean Accuracy:", round(mean_acc, 4),
    "| Std Dev:", round(sd_acc, 4), "\n")

print(cv_imbal_results)

library(caret)
library(e1071)
library(dplyr)

# =========================
# Balanced SVM Preprocessing
# =========================
rec_bal <- base_recipe(train_data) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_upsample(y) %>%
  prep(training = train_data, retain = TRUE)

train_bal <- juice(rec_bal)
test_bal  <- bake(rec_bal, new_data = test_data)

# =========================
# Train SVM (caret)
# =========================
set.seed(42)

svm_bal <- train(
  y ~ .,
  data = train_bal,
  method = "svmLinear",
  trControl = cv_ctrl_bal,
  metric = "F1",   # ⚠️ requires custom summaryFunction
  tuneLength = 5
)

print(svm_bal)

# =========================
# Final Evaluation
# =========================
svm_final_bal <- svm(
  y ~ ., 
  data = train_bal,
  kernel = "linear"
)

pred_bal_test <- predict(svm_final_bal, newdata = test_bal)

conf_bal <- confusionMatrix(
  pred_bal_test,
  test_bal$y,
  positive = "yes"
)

print(conf_bal)

# =========================
# Cross Validation (Manual)
# =========================
set.seed(123)

folds <- createFolds(train_bal$y, k = 5)
cv_bal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- train_bal[-folds[[i]], ]
  test_fold  <- train_bal[folds[[i]], ]
  
  model <- svm(
    y ~ ., 
    data = train_fold,
    kernel = "linear",
    cost = 1
  )
  
  preds <- predict(model, newdata = test_fold)
  
  # Ensure correct factor levels
  test_fold$y <- factor(test_fold$y, levels = c("no", "yes"))
  preds <- factor(preds, levels = c("no", "yes"))
  
  cm <- confusionMatrix(preds, test_fold$y, positive = "yes")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_bal_results[i, "F1"] <- f1
  cv_bal_results[i, "Accuracy"] <- acc
}

# =========================
# Summary
# =========================
mean_f1 <- mean(cv_bal_results$F1)
sd_f1   <- sd(cv_bal_results$F1)

mean_acc <- mean(cv_bal_results$Accuracy)
sd_acc   <- sd(cv_bal_results$Accuracy)

cat("Balanced SVM - Mean F1:", round(mean_f1, 4),
    "| Std Dev:", round(sd_f1, 4), "\n")

cat("Balanced SVM - Mean Accuracy:", round(mean_acc, 4),
    "| Std Dev:", round(sd_acc, 4), "\n")

print(cv_bal_results)

# =========================
# 4. KNN 
# =========================

library(class)
library(caret)

# =========================
# Balanced KNN (5-Fold CV)
# =========================
set.seed(123)

folds_bal <- createFolds(train_bal_scaled$y, k = 5)
cv_knn_bal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- train_bal_scaled[-folds_bal[[i]], ]
  test_fold  <- train_bal_scaled[folds_bal[[i]], ]
  
  # Split predictors and target
  x_train <- train_fold[, setdiff(names(train_fold), "y")]
  y_train <- train_fold$y
  
  x_test <- test_fold[, setdiff(names(test_fold), "y")]
  y_test <- test_fold$y
  
  # KNN prediction
  pred <- knn(train = x_train, test = x_test, cl = y_train, k = 5)
  
  # Ensure factor levels
  y_test <- factor(y_test, levels = c("no", "yes"))
  pred   <- factor(pred, levels = c("no", "yes"))
  
  # Evaluation
  cm <- confusionMatrix(pred, y_test, positive = "yes")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_knn_bal_results[i, "F1"] <- f1
  cv_knn_bal_results[i, "Accuracy"] <- acc
}

# Summary
cat("Balanced KNN - Mean F1:", round(mean(cv_knn_bal_results$F1), 4),
    "| SD:", round(sd(cv_knn_bal_results$F1), 4), "\n")

cat("Balanced KNN - Mean Accuracy:", round(mean(cv_knn_bal_results$Accuracy), 4),
    "| SD:", round(sd(cv_knn_bal_results$Accuracy), 4), "\n")

print(cv_knn_bal_results)


# =========================
# Imbalanced KNN (5-Fold CV)
# =========================
set.seed(123)

folds_imbal <- createFolds(train_imbal_scaled$y, k = 5)
cv_knn_imbal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- train_imbal_scaled[-folds_imbal[[i]], ]
  test_fold  <- train_imbal_scaled[folds_imbal[[i]], ]
  
  x_train <- train_fold[, setdiff(names(train_fold), "y")]
  y_train <- train_fold$y
  
  x_test <- test_fold[, setdiff(names(test_fold), "y")]
  y_test <- test_fold$y
  
  pred <- knn(train = x_train, test = x_test, cl = y_train, k = 5)
  
  y_test <- factor(y_test, levels = c("no", "yes"))
  pred   <- factor(pred, levels = c("no", "yes"))
  
  cm <- confusionMatrix(pred, y_test, positive = "yes")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_knn_imbal_results[i, "F1"] <- f1
  cv_knn_imbal_results[i, "Accuracy"] <- acc
}

# Summary
cat("Imbalanced KNN - Mean F1:", round(mean(cv_knn_imbal_results$F1), 4),
    "| SD:", round(sd(cv_knn_imbal_results$F1), 4), "\n")

cat("Imbalanced KNN - Mean Accuracy:", round(mean(cv_knn_imbal_results$Accuracy), 4),
    "| SD:", round(sd(cv_knn_imbal_results$Accuracy), 4), "\n")

print(cv_knn_imbal_results)


# =========================
# Function: Plot Confusion Matrix Heatmap
# =========================
plot_confusion <- function(cm, title) {
  
  # Convert confusion matrix to dataframe
  df <- as.data.frame(cm$table)
  colnames(df) <- c("Prediction", "Reference", "Freq")
  
  ggplot(df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5, color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    labs(
      title = title,
      x = "Predicted",
      y = "Actual"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16)
    )
}

# =========================
# Generate Plots
# =========================
p1 <- plot_confusion(conf_bal,  "KNN Confusion Matrix (Balanced)")
p2 <- plot_confusion(conf_imbal, "KNN Confusion Matrix (Imbalanced)")

# =========================
# Display Side by Side
# =========================
grid.arrange(p1, p2, ncol = 2)


# =========================
# BALANCED KNN (5-Fold CV)
# =========================
set.seed(123)

folds_bal <- createFolds(train_bal_scaled$y, k = 5)
cv_knn_bal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- train_bal_scaled[-folds_bal[[i]], ]
  test_fold  <- train_bal_scaled[folds_bal[[i]], ]
  
  # Split predictors and target
  x_train <- train_fold[, setdiff(names(train_fold), "y")]
  y_train <- train_fold$y
  
  x_test <- test_fold[, setdiff(names(test_fold), "y")]
  y_test <- test_fold$y
  
  # KNN requires matrix/numeric
  x_train <- as.matrix(x_train)
  x_test  <- as.matrix(x_test)
  
  # Prediction
  pred <- knn(train = x_train, test = x_test, cl = y_train, k = 5)
  
  # Ensure consistent factor levels
  y_test <- factor(y_test, levels = c("no", "yes"))
  pred   <- factor(pred, levels = c("no", "yes"))
  
  # Evaluation
  cm <- confusionMatrix(pred, y_test, positive = "yes")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_knn_bal_results[i, "F1"] <- f1
  cv_knn_bal_results[i, "Accuracy"] <- acc
}

# Summary
cat("Balanced KNN - Mean F1:", round(mean(cv_knn_bal_results$F1), 4),
    "| SD:", round(sd(cv_knn_bal_results$F1), 4), "\n")

cat("Balanced KNN - Mean Accuracy:", round(mean(cv_knn_bal_results$Accuracy), 4),
    "| SD:", round(sd(cv_knn_bal_results$Accuracy), 4), "\n")

print(cv_knn_bal_results)


# =========================
# IMBALANCED KNN (5-Fold CV)
# =========================
set.seed(123)

folds_imbal <- createFolds(train_imbal_scaled$y, k = 5)
cv_knn_imbal_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- train_imbal_scaled[-folds_imbal[[i]], ]
  test_fold  <- train_imbal_scaled[folds_imbal[[i]], ]
  
  x_train <- train_fold[, setdiff(names(train_fold), "y")]
  y_train <- train_fold$y
  
  x_test <- test_fold[, setdiff(names(test_fold), "y")]
  y_test <- test_fold$y
  
  x_train <- as.matrix(x_train)
  x_test  <- as.matrix(x_test)
  
  pred <- knn(train = x_train, test = x_test, cl = y_train, k = 5)
  
  y_test <- factor(y_test, levels = c("no", "yes"))
  pred   <- factor(pred, levels = c("no", "yes"))
  
  cm <- confusionMatrix(pred, y_test, positive = "yes")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_knn_imbal_results[i, "F1"] <- f1
  cv_knn_imbal_results[i, "Accuracy"] <- acc
}

# Summary
cat("Imbalanced KNN - Mean F1:", round(mean(cv_knn_imbal_results$F1), 4),
    "| SD:", round(sd(cv_knn_imbal_results$F1), 4), "\n")

cat("Imbalanced KNN - Mean Accuracy:", round(mean(cv_knn_imbal_results$Accuracy), 4),
    "| SD:", round(sd(cv_knn_imbal_results$Accuracy), 4), "\n")

print(cv_knn_imbal_results)


# =========================
# 5. ANN
# =========================

library(neuralnet)
library(caret)

# =========================
# BALANCED ANN
# =========================
train_bal <- train_scaled_balanced
test_bal  <- test_scaled_balanced

# Convert target to numeric (0/1)
train_bal$y <- ifelse(train_bal$y == "yes", 1, 0)
test_bal$y  <- ifelse(test_bal$y == "yes", 1, 0)

# Features
features <- setdiff(names(train_bal), "y")
formula_ann <- as.formula(paste("y ~", paste(features, collapse = " + ")))

set.seed(42)

ann_bal <- neuralnet(
  formula = formula_ann,
  data = train_bal,
  hidden = c(4),
  linear.output = FALSE,
  stepmax = 1e6
)

# Predict
pred_raw_bal <- compute(ann_bal, test_bal[, features])$net.result
pred_class_bal <- ifelse(pred_raw_bal > 0.5, 1, 0)

# Convert to factors for evaluation
pred_class_bal <- factor(pred_class_bal, levels = c(0, 1))
test_bal$y     <- factor(test_bal$y, levels = c(0, 1))

conf_bal <- confusionMatrix(
  pred_class_bal,
  test_bal$y,
  positive = "1"
)

print(conf_bal)


# =========================
# IMBALANCED ANN
# =========================
train_imbal <- train_scaled_imbalanced
test_imbal  <- test_scaled_imbalanced

train_imbal$y <- ifelse(train_imbal$y == "yes", 1, 0)
test_imbal$y  <- ifelse(test_imbal$y == "yes", 1, 0)

features <- setdiff(names(train_imbal), "y")
formula_ann <- as.formula(paste("y ~", paste(features, collapse = " + ")))

set.seed(42)

ann_imbal <- neuralnet(
  formula = formula_ann,
  data = train_imbal,
  hidden = c(4),
  linear.output = FALSE,
  stepmax = 1e6
)

# Predict
pred_raw_imbal <- compute(ann_imbal, test_imbal[, features])$net.result
pred_class_imbal <- ifelse(pred_raw_imbal > 0.5, 1, 0)

pred_class_imbal <- factor(pred_class_imbal, levels = c(0, 1))
test_imbal$y     <- factor(test_imbal$y, levels = c(0, 1))

conf_imbal <- confusionMatrix(
  pred_class_imbal,
  test_imbal$y,
  positive = "1"
)

print(conf_imbal)


# =========================
# CONFUSION MATRIX PLOT
# =========================
plot_confusion <- function(cm, title) {
  df <- as.data.frame(cm$table)
  colnames(df) <- c("Prediction", "Reference", "Freq")
  
  ggplot(df, aes(Prediction, Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 5, color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    labs(title = title, x = "Predicted", y = "Actual") +
    theme(plot.title = element_text(hjust = 0.5))
}

# =========================
# VISUALISE EXISTING MODELS
# =========================
p1 <- plot_confusion(conf_bal,  "ANN (Balanced)")
p2 <- plot_confusion(conf_imbal, "ANN (Imbalanced)")
grid.arrange(p1, p2, ncol = 2)


# =========================
# ANN CROSS VALIDATION
# =========================
set.seed(123)

data_to_use <- train_scaled_imbalanced  # change to balanced if needed

folds <- createFolds(data_to_use$y, k = 5)

cv_results <- data.frame(Fold = 1:5, F1 = NA, Accuracy = NA)

for (i in 1:5) {
  
  train_fold <- data_to_use[-folds[[i]], ]
  test_fold  <- data_to_use[folds[[i]], ]
  
  # Convert target
  train_fold$y <- ifelse(train_fold$y == "yes", 1, 0)
  test_fold$y  <- ifelse(test_fold$y == "yes", 1, 0)
  
  # Features
  features <- setdiff(names(train_fold), "y")
  formula_ann <- as.formula(paste("y ~", paste(features, collapse = " + ")))
  
  # Train ANN
  ann_model <- neuralnet(
    formula = formula_ann,
    data = train_fold,
    hidden = c(4),
    linear.output = FALSE,
    stepmax = 1e6
  )
  
  # Predict
  pred_raw <- compute(ann_model, test_fold[, features])$net.result
  pred_class <- ifelse(pred_raw > 0.5, 1, 0)
  
  # Convert for evaluation
  pred_class <- factor(pred_class, levels = c(0,1))
  test_fold$y <- factor(test_fold$y, levels = c(0,1))
  
  cm <- confusionMatrix(pred_class, test_fold$y, positive = "1")
  
  acc  <- cm$overall["Accuracy"]
  prec <- cm$byClass["Pos Pred Value"]
  rec  <- cm$byClass["Sensitivity"]
  
  f1 <- 2 * as.numeric(prec) * as.numeric(rec) /
    (as.numeric(prec) + as.numeric(rec))
  
  cv_results[i, "F1"] <- f1
  cv_results[i, "Accuracy"] <- acc
}

# =========================
# SUMMARY
# =========================
cat("ANN Mean F1:", round(mean(cv_results$F1), 4),
    "| SD:", round(sd(cv_results$F1), 4), "\n")

cat("ANN Mean Accuracy:", round(mean(cv_results$Accuracy), 4),
    "| SD:", round(sd(cv_results$Accuracy), 4), "\n")

print(cv_results)