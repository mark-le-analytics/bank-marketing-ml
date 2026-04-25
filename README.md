# 📊 Predictive Modelling for Term Deposit Subscription

## 📌 Overview
This project builds an end-to-end machine learning pipeline to predict whether a bank customer will subscribe to a long-term deposit. The objective is to improve marketing targeting, increase conversion rates, and optimise campaign efficiency.

The project uses real-world banking data and evaluates multiple classification models under both **balanced** and **imbalanced** scenarios to understand the impact of class imbalance on predictive performance.


## 🎯 Business Problem
Banks invest heavily in telemarketing campaigns, but most customers do not subscribe (~89% “No”, ~11% “Yes”).

This creates two key challenges:
- Wasted marketing costs on uninterested customers  
- Missed opportunities to identify potential subscribers  

👉 Goal:  
Build a model that accurately identifies **high-potential customers** before outreach.



## 📂 Dataset
- Source: Portuguese banking dataset (`bank-additional.csv`)
- Records: ~4,100 customers  
- Features: 20+ demographic, behavioural, and macroeconomic variables  
- Target: `y` (subscription: Yes/No)

### Key Characteristics
- Severe class imbalance (~11% positive)
- Mix of categorical and numerical variables  
- Includes macroeconomic indicators (e.g., Euribor rate)



## 🧹 Data Preparation

### Cleaning
- Converted `"unknown"` values → `NA`
- Removed data leakage variable (`duration`)
- Verified no duplicates or invalid values

### Feature Engineering
- One-hot encoding for categorical variables  
- Median/mode imputation  
- Removal of zero-variance predictors  

### Sampling Strategy
Two datasets were created:

| Dataset Type | Purpose |
|-------------|--------|
| **Balanced (Upsampled)** | Improve detection of minority class |
| **Imbalanced (Original)** | Reflect real-world distribution |



## 🤖 Models Implemented

The project compares five machine learning models:

- 🌳 Decision Tree  
- 🌲 Random Forest  
- ⚙️ Support Vector Machine (SVM)  
- 📍 k-Nearest Neighbours (KNN)  
- 🧠 Artificial Neural Network (ANN)  

Each model was evaluated on:
- Balanced dataset  
- Imbalanced dataset  


## 📏 Evaluation Metrics

Models were evaluated using:

- Accuracy  
- Precision  
- Recall  
- F1 Score  

> ⚠️ Accuracy alone is misleading due to class imbalance.  
> The project prioritises **Recall and F1-score**, as failing to identify potential subscribers is more costly in a marketing context.

---

## 📊 Model Performance Summary

### 📌 Balanced Dataset

| Model            | Accuracy | Precision | Recall | F1 Score |
|------------------|---------|----------|--------|----------|
| Decision Tree    | 83.72%  | 34.29%   | 53.33% | 41.74%   |
| Random Forest    | 86.80%  | 40.80%   | 45.90% | 43.20%   |
| SVM              | 74.08%  | 68.30%   | **90.40%** | **77.65%** |
| KNN              | 68.74%  | 20.88%   | 66.67% | 31.80%   |
| ANN              | 83.97%  | 34.48%   | 51.85% | 41.40%   |

---

### 📌 Imbalanced Dataset

| Model            | Accuracy | Precision | Recall | F1 Score |
|------------------|---------|----------|--------|----------|
| Decision Tree    | 89.88%  | 65.62%   | 15.56% | 25.15%   |
| Random Forest    | 89.20%  | 52.00%   | 19.30% | 28.10%   |
| SVM              | 90.01%  | 91.00%   | 61.22% | **94.62%** |
| KNN              | 89.07%  | 50.00%   | 21.48% | 30.05%   |
| ANN              | 88.26%  | 45.28%   | 35.56% | 39.80%   |

---

## 📉 Key Observations

### 1. Class Imbalance Problem
- Imbalanced models achieve **high accuracy (~90%)**
- But suffer from **extremely low recall (as low as 15%)**
- Meaning:  
  👉 Models mostly predict **“No subscription”**

---

### 2. Balanced Dataset Improves Detection
- Significant improvement in:
  - Recall
  - F1-score
- Especially for:
  - Decision Tree (+26% recall)
  - ANN (+16% recall)

---

### 3. SVM Dominates Performance

- **Balanced SVM**
  - Recall: **90.40%**
  - F1 Score: **77.65%**
  - Best at identifying true customers

- **Imbalanced SVM**
  - F1 Score: **94.62%**
  - Strong precision but slightly lower recall

👉 SVM consistently delivers the **best overall performance**

---

### 4. Random Forest & ANN
- More stable than Decision Trees
- Moderate recall and F1
- Less aggressive in detecting positives compared to SVM

---

### 5. KNN Limitations
- Sensitive to:
  - Scaling
  - Data distribution
- Lower precision and unstable performance across datasets

---

## 📊 Confusion Matrix Insights

Across all models:

- Imbalanced models:
  - Very high **True Negatives**
  - Very low **True Positives**
  - → Miss potential customers

- Balanced models:
  - Increased **True Positives**
  - Acceptable increase in False Positives
  - → Better business value

---

## 🧠 Business Insights

Key factors influencing subscription:

- Previous campaign outcome (`poutcome`)
- Contact method (`contact`)
- Economic indicators (e.g. `euribor3m`, `nr.employed`)

---

## 🏆 Final Recommendation

👉 **Balanced SVM is the optimal model**

### Why:
- Highest Recall (**90.40%**) → captures most potential customers  
- Strong F1 Score (**77.65%**) → balanced performance  
- Consistent cross-validation results  

### Trade-off:
- Slight increase in false positives  
- Acceptable in marketing campaigns  

---

## 💡 Key Takeaways

- Accuracy is not reliable for imbalanced classification  
- Data balancing is critical for business-focused models  
- Model selection should align with **business objective**, not just metrics  

👉 In this case:  
**Maximising customer acquisition > minimising false positives**

## 📁 Project Structure


---

## 🛠️ Tools & Technologies

- R  
- caret  
- recipes  
- randomForest  
- e1071 (SVM)  
- nnet / neuralnet  
- ggplot2  

---

## 🚀 How to Run

1. Open in RStudio
2. Install required packages
3. Run scripts in order

## 🧠 What This Project Demonstrates
- End-to-end ML pipeline
- Handling imbalanced datasets
- Model comparison and evaluation
- Business-driven metric selection
- Translating data insights into strategy

## 📌 Future Improvements
- Hyperparameter tuning (Grid / Bayesian)
- Ensemble methods (Boosting, Stacking)
- Feature selection optimisation
- Deployment (API / dashboard)

## 👤 Author

Mark LE
