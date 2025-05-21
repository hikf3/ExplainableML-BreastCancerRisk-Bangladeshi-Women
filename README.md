# ExplainableML-BreastCancerRisk-Bangladeshi-Women

This repository contains the codebase for the research study:  
**"Understanding Cancer Risk Among Bangladeshi Women: An Explainable Machine Learning Approach to Socio-Reproductive Factors Using Tertiary Hospital Data."** Link: https://www.preprints.org/manuscript/202504.2400/v1

We developed and evaluated machine learning models to identify socio-demographic and reproductive predictors of two breast cancer subtypes—**Hormone Receptor-Positive (HR⁺)** and **Triple-Negative Breast Cancer (TNBC)**—using structured data collected from a tertiary cancer hospital in Bangladesh. We further employed **Shapley value-based explainability** to interpret model predictions.

---

## 🧪 Methods Summary

### Data
- **Source:** National Institute of Cancer Research & Hospital (NICRH), Bangladesh  
- **Participants:** 443 cancer-free controls, 246 HR⁺, and 240 TNBC cases  
- **Features:** Socio-demographic (residence, education, profession), reproductive (age at first baby, parity, menarche, abortion history, BMI)

### Machine Learning Models
- Logistic Regression
- Logistic Regression with Lasso
- Support Vector Machine (SVM)
- Random Forest
- XGBoost

All models were evaluated using **stratified 5-fold cross-validation**.

### Model Explainability
- **Shapley values** were computed using the `fastshap` R package
- Best-performing model per subtype was used to extract instance-level Shapley values
- Feature importance was visualized:
  - Global SHAP mean ± 95% CI
  - Pairwise subtype comparisons (Wilcoxon tests)
  - SHAP trends across numeric feature values

---

## 📊 Key Results

- **Rural residence**, **low education**, and **abortion history** were important predictors of both HR⁺ and TNBC
- **Age at menarche** and **age at first baby** were more predictive for HR⁺
- **Gaps between reproductive events** were more predictive for TNBC
- **XGBoost** and **Random Forest** were the top-performing models (AUC > 0.75)

---



