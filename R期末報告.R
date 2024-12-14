install.packages("Epi")
install.packages("ResourceSelection")
install.packages("mgcv")
install.packages("DMwR2")
install.packages("randomForest")
install.packages("xgboost")
install.packages("ROSE")
# 1. 載入所需套件
library(dplyr)
library(ggplot2)
library(corrplot)
library(MASS)
library(Epi)
library(ResourceSelection)
library(mgcv)
library(pROC)
library(randomForest)
library(xgboost)
library(ROSE)
library(reshape2)

# 2. 資料前處理
data_stroke <- read.csv("/Users/tommy/Desktop/R程式設計/bio.csv")

# 轉換類別變數為因子
categorical_vars <- c("gender", "hypertension", "heart_disease", 
                      "ever_married", "Residence_type", "smoking_status", "stroke")
data_stroke[categorical_vars] <- lapply(data_stroke[categorical_vars], as.factor)

# 處理遺漏值和年齡分組
data_stroke <- na.omit(data_stroke)
data_stroke$age_group <- cut(data_stroke$age, 
                             breaks=c(0,20,40,60,80,100),
                             labels=c("0-20","21-40","41-60","61-80","81+"))

# 3. 資料平衡處理
set.seed(123)
balanced_data <- ROSE(stroke ~ ., data = data_stroke, N = 5000)$data

# 4. 探索性分析
summary(balanced_data)

# 視覺化分析
p1 <- ggplot(balanced_data, aes(x=age)) + 
  geom_histogram(bins=30, fill="skyblue", color="black") +
  labs(title="Age Distribution", x="Age", y="Count")

p2 <- ggplot(balanced_data, aes(x=stroke, y=age, fill=stroke)) + 
  geom_boxplot() +
  labs(title="Age Distribution by Stroke Status")

# BMI分布圖
p3 <- ggplot(balanced_data, aes(x=bmi)) + 
  geom_histogram(bins=30, fill="lightgreen", color="black") +
  labs(title="BMI Distribution", x="BMI", y="Count")

# 血糖分布圖
p4 <- ggplot(balanced_data, aes(x=avg_glucose_level)) + 
  geom_histogram(bins=30, fill="salmon", color="black") +
  labs(title="Glucose Level Distribution", x="Average Glucose Level", y="Count")

# 中風與各風險因子的關係箱型圖
p5 <- ggplot(balanced_data, aes(x=stroke, y=avg_glucose_level, fill=stroke)) + 
  geom_boxplot() +
  labs(title="Glucose Level Distribution by Stroke Status")

p6 <- ggplot(balanced_data, aes(x=stroke, y=bmi, fill=stroke)) + 
  geom_boxplot() +
  labs(title="BMI Distribution by Stroke Status")

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)

# 4.1 相關性分析
numeric_vars <- balanced_data[,c("age", "avg_glucose_level", "bmi")]
cor_matrix <- cor(numeric_vars, use="complete.obs")
corrplot(cor_matrix, method="color")

# 4.2 風險因子相關性熱圖
risk_factors_cor <- balanced_data %>%
  mutate(across(c(hypertension, heart_disease, stroke), as.numeric)) %>%
  dplyr::select(age, avg_glucose_level, bmi, hypertension, heart_disease, stroke)

# 計算相關係數矩陣
cor_matrix_all <- cor(risk_factors_cor)

# 創建熱圖
heatmap_plot <- ggplot(data = melt(cor_matrix_all), 
                       aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Correlation Heatmap of Risk Factors")

# 顯示熱圖
print(heatmap_plot)

# 5. 資料分割和模型準備
set.seed(1035)
split_index <- sample(1:2, nrow(balanced_data), replace=TRUE, prob=c(0.7, 0.3))
train_data <- balanced_data[split_index==1,]
test_data <- balanced_data[split_index==2,]

# 6. 模型訓練
# 6.1 GAM模型
gam_model <- gam(stroke ~ s(age) + s(avg_glucose_level) + s(bmi) + 
                   hypertension + heart_disease + gender,
                 family = binomial,
                 data = train_data)

# 6.2 隨機森林模型
rf_model <- randomForest(stroke ~ avg_glucose_level + bmi + age + 
                           hypertension + heart_disease, 
                         data = train_data,
                         ntree = 500)

# 6.3 XGBoost模型
# 準備XGBoost資料
train_matrix <- model.matrix(~ -1 + avg_glucose_level + bmi + age + 
                               hypertension + heart_disease, data = train_data)
test_matrix <- model.matrix(~ -1 + avg_glucose_level + bmi + age + 
                              hypertension + heart_disease, data = test_data)
xgb_train <- xgb.DMatrix(train_matrix, label = as.numeric(train_data$stroke) - 1)
xgb_test <- xgb.DMatrix(test_matrix, label = as.numeric(test_data$stroke) - 1)

# XGBoost參數和訓練
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1
)

xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 100,
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 10,
  verbose = 0
)

# 7. 模型評估
# 產生預測結果
gam_pred <- predict(gam_model, test_data, type = "response")
gam_class <- factor(ifelse(gam_pred > 0.5, "1", "0"), levels = levels(test_data$stroke))

rf_pred <- predict(rf_model, test_data)
rf_prob <- predict(rf_model, test_data, type = "prob")[,2]

xgb_pred <- predict(xgb_model, xgb_test)
xgb_class <- factor(ifelse(xgb_pred > 0.5, "1", "0"), levels = levels(test_data$stroke))

# ROC曲線和AUC值
gam_roc <- roc(test_data$stroke, as.numeric(gam_pred))
rf_roc <- roc(test_data$stroke, rf_prob)
xgb_roc <- roc(test_data$stroke, as.numeric(xgb_pred))

# 模型比較
model_comparison <- data.frame(
    "模型" = c("GAM", "隨機森林", "XGBoost"),
    "準確率" = c(
      mean(gam_class == test_data$stroke),
      mean(rf_pred == test_data$stroke),
      mean(xgb_class == test_data$stroke)
    ),
  AUC = c(
    auc(gam_roc),
    auc(rf_roc),
    auc(xgb_roc)
  )
)
print(model_comparison)
# 7.1 擴充模型評估指標
model_evaluation <- function(actual, predicted, predicted_prob) {
  conf_matrix <- table(actual, predicted)
  
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  sensitivity <- conf_matrix[2,2] / sum(conf_matrix[2,])
  specificity <- conf_matrix[1,1] / sum(conf_matrix[1,])
  precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
  f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  return(data.frame(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity,
    F1_Score = f1_score
  ))
}

# 更新模型比較結果
models_comparison <- rbind(
  cbind(Model = "GAM", 
        model_evaluation(test_data$stroke, gam_class, gam_pred),
        AUC = auc(gam_roc)),
  cbind(Model = "RandomForest", 
        model_evaluation(test_data$stroke, rf_pred, rf_prob),
        AUC = auc(rf_roc)),
  cbind(Model = "XGBoost", 
        model_evaluation(test_data$stroke, xgb_class, xgb_pred),
        AUC = auc(xgb_roc))
)

# 8. 分層分析和視覺化
age_group_analysis <- balanced_data %>%
  group_by(age_group) %>%
  summarize(
    stroke_rate = mean(as.numeric(as.character(stroke))),
    avg_glucose = mean(avg_glucose_level),
    hypertension_rate = mean(as.numeric(as.character(hypertension))),
    n = n()
  )

# 最終視覺化
p_final <- ggplot(age_group_analysis) +
  geom_bar(aes(x = age_group, y = stroke_rate), stat = "identity", fill = "skyblue") +
  geom_line(aes(x = age_group, y = hypertension_rate, group = 1), color = "red", size = 1) +
  geom_point(aes(x = age_group, y = hypertension_rate), color = "red", size = 3) +
  scale_y_continuous(name = "Rate") +
  labs(title = "Stroke and Hypertension Rates by Age Group",
       x = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_final)

# 8.1 擴充年齡層分析
age_risk_analysis <- balanced_data %>%
  group_by(age_group) %>%
  summarise(
    stroke_rate = mean(as.numeric(as.character(stroke))),
    hypertension_rate = mean(as.numeric(as.character(hypertension))),
    heart_disease_rate = mean(as.numeric(as.character(heart_disease))),
    avg_glucose = mean(avg_glucose_level),
    avg_bmi = mean(bmi),
    n_samples = n()
  )

# 視覺化年齡層風險特徵
p_age_risk <- ggplot(age_risk_analysis) +
  geom_line(aes(x = age_group, y = stroke_rate, group = 1, color = "Stroke Rate")) +
  geom_line(aes(x = age_group, y = hypertension_rate, group = 1, color = "Hypertension Rate")) +
  geom_line(aes(x = age_group, y = heart_disease_rate, group = 1, color = "Heart Disease Rate")) +
  scale_color_manual(values = c("red", "blue", "green")) +
  labs(title = "Risk Factors by Age Group",
       y = "Rate",
       color = "Risk Factor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_age_risk)

# 8.2 風險因子組合分析
risk_combination_analysis <- balanced_data %>%
  group_by(hypertension, heart_disease) %>%
  summarise(
    stroke_rate = mean(as.numeric(as.character(stroke))),
    avg_age = mean(age),
    avg_glucose = mean(avg_glucose_level),
    avg_bmi = mean(bmi),
    n = n()
  )

# 視覺化風險組合
p_risk_combination <- ggplot(risk_combination_analysis, 
                             aes(x=interaction(hypertension, heart_disease), 
                                 y=stroke_rate)) +
  geom_bar(stat="identity", fill="lightblue") +
  geom_text(aes(label=sprintf("%.1f%%", stroke_rate*100)), vjust=-0.5) +
  labs(title="Stroke Rate by Risk Factor Combination",
       x="Risk Factors (Hypertension_HeartDisease)",
       y="Stroke Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_risk_combination)

# 8.3 年齡和風險因子交互作用
age_risk_interaction <- balanced_data %>%
  mutate(age_group = cut(age, breaks=seq(0, 100, by=20))) %>%
  group_by(age_group, hypertension, heart_disease) %>%
  summarise(
    stroke_rate = mean(as.numeric(as.character(stroke))),
    n = n()
  )

interaction_plot <- ggplot(age_risk_interaction, 
                           aes(x=age_group, y=stroke_rate, 
                               color=interaction(hypertension, heart_disease))) +
  geom_line(aes(group=interaction(hypertension, heart_disease))) +
  geom_point() +
  labs(title="Stroke Rate by Age and Risk Factors",
       x="Age Group",
       y="Stroke Rate",
       color="Risk Factors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(interaction_plot)

# 9. 特徵重要性分析
# 9.1 隨機森林特徵重要性
rf_importance <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[,1]
) %>%
  arrange(desc(Importance))

# 視覺化特徵重要性
p_importance <- ggplot(rf_importance, 
                       aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance from Random Forest",
       x = "Features",
       y = "Importance Score") +
  theme_minimal()

print(p_importance)

# 10. 模型選擇準則
model_selection_criteria <- models_comparison %>%
  mutate(
    Balanced_Score = (Sensitivity + Specificity) / 2,
    Final_Score = (AUC + Balanced_Score) / 2
  ) %>%
  arrange(desc(Final_Score))

# 視覺化模型比較
p_model_comparison <- ggplot(model_selection_criteria, 
                             aes(x = Model, y = Final_Score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Model Comparison - Final Scores",
       y = "Final Score") +
  theme_minimal()

print(p_model_comparison)

# 10.1 預測結果視覺化
# ROC曲線比較
# 獲取 ROC 曲線的座標
gam_coords <- coords(gam_roc, "all")
rf_coords <- coords(rf_roc, "all")
xgb_coords <- coords(xgb_roc, "all")

# 創建數據框
roc_data <- rbind(
  data.frame(specificity = gam_coords$specificity, 
             sensitivity = gam_coords$sensitivity, 
             Model = "GAM"),
  data.frame(specificity = rf_coords$specificity, 
             sensitivity = rf_coords$sensitivity, 
             Model = "Random Forest"),
  data.frame(specificity = xgb_coords$specificity, 
             sensitivity = xgb_coords$sensitivity, 
             Model = "XGBoost")
)

# 繪製 ROC 曲線
roc_plot <- ggplot(roc_data, aes(x = 1-specificity, y = sensitivity, color = Model)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curves Comparison",
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme_minimal() +
  scale_color_manual(values = c("GAM" = "blue", 
                                "Random Forest" = "red", 
                                "XGBoost" = "green")) +
  # 添加 AUC 值到圖例
  annotate("text", x = 0.75, y = 0.25, 
           label = paste("AUC (GAM):", round(auc(gam_roc), 3))) +
  annotate("text", x = 0.75, y = 0.20, 
           label = paste("AUC (RF):", round(auc(rf_roc), 3))) +
  annotate("text", x = 0.75, y = 0.15, 
           label = paste("AUC (XGB):", round(auc(xgb_roc), 3)))

# 顯示圖形
print(roc_plot)

# 預測概率分布
pred_dist <- data.frame(
  Actual = factor(test_data$stroke),
  GAM = gam_pred,
  RF = rf_prob,
  XGB = xgb_pred
) %>%
  reshape2::melt(id.vars="Actual")

pred_dist_plot <- ggplot(pred_dist, aes(x=value, fill=Actual)) +
  geom_density(alpha=0.5) +
  facet_wrap(~variable) +
  labs(title="Prediction Probability Distribution by Model",
       x="Predicted Probability",
       y="Density") +
  theme_minimal()

print(pred_dist_plot)

# 輸出所有比較結果
print("詳細模型評估結果：")
print(model_selection_criteria)
