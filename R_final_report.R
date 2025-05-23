install.packages("Epi")
install.packages("ResourceSelection")
install.packages("mgcv")
install.packages("DMwR2")
install.packages("randomForest")
install.packages("xgboost")
install.packages("ROSE")
install.packages("ggmosaic")
install.packages("ggrepel")
# 載入所需套件
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
library(readr)
library(tidyverse)
library(ggmosaic)
library(ggrepel)

# 1. 資料前處理
# 讀取資料
stroke_data <- read.csv("/Users/tommy/Downloads/healthcare-dataset-stroke-data.csv", 
                        header = TRUE,
                        sep = ",",          
                        quote = "",         
                        strip.white = TRUE, 
                        stringsAsFactors = FALSE)

# 檢查資料結構
str(stroke_data)

# 清理數據
stroke_data_clean <- stroke_data %>%
  mutate(
    # 移除ID欄位的引號
    X.id = gsub("\"", "", X.id),
    # 移除stroke欄位的引號
    stroke = gsub("\"", "", stroke.),
    stroke. = NULL,
    # 轉換資料類型
    X.id = as.integer(X.id),
    gender = as.factor(gender),
    hypertension = as.factor(hypertension),
    heart_disease = as.factor(heart_disease),
    ever_married = as.factor(ever_married),
    work_type = as.factor(work_type),
    Residence_type = as.factor(Residence_type),
    smoking_status = as.factor(smoking_status),
    stroke = as.factor(stroke),
    # 處理bmi欄位
    bmi = as.numeric(bmi)
  )
# 檢視清理後的數據結構
str(stroke_data_clean)
# 檢查缺失值
print("Missing Values:")
colSums(is.na(stroke_data_clean))
# 檢查遺失值的數量
sum(is.na(stroke_data_clean$bmi))
# 處理遺失值
stroke_data_clean <- stroke_data_clean %>%
  drop_na()
# 檢查性別並處理性別的欄位，移除other
table(stroke_data_clean$gender)
stroke_data_clean <- stroke_data_clean[-which(stroke_data_clean$gender=='Other'),]
table(stroke_data_clean$gender)
# 查看資料集的維度
dim(stroke_data_clean)
# 將清理後的資料寫出為CSV
write.csv(stroke_data_clean, 
          file = "/Users/tommy/Downloads/stroke_data_cleaned.csv",
          row.names = FALSE)

# 2. 資料匯入
data_stroke <- read.csv("/Users/tommy/Desktop/R程式設計/stroke_data_cleaned.csv")
data_stroke <- data_stroke[,-1]
# 移除 smoking_status 為 Unknown 的資料
data_clean <- subset(data_stroke, smoking_status != "Unknown")

# ever_married 轉換 (Yes=1, No=0)
data_clean$ever_married <- as.numeric(data_clean$ever_married == "Yes")

# work_type 轉換
# Private=0, Self-employed=1, Govt_job=2, children=3, Never_worked=4
data_clean$work_type <- factor(data_clean$work_type, 
                               levels = c("Private", "Self-employed", "Govt_job", 
                                          "children", "Never_worked"))
data_clean$work_type <- as.numeric(data_clean$work_type) - 1

# Residence_type 轉換 (Urban=1, Rural=0)
data_clean$Residence_type <- as.numeric(data_clean$Residence_type == "Urban")

# smoking_status 轉換
# never smoked=0, formerly smoked=1, smokes=2
data_clean$smoking_status <- factor(data_clean$smoking_status, 
                                    levels = c("never smoked", "formerly smoked", "smokes"))
data_clean$smoking_status <- as.numeric(data_clean$smoking_status) - 1

# 將清理後的資料寫出為CSV
write.csv(data_clean, 
          file = "/Users/tommy/Desktop/R程式設計/data_cleaned.csv",
          row.names = FALSE)

# 3. 探索性資料
# 探索資料
summary(data_clean)
head(data_clean)
# 年齡分佈圖
p1 <- ggplot(data_clean, aes(x=age)) + 
  geom_histogram(bins=30, fill="skyblue", color="black") +
  labs(title="Age Distribution", x="Age", y="Count") +
  theme_classic()
print(p1)

# 中風與年齡分組長條圖
gender_stroke<-ggplot(data_clean, aes(x = age, fill = as.factor(stroke))) +
  geom_histogram(position = "fill") +
  labs(title = "Stroke Rate by Age", x = "Age", y = "Stroke Rate", fill = "STROKE")
print(gender_stroke)

age_group_analysis <- data_clean %>%
  mutate(age_group = cut(age, breaks = seq(0, 100, by = 10))) %>%
  group_by(age_group) %>%
  summarise(stroke_rate = mean(as.numeric(as.character(stroke))))

p2 <- ggplot(age_group_analysis, aes(x = age_group, y = stroke_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Stroke Rate by Age Group", x = "Age Group", y = "Stroke Rate") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

# 計算性別分布及百分比
gender_distribution <- data_clean %>%
  group_by(gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count/sum(count) * 100,
         label = sprintf("%.1f%%", percentage))
# 繪製有百分比的圓餅圖
gender_pie <- ggplot(gender_distribution, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Gender Distribution", fill = "Gender") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink"),
                    labels = c("Male", "Female")) +
  theme_classic()
print(gender_pie)

# 計算性別與中風關係的分布及百分比
gender_stroke_distribution <- data_clean %>%
  group_by(gender, stroke) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count/sum(count) * 100,
         label = sprintf("%.1f%%", percentage))
# 繪製有百分比的甜甜圈圖
gender_donut <- ggplot(gender_stroke_distribution, 
                       aes(x = 2, y = count, fill = interaction(gender, stroke))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Gender and Stroke relationship", fill = "Gender-Stroke") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(
    values = c(
      "Female.0" = "pink", "Female.1" = "red",
      "Male.0" = "skyblue", "Male.1" = "blue"),
    labels = c("Female didn't get stroke", "Male didn't get stroke",
               "Female get stroke", "Male get stroke")) +
  xlim(0.5, 3.5)
print(gender_donut)

# BMI分布圖
p3 <- ggplot(data_clean, aes(x=bmi)) + 
  geom_histogram(bins=30, fill="lightgreen", color="black") +
  labs(title="BMI Distribution", x="BMI", y="Count") +
  theme_classic()
print(p3)

# 血糖分布圖
p4 <- ggplot(data_clean, aes(x=avg_glucose_level)) +
  geom_histogram(bins=30, fill="salmon", color="black") +
  labs(title="Glucose Level Distribution", x="Average Glucose Level", y="Count") +
  theme_classic()
print(p4)

# 3.1 風險因子相關性熱圖
# 數據準備
risk_factors_cor <- data_clean %>%
  mutate(across(c(hypertension, heart_disease, stroke), as.numeric)) %>%
  dplyr::select(age, bmi, avg_glucose_level, stroke)

# 計算相關係數矩陣
cor_matrix_all <- cor(risk_factors_cor)

# 將相關係數矩陣轉換為長格式
cor_melted <- melt(cor_matrix_all)

# 創建帶有相關係數的熱圖
heatmap_plot <- ggplot(data = cor_melted, 
                       aes(x = Var1, y = Var2, fill = value)) + 
  # 添加色塊
  geom_tile() +
  # 添加相關係數文字
  geom_text(aes(label = sprintf("%.2f", value)), 
            color = "black", 
            size = 3.5) +
  # 設定顏色漸層
  scale_fill_gradient2(low = "blue", 
                       mid = "white",
                       high = "red", 
                       midpoint = 0,
                       limits = c(-1, 1)) +
  # 設定主題和標籤
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  # 添加標題和軸標籤
  labs(title = "Correlation Heatmap of Risk Factors",
       x = "",
       y = "",
       fill = "Correlation") +
  # 確保圖形是正方形
  coord_fixed()
# 顯示熱圖
print(heatmap_plot)

# 4. 資料分割和模型準備
set.seed(1035)
split_index <- sample(1:2, nrow(data_clean), replace=TRUE, prob=c(0.7, 0.3))
train_data <- data_clean[split_index==1,]
test_data <- data_clean[split_index==2,]

# 確保因子型別正確
train_data$stroke <- as.factor(train_data$stroke)
test_data$stroke <- as.factor(test_data$stroke)
train_data$hypertension <- as.factor(train_data$hypertension)
test_data$hypertension <- as.factor(test_data$hypertension)
train_data$heart_disease <- as.factor(train_data$heart_disease)
test_data$heart_disease <- as.factor(test_data$heart_disease)

# 5. 模型訓練
# 5.1 GAM模型
gam_model <- gam(stroke ~ s(age) + s(avg_glucose_level) + s(bmi) + 
                   hypertension + heart_disease + gender,
                 family = binomial,
                 data = train_data)

# 5.2 隨機森林模型 - 修正為分類問題
rf_model <- randomForest(stroke ~ avg_glucose_level + bmi + age + 
                           hypertension + heart_disease, 
                         data = train_data,
                         ntree = 500,
                         type = "classification")

# 5.3 XGBoost模型
# 準備XGBoost資料
train_matrix <- model.matrix(~ -1 + avg_glucose_level + bmi + age + 
                               hypertension + heart_disease, data = train_data)
test_matrix <- model.matrix(~ -1 + avg_glucose_level + bmi + age + 
                              hypertension + heart_disease, data = test_data)

# 正確處理標籤
xgb_train <- xgb.DMatrix(train_matrix, 
                         label = as.numeric(as.character(train_data$stroke)))
xgb_test <- xgb.DMatrix(test_matrix, 
                        label = as.numeric(as.character(test_data$stroke)))

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

# 6. 模型評估
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

# 6.1 擴充模型評估指標
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

# 7. 分層分析和視覺化
# 首先創建年齡組別
age_group_analysis <- data_clean %>%
  # 先創建年齡組別
  mutate(age_group = cut(age, 
                         breaks = seq(0, max(age, na.rm = TRUE) + 10, by = 10),
                         labels = paste(seq(0, max(age, na.rm = TRUE), by = 10), 
                                        seq(10, max(age, na.rm = TRUE) + 10, by = 10),
                                        sep = "-"))) %>%
  # 然後進行分組統計
  group_by(age_group) %>%
  summarize(
    stroke_rate = mean(as.numeric(as.character(stroke))),
    avg_glucose = mean(avg_glucose_level),
    hypertension_rate = mean(as.numeric(as.character(hypertension))),
    n = n()
  )

# 檢查結果
print("Age group analysis results:")
print(age_group_analysis)

# 最終視覺化
p_final <- ggplot(age_group_analysis) +
  # 中風率長條圖
  geom_bar(aes(x = age_group, y = stroke_rate), 
           stat = "identity", 
           fill = "skyblue", 
           alpha = 0.7) +
  # 高血壓率折線圖
  geom_line(aes(x = age_group, y = hypertension_rate, group = 1), 
            color = "red", 
            size = 1) +
  # 高血壓率數據點
  geom_point(aes(x = age_group, y = hypertension_rate), 
             color = "red", 
             size = 3) +
  # 設定y軸標題
  scale_y_continuous(name = "Rate",
                     labels = scales::percent_format(accuracy = 1)) +
  # 圖表標題和軸標籤
  labs(title = "Stroke and Hypertension Rates by Age Group",
       x = "Age Group",
       y = "Rate") +
  # 主題設定
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
# 顯示圖表
print(p_final)

# 7.1 擴充年齡層分析
age_risk_analysis <- data_clean %>%
  # 先創建年齡組別
  mutate(age_group = cut(age, 
                         breaks = seq(0, max(age, na.rm = TRUE) + 10, by = 10),
                         labels = paste(seq(0, max(age, na.rm = TRUE), by = 10), 
                                        seq(10, max(age, na.rm = TRUE) + 10, by = 10),
                                        sep = "-"))) %>%
  # 進行分組統計
  group_by(age_group) %>%
  summarise(
    stroke_rate = mean(as.numeric(as.character(stroke))),
    hypertension_rate = mean(as.numeric(as.character(hypertension))),
    heart_disease_rate = mean(as.numeric(as.character(heart_disease))),
    avg_glucose = mean(avg_glucose_level),
    avg_bmi = mean(bmi),
    n_samples = n()
  )

# 檢查結果
print("Age risk analysis results:")
print(age_risk_analysis)

# 視覺化年齡層風險特徵
p_age_risk <- ggplot(age_risk_analysis) +
  # 中風率折線
  geom_line(aes(x = age_group, y = stroke_rate, group = 1, 
                color = "Stroke Rate"), size = 1) +
  geom_point(aes(x = age_group, y = stroke_rate, 
                 color = "Stroke Rate"), size = 3) +
  
  # 高血壓率折線
  geom_line(aes(x = age_group, y = hypertension_rate, group = 1, 
                color = "Hypertension Rate"), size = 1) +
  geom_point(aes(x = age_group, y = hypertension_rate, 
                 color = "Hypertension Rate"), size = 3) +
  
  # 心臟病率折線
  geom_line(aes(x = age_group, y = heart_disease_rate, group = 1, 
                color = "Heart Disease Rate"), size = 1) +
  geom_point(aes(x = age_group, y = heart_disease_rate, 
                 color = "Heart Disease Rate"), size = 3) +
  
  # 顏色設定
  scale_color_manual(values = c("Stroke Rate" = "red",
                                "Hypertension Rate" = "blue",
                                "Heart Disease Rate" = "green")) +
  
  # 座標軸和標題設定
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Risk Factors by Age Group",
       x = "Age Group",
       y = "Rate",
       color = "Risk Factor") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# 顯示圖表
print(p_age_risk)

# 創建平均血糖和BMI的趨勢圖
p_metrics <- ggplot(age_risk_analysis) +
  # 血糖值折線
  geom_line(aes(x = age_group, y = avg_glucose, group = 1, 
                color = "Average Glucose"), size = 1) +
  geom_point(aes(x = age_group, y = avg_glucose, 
                 color = "Average Glucose"), size = 3) +
  
  # BMI折線
  geom_line(aes(x = age_group, y = avg_bmi, group = 1, 
                color = "Average BMI"), size = 1) +
  geom_point(aes(x = age_group, y = avg_bmi, 
                 color = "Average BMI"), size = 3) +
  
  # 雙Y軸設定
  scale_y_continuous(
    name = "Average Glucose Level",
    sec.axis = sec_axis(~./3, name = "Average BMI")
  ) +
  
  # 顏色設定
  scale_color_manual(values = c("Average Glucose" = "purple",
                                "Average BMI" = "orange")) +
  
  # 標題和標籤
  labs(title = "Average Glucose Level and BMI by Age Group",
       x = "Age Group",
       color = "Metric") +
  
  # 主題設定
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95")
  )

# 顯示第二個圖表
print(p_metrics)

# 7.2 風險因子組合分析
risk_combination_analysis <- data_clean %>%
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

# 7.3 年齡和風險因子交互作用
age_risk_interaction <- data_clean %>%
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

# 8. 特徵重要性分析
# 8.1 隨機森林特徵重要性
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

# 9. 模型選擇準則
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

# 9.1 預測結果視覺化
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

# 載入必要的套件
library(car)
library(dplyr)


# 將類別變數轉換為因子型態
data_clean$gender <- as.factor(data_clean$gender)
data_clean$hypertension <- as.factor(data_clean$hypertension)
data_clean$heart_disease <- as.factor(data_clean$heart_disease)
data_clean$ever_married <- as.factor(data_clean$ever_married)
data_clean$work_type <- as.factor(data_clean$work_type)
data_clean$Residence_type <- as.factor(data_clean$Residence_type)
data_clean$smoking_status <- as.factor(data_clean$smoking_status)
data_clean$stroke <- as.factor(data_clean$stroke)

# 建立線性模型用於VIF分析
# 選擇數值型變數進行VIF分析
numeric_model <- lm(avg_glucose_level ~ age + bmi, data = data)
vif_results <- vif(numeric_model)

# 針對類別變數，建立完整模型
full_model <- glm(stroke ~ age + avg_glucose_level + bmi + 
                    hypertension + heart_disease + ever_married + 
                    work_type + Residence_type + smoking_status,
                  family = binomial(link = "logit"),
                  data = data)

# 計算類別變數的VIF
categorical_vif <- vif(full_model)

# 輸出結果
print("數值變數VIF結果:")
print(vif_results)
print("\n類別變數包含的完整模型VIF結果:")
print(categorical_vif)


# 檢查資料是否正確讀取
str(data_clean)

# 將類別變數轉換為因子型態
data_clean$gender <- as.factor(data_clean$gender)
data_clean$hypertension <- as.factor(data_clean$hypertension)
data_clean$heart_disease <- as.factor(data_clean$heart_disease)
data_clean$ever_married <- as.factor(data_clean$ever_married)
data_clean$work_type <- as.factor(data_clean$work_type)
data_clean$Residence_type <- as.factor(data_clean$Residence_type)
data_clean$smoking_status <- as.factor(data_clean$smoking_status)
data_clean$stroke <- as.factor(data_clean$stroke)

# 檢查數值變數的VIF
numeric_vars <- data_clean %>% 
  select(age, avg_glucose_level, bmi)
numeric_model <- lm(avg_glucose_level ~ age + bmi, data = numeric_vars)
vif_numeric <- vif(numeric_model)

# 建立完整模型（排除不必要的變數）
full_model <- glm(stroke ~ age + avg_glucose_level + bmi + 
                    hypertension + heart_disease + ever_married + 
                    work_type + Residence_type + smoking_status,
                  family = binomial(link = "logit"),
                  data = data_clean)

# 計算完整模型的VIF
vif_full <- vif(full_model)

# 輸出結果
cat("數值變數VIF結果:\n")
print(vif_numeric)
cat("\n完整模型VIF結果:\n")
print(vif_full)

# 基於VIF結果的變數選擇建議
cat("\n變數選擇建議：\n")
high_vif <- names(which(vif_full > 5))  # VIF > 5 視為較高的共線性
if(length(high_vif) > 0) {
  cat("建議考慮移除或重新處理以下變數（VIF > 5）：\n")
  print(high_vif)
} else {
  cat("所有變數的VIF值都在可接受範圍內（< 5）\n")
}