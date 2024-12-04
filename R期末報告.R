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

print(p1)
print(p2)

# 相關性分析
numeric_vars <- balanced_data[,c("age", "avg_glucose_level", "bmi")]
cor_matrix <- cor(numeric_vars, use="complete.obs")
corrplot(cor_matrix, method="color")

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