install.packages("Epi")
install.packages("ResourceSelection")
# 載入所需套件
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(caret)
library(MASS)
library(Epi)
library(ResourceSelection)

# 讀取資料
data_stroke <- read.csv("/Users/tommy/Desktop/bio.csv")

# 轉換類別變數為因子
categorical_vars <- c("gender", "hypertension", "heart_disease", 
                      "ever_married", "Residence_type", "smoking_status", "stroke")
data_stroke[categorical_vars] <- lapply(data_stroke[categorical_vars], as.factor)

# 1. 探索性資料分析 ----

# 1.1 基本統計量
summary(data_stroke)

# 1.2 視覺化分析
# 年齡分布
p1 <- ggplot(data_stroke, aes(x=age)) + 
  geom_histogram(bins=30, fill="skyblue", color="black") +
  labs(title="Age Distribution", x="Age", y="Count")

# 中風與年齡關係
p2 <- ggplot(data_stroke, aes(x=stroke, y=age, fill=stroke)) + 
  geom_boxplot() +
  labs(title="Age Distribution by Stroke Status")

# 相關性分析
numeric_vars <- data_stroke[,c("age", "avg_glucose_level", "bmi")]
cor_matrix <- cor(numeric_vars, use="complete.obs")
corrplot(cor_matrix, method="color")

# 2. 資料分割 ----
set.seed(1035)
Splitdata <- function(data, p = 0.7) {
  index <- sample(1:2, nrow(data), replace = TRUE, prob = c(p, 1-p))
  train <- data[index == 1, ]
  test <- data[index == 2, ]
  return(list(train = train, test = test))
}

split_data <- Splitdata(data_stroke)
train_data <- split_data$train
test_data <- split_data$test

# 3. 模型建立與比較 ----

# 3.1 邏輯斯迴歸
glm_model <- glm(stroke ~ avg_glucose_level + hypertension + heart_disease + age,
                 data = train_data, family = binomial)
summary(glm_model)

# GLM預測與評估
glm_probs <- predict(glm_model, test_data, type="response")
glm_pred <- ifelse(glm_probs > 0.5, "1", "0")
glm_cm <- table(glm_pred, test_data$stroke)
glm_accuracy <- sum(diag(glm_cm))/sum(glm_cm)

# 3.2 線性判別分析(LDA)
lda_model <- lda(stroke ~ avg_glucose_level + hypertension + heart_disease + age,
                 data = train_data)
lda_pred <- predict(lda_model, test_data)
lda_cm <- table(lda_pred$class, test_data$stroke)
lda_accuracy <- sum(diag(lda_cm))/sum(lda_cm)

# 3.3 二次判別分析(QDA)
qda_model <- qda(stroke ~ avg_glucose_level + hypertension + heart_disease + age,
                 data = train_data)
qda_pred <- predict(qda_model, test_data)
qda_cm <- table(qda_pred$class, test_data$stroke)
qda_accuracy <- sum(diag(qda_cm))/sum(qda_cm)

# 4. 模型評估 ----

# 4.1 ROC曲線分析
roc1 <- ROC(form = stroke ~ age, data = data_stroke, plot = "ROC")
roc2 <- ROC(form = stroke ~ age + hypertension, data = data_stroke, plot = "ROC")
roc3 <- ROC(form = stroke ~ age + hypertension + avg_glucose_level, 
            data = data_stroke, plot = "ROC")
roc4 <- ROC(form = stroke ~ avg_glucose_level + hypertension + heart_disease + age, 
            data = data_stroke, plot = "ROC")

# 4.2 Hosmer-Lemeshow適合度檢定
hoslem.test(glm_model$y, fitted(glm_model))

# 5. 視覺化結果 ----

# 5.1 各變數與中風的關係
# BMI分布
p3 <- ggplot(data_stroke, aes(x=bmi, fill=stroke)) + 
  geom_density(alpha=0.5) +
  labs(title="BMI Distribution by Stroke Status")

# 血糖分布
p4 <- ggplot(data_stroke, aes(x=avg_glucose_level, fill=stroke)) + 
  geom_density(alpha=0.5) +
  labs(title="Glucose Level Distribution by Stroke Status")

# 年齡組別分析
data_stroke$age_group <- cut(data_stroke$age, 
                             breaks=c(0,20,40,60,80,100),
                             labels=c("0-20","21-40","41-60","61-80","81+"))

p5 <- ggplot(data_stroke, aes(x=age_group, fill=stroke)) + 
  geom_bar(position="fill") +
  labs(title="Stroke Proportion by Age Group",
       y="Proportion",
       x="Age Group")

# 6. 比較模型效能 ----
model_comparison <- data.frame(
  Model = c("GLM", "LDA", "QDA"),
  Accuracy = c(glm_accuracy, lda_accuracy, qda_accuracy)
)

print(model_comparison)

