library(readxl)
library(lubridate)
library(forecast)
library(ggplot2)
library(dplyr)

### 预处理 ####
# 1. 读取数据 
library(stringr)
library(dplyr)
library(lubridate)

# 读取和编码转换
lines_raw <- readLines("E:/new_tdx/T0002/export/601398.xls", encoding = "raw")
lines <- iconv(lines_raw, from = "GB18030", to = "UTF-8")

# 解析列名，去除空白
col_names <- strsplit(lines[2], "\t")[[1]] %>% str_trim()

# 去除列名中的NA
col_names <- col_names[!is.na(col_names)]

# 解析数据行
data_text <- paste(lines[-c(1,2)], collapse = "\n")

df <- read.table(text = data_text,
                 sep = "\t",
                 header = FALSE,
                 stringsAsFactors = FALSE,
                 fill = TRUE,
                 strip.white = TRUE)

# 数据列数可能比列名多，截取匹配长度
df <- df[, 1:length(col_names)]

# 赋列名
colnames(df) <- col_names

# 重命名关键列并转换数据类型
df <- df %>%
  rename(
    Date = 时间,
    Open = 开盘,
    High = 最高,
    Low = 最低,
    Close = 收盘,
    Volume = 成交量
  ) %>%
  mutate(
    Date = ymd(Date),
    Open = as.numeric(Open),
    High = as.numeric(High),
    Low = as.numeric(Low),
    Close = as.numeric(Close),
    Volume = as.numeric(Volume)
  ) %>%
  filter(!is.na(Date), !is.na(Close)) %>%
  arrange(Date)

head(df)

# 
# 
# library(dplyr)
# library(forecast)
# library(ggplot2)
# 
# # 1. 设置时间范围和分割点
# start_date <- as.Date("2023-08-23")
# train_end <- as.Date("2025-01-01")
# 
# # 2. 划分数据集，使用 between() 简化逻辑
# train_data <- df %>% filter(between(Date, start_date, train_end))
# test_data <- df %>% filter(Date > train_end)
# 
# # 3. 处理缺失值（示例：简单删除，或用插值等）
# train_data <- train_data %>% filter(!is.na(Close))
# 
# # 4. 创建时间序列对象
# # 频率建议根据实际交易日历调整，如5(周交易日)或252(年交易日)
# ts_train <- ts(train_data$Close, frequency = 5)  
# 
# # 5. 模型拟合，支持并行（需要 doParallel 等包，这里示范普通版本）
# fit_arima <- auto.arima(ts_train)
# fit_ets <- ets(ts_train)
# 
# # 6. 预测未来h天（测试集长度）
# h <- nrow(test_data)
# forecast_arima <- forecast(fit_arima, h = h)
# forecast_ets <- forecast(fit_ets, h = h)
# 
# # 7. 准备数据框，计算误差指标
# df_forecast <- test_data %>%
#   select(Date, Actual = Close) %>%
#   mutate(
#     ARIMA_Pred = as.numeric(forecast_arima$mean),
#     ETS_Pred = as.numeric(forecast_ets$mean),
#     ARIMA_Error = abs(Actual - ARIMA_Pred),
#     ETS_Error = abs(Actual - ETS_Pred)
#   )
# 
# mae_arima <- mean(df_forecast$ARIMA_Error, na.rm = TRUE)
# mae_ets <- mean(df_forecast$ETS_Error, na.rm = TRUE)
# 
# cat(sprintf("ARIMA MAE: %.4f\nETS MAE: %.4f\n", mae_arima, mae_ets))
# 
# # 8. 可视化
# ggplot(df_forecast, aes(x = Date)) +
#   geom_line(aes(y = Actual, color = "实际值"), size = 1.2) +
#   geom_line(aes(y = ARIMA_Pred, color = "ARIMA预测"), linetype = "dashed") +
#   geom_line(aes(y = ETS_Pred, color = "ETS预测"), linetype = "dotted") +
#   labs(title = "收盘价预测 vs 实际值",
#        x = "日期", y = "收盘价") +
#   scale_color_manual(values = c("实际值" = "black", "ARIMA预测" = "blue", "ETS预测" = "red")) +
#   theme_minimal() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.title = element_blank())
# 
# #9.优化
# library(forecast)
# 
# # 训练集对数收盘价
# log_train <- log(train_data$Close)
# 
# # 差分
# log_train_diff <- diff(log_train)
# ts_train_diff <- ts(log_train_diff, frequency = 5)
# 
# # 拟合差分序列ARIMA
# fit_arima_diff <- auto.arima(ts_train_diff)
# 
# # 预测
# forecast_diff <- forecast(fit_arima_diff, h = nrow(test_data))
# 
# # 累计还原预测结果
# last_log_val <- tail(log_train, 1)
# pred_log_vals <- last_log_val + cumsum(forecast_diff$mean)
# pred_prices <- exp(pred_log_vals)
# 
# # 结果对比
# df_forecast <- test_data %>%
#   mutate(
#     Predicted = pred_prices
#   )
# 
# # 可视化
# library(ggplot2)
# ggplot(df_forecast, aes(x = Date)) +
#   geom_line(aes(y = Close), color = "black", size = 1.2) +
#   geom_line(aes(y = Predicted), color = "blue", linetype = "dashed") +
#   labs(title = "基于差分ARIMA的收盘价预测", y = "收盘价", x = "日期") +
#   theme_minimal()
# 
# #机器学习
# library(dplyr)
# library(TTR)
# library(ggplot2)
# 
# # 假设 df 已经包含 Close 列和 Date 列
# 
# # 计算 MACD，返回的是一个矩阵
# macd_result <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = EMA)
# 
# # 将 MACD 和信号线加入数据框
# df <- df %>%
#   mutate(
#     MACD = macd_result[, "macd"],
#     Signal = macd_result[, "signal"],
#     Histogram = MACD - Signal
#   )
# 
# # 查看前几行，确认计算结果
# head(df)
# 
# # 绘图：收盘价和 MACD 指标
# p1 <- ggplot(df, aes(x = Date)) +
#   geom_line(aes(y = Close), color = "black") +
#   labs(title = "收盘价", y = "价格")
# 
# p2 <- ggplot(df, aes(x = Date)) +
#   geom_line(aes(y = MACD), color = "blue") +
#   geom_line(aes(y = Signal), color = "red") +
#   geom_bar(aes(y = Histogram), stat = "identity", fill = "grey", alpha = 0.5) +
#   labs(title = "MACD指标", y = "值")
# 
# # 显示两个图（如果你用 RStudio，两个图会分别显示）
# print(p1)
# print(p2)
# 
# 
# #优化
# 
# # 假设df是你的数据框，包含 Date 和 Close 列，且已经按时间排序
# # 这里我们用df示范
# 
# library(dplyr)
# library(TTR)          # 技术指标计算
# library(randomForest) # 随机森林
# library(ggplot2)
# library(lubridate)
# 
# # 假设df已经是你读入的原始数据框，包含 Date 和 Close 等列
# 
# # 1. 时间范围和划分训练测试集
# start_date <- as.Date("2023-08-23")
# train_end <- as.Date("2025-01-01")
# 
# train_data <- df %>% filter(between(Date, start_date, train_end))
# test_data <- df %>% filter(Date > train_end)
# 
# # 2. 计算MACD指标（示范只用Close价格）
# macd_vals <- MACD(train_data$Close, nFast=12, nSlow=26, nSig=9, maType="EMA")
# train_data <- train_data %>% 
#   mutate(MACD = macd_vals[, "macd"],
#          Signal = macd_vals[, "signal"])
# 
# # 3. 构造滞后特征（收盘价滞后1~3天）
# train_data <- train_data %>%
#   arrange(Date) %>%
#   mutate(
#     Close_lag1 = lag(Close, 1),
#     Close_lag2 = lag(Close, 2),
#     Close_lag3 = lag(Close, 3)
#   )
# 
# # 4. 同理给测试集加特征。测试集需要用合适的方法计算MACD和滞后（一般用全量数据计算，这里简单示范）
# macd_test <- MACD(test_data$Close, nFast=12, nSlow=26, nSig=9, maType="EMA")
# test_data <- test_data %>%
#   mutate(MACD = macd_test[, "macd"],
#          Signal = macd_test[, "signal"]) %>%
#   arrange(Date) %>%
#   mutate(
#     Close_lag1 = lag(Close, 1),
#     Close_lag2 = lag(Close, 2),
#     Close_lag3 = lag(Close, 3)
#   )
# 
# # 5. 去除缺失值（随机森林不支持含NA的训练和预测）
# train_data <- train_data %>% 
#   filter(!is.na(Close), !is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
#          !is.na(MACD), !is.na(Signal))
# 
# test_data <- test_data %>% 
#   filter(!is.na(Close), !is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
#          !is.na(MACD), !is.na(Signal))
# 
# # 6. 训练随机森林回归模型，预测Close价格
# rf_model <- randomForest(
#   Close ~ Close_lag1 + Close_lag2 + Close_lag3 + MACD + Signal,
#   data = train_data,
#   ntree = 500
# )
# 
# # 7. 预测测试集
# test_data <- test_data %>%
#   mutate(Predicted = predict(rf_model, newdata = test_data))
# 
# # 8. 计算误差
# test_data <- test_data %>%
#   mutate(Error = abs(Close - Predicted))
# 
# mae_rf <- mean(test_data$Error, na.rm = TRUE)
# cat(sprintf("随机森林预测MAE: %.4f\n", mae_rf))
# 
# # 9. 可视化实际 vs 预测收盘价
# ggplot(test_data, aes(x = Date)) +
#   geom_line(aes(y = Close, color = "实际收盘价"), size = 1.2) +
#   geom_line(aes(y = Predicted, color = "随机森林预测"), linetype = "dashed") +
#   labs(title = "随机森林模型收盘价预测对比", y = "收盘价", x = "日期") +
#   scale_color_manual(values = c("实际收盘价" = "black", "随机森林预测" = "blue")) +
#   theme_minimal() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
# 
# #另一种
# library(dplyr)
# library(TTR)          # 技术指标计算
# library(randomForest) # 随机森林
# library(ggplot2)
# library(lubridate)
# 
# # 假设df已经是你读入的原始数据框，包含 Date 和 Close 等列
# 
# # 1. 时间范围和划分训练测试集
# start_date <- as.Date("2023-08-23")
# train_end <- as.Date("2025-01-01")
# 
# train_data <- df %>% filter(between(Date, start_date, train_end))
# test_data <- df %>% filter(Date > train_end)
# 
# # 2. 计算MACD指标（示范只用Close价格）
# macd_vals <- MACD(train_data$Close, nFast=12, nSlow=26, nSig=9, maType="EMA")
# train_data <- train_data %>% 
#   mutate(MACD = macd_vals[, "macd"],
#          Signal = macd_vals[, "signal"])
# 
# # 3. 构造滞后特征（收盘价滞后1~3天）
# train_data <- train_data %>%
#   arrange(Date) %>%
#   mutate(
#     Close_lag1 = lag(Close, 1),
#     Close_lag2 = lag(Close, 2),
#     Close_lag3 = lag(Close, 3)
#   )
# 
# # 4. 同理给测试集加特征。测试集需要用合适的方法计算MACD和滞后（一般用全量数据计算，这里简单示范）
# macd_test <- MACD(test_data$Close, nFast=12, nSlow=26, nSig=9, maType="EMA")
# test_data <- test_data %>%
#   mutate(MACD = macd_test[, "macd"],
#          Signal = macd_test[, "signal"]) %>%
#   arrange(Date) %>%
#   mutate(
#     Close_lag1 = lag(Close, 1),
#     Close_lag2 = lag(Close, 2),
#     Close_lag3 = lag(Close, 3)
#   )
# 
# # 5. 去除缺失值（随机森林不支持含NA的训练和预测）
# train_data <- train_data %>% 
#   filter(!is.na(Close), !is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
#          !is.na(MACD), !is.na(Signal))
# 
# test_data <- test_data %>% 
#   filter(!is.na(Close), !is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
#          !is.na(MACD), !is.na(Signal))
# 
# # 6. 训练随机森林回归模型，预测Close价格
# rf_model <- randomForest(
#   Close ~ Close_lag1 + Close_lag2 + Close_lag3 + MACD + Signal,
#   data = train_data,
#   ntree = 500
# )
# 
# # 7. 预测测试集
# test_data <- test_data %>%
#   mutate(Predicted = predict(rf_model, newdata = test_data))
# 
# # 8. 计算误差
# test_data <- test_data %>%
#   mutate(Error = abs(Close - Predicted))
# 
# mae_rf <- mean(test_data$Error, na.rm = TRUE)
# cat(sprintf("随机森林预测MAE: %.4f\n", mae_rf))
# 
# # 9. 可视化实际 vs 预测收盘价
# ggplot(test_data, aes(x = Date)) +
#   geom_line(aes(y = Close, color = "实际收盘价"), size = 1.2) +
#   geom_line(aes(y = Predicted, color = "随机森林预测"), linetype = "dashed") +
#   labs(title = "随机森林模型收盘价预测对比", y = "收盘价", x = "日期") +
#   scale_color_manual(values = c("实际收盘价" = "black", "随机森林预测" = "blue")) +
#   theme_minimal() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())


### XGBoost滚动预测版本 ####
library(dplyr)
library(TTR)
library(xgboost)
library(ggplot2)
library(lubridate)

# 假设df已经包含 Date 和 Close 列，且按日期升序排列

# 1. 计算全量MACD指标
macd_all <- MACD(df$Close, nFast=12, nSlow=26, nSig=9, maType="EMA")
df <- df %>%
  mutate(
    MACD = macd_all[, "macd"],
    Signal = macd_all[, "signal"]
  )

# 2. 生成滞后特征（Close_lag1, lag2, lag3）
df <- df %>%
  arrange(Date) %>%
  mutate(
    Close_lag1 = lag(Close, 1),
    Close_lag2 = lag(Close, 2),
    Close_lag3 = lag(Close, 3)
  )

# 3. 设置训练和测试日期范围
train_end <- as.Date("2025-01-01")
test_data <- df %>% filter(Date > train_end)

# 4. 滚动预测准备
preds <- numeric(nrow(test_data))
dates <- test_data$Date

# 5. 滚动预测循环
for(i in seq_len(nrow(test_data))) {
  current_date <- dates[i]
  
  # 训练数据截止到预测日前一天
  train_set <- df %>%
    filter(Date < current_date) %>%
    filter(!is.na(Close), !is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
           !is.na(MACD), !is.na(Signal))
  
  # 预测日数据（测试点）
  test_point <- df %>%
    filter(Date == current_date) %>%
    filter(!is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
           !is.na(MACD), !is.na(Signal))
  
  if(nrow(test_point) == 0 || nrow(train_set) < 20) {  # XGBoost稍微多点训练样本
    preds[i] <- NA
    next
  }
  
  # 准备训练矩阵
  train_matrix <- xgb.DMatrix(
    data = as.matrix(train_set %>% select(Close_lag1, Close_lag2, Close_lag3, MACD, Signal)),
    label = train_set$Close
  )
  
  # 准备测试矩阵
  test_matrix <- xgb.DMatrix(
    data = as.matrix(test_point %>% select(Close_lag1, Close_lag2, Close_lag3, MACD, Signal))
  )
  
  # 参数设置（可根据需求调参）
  params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 4,
    subsample = 0.8,
    colsample_bytree = 0.8,
    nthread = 2
  )
  
  # 训练模型
  xgb_model <- xgb.train(
    params = params,
    data = train_matrix,
    nrounds = 100,
    verbose = 0
  )
  
  # 预测
  preds[i] <- predict(xgb_model, test_matrix)
}

# 6. 合并预测结果
results <- test_data %>%
  mutate(Predicted = preds) %>%
  filter(!is.na(Predicted))

# 7. 计算MAE
mae <- mean(abs(results$Close - results$Predicted), na.rm = TRUE)
cat(sprintf("XGBoost滚动预测 MAE: %.4f\n", mae))

# 8. 绘图对比预测与实际
ggplot(results, aes(x = Date)) +
  geom_line(aes(y = Close, color = "实际收盘价"), size = 1.2) +
  geom_line(aes(y = Predicted, color = "XGBoost滚动预测"), linetype = "dashed") +
  labs(title = "XGBoost滚动预测: 收盘价实际值与预测对比", y = "收盘价", x = "日期") +
  scale_color_manual(values = c("实际收盘价" = "black", "XGBoost滚动预测" = "blue")) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

library(Metrics)  # 如果没有安装，先install.packages("Metrics")

# 1. 计算更多误差指标
mae <- mean(abs(results$Close - results$Predicted), na.rm = TRUE)
rmse <- sqrt(mean((results$Close - results$Predicted)^2, na.rm = TRUE))
mape <- mean(abs((results$Close - results$Predicted) / results$Close), na.rm = TRUE) * 100

# 计算R平方
SSE <- sum((results$Close - results$Predicted)^2, na.rm = TRUE)
SST <- sum((results$Close - mean(results$Close))^2, na.rm = TRUE)
r_squared <- 1 - SSE/SST

cat(sprintf("XGBoost滚动预测误差分析：\nMAE = %.4f\nRMSE = %.4f\nMAPE = %.2f%%\nR² = %.4f\n", 
            mae, rmse, mape, r_squared))

# 2. 绘制误差随时间变化（残差图）
results <- results %>%
  mutate(Residual = Close - Predicted)

ggplot(results, aes(x = Date, y = Residual)) +
  geom_line(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "XGBoost滚动预测残差随时间变化", y = "残差 (实际值 - 预测值)", x = "日期") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. 绘制误差分布直方图
ggplot(results, aes(x = Residual)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "XGBoost滚动预测残差分布", x = "残差 (实际值 - 预测值)", y = "频数") +
  theme_minimal()

# 实际值 vs 预测值 散点图及拟合线
ggplot(results, aes(x = Close, y = Predicted)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "实际收盘价与XGBoost预测值散点图",
       x = "实际收盘价",
       y = "预测收盘价") +
  theme_minimal()

### 自动调参 ####
# library(dplyr)
# library(TTR)
# library(xgboost)
# library(caret)
# library(ggplot2)
# library(lubridate)
# 
# # 1. 计算全量MACD指标
# macd_all <- MACD(df$Close, nFast=12, nSlow=26, nSig=9, maType="EMA")
# df <- df %>%
#   mutate(
#     MACD = macd_all[, "macd"],
#     Signal = macd_all[, "signal"]
#   ) %>%
#   arrange(Date) %>%
#   mutate(
#     Close_lag1 = lag(Close, 1),
#     Close_lag2 = lag(Close, 2),
#     Close_lag3 = lag(Close, 3)
#   )
# 
# # 2. 定义训练结束时间，划分测试集
# train_end <- as.Date("2025-01-01")
# test_data <- df %>% filter(Date > train_end)
# 
# # 3. 自动调参函数，传入训练集特征和标签，返回最优参数
# xgb_tune <- function(train_features, train_labels) {
#   # caret包要求的数据格式
#   train_df <- data.frame(train_features)
#   train_df$label <- train_labels
#   
#   # 控制参数，使用5折交叉验证
#   fitControl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
#   
#   # 调参网格
#   tuneGrid <- expand.grid(
#     nrounds = c(50, 100, 150),
#     max_depth = c(3, 5, 7),
#     eta = c(0.01, 0.1, 0.3),
#     gamma = 0,
#     colsample_bytree = c(0.7, 0.8, 1),
#     min_child_weight = 1,
#     subsample = c(0.7, 0.8, 1)
#   )
#   
#   set.seed(123)
#   xgb_train <- train(
#     label ~ .,
#     data = train_df,
#     method = "xgbTree",
#     trControl = fitControl,
#     tuneGrid = tuneGrid,
#     verbose = FALSE,
#     metric = "RMSE"
#   )
#   
#   return(xgb_train$bestTune)
# }
# 
# # 4. 滚动预测
# preds <- numeric(nrow(test_data))
# dates <- test_data$Date
# 
# for(i in seq_len(nrow(test_data))) {
#   current_date <- dates[i]
#   
#   # 滚动训练集
#   train_set <- df %>%
#     filter(Date < current_date) %>%
#     filter(!is.na(Close), !is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
#            !is.na(MACD), !is.na(Signal))
#   
#   test_point <- df %>%
#     filter(Date == current_date) %>%
#     filter(!is.na(Close_lag1), !is.na(Close_lag2), !is.na(Close_lag3),
#            !is.na(MACD), !is.na(Signal))
#   
#   if(nrow(test_point) == 0 || nrow(train_set) < 50) {
#     preds[i] <- NA
#     next
#   }
#   
#   train_features <- train_set %>% select(Close_lag1, Close_lag2, Close_lag3, MACD, Signal)
#   train_labels <- train_set$Close
#   
#   # 自动调参（注意调参过程耗时）
#   best_params <- xgb_tune(train_features, train_labels)
#   
#   # 训练最终模型
#   dtrain <- xgb.DMatrix(data = as.matrix(train_features), label = train_labels)
#   dtest <- xgb.DMatrix(data = as.matrix(test_point %>% select(Close_lag1, Close_lag2, Close_lag3, MACD, Signal)))
#   
#   final_model <- xgb.train(
#     params = list(
#       objective = "reg:squarederror",
#       eta = best_params$eta,
#       max_depth = best_params$max_depth,
#       subsample = best_params$subsample,
#       colsample_bytree = best_params$colsample_bytree,
#       min_child_weight = best_params$min_child_weight,
#       gamma = best_params$gamma,
#       nthread = 2
#     ),
#     data = dtrain,
#     nrounds = best_params$nrounds,
#     verbose = 0
#   )
#   
#   preds[i] <- predict(final_model, dtest)
# }
# 
# # 5. 结果合并与评估
# results <- test_data %>%
#   mutate(Predicted = preds) %>%
#   filter(!is.na(Predicted))
# 
# mae <- mean(abs(results$Close - results$Predicted))
# cat(sprintf("XGBoost滚动预测自动调参 MAE: %.4f\n", mae))
# 
# # 6. 绘图
# ggplot(results, aes(x = Date)) +
#   geom_line(aes(y = Close, color = "实际收盘价"), size = 1.2) +
#   geom_line(aes(y = Predicted, color = "XGBoost滚动预测（自动调参）"), linetype = "dashed") +
#   labs(title = "XGBoost滚动预测（自动调参）: 收盘价实际值与预测对比", y = "收盘价", x = "日期") +
#   scale_color_manual(values = c("实际收盘价" = "black", "XGBoost滚动预测（自动调参）" = "blue")) +
#   theme_minimal() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
# 
# # 7. 误差分析
# 
# # 计算误差 Residuals = 实际值 - 预测值
# results <- results %>%
#   mutate(Residual = Close - Predicted)
# 
# # 误差统计描述
# residual_stats <- results %>%
#   summarise(
#     Mean_Error = mean(Residual),
#     SD_Error = sd(Residual),
#     Max_Error = max(Residual),
#     Min_Error = min(Residual),
#     MAE = mean(abs(Residual)),
#     RMSE = sqrt(mean(Residual^2))
#   )
# print(residual_stats)
# 
# # 误差随时间变化图
# ggplot(results, aes(x = Date, y = Residual)) +
#   geom_line(color = "red") +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(title = "预测误差随时间变化", x = "日期", y = "误差（实际 - 预测）") +
#   theme_minimal() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # 误差密度图
# ggplot(results, aes(x = Residual)) +
#   geom_density(fill = "lightblue", alpha = 0.6) +
#   labs(title = "预测误差分布密度", x = "误差（实际 - 预测）", y = "密度") +
#   theme_minimal()
# 
# # 误差箱线图
# ggplot(results, aes(y = Residual)) +
#   geom_boxplot(fill = "orange", alpha = 0.6) +
#   labs(title = "预测误差箱线图", y = "误差（实际 - 预测）") +
#   theme_minimal()

