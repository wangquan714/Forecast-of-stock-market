# Forecast-of-stock-market
本方法使用了R语言，对中国工商银行近5年的股票走势进行了研究与拟合，并得到了较好的预测结果。
对股票数据进行了预处理之后，我尝试了四种模型进行拟合与预测。结果显示，XGBoost滚动预测版本在运行时间最短，拟合效果最佳，其对未来半年的预测均方误差最终稳定在0.2以内。因此，我仅保留该版本，注释了其余方法。

/After preprocessing the stock data (post-weighting), I tested four models for fitting and prediction. The XGBoost rolling prediction performed best, with the shortest runtime and highest accuracy. Its one-year-ahead predictions had a mean squared error consistently within 0.2. Thus, I kept only this version and commented out the others.
