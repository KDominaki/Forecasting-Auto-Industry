
# ---- Data ----

years <- 2005:2022
demand <- c(334,343,344,325,340,337,343,326,332,339,336,332,324,337,328,334,345,346)
df <- data.frame(year = years, demand = demand)

ts_data <- ts(df$demand, start=2005, frequency=1)



# ---- Plots ----

plot(ts_data, main="UK Ford Focus Revenue (2005-2022)",
     lwd = 2, col = "blue", ylab="Revenue (£Ms)", xlab="Year")

ggplot(df, aes(x=year, y=demand)) +
  geom_line() + geom_smooth(method="lm", se=FALSE) +
  labs(title="Trend line for revenue")



# ---- Stationarity tests ----

adf.test(ts_data) #-> >0.05
kpss.test(ts_data) #-> >0.05



# ---- Differencing ----

ts_diff1 <- diff(ts_data, differences=1)

adf.test(ts_diff1) #-> <0.05
kpss.test(ts_diff1) #-> >0.05

plot(ts_diff1, main="TS after differencing")



# ---- ACF/PACF ----

Acf(ts_data); Pacf(ts_data) # -> before dif
Acf(ts_diff1); Pacf(ts_diff1) # -> after dif



# ---- Auto ARIMA ----

model_auto <- auto.arima(ts_data, ic="aicc", seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
summary(model_auto)



# ---- Forecast 4 years ----

fc <- forecast(model_auto, h=4, level=c(80,95))
print(fc)



# ---- Diagnostics ----

#Residuals check
checkresiduals(model_auto)
Box.test(resid(model_auto), lag=10, type="Ljung-Box") # p-value = 0.6095

#Residual QQ plot for greater detail
residuals_arima <- residuals(model_auto)

qqnorm(residuals_arima, main = "Residuals QQ-Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(residuals_arima, col="red")



# ---- Train/Test Split ----

#Split data: use 2005–2018 as training, 2019–2022 as test
train <- window(ts_data, end = c(2018))
test  <- window(ts_data, start = c(2019))



# ---- Fit Models on Training Data ----

# 1. ARIMA
model_train <- auto.arima(train, ic = "aicc", seasonal = FALSE,
                          stepwise = FALSE, approximation = FALSE)
fc_train <- forecast(model_train, h = length(test))

# 2. Naive Forecast
naive_train <- naive(train, h = length(test))

# 3. ETS Model
ets_train <- ets(train)
ets_fc_train <- forecast(ets_train, h = length(test))



# ---- Compare Accuracy on the SAME test set ----

accuracy(fc_train, test)      # ARIMA vs Actual test
accuracy(naive_train, test)   # Naive vs Actual test
accuracy(ets_fc_train, test)  # ETS vs Actual test







