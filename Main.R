
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

plot(ts_diff1, main="First diff")


# ---- ACF/PACF ----

Acf(ts_data); Pacf(ts_data) # -> before dif
Acf(ts_diff1); Pacf(ts_diff1) # -> after dif


# ---- Auto ARIMA (AICc preferred) ----

model_auto <- auto.arima(ts_data, ic="aicc", seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
summary(model_auto)


# 6. Forecast 4 years
fc <- forecast(model_auto, h=4, level=c(80,95))
print(fc)
plot(fc)

# 7. Diagnostics
checkresiduals(model_auto)
Box.test(resid(model_auto), lag=10, type="Ljung-Box")

# 8. tsCV
e <- tsCV(ts_data, function(x,h) forecast(auto.arima(x, ic="aicc", seasonal=FALSE), h=h)$mean, h=1)
sqrt(mean(e^2, na.rm=TRUE))
mean(abs(e/ts_data)[-1], na.rm=TRUE)*100

# 9. Benchmarks
naive_fc <- naive(ts_data, h=4); mean_fc <- meanf(ts_data, h=4); drift_fc <- rwf(ts_data, h=4, drift=TRUE)
accuracy(fc); accuracy(naive_fc)