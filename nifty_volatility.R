

library(rugarch)
library(quantmod)
library(ggplot2)
library(dplyr)





getSymbols("^NSEI", src="yahoo", from="2020-01-01",auto.assign = TRUE)

ret <- na.omit(diff(log(Cl(NSEI))))

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0))
)
fit <- ugarchfit(spec, data = ret)

vol_data <- data.frame(
  Date = index(ret),
  Volatility = sigma(fit)
)

ggplot(vol_data, aes(Date, Volatility)) +
  geom_line(color = "darkred", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "NIFTY Volatility Clustering (GARCH(1,1))",
    x = "Date", y = "Conditional Volatility"
  )+
  theme_minimal()


forecast <- ugarchforecast(fit, n.ahead = 30)
future_vol <- sigma(forecast)

forecast_df <- data.frame(
  Date = seq(max(index(ret)) + 1, by = "day", length.out = 30),
  ForecastVolatility = as.numeric(future_vol)
)

ggplot(forecast_df, aes(Date, ForecastVolatility)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Forecasted Volatility (Next 30 Days, GARCH(1,1))",
    x = "Date", y = "Forecasted σ_t"
  )

cat("\nGARCH(1,1) Model Summary:\n")
show(f