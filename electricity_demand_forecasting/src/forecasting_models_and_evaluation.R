
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "zoo", "forecast","urca", "tseries"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)


"""# mannually conmpute parameters for SARIMA"""

cat("Notes: p from PACF initial cutoff; q from ACF initial cutoff; P/Q from spike at lag 7 multiples.\n")
for(resp in all6){
  cat("\n=============================\n")
  cat("Respondent:", resp, "\n")
  cat("=============================\n")
  ts_proc <- diff_list[[resp]]
  n <- length(ts_proc)
  sig_level <- 1.96 / sqrt(n)

  acf_obj <- acf(ts_proc, plot = FALSE, lag.max = 28)
  pacf_obj <- pacf(ts_proc, plot = FALSE, lag.max = 28)

  acf_vals <- as.vector(acf_obj$acf[-1])
  pacf_vals <- as.vector(pacf_obj$acf)

  find_cutoff <- function(vals, sig){
    sig_lags <- which(abs(vals) > sig)
    if(length(sig_lags) == 0) return(0)
    r <- rle(abs(vals) > sig)
    if(r$values[1]) {
      return(r$lengths[1]) }
      else { return(0) } }
#p (from PACF), q (from ACF)
p_hat <- find_cutoff(pacf_vals, sig_level)
q_hat <- find_cutoff(acf_vals, sig_level)

seasonal_lags <- c(7,14,21)
seasonal_acf_spikes <- sapply(seasonal_lags, function(L) {
  if(L <= length(acf_vals)) return(abs(acf_vals[L]) > sig_level) else
  return(FALSE) })
  seasonal_pacf_spikes <- sapply(seasonal_lags, function(L) {
    if(L <= length(pacf_vals)) return(abs(pacf_vals[L]) > sig_level) else return(FALSE) })
    P_hat <- if(any(seasonal_pacf_spikes)) 1 else 0
    Q_hat <- if(any(seasonal_acf_spikes)) 1 else 0

    d_hat <- order_list[[resp]]
    D_hat <- 0
    sugg <- list(p = p_hat, d = d_hat, q = q_hat, P = P_hat, D = D_hat, Q = Q_hat, s = 7)
    suggestions[[resp]] <- sugg
    cat("Suggested orders:", paste0("(", p_hat, ",", d_hat, ",", q_hat, ")(", P_hat, ",", D_hat, ",", Q_hat, ")[7]"), "\n") }

"""# SARIMA"""

H <- 12 * 7

for(resp in all6){
  cat("\n======================\n", resp, "\n======================\n")

  # ---- Use stationary/differenced series ----
  ts_resp <- ts(diff_list[[resp]])  # ensure proper ts object
  par <- suggestions[[resp]]

  # ---- Cap AR/MA orders for stability ----
  p <- min(par$p, 5)
  q <- min(par$q, 5)
  d <- par$d
  P <- par$P
  D <- par$D
  Q <- par$Q
  s <- par$s


  max_h <- length(ts_resp) - 1  #leaving at least 1 for train
  h_use <- min(H, max_h)

  #train test split
  train_end <- length(ts_resp) - h_use
  train <- window(ts_resp, end = train_end)
  test  <- window(ts_resp, start = train_end + 1)

  #SARIMA (train)
  fit_sarima <- Arima(
    train,
    order = c(p,d,q),
    seasonal = list(order=c(P,D,Q), period=s),
    include.constant = FALSE
  )

  cat("AICc:", fit_sarima$aicc, "\n")
  checkresiduals(fit_sarima)

  #FORECAST
  fc <- forecast(fit_sarima, h = h_use)

  #PLOT
  plot(fc, main = paste("SARIMA Forecast -", resp),
       xlab = "Time", ylab = "Demand")
  lines(test, col = "red")

  #accuracy
  acc <- accuracy(fc, test)
  print(acc)
}

"""looking at the accuracy table, model is not great, some autocorrelation remains

# Doing auto.arima() and ETS
"""

h_max <- 12*7

for(resp in all6){

  ts_resp <- ts(diff_list[[resp]])  # ensure ts object

  h_use <- min(h_max, length(ts_resp)-1)
  if(h_use < 1){
    warning(paste("Series", resp, "too short for forecast, skipping"))
    next
  }

  #ARIMA
  fit_arima <- auto.arima(ts_resp)
  forecast_arima <- forecast(fit_arima, h = h_use)
  plot(forecast_arima, main = paste("ARIMA Forecast -", resp))

  #ETS
  fit_ets <- ets(ts_resp)
  forecast_ets <- forecast(fit_ets, h = h_use)
  plot(forecast_ets, main = paste("ETS Forecast -", resp))

  #SARIMA(automatic)
  fit_sarima <- auto.arima(ts_resp, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
  forecast_sarima <- forecast(fit_sarima, h = h_use)
  plot(forecast_sarima, main = paste("SARIMA Forecast -", resp))
}

h_max <- 12*7
peak_forecasts <- data.frame(
  Respondent = character(),
  Peak_Demand = numeric(),
  Peak_Day = numeric()
)

for(resp in all6){
  ts_resp <- ts(diff_list[[resp]])

  h_use <- min(h_max, length(ts_resp)-1)

  #SARIMA (auto)
  fit_sarima <- auto.arima(ts_resp, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
  fc <- forecast(fit_sarima, h = h_use)

  #peak forecast
  peak_value <- max(fc$mean, na.rm = TRUE)
  peak_day <- which.max(fc$mean)

  peak_forecasts <- rbind(peak_forecasts,
                          data.frame(Respondent = resp,
                                     Peak_demand = peak_value,
                                     Peak_day = peak_day))
}

print(peak_forecasts)

