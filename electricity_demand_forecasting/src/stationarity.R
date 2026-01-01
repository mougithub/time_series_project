
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "zoo", "forecast","urca", "tseries"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)



"""# Stationarity CHeck"""

library(tseries)

diff_list <- list()
order_list <- list()

for(resp in all6){

  df_resp_example <- df_all6 %>% filter(Respondent == resp) %>% arrange(Date)
  ts_resp_example <- ts(df_resp_example$Value, frequency = 7)

  # ADF test
  result_adf_test <- adf.test(ts_resp_example, alternative = "stationary")
  p_value <- result_adf_test$p.value

  if (p_value < 0.05) {
    cat(paste0(resp, ": Stationary (p-value = ", format(p_value, digits = 4), ")\n"))
  } else {
    cat(paste0(resp, ": Non-Stationary (p-value = ", format(p_value, digits = 4), ")\n"))
  }

  # Decide differencing
  d <- ifelse(p_value < 0.05, 0, 1)
  cat("Differencing needed =", d, "\n")

  # Apply differencing **correctly**
  ts_processed <- if (d == 1) diff(ts_resp_example) else ts_resp_example

  diff_list[[resp]] <- ts_processed
  order_list[[resp]] <- d
}

"""# ---------- STL Decomposition ----------"""

for(resp in all6){

    stl_decomp <- stl(ts_resp, s.window = "periodic")
    plot(stl_decomp, main = paste("STL Decomposition:", resp))
}

"""# ACF/PACF"""

for(resp in all6){
  ts_proc <- diff_list[[resp]]

  # ACF
  acf_plot <- ggAcf(ts_proc) +
    ggtitle(paste("ACF -", resp))
  print(acf_plot)

  # PACF
  pacf_plot <- ggPacf(ts_proc) +
    ggtitle(paste("PACF -", resp))
  print(pacf_plot)
}

