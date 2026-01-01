
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "zoo", "forecast","urca", "tseries"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)


all6 <- c("Duke Energy Progress East",
          "Duke Energy Carolinas",
          "New York Independent System Operator",
          "PJM Interconnection, LLC",
          "Southern Company Services, Inc. - Trans",
          "Tennessee Valley Authority")

df_all6 <- df %>% filter(Respondent %in% all6)

df_all6_avg_value_by_respondent <- df_all6 %>%
  group_by(Respondent) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE))

plot_all6_avg_value_respondent <- ggplot(df_all6_avg_value_by_respondent, aes(x = Respondent, y = Average_Value, fill = Respondent)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Value by Respondent (All 6)", x = "Respondent", y = "Average Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  theme(plot.title = element_text(size = 18))

options(repr.plot.width = 10, repr.plot.height = 10)
print(plot_all6_avg_value_respondent)

plot_all6_demand <- ggplot(df_all6, aes(x = Date, y = Value, color = Respondent)) +
  geom_line(linewidth = 1) + # Changed 'size' to 'linewidth'
  labs(
    title = "Electricity Demand Over Time (All 6 Respondents)",
    x = "Date",
    y = "Demand (MWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_brewer(palette = "Set2")

options(repr.plot.width = 12, repr.plot.height = 6)
print(plot_all6_demand)

df_all6 <- df_all6 %>%
  group_by(Respondent) %>%
  arrange(Date) %>%
  mutate(
    roll_mean = zoo::rollmean(Value, k = 7, fill = NA, align = "right"),
    roll_sd = zoo::rollapply(Value, width = 7, FUN = sd, fill = NA, align = "right"),
    anomaly = abs(Value - roll_mean) > 2 * roll_sd
  )

#plot anomalies
plot_anomaly <- ggplot(df_all6, aes(x = Date, y = Value, color = Respondent)) +
  geom_line() +
  geom_point(data = subset(df_all6, anomaly == TRUE), aes(x = Date, y = Value), color = "red") +
  labs(title = "Electricity Demand with Anomalies Highlighted", x = "Date", y = "Demand (MWh)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16))

options(repr.plot.width = 12, repr.plot.height = 6)
print(plot_anomaly)

"""# ---------- Weekday Pattern ----------"""

weekday_patterns_all <- df_all6 %>%
  mutate(Weekday = factor(weekdays(Date),
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                     "Friday", "Saturday", "Sunday"))) %>%
  group_by(Respondent, Weekday) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE), .groups = "drop")


ggplot(weekday_patterns_all, aes(x = Weekday, y = Average_Value, group = Respondent, color = Respondent)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Weekday demand by respondent",
       x = "Weekday",
       y = "Average demand (MWh)",
       color = "Respondent") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14))


