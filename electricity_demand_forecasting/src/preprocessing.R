
install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "zoo", "forecast","urca", "tseries"))

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(forecast)

df <- read_csv("electricity_demand.csv")
print(head(df))
summary(df)
str(df)
plot <- ggplot(df, aes(x = Date, y = Value)) +
  geom_line() +
  labs(
    title = "Value Over Time",
    x = "Date",
    y = "Value"
  )
print(plot)

#df <- df %>% select(-`...1`)
df <- df %>% select(-Type, -Timezone, -Units)
print(head(df))
df <- df %>%
  mutate(
    Date = as.Date(Date),
    Value = as.numeric(Value),
    Respondent = as.factor(Respondent)
  )
df %>% summarise_all(~sum(is.na(.)))
sum(is.na(df$Respondent))

#unique respondents
print(unique(df$Respondent))
print(unique(df$Code))

"""# ^^Cleaned data, no missing values and 6 unique respondents
