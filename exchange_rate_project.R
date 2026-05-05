# Exchange Rate Volatility Analysis 

# loading CSV files 
china <- read.csv("exchangeratedata/DEXCHUS.csv")
india <- read.csv("exchangeratedata/DEXINUS.csv")
japan <- read.csv("exchangeratedata/DEXJPUS.csv")
swiss <- read.csv("exchangeratedata/DEXSZUS.csv")
euro <- read.csv("exchangeratedata/DEXUSEU.csv")
uk <- read.csv("exchangeratedata/DEXUSUK.csv")

# renaming columns
names(china) <- c("date", "china")
names(india) <- c("date", "india")
names(japan) <- c("date", "japan")
names(swiss) <- c("date", "swiss")
names(euro) <- c("date", "euro")
names(uk) <- c("date", "uk")

china$date <- as.Date(china$date)
india$date <- as.Date(india$date)
japan$date <- as.Date(japan$date)
swiss$date <- as.Date(swiss$date)
euro$date <- as.Date(euro$date)
uk$date <- as.Date(uk$date)

# merging all data sets by date 
exchange_rates <- merge(china, india, by = "date", all = TRUE)
exchange_rates <- merge(exchange_rates, japan, by = "date", all = TRUE)
exchange_rates <- merge(exchange_rates, swiss, by = "date", all = TRUE)
exchange_rates <- merge(exchange_rates, euro, by = "date", all = TRUE)
exchange_rates <- merge(exchange_rates, uk, by = "date", all = TRUE)

# checking merged data
head(exchange_rates)
str(exchange_rates)
summary(exchange_rates)

# checking missing values
colSums(is.na(exchange_rates))

# removing missing values
exchange_rates_clean <- na.omit(exchange_rates)

# checking cleaned data
head(exchange_rates_clean)
colSums(is.na(exchange_rates_clean))

# standardizing exchange rates so foreign currency per 1 U.S. dollar
exchange_rates_clean$euro_per_usd <- 1 / exchange_rates_clean$euro
exchange_rates_clean$pound_per_usd <- 1 / exchange_rates_clean$uk

# creating final standardized exchange rate dataset
fx_rates <- data.frame(
  date = exchange_rates_clean$date,
  yuan_per_usd = exchange_rates_clean$china,
  rupee_per_usd = exchange_rates_clean$india,
  yen_per_usd = exchange_rates_clean$japan,
  franc_per_usd = exchange_rates_clean$swiss,
  euro_per_usd = exchange_rates_clean$euro_per_usd,
  pound_per_usd = exchange_rates_clean$pound_per_usd
)
head(fx_rates)

write.csv(fx_rates, "cleaned_exchange_rates.csv", row.names = FALSE)

# computing daily log returns
log_returns <- data.frame(
  date = fx_rates$date[-1],
  yuan_return = diff(log(fx_rates$yuan_per_usd)),
  rupee_return = diff(log(fx_rates$rupee_per_usd)),
  yen_return = diff(log(fx_rates$yen_per_usd)),
  franc_return = diff(log(fx_rates$franc_per_usd)),
  euro_return = diff(log(fx_rates$euro_per_usd)),
  pound_return = diff(log(fx_rates$pound_per_usd))
)
head(log_returns)

write.csv(log_returns, "log_returns.csv", row.names = FALSE)

# summary statistics for daily log returns
summary_stats <- data.frame(
  currency = c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
               "Swiss Franc", "Euro", "British Pound"),
  mean_return = c(
    mean(log_returns$yuan_return),
    mean(log_returns$rupee_return),
    mean(log_returns$yen_return),
    mean(log_returns$franc_return),
    mean(log_returns$euro_return),
    mean(log_returns$pound_return)
  ),
  min_return = c(
    min(log_returns$yuan_return),
    min(log_returns$rupee_return),
    min(log_returns$yen_return),
    min(log_returns$franc_return),
    min(log_returns$euro_return),
    min(log_returns$pound_return)
  ),
  max_return = c(
    max(log_returns$yuan_return),
    max(log_returns$rupee_return),
    max(log_returns$yen_return),
    max(log_returns$franc_return),
    max(log_returns$euro_return),
    max(log_returns$pound_return)
  ),
  daily_volatility = c(
    sd(log_returns$yuan_return),
    sd(log_returns$rupee_return),
    sd(log_returns$yen_return),
    sd(log_returns$franc_return),
    sd(log_returns$euro_return),
    sd(log_returns$pound_return)
  )
)
print(summary_stats)

write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)

# computing volatility as standard deviation of daily log returns
volatility <- data.frame(
  currency = c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
               "Swiss Franc", "Euro", "British Pound"),
  sd_log_return = c(
    sd(log_returns$yuan_return),
    sd(log_returns$rupee_return),
    sd(log_returns$yen_return),
    sd(log_returns$franc_return),
    sd(log_returns$euro_return),
    sd(log_returns$pound_return)
  )
)

# ranking currencies from most volatile to least volatile
volatility <- volatility[order(volatility$sd_log_return, decreasing = TRUE), ]
print(volatility)

write.csv(volatility, "volatility_ranking.csv", row.names = FALSE)

# plotting volatility ranking
png("plots/volatility_ranking.png", width = 900, height = 600)
barplot(volatility$sd_log_return,
        names.arg = volatility$currency,
        main = "Currency Volatility Ranking",
        xlab = "Currency",
        ylab = "Standard Deviation of Daily Log Returns",
        las = 2)
dev.off()

# comparing exchange rate movements using correlations of log returns
correlation_matrix <- cor(log_returns[, -1])
print(correlation_matrix)

write.csv(correlation_matrix, "pairwise_correlations.csv")

# plotting correlation matrix
png("plots/pairwise_correlations.png", width = 900, height = 700)

image(1:ncol(correlation_matrix),
      1:nrow(correlation_matrix),
      correlation_matrix,
      axes = FALSE,
      main = "Pairwise Correlations of FX Log Returns",
      xlab = "",
      ylab = "")

axis(1, at = 1:ncol(correlation_matrix), labels = colnames(correlation_matrix), las = 2)
axis(2, at = 1:nrow(correlation_matrix), labels = rownames(correlation_matrix), las = 2)

text(expand.grid(1:ncol(correlation_matrix), 1:nrow(correlation_matrix)),
     labels = round(as.vector(correlation_matrix), 2))

dev.off()

# Normalizing exchange rates
normalized_rates <- fx_rates
normalized_rates$yuan_per_usd <- 100 * fx_rates$yuan_per_usd / fx_rates$yuan_per_usd[1]
normalized_rates$rupee_per_usd <- 100 * fx_rates$rupee_per_usd / fx_rates$rupee_per_usd[1]
normalized_rates$yen_per_usd <- 100 * fx_rates$yen_per_usd / fx_rates$yen_per_usd[1]
normalized_rates$franc_per_usd <- 100 * fx_rates$franc_per_usd / fx_rates$franc_per_usd[1]
normalized_rates$euro_per_usd <- 100 * fx_rates$euro_per_usd / fx_rates$euro_per_usd[1]
normalized_rates$pound_per_usd <- 100 * fx_rates$pound_per_usd / fx_rates$pound_per_usd[1]
head(normalized_rates)

write.csv(normalized_rates, "normalized_exchange_rates.csv", row.names = FALSE)

# Plot normalized exchange rates
png("plots/normalized_exchange_rates.png", width = 1000, height = 700)
plot(normalized_rates$date, normalized_rates$yuan_per_usd,
     type = "l",
     ylim = range(normalized_rates[, -1]),
     main = "Normalized Exchange Rates Against the U.S. Dollar",
     xlab = "Date",
     ylab = "Index Value, First Observation = 100")
lines(normalized_rates$date, normalized_rates$rupee_per_usd)
lines(normalized_rates$date, normalized_rates$yen_per_usd)
lines(normalized_rates$date, normalized_rates$franc_per_usd)
lines(normalized_rates$date, normalized_rates$euro_per_usd)
lines(normalized_rates$date, normalized_rates$pound_per_usd)
legend("topleft",
       legend = c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
                  "Swiss Franc", "Euro", "British Pound"),
       lty = 1,
       cex = 0.8)
dev.off()

# Individual exchange-rate plots
currency_names <- c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
                    "Swiss Franc", "Euro", "British Pound")
rate_columns <- c("yuan_per_usd", "rupee_per_usd", "yen_per_usd",
                  "franc_per_usd", "euro_per_usd", "pound_per_usd")
for (i in 1:length(rate_columns)) {
  file_name <- paste0("plots/", rate_columns[i], "_exchange_rate.png")
  png(file_name, width = 900, height = 600)
  plot(fx_rates$date, fx_rates[[rate_columns[i]]],
       type = "l",
       main = paste(currency_names[i], "per U.S. Dollar"),
       xlab = "Date",
       ylab = "Exchange Rate")
  dev.off()
}

# Individual log-return plots
return_columns <- c("yuan_return", "rupee_return", "yen_return",
                    "franc_return", "euro_return", "pound_return")
for (i in 1:length(return_columns)) {
  file_name <- paste0("plots/", return_columns[i], ".png")
  png(file_name, width = 900, height = 600)
  plot(log_returns$date, log_returns[[return_columns[i]]],
       type = "l",
       main = paste(currency_names[i], "Daily Log Returns"),
       xlab = "Date",
       ylab = "Log Return")
  abline(h = 0)
  dev.off()
}

# Cross-exchange-rate comparisons by dividing their USD exchange rates
cross_rates <- data.frame(
  date = fx_rates$date,
  rupee_per_yen = fx_rates$rupee_per_usd / fx_rates$yen_per_usd,
  rupee_per_yuan = fx_rates$rupee_per_usd / fx_rates$yuan_per_usd,
  rupee_per_euro = fx_rates$rupee_per_usd / fx_rates$euro_per_usd,
  rupee_per_pound = fx_rates$rupee_per_usd / fx_rates$pound_per_usd,
  yen_per_euro = fx_rates$yen_per_usd / fx_rates$euro_per_usd,
  yen_per_pound = fx_rates$yen_per_usd / fx_rates$pound_per_usd,
  franc_per_euro = fx_rates$franc_per_usd / fx_rates$euro_per_usd,
  franc_per_pound = fx_rates$franc_per_usd / fx_rates$pound_per_usd
)
head(cross_rates)

write.csv(cross_rates, "cross_exchange_rates.csv", row.names = FALSE)

# Normalize cross rates so each starts at 100
normalized_cross_rates <- cross_rates

normalized_cross_rates$rupee_per_yen <- 100 * cross_rates$rupee_per_yen / cross_rates$rupee_per_yen[1]
normalized_cross_rates$rupee_per_yuan <- 100 * cross_rates$rupee_per_yuan / cross_rates$rupee_per_yuan[1]
normalized_cross_rates$rupee_per_euro <- 100 * cross_rates$rupee_per_euro / cross_rates$rupee_per_euro[1]
normalized_cross_rates$rupee_per_pound <- 100 * cross_rates$rupee_per_pound / cross_rates$rupee_per_pound[1]
normalized_cross_rates$yen_per_euro <- 100 * cross_rates$yen_per_euro / cross_rates$yen_per_euro[1]
normalized_cross_rates$yen_per_pound <- 100 * cross_rates$yen_per_pound / cross_rates$yen_per_pound[1]
normalized_cross_rates$franc_per_euro <- 100 * cross_rates$franc_per_euro / cross_rates$franc_per_euro[1]
normalized_cross_rates$franc_per_pound <- 100 * cross_rates$franc_per_pound / cross_rates$franc_per_pound[1]

write.csv(normalized_cross_rates, "normalized_cross_exchange_rates.csv", row.names = FALSE)

# Plot normalized cross-exchange rates
png("plots/normalized_cross_exchange_rates.png", width = 1000, height = 700)
plot(normalized_cross_rates$date, normalized_cross_rates$rupee_per_yen,
     type = "l",
     ylim = range(normalized_cross_rates[, -1]),
     main = "Normalized Cross-Exchange Rates",
     xlab = "Date",
     ylab = "Index Value, First Observation = 100")
lines(normalized_cross_rates$date, normalized_cross_rates$rupee_per_yuan)
lines(normalized_cross_rates$date, normalized_cross_rates$rupee_per_euro)
lines(normalized_cross_rates$date, normalized_cross_rates$rupee_per_pound)
lines(normalized_cross_rates$date, normalized_cross_rates$yen_per_euro)
lines(normalized_cross_rates$date, normalized_cross_rates$yen_per_pound)
lines(normalized_cross_rates$date, normalized_cross_rates$franc_per_euro)
lines(normalized_cross_rates$date, normalized_cross_rates$franc_per_pound)
legend("topleft",
       legend = c("Rupee/Yen", "Rupee/Yuan", "Rupee/Euro", "Rupee/Pound",
                  "Yen/Euro", "Yen/Pound", "Franc/Euro", "Franc/Pound"),
       lty = 1,
       cex = 0.7)
dev.off()

# rolling volatility function
rolling_sd <- function(x, window) {
  result <- rep(NA, length(x))
  for (i in window:length(x)) {
    result[i] <- sd(x[(i - window + 1):i])
  }
  return(result)
}

# 30-day rolling volatility for each currency
rolling_volatility <- data.frame(
  date = log_returns$date,
  yuan_rolling_vol = rolling_sd(log_returns$yuan_return, 30),
  rupee_rolling_vol = rolling_sd(log_returns$rupee_return, 30),
  yen_rolling_vol = rolling_sd(log_returns$yen_return, 30),
  franc_rolling_vol = rolling_sd(log_returns$franc_return, 30),
  euro_rolling_vol = rolling_sd(log_returns$euro_return, 30),
  pound_rolling_vol = rolling_sd(log_returns$pound_return, 30)
)
write.csv(rolling_volatility, "rolling_volatility.csv", row.names = FALSE)

# Plotting 30-day rolling volatility
png("plots/rolling_volatility.png", width = 1000, height = 700)
plot(rolling_volatility$date, rolling_volatility$yuan_rolling_vol,
     type = "l",
     ylim = range(rolling_volatility[, -1], na.rm = TRUE),
     main = "30-Day Rolling Volatility of FX Log Returns",
     xlab = "Date",
     ylab = "30-Day Rolling Standard Deviation")
lines(rolling_volatility$date, rolling_volatility$rupee_rolling_vol)
lines(rolling_volatility$date, rolling_volatility$yen_rolling_vol)
lines(rolling_volatility$date, rolling_volatility$franc_rolling_vol)
lines(rolling_volatility$date, rolling_volatility$euro_rolling_vol)
lines(rolling_volatility$date, rolling_volatility$pound_rolling_vol)
legend("topleft",
       legend = c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
                  "Swiss Franc", "Euro", "British Pound"),
       lty = 1,
       cex = 0.8)
dev.off()

# Largest daily movements for each currency
largest_moves <- data.frame(
  currency = character(),
  largest_positive_return = numeric(),
  positive_date = as.Date(character()),
  largest_negative_return = numeric(),
  negative_date = as.Date(character())
)
return_columns <- c("yuan_return", "rupee_return", "yen_return",
                    "franc_return", "euro_return", "pound_return")
currency_names <- c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
                    "Swiss Franc", "Euro", "British Pound")
for (i in 1:length(return_columns)) {
  returns <- log_returns[[return_columns[i]]]
  max_index <- which.max(returns)
  min_index <- which.min(returns)
  largest_moves <- rbind(
    largest_moves,
    data.frame(
      currency = currency_names[i],
      largest_positive_return = returns[max_index],
      positive_date = log_returns$date[max_index],
      largest_negative_return = returns[min_index],
      negative_date = log_returns$date[min_index]
    )
  )
}
print(largest_moves)

write.csv(largest_moves, "largest_daily_moves.csv", row.names = FALSE)

# Final summary table
final_summary <- data.frame(
  currency = c("Chinese Yuan", "Indian Rupee", "Japanese Yen",
               "Swiss Franc", "Euro", "British Pound"),
  first_exchange_rate = c(
    fx_rates$yuan_per_usd[1],
    fx_rates$rupee_per_usd[1],
    fx_rates$yen_per_usd[1],
    fx_rates$franc_per_usd[1],
    fx_rates$euro_per_usd[1],
    fx_rates$pound_per_usd[1]
  ),
  last_exchange_rate = c(
    fx_rates$yuan_per_usd[nrow(fx_rates)],
    fx_rates$rupee_per_usd[nrow(fx_rates)],
    fx_rates$yen_per_usd[nrow(fx_rates)],
    fx_rates$franc_per_usd[nrow(fx_rates)],
    fx_rates$euro_per_usd[nrow(fx_rates)],
    fx_rates$pound_per_usd[nrow(fx_rates)]
  ),
  total_percent_change = c(
    100 * (fx_rates$yuan_per_usd[nrow(fx_rates)] / fx_rates$yuan_per_usd[1] - 1),
    100 * (fx_rates$rupee_per_usd[nrow(fx_rates)] / fx_rates$rupee_per_usd[1] - 1),
    100 * (fx_rates$yen_per_usd[nrow(fx_rates)] / fx_rates$yen_per_usd[1] - 1),
    100 * (fx_rates$franc_per_usd[nrow(fx_rates)] / fx_rates$franc_per_usd[1] - 1),
    100 * (fx_rates$euro_per_usd[nrow(fx_rates)] / fx_rates$euro_per_usd[1] - 1),
    100 * (fx_rates$pound_per_usd[nrow(fx_rates)] / fx_rates$pound_per_usd[1] - 1)
  ),
  average_daily_log_return = c(
    mean(log_returns$yuan_return),
    mean(log_returns$rupee_return),
    mean(log_returns$yen_return),
    mean(log_returns$franc_return),
    mean(log_returns$euro_return),
    mean(log_returns$pound_return)
  ),
  daily_volatility = c(
    sd(log_returns$yuan_return),
    sd(log_returns$rupee_return),
    sd(log_returns$yen_return),
    sd(log_returns$franc_return),
    sd(log_returns$euro_return),
    sd(log_returns$pound_return)
  )
)
final_summary <- final_summary[order(final_summary$daily_volatility, decreasing = TRUE), ]
print(final_summary)

write.csv(final_summary, "final_summary.csv", row.names = FALSE)