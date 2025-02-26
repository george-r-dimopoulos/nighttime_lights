# Install and load necessary packages
library(readr)
library(dplyr)
library(stargazer)
library(ggplot2)
library(fixest)

# Read the data
data <- read_csv(". Stanford/3. ECON 109/Final Project/Combined_Data.csv")

# Generate month and year columns
months <- c("October", "November", "December", "January", "February")
n_rows <- nrow(data)
data$month <- rep(months, length.out = n_rows)

start_year <- 2012
years <- numeric(n_rows)
for (i in seq_len(n_rows)) {
  offset <- (i - 1) %/% length(months)
  years[i] <- start_year + offset
}
data$year <- ifelse(data$month %in% c("January", "February"), years + 1, years)
data$date <- as.Date(paste(data$year, data$month, "01", sep = "-"), "%Y-%B-%d")

# Define cutoff dates
crimea_cutoff <- as.Date("2014-03-01")
skripal_cutoff <- as.Date("2018-08-01")
ukraine_cutoff <- as.Date("2022-03-01")

# Create post-sanction period indicators
data <- data %>%
  mutate(post_crimea = ifelse(date >= crimea_cutoff, 1, 0),
         post_skripal = ifelse(date >= skripal_cutoff, 1, 0),
         post_ukraine = ifelse(date >= ukraine_cutoff, 1, 0))

# Calculate mean radiance
data <- data %>%
  rowwise() %>%
  mutate(mean_radiance = mean(c_across(West:Kras), na.rm = TRUE))

# Run separate fixed effects regressions for each sanction
model_crimea <- feols(mean_radiance ~ post_crimea | month, data = data)
model_skripal <- feols(mean_radiance ~ post_skripal | month, data = data)
model_ukraine <- feols(mean_radiance ~ post_ukraine | month, data = data)


# Output regression results
etable(model_crimea)
etable(model_skripal)
etable(model_ukraine)


# Visualization
plot <- ggplot(data, aes(x = date, y = mean_radiance, group = 1)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.numeric(crimea_cutoff), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(skripal_cutoff), linetype = "dotted", color = "red") +
  geom_vline(xintercept = as.numeric(ukraine_cutoff), linetype = "dotted", color = "red") +
  labs(title = "Mean Radiance Before and After Sanctions", x = "Date", y = "Mean Radiance") +
  scale_x_date(breaks = c(crimea_cutoff, skripal_cutoff, ukraine_cutoff),
               labels = c("2014-03", "2018-08", "2022-03")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = margin(2, 2, 2, 2, "cm"))

# Save the plot with larger size
ggsave(". Stanford/3. ECON 109/Final Project/Mean_Radiance_Sanctions_Plot_Large.png", plot = plot, width = 12, height = 6)
