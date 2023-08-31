df_user <- read.csv("C:/Users/weiziyao/Desktop/dissertation/Dissertation/user.csv")
summary(df_user)

library(dplyr)

# Counts the number of users in each country
country_counts <- df_user %>%
  group_by(country) %>%
  summarise(user_count = n())

# Print the number of users by country
print("Country user counts:")
print(country_counts,n=36)

# Calculate the average user age and average baby age for each country
country_stats <- df_user %>%
  group_by(country) %>%
  summarise(avg_user_age = mean(age),
            avg_baby_age = mean(baby_age.years))

# Print the average user age and average baby age for each country
print("\n Average user age and average baby age per country:")
print(country_stats)

# dataset referral source
referral_source_counts <- df_user %>%
  group_by(referral_source) %>%
  summarise(user_count = n())

print(referral_source_counts)

df_rawdata<-read.csv("C:/Users/weiziyao/Desktop/dissertation/Dissertation/rawdata.csv")
# Use the table function to count the number of times each user fills out the questionnaire
user_counts <- table(df_rawdata$user_id)

# Count the distribution of the number of times each user filled out the questionnaire
count_distribution <- table(user_counts)

# Print the results
print(count_distribution)
summary(df_rawdata)

# Clear multiple duplicate submissions
data_clean <- df_rawdata %>%
  group_by(user_id) %>%
  slice(1) %>%
  ungroup()

summary(data_clean)
# Clearing Missing Values
data_clean <- data_clean[!is.na(data_clean$completer_age), ]
data_clean <- data_clean[!is.na(data_clean$slept_through_night), ]
data_clean <- data_clean[!is.na(data_clean$sleeps_longest_stretch_time), ]

col_has_missing <- is.na(data_clean$slept_through_night)
print(col_has_missing)  #  Returns a logical vector, TRUE for missing values and FALSE for non-missing values.

data_clean$what_eat_drink_breast_milk[is.na(data_clean$what_eat_drink_breast_milk)] <- 0
data_clean$what_eat_drink_baby_formula[is.na(data_clean$what_eat_drink_baby_formula)] <- 0
data_clean$what_eat_drink_cows_milk[is.na(data_clean$what_eat_drink_cows_milk)] <- 0
data_clean$what_eat_drink_pureed_foods[is.na(data_clean$what_eat_drink_pureed_foods)] <- 0
data_clean$what_eat_drink_solid_foods[is.na(data_clean$what_eat_drink_solid_foods)] <- 0
data_clean$what_eat_drink_baby_cereals[is.na(data_clean$what_eat_drink_baby_cereals)] <- 0

data_clean$baby_total_sleep_time <- gsub(";", ":", data_clean$baby_total_sleep_time)
data_clean <- data_clean[data_clean$Q_completed != 0, ]

library(dplyr)

# Define a function to convert a time string to hours
time_to_hours <- function(time_str) {
  hour <- as.numeric(sub("^(\\d+):(\\d+)$", "\\1", time_str))
  minute <- as.numeric(sub("^(\\d+):(\\d+)$", "\\2", time_str))
  hours <- hour + minute / 60
  return(hours)
}


# Converting time strings to hours and adding new columns
data_clean$sleeps_longest_stretch_time_hours <- time_to_hours(data_clean$sleeps_longest_stretch_time)
data_clean$sleeps_total_time_hours <- time_to_hours(data_clean$sleeps_total_time)
data_clean$baby_sleep_time_hours <- time_to_hours(data_clean$baby_sleep_time)
data_clean$baby_wake_time_hours <- time_to_hours(data_clean$baby_wake_time)
data_clean$baby_total_sleep_time_hours <- time_to_hours(data_clean$baby_total_sleep_time)
data_clean$baby_sleep_daytime_time_hours <- time_to_hours(data_clean$baby_sleep_daytime_time)

# Descriptive statistical analyses
#age analysis
#Use the summary() function to view descriptive statistics
summary(data_clean$Week_at_registration)

# Calculate the mean
mean_week <- mean(data_clean$Week_at_registration, na.rm = TRUE)
cat("Mean week at registration:", mean_week, "\n")

# Calculate the median
median_week <- median(data_clean$Week_at_registration, na.rm = TRUE)
cat("Median week at registration:", median_week, "\n")

# Calculate the maximum value
max_week <- max(data_clean$Week_at_registration, na.rm = TRUE)
cat("Maximum week at registration:", max_week, "\n")

# Calculate the minimum value
min_week <- min(data_clean$Week_at_registration, na.rm = TRUE)
cat("Minimum week at registration:", min_week, "\n")

# Calculate the standard deviation
sd_week <- sd(data_clean$Week_at_registration, na.rm = TRUE)
cat("Week at registration standard deviation:", sd_week, "\n")

# Check Week_at_registration column contains missing values
missing_values <- is.na(data_clean$Week_at_registration)

# Printing missing value cases
print(missing_values)

#  Counting the number of missing values
num_missing <- sum(missing_values)
print(paste("Number of missing values:", num_missing))

# Viewing Rows Containing Missing Values
rows_with_missing <- data_clean[missing_values, ]
print("Rows with missing values:")
print(rows_with_missing)

# Plotting histograms
library(e1071)
library(ggplot2)
ggplot(data_clean, aes(x = Week_at_registration)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Age Distribution at Registration",
       x = "Age (weeks)",
       y = "Frequency")
# Calculate the skewness value
skew_value <- skewness(data_clean$Week_at_registration)
print(paste("Skewness value:", skew_value))

# normality test
shapiro_test <- shapiro.test(data_clean$Week_at_registration)
print(shapiro_test)

# Plotting Box Lines
ggplot(data_clean, aes(y = Week_at_registration)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Age Distribution at Registration",
       x = "Age",
       y = "Weeks")
#sleep time analysis

# Use summary() function to view descriptive statistics for sleeps_total_time_hours
summary(data_clean$sleeps_total_time_hours)

# Calculate mean of sleeps_total_time_hours
mean_sleeps_total_time <- mean(data_clean$sleeps_total_time_hours, na.rm = TRUE)
cat("Mean of sleeps_total_time_hours:", mean_sleeps_total_time, "hours\n")

# Calculate median of sleeps_total_time_hours
median_sleeps_total_time <- median(data_clean$sleeps_total_time_hours, na.rm = TRUE)
cat("Median of sleeps_total_time_hours:", median_sleeps_total_time, "hours\n")

# Calculate maximum value of sleeps_total_time_hours
max_sleeps_total_time <- max(data_clean$sleeps_total_time_hours, na.rm = TRUE)
cat("Maximum of sleeps_total_time_hours:", max_sleeps_total_time, "hours\n")

# Calculate minimum value of sleeps_total_time_hours
min_sleeps_total_time <- min(data_clean$sleeps_total_time_hours, na.rm = TRUE)
cat("Minimum of sleeps_total_time_hours:", min_sleeps_total_time, "hours\n")

# Calculate standard deviation of sleeps_total_time_hours
sd_sleeps_total_time <- sd(data_clean$sleeps_total_time_hours, na.rm = TRUE)
cat("Standard deviation of sleeps_total_time_hours:", sd_sleeps_total_time, "hours\n")
library(ggplot2)

# Plotting histograms
ggplot(data_clean, aes(x = sleeps_total_time_hours)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Total Sleep Time",
       x = "Total Sleep Time (hours)",
       y = "Frequency")
# Plotting Box Lines
ggplot(data_clean, aes(y = sleeps_total_time_hours)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Box Plot of Total Sleep Time",
       y = "Total Sleep Time (hours)")
# Calculate the skewness value
skew_value <- skewness(data_clean$sleeps_total_time_hours)
print(paste("Skewness value:", skew_value))

# normality test
shapiro_test <- shapiro.test(data_clean$sleeps_total_time_hours)
print(shapiro_test)

# Use summary() function to view descriptive statistics for sleeps_longest_stretch_time_hours
summary(data_clean$sleeps_longest_stretch_time_hours)

# Calculate mean of sleeps_longest_stretch_time_hours
mean_sleeps_longest_stretch <- mean(data_clean$sleeps_longest_stretch_time_hours, na.rm = TRUE)
cat("Mean of sleeps_longest_stretch_time_hours:", mean_sleeps_longest_stretch, "hours\n")

# Calculate median of sleeps_longest_stretch_time_hours
median_sleeps_longest_stretch <- median(data_clean$sleeps_longest_stretch_time_hours, na.rm = TRUE)
cat("Median of sleeps_longest_stretch_time_hours:", median_sleeps_longest_stretch, "hours\n")

# Calculate maximum value of sleeps_longest_stretch_time_hours
max_sleeps_longest_stretch <- max(data_clean$sleeps_longest_stretch_time_hours, na.rm = TRUE)
cat("Maximum of sleeps_longest_stretch_time_hours:", max_sleeps_longest_stretch, "hours\n")

# Calculate minimum value of sleeps_longest_stretch_time_hours
min_sleeps_longest_stretch <- min(data_clean$sleeps_longest_stretch_time_hours, na.rm = TRUE)
cat("Minimum of sleeps_longest_stretch_time_hours:", min_sleeps_longest_stretch, "hours\n")

# Calculate standard deviation of sleeps_longest_stretch_time_hours
sd_sleeps_longest_stretch <- sd(data_clean$sleeps_longest_stretch_time_hours, na.rm = TRUE)
cat("Standard deviation of sleeps_longest_stretch_time_hours:", sd_sleeps_longest_stretch, "hours\n")

# Histogram of sleeps_longest_stretch_time_hours
ggplot(data_clean, aes(x = sleeps_longest_stretch_time_hours)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Longest Stretch of Sleep Distribution",
       x = "Longest Stretch of Sleep (hours)",
       y = "Frequency")

#boxplot of sleeps_longest_stretch_time_hours
ggplot(data_clean, aes(y = sleeps_longest_stretch_time_hours)) +
  geom_boxplot() +
  labs(title = "Longest Stretch of Sleep Box Plot",
       y = "Longest Stretch of Sleep (hours)")
# Calculate the skewness value
skew_value <- skewness(data_clean$sleeps_longest_stretch_time_hours)
print(paste("Skewness value:", skew_value))

# normality test
shapiro_test <- shapiro.test(data_clean$sleeps_longest_stretch_time_hours)
print(shapiro_test)


data_clean$baby_sleep_daytime_time_hours[is.na(data_clean$baby_sleep_daytime_time_hours)] <- 0
# Use summary() function to view descriptive statistics for baby_sleep_daytime_time_hours
summary(data_clean$baby_sleep_daytime_time_hours)
data_clean$baby_sleep_daytime_time_hours
# Calculate mean of baby_sleep_daytime_time_hours
mean_baby_sleep_daytime <- mean(data_clean$baby_sleep_daytime_time_hours, na.rm = TRUE)
cat("Mean of baby_sleep_daytime_time_hours:", mean_baby_sleep_daytime, "hours\n")

# Calculate median of baby_sleep_daytime_time_hours
median_baby_sleep_daytime <- median(data_clean$baby_sleep_daytime_time_hours, na.rm = TRUE)
cat("Median of baby_sleep_daytime_time_hours:", median_baby_sleep_daytime, "hours\n")

# Calculate maximum value of baby_sleep_daytime_time_hours
max_baby_sleep_daytime <- max(data_clean$baby_sleep_daytime_time_hours, na.rm = TRUE)
cat("Maximum of baby_sleep_daytime_time_hours:", max_baby_sleep_daytime, "hours\n")

# Calculate minimum value of baby_sleep_daytime_time_hours
min_baby_sleep_daytime <- min(data_clean$baby_sleep_daytime_time_hours, na.rm = TRUE)
cat("Minimum of baby_sleep_daytime_time_hours:", min_baby_sleep_daytime, "hours\n")

# Calculate standard deviation of baby_sleep_daytime_time_hours
sd_baby_sleep_daytime <- sd(data_clean$baby_sleep_daytime_time_hours, na.rm = TRUE)
cat("Standard deviation of baby_sleep_daytime_time_hours:", sd_baby_sleep_daytime, "hours\n")

#histogram of baby_sleep_daytime_time_hours
ggplot(data_clean, aes(x = baby_sleep_daytime_time_hours)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Baby Sleep Daytime Time Distribution",
       x = "Baby Sleep Daytime Time (hours)",
       y = "Frequency")

#boxplot of baby_sleep_daytime_time_hours
ggplot(data_clean, aes(y = baby_sleep_daytime_time_hours)) +
  geom_boxplot() +
  labs(title = "Baby Sleep Daytime Time Box Plot",
       y = "Baby Sleep Daytime Time (hours)")

# Calculate the skewness value
skew_value <- skewness(data_clean$baby_sleep_daytime_time_hours)
print(paste("Skewness value:", skew_value))

# Calculate the skewness value
shapiro_test <- shapiro.test(data_clean$baby_sleep_daytime_time_hours)
print(shapiro_test)

# group the age
data_clean$age_group <- cut(data_clean$Age_in_months_at_reg, 
                              breaks = c(0, 3, 6, 9, 12, 15, Inf),
                              labels = c("0-3months", "3-6months", "6-9months", "9-12months", "12-15months", "15months+"),
                              include.lowest = TRUE)
# Creation of a new categorical variable to indicate breastfeeding
data_clean <- data_clean %>%
  mutate(
    feeding_status = case_when(
      what_eat_drink_breast_milk == 1 & what_eat_drink_baby_formula == 0 & what_eat_drink_cows_milk == 0 ~ "All breast",
      what_eat_drink_breast_milk == 1 & (what_eat_drink_baby_formula == 1 | what_eat_drink_cows_milk == 1) ~ "Part breast",
      what_eat_drink_breast_milk == 0 ~ "No breast",
      TRUE ~ "others" 
    )
  )

#chi-square test
contingency_table <- table(data_clean$age_group, data_clean$feeding_status)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

# Visualising Relationships
library(ggplot2)

ggplot(data_clean, aes(x = age_group, fill = feeding_status)) +
  geom_bar(position = "stack") +
  labs(title = "Relationship between Age Group and Feeding Status",
       x = "Age Group",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
summary(data_clean)
# Grouping by age group and feeding style and calculation of the mean and SD of the indicator
grouped_summary <- data_clean %>%
  group_by(age_group, feeding_status) %>%
  summarise(
    avg_sleeps_longest_stretch = mean(sleeps_longest_stretch_time_hours),
    avg_baby_total_sleep = mean(baby_total_sleep_time_hours),
    avg_baby_sleep_daytime = mean(baby_sleep_daytime_time_hours),
    var_sleeps_longest_stretch = var(sleeps_longest_stretch_time_hours),
    var_baby_total_sleep = var(baby_total_sleep_time_hours),
    var_baby_sleep_daytime = var(baby_sleep_daytime_time_hours)
  )

print(grouped_summary)
# Create a data box containing the results of the chi-square test
chi_square_results <- data.frame(
  Test = "Pearson's Chi-squared test",
  Data = "contingency_table",
  X_squared = 210.15,
  df = 10,
  p_value = "< 2.2e-16"
)
library(kableExtra)
# Creating kable tables
table_result <- chi_square_results %>%
  kable(format = "html", escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  add_header_above(c(" ", "Chi-squared Test Result" = 4)) %>%
  row_spec(0, bold = TRUE)


print(table_result)


library(dplyr)
library(ggplot2)
library(purrr)

# Creating a function to perform a normality test
normality_test <- function(sub_data, variable) {
  shapiro_test <- shapiro.test(sub_data[[variable]])
  return(shapiro_test$p.value)
}

data_split_by_age <- split(data_clean, data_clean$age_group)

results <- list()

for (var in c("baby_total_sleep_time_hours", "baby_sleep_daytime_time_hours", "sleeps_longest_stretch_time_hours")) {
  p_values <- sapply(data_split_by_age, function(data) normality_test(data, var))
  
  # Create a tibble to save the results
  p_values_df <- tibble(age_group = names(p_values), p_value = p_values)
  
  # Save results
  results[[var]] <- p_values_df
  
  # Print test results
  print(paste("variable:", var))
  print(p_values_df)
  
  # plot q-q
  p <- ggplot(data_clean, aes(sample = !!as.name(var))) +
    geom_qq() +
    geom_qq_line() +
    labs(title = paste("QQ-PLOT - ", var)) +
    theme_minimal() +
    facet_wrap(~age_group) # Depending on the age group
  
  print(p) 
}







library(ggplot2)

# Bar graph with age group as x-axis, average sleep time indicator as y-axis, and colour grouping to indicate feeding style
ggplot(grouped_summary, aes(x = age_group, y = avg_sleeps_longest_stretch, fill = feeding_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Sleep Longest Stretch Time by Age Group and Feeding Status",
       x = "Age Group",
       y = "Average Sleep Longest Stretch Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Charting the average baby's total sleep time
ggplot(grouped_summary, aes(x = age_group, y = avg_baby_total_sleep, fill = feeding_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Baby Total Sleep Time by Age Group and Feeding Status",
       x = "Age Group",
       y = "Average Baby Total Sleep Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Charting the average baby's daytime sleep time
ggplot(grouped_summary, aes(x = age_group, y = avg_baby_sleep_daytime, fill = feeding_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Baby Daytime Sleep Time by Age Group and Feeding Status",
       x = "Age Group",
       y = "Average Baby Daytime Sleep Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(knitr)

# Beautify the output form
result_table <- data.frame(
  age_group = c("0-3months", "0-3months", "0-3months", "3-6months", "3-6months", "3-6months", 
                "6-9months", "6-9months", "6-9months", "9-12months", "9-12months", "9-12months",
                "12-15months", "12-15months", "12-15months", "15months+", "15months+", "15months+"),
  feeding_status = c("All breast", "No breast", "Part breast", "All breast", "No breast", "Part breast", 
                     "All breast", "No breast", "Part breast", "All breast", "No breast", "Part breast", 
                     "All breast", "No breast", "Part breast", "All breast", "No breast", "Part breast"),
  avg_sleeps_longest_stretch = c(5.64, 8.25, 6.51, 5.92, 8.79, 7.18, 5.82, 10.3, 6.72, 5.42, 9.88, 6.88, 
                                 5.38, 10.5, 6.99, 5.82, 10.1, 7.06),
  avg_sleeps_total = c(9.60, 10.3, 10.1, 10.4, 10.7, 10.6, 10.8, 11.2, 10.9, 10.5, 11.0, 10.7, 
                       10.8, 11.4, 10.6, 10.7, 11.2, 10.6),
  avg_baby_total_sleep = c(10.3, 10.3, 11.2, 10.9, 11.1, 11.2, 11.2, 11.2, 11.3, 11.0, 11.3, 10.9, 
                           11.0, 11.4, 10.9, 10.9, 11.3, 10.8),
  avg_baby_sleep_daytime = c(5.04, 4.54, 4.53, 3.17, 3.25, 3.02, 2.62, 2.56, 2.48, 2.55, 2.28, 2.12, 
                             2.30, 2.01, 2.00, 1.71, 1.80, 1.67)
)

# Using the kable function to make tables
kable(result_table, format = "html", caption = "Average Sleep Time by Age Group and Feeding Status")


#Define the variables to be analysed
variables <- c("sleeps_longest_stretch_time_hours", "baby_sleep_daytime_time_hours", "baby_total_sleep_time_hours")

# The only age group
age_groups <- unique(data_clean$age_group)

results <- list()

# Kruskal-Wallis test for each variable
for (var in variables) {
  results[[var]] <- list()
  for (age in age_groups) {
    subset_data <- subset(data_clean, age_group == age)
    kruskal_result <- kruskal.test(as.formula(paste(var, "~ feeding_status")), data = subset_data)
    results[[var]][[as.character(age)]] <- kruskal_result
  }
}

# print result
for (var in variables) {
  cat(paste("Results for:", var), "\n")
  for (age in age_groups) {
    cat(paste("Age Group:", age), "\n")
    print(results[[var]][[as.character(age)]])
    cat("\n")
  }
}

library(dunn.test)
# Define a function to perform a Dunn test and print the results
dunn_posthoc_test <- function(data, variable, age_group) {
  cat(paste("Results for:", variable, "\nAge Group:", age_group, "\n"))
  result <- dunn.test(data[[variable]], data$feeding_status, method="bonferroni")
  print(result)
  cat("\n\n")
}

# Conducting Dunn's post-hoc test
age_groups <- unique(data_clean$age_group)

for (age in age_groups) {
  subset_data <- data_clean[data_clean$age_group == age,]
  
  # for sleeps_longest_stretch_time_hours
  dunn_posthoc_test(subset_data, "sleeps_longest_stretch_time_hours", age)
  
  # for baby_sleep_daytime_time_hours
  dunn_posthoc_test(subset_data, "baby_sleep_daytime_time_hours", age)
  
  # for baby_total_sleep_time_hours
  dunn_posthoc_test(subset_data, "baby_total_sleep_time_hours", age)
}

summary(data_clean$feeding_status)

library(dplyr)
# Median and quartiles by age group
summary_data <- data_clean %>%
  group_by(feeding_status, age_group) %>%
  summarise(
    median_value = median(sleeps_longest_stretch_time_hours),
    Q25 = quantile(sleeps_longest_stretch_time_hours, 0.25),
    Q75 = quantile(sleeps_longest_stretch_time_hours, 0.75)
  )
print(summary_data)

# Median and quartiles by age group
summary_data2 <- data_clean %>%
  group_by(feeding_status, age_group) %>%
  summarise(
    median_value = median(baby_total_sleep_time_hours),
    Q25 = quantile(baby_total_sleep_time_hours, 0.25),
    Q75 = quantile(baby_total_sleep_time_hours, 0.75)
  )
print(summary_data2)

# Median and quartiles by age group
summary_data3 <- data_clean %>%
  group_by(feeding_status, age_group) %>%
  summarise(
    median_value = median(baby_sleep_daytime_time_hours),
    Q25 = quantile(baby_sleep_daytime_time_hours, 0.25),
    Q75 = quantile(baby_sleep_daytime_time_hours, 0.75)
  )
print(summary_data3)



# Counting the number of people in each category
feeding_status_counts <- data_clean %>%
  group_by(feeding_status) %>%
  summarise(count = n())

# Calculate the percentage of each category
total_count <- sum(feeding_status_counts$count)
feeding_status_counts <- feeding_status_counts %>%
  mutate(percentage = (count / total_count) * 100)

# Print count and duty cycle
print(feeding_status_counts)

# pie chart
pie_chart <- ggplot(feeding_status_counts, aes(x = "", y = count, fill = feeding_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Feeding Status") +
  geom_text(aes(label = paste0(count, " (", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  theme_void()


# print pie chart
print(pie_chart)
# Count the number of people in each age group and category
age_feeding_status_counts <- data_clean %>%
  group_by(age_group, feeding_status) %>%
  summarise(count = n())

# Calculate the total number of persons in each group
age_total_counts <- age_feeding_status_counts %>%
  group_by(age_group) %>%
  summarise(total_count = sum(count))

# Calculate the percentage
age_feeding_status_counts <- age_feeding_status_counts %>%
  left_join(age_total_counts, by = "age_group") %>%
  mutate(percentage = (count / total_count) * 100)

# print result
print(age_feeding_status_counts)




summary(data_clean$slept_through_night)
# Count for each age group and slept_through_night
age_slept_counts <- data_clean %>%
  group_by(age_group) %>%
  summarise(
    slept_count = sum(slept_through_night),
    total_count = n(),
    percentage_slept = (slept_count / total_count) * 100
  )

# print result
print(age_slept_counts)
# List the names of the columns for which need to be replace a blank value of 0
columns_to_replace <- c("where_sleep_alone_separate_room", "where_sleep_other_children_separate_room", "where_sleep_carer_room_crib", "where_sleep_carer_room_sidecar", "where_sleep_carer_bed", "where_sleep_carer_bed_with_partner", "where_sleep_carer_bed_with_partner_children")

# Replaces a blank value in the specified column with 0
data_clean[columns_to_replace] <- lapply(data_clean[columns_to_replace], function(col) {
  col[is.na(col) | col == ""] <- 0
  return(col)
})


library(dplyr)
library(ggplot2)

#make a list of the columns 
columns_to_analyze <- c("where_sleep_alone_separate_room", "where_sleep_other_children_separate_room", "where_sleep_carer_room_crib", "where_sleep_carer_room_sidecar", "where_sleep_carer_bed", "where_sleep_carer_bed_with_partner", "where_sleep_carer_bed_with_partner_children")

# Create an empty data box to store the statistics.
count_df <- data.frame(Location = character(0), Count = numeric(0))

# Counting each sleep location
for (col in columns_to_analyze) {
  count <- sum(data_clean[[col]] == 1, na.rm = TRUE)
  count_df <- rbind(count_df, data.frame(Location = col, Count = count))
}

# visualisation
ggplot(count_df, aes(x = Location, y = Count, fill = Location)) +
  geom_bar(stat = "identity") +
  labs(x = "Sleep Location", y = "Count", title = "Distribution of Sleep Locations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# calculate percentage
count_df$Proportion <- count_df$Count / sum(count_df$Count)

library(ggrepel) 

# Plot the pie chart and add label lines using geom_label_repel
pie_chart <- ggplot(count_df, aes(x = "", y = Proportion, fill = Location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Location") +
  theme_void() +  # Remove background and axes
  geom_label_repel(aes(label = paste0(Count, " (", scales::percent(Proportion), ")")),
                   box.padding = 0.5, segment.color = "grey50", min.segment.length = 0.2)

print(pie_chart)
library(ggplot2)

# plot bar chart

bar_chart <- ggplot(count_df, aes(x = reorder(Location, -Proportion), fill = Location)) +
  geom_bar(aes(y = Count), stat = "identity", position = "dodge") +
  geom_line(aes(y = Proportion * max(Count) * 1.2), color = "blue", group = 1) +
  geom_text(aes(y = Proportion * max(Count) * 1.25, label = scales::percent(Proportion)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Location", y = "Count", fill = "Location") +
  scale_x_discrete(labels = function(x) gsub("where_sleep_", "", x)) +  # Use the gsub function to remove the prefix
  scale_y_continuous(sec.axis = sec_axis(~./max(count_df$Count), name = "Proportion")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),  # Display horizontal labels vertically
        legend.position = "none",
        axis.title.y.right = element_text(color = "blue"))

print(bar_chart)


#Generate dummy variable sleep_location
data_clean <- data_clean %>%
  mutate(
    sleep_location = case_when(
      where_sleep_alone_separate_room == 1 ~ "where_sleep_alone_separate_room",
      where_sleep_other_children_separate_room == 1 ~ "where_sleep_other_children_separate_room",
      where_sleep_carer_room_crib == 1 ~ "where_sleep_carer_room_crib",
      where_sleep_carer_room_sidecar == 1 ~ "where_sleep_carer_room_sidecar",
      where_sleep_carer_bed == 1 ~ "where_sleep_carer_bed",
      where_sleep_carer_bed_with_partner == 1 ~ "where_sleep_carer_bed_with_partner",
      where_sleep_carer_bed_with_partner_children == 1 ~ "where_sleep_carer_bed_with_partner_children",
      TRUE ~ "others"
    )
  )

# print result
print(data_clean$sleep_location)
library(ggplot2)
library(dplyr)

# Statistical description
location_summary <- data_clean %>%
  group_by(sleep_location) %>%
  summarise(count = n())

# Calculate percentage
total_count <- sum(location_summary$count)
location_summary <- location_summary %>%
  mutate(proportion = count / total_count)

# Plot bar charts and line graphs
bar_chart <- ggplot(location_summary, aes(x = reorder(sleep_location, -proportion), y = count, fill = sleep_location)) +
  geom_bar(stat = "identity") +
  labs(x = "Sleep Location", y = "Count", fill = "Sleep Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~ . / total_count, name = "Proportion")) +
  geom_line(aes(y = proportion * max(location_summary$count) * 1.1), color = "blue", group = 1) +
  geom_text(aes(y = proportion * max(location_summary$count) * 1.15, label = scales::percent(proportion)),
            position = position_dodge(width = 0.9), vjust = -0.5, color = "blue")

print(bar_chart)

print(location_summary)

# Convert sleep_location to a number
data_clean$sleep_location_encoded <- as.numeric(factor(data_clean$sleep_location, levels = c("where_sleep_alone_separate_room", "where_sleep_carer_bed", "where_sleep_carer_bed_with_partner", "where_sleep_carer_bed_with_partner_children", "where_sleep_carer_room_crib", "where_sleep_carer_room_sidecar", "where_sleep_other_children_separate_room","others")))

# View the converted data
summary(data_clean$sleep_location_encoded)
# "where_sleep_alone_separate_room" is encoded as 1
#"where_sleep_carer_bed" is encoded as 2
#"where_sleep_carer_bed_with_partner" is encoded as 3
#"where_sleep_carer_bed_with_partner_children" is coded as 4
#"where_sleep_carer_room_crib" is coded as 5
#"where_sleep_carer_room_sidecar" is coded as 6
#"where_sleep_other_children_separate_room" is coded as 7
#"others" is coded as 8

library(dplyr)

result <- data_clean %>%
  group_by(age_group, sleep_location) %>%
  summarize(
    avg_sleeps_longest_stretch = mean(sleeps_longest_stretch_time_hours),
    avg_baby_total_sleep = mean(baby_total_sleep_time_hours),
    avg_baby_sleep_daytime = mean(baby_sleep_daytime_time_hours),
    sd_sleeps_longest_stretch = sd(sleeps_longest_stretch_time_hours),
    sd_baby_total_sleep = sd(baby_total_sleep_time_hours),
    sd_baby_sleep_daytime = sd(baby_sleep_daytime_time_hours)
  )

print(result,n=46)

#Save the result dataframe as a CSV file
write.csv(result, "C:/Users/weiziyao/Desktop/dissertation/result.csv", row.names = FALSE)


#  Define the variables to be analysed
variables <- c("sleeps_longest_stretch_time_hours", "baby_sleep_daytime_time_hours", "baby_total_sleep_time_hours")

age_groups <- unique(data_clean$age_group)

results <- list()

# Kruskal-Wallis test for each variable
for (var in variables) {
  results[[var]] <- list()
  for (age in age_groups) {
    subset_data <- subset(data_clean, age_group == age)
    kruskal_result <- kruskal.test(as.formula(paste(var, "~ sleep_location")), data = subset_data)
    results[[var]][[as.character(age)]] <- kruskal_result
  }
}

# print result
for (var in variables) {
  cat(paste("Results for:", var), "\n")
  for (age in age_groups) {
    cat(paste("Age Group:", age), "\n")
    print(results[[var]][[as.character(age)]])
    cat("\n")
  }
}
library(dunn.test)


# Create a blank text file
file_path <- "C:/Users/weiziyao/Desktop/dissertation/dunn_test_results.txt"
file_conn <- file(file_path, "w")
close(file_conn)

# Define a function to perform a Dunn test and save the results to a file
dunn_posthoc_test_and_save <- function(data, variable, age_group) {
  result <- dunn.test(data[[variable]], data$sleep_location, method = "bonferroni")
  
  # Convert results to character format
  result_text <- capture.output(print(result))
  
  # Save results to file
  cat(paste("Results for:", variable, "\nAge Group:", age_group, "\n"), file = file_path, append = TRUE)
  cat(result_text, file = file_path, append = TRUE)
  cat("\n\n", file = file_path, append = TRUE)
}

# Perform Dunn's post-hoc test and save the results to a file
age_groups <- unique(data_clean$age_group)

for (age in age_groups) {
  subset_data <- data_clean[data_clean$age_group == age,]
  
  # for sleeps_longest_stretch_time_hours
  dunn_posthoc_test_and_save(subset_data, "sleeps_longest_stretch_time_hours", age)
  
  # for baby_sleep_daytime_time_hours
  dunn_posthoc_test_and_save(subset_data, "baby_sleep_daytime_time_hours", age)
  
  # for baby_total_sleep_time_hours
  dunn_posthoc_test_and_save(subset_data, "baby_total_sleep_time_hours", age)
}
#chi-square test
contingency_table2 <- table(data_clean$age_group, data_clean$sleep_location)
chi_square_test2 <- chisq.test(contingency_table2)
print(chi_square_test2)


library(ggplot2)

ggplot(data_clean, aes(y = age_group, fill = sleep_location)) +
  geom_bar(position = "stack") +
  labs(title = "Relationship between Age Group and Sleep Location",
       x = "Count",
       y = "Age Group") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"))  # Resizing the legend

ggplot(data_clean, aes(y = age_group, fill = sleep_location)) +
  geom_bar(position = "stack") +
  labs(title = "Relationship between Age Group and Sleep Location",
       x = "Count",
       y = "Age Group") +
  scale_fill_discrete(labels = function(x) gsub("where_sleep_", "", x)) +  # Use the gsub function to remove the prefix
  theme(axis.text.y = element_text(angle = 0, hjust = 1),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"))  # Resizing the legend




data_clean$cosleep <- ifelse(data_clean$sleep_location %in% c("where_sleep_alone_separate_room", "where_sleep_other_children_separate_room"), 0, 1)
summary(data_clean$cosleep)
result3 <- data_clean %>%
  group_by(age_group, cosleep) %>%
  summarize(
    avg_sleeps_longest_stretch = mean(sleeps_longest_stretch_time_hours),
    avg_baby_total_sleep = mean(baby_total_sleep_time_hours),
    avg_baby_sleep_daytime = mean(baby_sleep_daytime_time_hours),
    sd_sleeps_longest_stretch = sd(sleeps_longest_stretch_time_hours),
    sd_baby_total_sleep = sd(baby_total_sleep_time_hours),
    sd_baby_sleep_daytime = sd(baby_sleep_daytime_time_hours)
  )
print(result3)
write.csv(result3, "C:/Users/weiziyao/Desktop/dissertation/result3.csv", row.names = FALSE)


library(ggplot2)

# Plotting histograms of co-sleeping status for each age group
ggplot(data_clean, aes(x = factor(cosleep), fill = factor(cosleep))) +
  geom_bar(position = "dodge") +
  facet_wrap(~age_group) +
  labs(title = "Distribution of Co-sleeping Status by Age Group",
       x = "Co-sleeping Status (0 = No, 1 = Yes)", y = "Count", fill = "Co-sleeping Status") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue"), name = "Co-sleeping", breaks = c(0, 1), labels = c("No", "Yes"))


# Plot histogram of sleeps_longest_stretch_time_hours
ggplot(data_clean, aes(x = sleeps_longest_stretch_time_hours, fill = factor(cosleep))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  facet_wrap(~age_group) +
  labs(title = "Distribution of Sleeps Longest Stretch by Co-sleeping and Age Group",
       x = "Hours of Sleeps Longest Stretch", y = "Count", fill = "Co-sleeping Status") +
  theme_minimal()

# Plotting box lines for sleeps_longest_stretch_time_hours
ggplot(data_clean, aes(x = factor(cosleep), y = sleeps_longest_stretch_time_hours, fill = factor(cosleep))) +
  geom_boxplot() +
  facet_wrap(~age_group) +
  labs(title = "Boxplot of Sleeps Longest Stretch by Co-sleeping and Age Group",
       x = "Co-sleeping Status", y = "Hours of Sleeps Longest Stretch", fill = "Co-sleeping Status") +
  theme_minimal()

# Plot histogram of baby_total_sleep_time_hours
ggplot(data_clean, aes(x = baby_total_sleep_time_hours, fill = factor(cosleep))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  facet_wrap(~age_group) +
  labs(title = "Distribution of Total Baby Sleep Time by Co-sleeping and Age Group",
       x = "Hours of Total Baby Sleep", y = "Count", fill = "Co-sleeping Status") +
  theme_minimal()

# Plotting a box plot of baby_total_sleep_time_hours
ggplot(data_clean, aes(x = factor(cosleep), y = baby_total_sleep_time_hours, fill = factor(cosleep))) +
  geom_boxplot() +
  facet_wrap(~age_group) +
  labs(title = "Boxplot of Total Baby Sleep Time by Co-sleeping and Age Group",
       x = "Co-sleeping Status", y = "Hours of Total Baby Sleep", fill = "Co-sleeping Status") +
  theme_minimal()

# Plot histogram of baby_sleep_daytime_time_hours
ggplot(data_clean, aes(x = baby_sleep_daytime_time_hours, fill = factor(cosleep))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  facet_wrap(~age_group) +
  labs(title = "Distribution of Baby Daytime Sleep Time by Co-sleeping and Age Group",
       x = "Hours of Baby Daytime Sleep", y = "Count", fill = "Co-sleeping Status") +
  theme_minimal()

# Plotting box plots of baby_sleep_daytime_time_hours
ggplot(data_clean, aes(x = factor(cosleep), y = baby_sleep_daytime_time_hours, fill = factor(cosleep))) +
  geom_boxplot() +
  facet_wrap(~age_group) +
  labs(title = "Boxplot of Baby Daytime Sleep Time by Co-sleeping and Age Group",
       x = "Co-sleeping Status", y = "Hours of Baby Daytime Sleep", fill = "Co-sleeping Status") +
  theme_minimal()


library(lme4)

# Build the model
model <- lmer(sleeps_longest_stretch_time_hours ~ 1+cosleep + feeding_status + (1|age_group), data=data_clean)

# View a summary of the model
summary(model)

# Build the model
model <- lm(sleeps_longest_stretch_time_hours ~ feeding_status, data=data_clean)

# View model summary
summary(model)

# Build the model
model <- lmer(sleeps_longest_stretch_time_hours ~ cosleep + (1|age_group), data=data_clean)

# View a summary of the model
summary(model)

library(lme4)
library(lmerTest)
library(DHARMa)
library(car)
library(ggplot2)
model1 <- lmer(sleeps_longest_stretch_time_hours ~ 1+cosleep + (1+cosleep| age_group), data=data_clean)
summary(model1)
ranef(model1)
resid_vals <- residuals(model1)
hist(resid_vals, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid_vals)
qqline(resid_vals)
fitted_vals <- fitted(model1)
plot(fitted_vals, resid_vals, xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fitted Values")
abline(h = 0, col = "red")
print(ranef(model1))
random_effects_resids <- residuals(model1, type = "deviance")

ggplot(data_clean, aes(x=age_group, y=random_effects_resids)) + geom_boxplot() + theme_minimal()
simulationOutput <- simulateResiduals(fittedModel = model1)
plot(simulationOutput)

require(sjPlot)
plot_model(model1, 
             type="re")





library(lme4)
library(lmerTest)
library(DHARMa)
library(car)
library(ggplot2)
model2 <- lmer(baby_total_sleep_time_hours ~ 1+cosleep + (1| age_group), data=data_clean)
summary(model2)
resid_vals <- residuals(model2)
hist(resid_vals, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid_vals)
qqline(resid_vals)
fitted_vals <- fitted(model2)
plot(fitted_vals, resid_vals, xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fitted Values")
abline(h = 0, col = "red")
print(ranef(model2))
random_effects_resids <- residuals(model2, type = "deviance")

ggplot(data_clean, aes(x=age_group, y=random_effects_resids)) + geom_boxplot() + theme_minimal()
simulationOutput <- simulateResiduals(fittedModel = model2)
plot(simulationOutput)
require(sjPlot)
plot_model(model2, 
           type="re")

library(lme4)
library(lmerTest)
library(DHARMa)
library(car)
library(ggplot2)
model3 <- lmer(baby_sleep_daytime_time_hours ~ 1+cosleep + (1| age_group), data=data_clean)
summary(model3)
resid_vals <- residuals(model3)
hist(resid_vals, main="Histogram of Residuals", xlab="Residuals")
qqnorm(resid_vals)
qqline(resid_vals)
fitted_vals <- fitted(model3)
plot(fitted_vals, resid_vals, xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fitted Values")
abline(h = 0, col = "red")
print(ranef(model3))
random_effects_resids <- residuals(model3, type = "deviance")

ggplot(data_clean, aes(x=age_group, y=random_effects_resids)) + geom_boxplot() + theme_minimal()
simulationOutput <- simulateResiduals(fittedModel = model3)
plot(simulationOutput)

require(sjPlot)
plot_model(model3, 
           type="re")

AIC(model1)
AIC(model2)
AIC(model3)