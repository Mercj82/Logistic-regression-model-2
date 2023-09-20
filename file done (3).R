install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
install.packages("corrplot")
library(corrplot)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(ggplot2)
library(tidyr)
#upload packages and libraries
MD <- read_csv("C:/Users/merce/Downloads/medical_raw_data.csv")
#uploading raw data
View(MD)
#View data frame
str(MD)
#Viewing data types and their examples
duplicates <- duplicated(MD)
#Checking data frame for duplicates
print(MD[duplicates, ])
#Print duplicate row 0 found
MD <- MD[, -1]
#Delete the first column due to it being repetitive 
MD <- MD %>% 
  mutate(index = CaseOrder) %>% 
  select(-CaseOrder)
#Setting Index
missing_counts <- colSums(is.na(MD))
#Checking the missing values of each column
print(missing_counts)
#Show the sum of missing value
colnames(MD)[colnames(MD) == "Item1"] <- "Timely admission"
colnames(MD)[colnames(MD) == "Item2"] <- "Timely treatment"
colnames(MD)[colnames(MD) == "Item3"] <- "Timely visits"
colnames(MD)[colnames(MD) == "Item4"] <- "Reliability"
colnames(MD)[colnames(MD) == "Item5"] <- "Options"
colnames(MD)[colnames(MD) == "Item6"] <- "Hours of treatment"
colnames(MD)[colnames(MD) == "Item7"] <- "Courteous staff"
colnames(MD)[colnames(MD) == "Item8"] <- "Evidence of active listening from doctor"
#Change Item 1-8 names to relative descriptions
colnames(MD)
#View all column names
convert_to_numeric <- function(x) {
  ifelse(x == "Yes", 1, 0)
}
#Convert columns that use variables Yes and No to numeric 
MD[, c("HighBlood", "Stroke", "Complication_risk", "Arthritis", "Diabetes", 
"Hyperlipidemia", "BackPain", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "ReAdmis", "Soft_drink")] <- lapply(MD[, c("HighBlood", "Stroke", "Complication_risk", "Arthritis", "Diabetes", "Hyperlipidemia",
                 "BackPain", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "ReAdmis", "Soft_drink")], convert_to_numeric)
#Converting Yes/NO to  numeric
MD$Zip <- as.character(MD$Zip)
#Convert zip codes to character type
MD$Zip <- str_pad(MD$Zip, width = 5, pad = "0")
#Add leading zeros to zip code
ggplot(data = MD, aes(x = Lng, y = Lat)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()
# lat and lng
children_median <- median(MD$Children, na.rm = TRUE)
#Replace null values for median 
MD$Children[is.na(MD$Children)] <- children_median
# Replace missing values with the mean
median_income <- median(MD$Income, na.rm = TRUE)
# Calculate the mean of the non-missing values
MD$Income[is.na(MD$Income)] <- median_income
# Replace missing values with the mean
mean_Age <- mean(MD$Age, na.rm = TRUE)
# Calculate the mean of the non-missing values
MD$Age[is.na(MD$Age)] <- mean_Age
#Replace missing values with the mean
mean_Initial_days <- mean(MD$Initial_days, na.rm = TRUE)
# Calculate the mean of the non-missing values
MD$Initial_days[is.na(MD$Initial_days)] <- mean_Initial_days
#Replace missing values with the mean
missing_sum <- colSums(is.na(MD))
# Calculate the sum of missing values
print(missing_sum)
# Print the sum of missing values
# Print the sum of missing values
MD$Age <- round(MD$Age)
#Round the variable age
print(MD$Age)
#Print the new rounded age column 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Function to calculate mode
mode_overweight <- Mode(MD$Overweight)
#calculate mode for overweight column
MD$Overweight[is.na(MD$Overweight)] <- mode_overweight
#Replace null values with mode
mode_Anxiety <- Mode(MD$Anxiety)
#calculate mode for anxiety
MD$Anxiety[is.na(MD$Anxiety)] <- Mode(MD$Anxiety)
#Impute mode in anxiety column 
mode_Soft_drink <- Mode(MD$Soft_drink)
#Calculate mode for soft drink
MD$Soft_drink[is.na(MD$Soft_drink)] <- Mode(MD$Soft_drink)
#Fill in NA value with mode
View(MD)
#View MD
variables_of_interest <- c("Lat", "Lng", "Population", "Children", "Age", "Income",
                           "ReAdmis", "VitD_levels", "Doc_visits", "Full_meals_eaten",
                           "VitD_supp", "Soft_drink", "HighBlood", "Stroke",
                           "Complication_risk", "Overweight", "Arthritis", "Diabetes",
                           "Hyperlipidemia", "BackPain", "Anxiety", "Allergic_rhinitis",
                           "Reflux_esophagitis", "Asthma", "Initial_days", "TotalCharge",
                           "Additional_charges", "Timely admission", "Timely treatment",
                           "Timely visits", "Reliability", "Options", "Hours of treatment")
#Create new dataframe
df <- MD[, variables_of_interest]
#Create new data frame
non_numeric_cols <- sapply(df, function(x) !is.numeric(x))
#Identify the columns in df that are not numeric
df[!non_numeric_cols] <- lapply(df[!non_numeric_cols], as.numeric)
#Convert  the non nuemric column excluding char or factors
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr
  outliers <- x[x < lower_fence | x > upper_fence]
  return(outliers)
}
# Detect outliers in each column
outliers_list <- lapply(df, detect_outliers)
# Identify columns with outliers
columns_with_outliers <- names(df)[sapply(outliers_list, length) > 0]
#Identify columns with outliers
print(columns_with_outliers)
#print column outliers
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Lat)) +
  labs(x = "", y = "Lat") +
  theme_bw() +
  ggtitle("Box Plot of Lat")
#Boxplot Lat
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Lng)) +
  labs(x = "", y = "Lng") +
  theme_bw() +
  ggtitle("Box Plot of Lng")
#Boxplot for LNG
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Population)) +
  labs(x = "", y = "Population") +
  theme_bw() +
  ggtitle("Box Plot of Population")
#Boxplot for Population
ggplot(MD) +
  geom_boxplot(aes(x = "", y = Income)) +
  labs(x = "", y = "Income") +
  theme_bw() +
  ggtitle("Box Plot of Income")
#Boxplot for Income
boxplot_TotalCharge <- boxplot(df$TotalCharge)
#boxplot for total charge 
boxplot_Additional_charges <- boxplot(df$Additional_charges)
#Additional Charge boxplot
boxplot_Timely_admission <- boxplot(df$`Timely admission`)
#boxplot for TA
boxplot_Timely_treatment <- boxplot(df$`Timely treatment`)
#Boxplot TT
boxplot_Timely_visits <- boxplot(df$`Timely visits`)
#Boxplot TV 
boxplot_Reliability <- boxplot(df$Reliability)
#boxplot Reliabilty
boxplot_Options <- boxplot(df$Options)
#Boxplot options 
boxplot_Hours_of_treatment <- boxplot(df$`Hours of treatment`)
#boxplot Hours of Treatment
percentage_outliers <- length(columns_with_outliers) / nrow(MD) * 100
# Find the percentage of outliers
percentage_remaining <- 100 - percentage_outliers
#Calculate the percentage remaining
cat("Percentage of outliers:", percentage_outliers, "%\n")
cat("Percentage of data remaining:", percentage_remaining, "%\n")
#print results
Unclean <- read_csv("C:/Users/merce/Downloads/medical_raw_data.csv")
#Unclean data
columns <- c("Children", "Soft_drink", "Anxiety", "Income", "Overweight", "Initial_days", "Age")
# columns for unclean histogram
Unclean[columns] <- lapply(Unclean[columns], function(x) as.numeric(x, na.rm = TRUE))
#Turn char values to numeric
par(mfrow = c(2, 4))
#set up layout
for (col in columns) {
  values <- Unclean[[col]]
  values <- values[!is.na(values)]
  # Filter out missing values
  if (!is.null(values) && length(values) > 0) 
    hist(values, main = col, xlab = col, col = "lightblue")
}
#Create histogram for unclean

columns <- c("Children", "Soft_drink", "Anxiety", "Income", "Overweight", "Initial_days", "Age")
#Select column from new_MD
par(mfrow = c(2, 4))
#set the layout of the subplots
for (col in columns) {
  hist(MD[[col]], main = col, xlab = col, col = "lightblue")
}
#Histogram for cleaned data MD 
df <- MD[, c("Income", "Lat", "Lng", "VitD_levels", "Initial_days", "Additional_charges", "TotalCharge")]
#selected variables for eigen values
PCA.pca <- prcomp(df, center = TRUE, scale. = TRUE)
#Perform PCA
loading_matrix <- PCA.pca$rotation
#loading matrix
print(loading_matrix)
#print loading matrix
singular_values <- PCA.pca$sdev^2
#create sequence
plot(1:length(singular_values), singular_values, type = "b",
     xlab = "Principal Component", ylab = "Variance Explained",
     main = "Scree Plot")
#create scree plot
file_path <- "C:/Users/merce/Downloads/MD.csv"
#file path
write.csv(MD, file = file_path, row.names = FALSE)
#Create csv