install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
library(ggplot2)
install.packages("vcd")
library(vcd)
install.packages("gmodels")
library(gmodels)
#Install packages and libraries
MD <- read_csv("C:/Users/merce/Downloads/MD.csv")
View(MD)
#Upload CSV file and View
variables <- MD[, c("HighBlood", "Stroke","Overweight", "Arthritis", "Diabetes", "Hyperlipidemia", "BackPain", "Anxiety", "Allergic_rhinitis", "Reflux_esophagitis", "Asthma", "Income", "TotalCharge", "Additional_charges", "ReAdmis")]
# Create data frame "variable"
variables
# view data frame "Variables"
summary(variables)
#summarize variables 
MD$HighBlood <- factor(MD$HighBlood, levels = c(0, 1), labels = c("No", "Yes"))
MD$Stroke <- factor(MD$Stroke, levels = c(0, 1), labels = c("No", "Yes"))
MD$Overweight <- factor(MD$Overweight, levels = c(0, 1), labels = c("No", "Yes"))
MD$Arthritis <- factor(MD$Arthritis, levels = c(0, 1), labels = c("No", "Yes"))
MD$Diabetes <- factor(MD$Diabetes, levels = c(0, 1), labels = c("No", "Yes"))
MD$Hyperlipidemia <- factor(MD$Hyperlipidemia, levels = c(0, 1), labels = c("No", "Yes"))
MD$BackPain <- factor(MD$BackPain, levels = c(0, 1), labels = c("No", "Yes"))
MD$Anxiety <- factor(MD$Anxiety, levels = c(0, 1), labels = c("No", "Yes"))
MD$Allergic_rhinitis <- factor(MD$Allergic_rhinitis, levels = c(0, 1), labels = c("No", "Yes"))
MD$Reflux_esophagitis <- factor(MD$Reflux_esophagitis, levels = c(0, 1), labels = c("No", "Yes"))
MD$Asthma <- factor(MD$Asthma, levels = c(0, 1), labels = c("No", "Yes"))
MD$ReAdmis <- factor(MD$ReAdmis)
#change to categorical data
category_counts <- table(MD$Asthma)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Asthma")
#Pie chart for Asthma
category_counts <- table(MD$HighBlood)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of HighBlood")
#Pie chart for High Blood
category_counts <- table(MD$Stroke)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Stroke")
#pie chart for stroke
category_counts <- table(MD$Overweight)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Overweight")
#pie chart Overweight
category_counts <- table(MD$Arthritis)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Arthritis")
#pie chart arthritis
category_counts <- table(MD$Diabetes)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Diabetes")
#pie chart Diabetes
category_counts <- table(MD$Hyperlipidemia)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Hyperlipidemia")
#Pie chart Hyperlipidemia
category_counts <- table(MD$BackPain)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of BackPain")
#Back pain pie chart
category_counts <- table(MD$Anxiety)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Anxiety")
#pie chart for anxiety
category_counts <- table(MD$Allergic_rhinitis)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Allergic rhinitis")
#pie chart Allergic Rhinitis
category_counts <- table(MD$Reflux_esophagitis)
pie(category_counts, col = rainbow(length(category_counts)),
    main = "Pie Chart of Reflux_esophagitis")
#pie chart Reflux Esophagitis
ggplot(MD, aes(x = Income)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Income",
       x = "Income",
       y = "Count") +
  theme_minimal()
#Income uni variate chart
ggplot(MD, aes(x = TotalCharge)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Histogram of TotalCharge",
       x = "TotalCharge",
       y = "Count") +
  theme_minimal()
#Histogram TC
ggplot(MD, aes(x = Additional_charges)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Additional Charges",
       x = "Additional Charges",
       y = "Count") +
  theme_minimal()
#additional charge
ggplot(MD, aes(x = factor(ReAdmis))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of ReAdmis",
       x = "ReAdmis",
       y = "Count") +
  theme_minimal()
#Readmis
ggplot(MD, aes(x = Asthma, fill = ReAdmis)) +
  geom_bar() +
  xlab("Asthma") +
  ylab("Count") +
  ggtitle("Stacked Bar Chart of Asthma and ReAdmis")
#Stacked bar chart
ggplot(MD, aes(x = HighBlood, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "HighBlood", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of HighBlood and ReAdmis")
#Stacked Bar chart HB and RA
ggplot(MD, aes(x = HighBlood, fill = Asthma)) +
  geom_bar(position = "fill") +
  labs(x = "Arthritis", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Arthritis and ReAdmis")
#stack barchart arthritis and ReAdmis
ggplot(MD, aes(x = Stroke, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "Stroke", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Stroke and ReAdmis")
#stacked bar chart Stroke and ReAdmis
ggplot(MD, aes(x = Diabetes, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "Diabetes", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Diabetes and ReAdmis")
#stacked barchart Diabetes and ReAdmis
ggplot(MD, aes(x = Hyperlipidemia, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "Hyperlipidemia", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Hyperlipidemia and ReAdmis")
#stacked bar chart HL and Readmis
ggplot(MD, aes(x = BackPain, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "BackPain", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of BackPain and ReAdmis")
#Stacked bar chart BP and ReAdmis
ggplot(MD, aes(x = Anxiety, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "Anxiety", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Anxiety and ReAdmis")
#stacked barchart Anxiety and Readmis
ggplot(MD, aes(x = Allergic_rhinitis, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "Allergic_rhinitis", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Allergic_rhinitis and ReAdmis")
#Stacked bar chart AR and ReAdmis'
ggplot(MD, aes(x = Reflux_esophagitis, fill = ReAdmis)) +
  geom_bar(position = "fill") +
  labs(x = "Reflux_esophagitis", y = "Proportion", fill = "ReAdmis") +
  ggtitle("Stacked Bar Plot of Reflux_esophagitis and ReAdmis")
#Stacked barchart for RE and ReAdmis
ggplot(MD, aes(x = ReAdmis, y = Income)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "ReAdmis", y = "Income", fill = "ReAdmis") +
  ggtitle("Boxplot of Income and ReAdmis")
#ReAdmis and Income box plot
ggplot(MD, aes(x = ReAdmis, y = Additional_charges)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "ReAdmis", y = "Additional_charges", fill = "ReAdmis") +
  ggtitle("Boxplot of Additional Charges and ReAdmis")
#boxplot RA and AC
ggplot(MD, aes(x = ReAdmis, y = TotalCharge)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "ReAdmis", y = "TotalCharge", fill = "ReAdmis") +
  ggtitle("Boxplot of TotalCharge and ReAdmis")
#ReAdmis and Total Charge
MD <- read_csv("C:/Users/merce/Downloads/MD.csv")
#import MD again
Lmodel <- glm(ReAdmis ~ HighBlood + Stroke + Overweight + Arthritis + Diabetes + Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + Income + TotalCharge + Additional_charges, data = MD, family = binomial)
#create logistic model
summary(Lmodel)
#initial models summary stats
initial_model <- glm(ReAdmis ~ HighBlood + Stroke + Overweight + Arthritis + Diabetes + Hyperlipidemia + BackPain + Anxiety + Allergic_rhinitis + Reflux_esophagitis + Asthma + Income + TotalCharge + Additional_charges, data = MD, family = binomial)
#refit initial model
final_model <- step(initial_model, direction = "backward")
#backwards step elimination
summary(final_model)
#summary model
coefficients <- coef(final_model)
print(coefficients)
#reduced model coefficients
aic_initial <- AIC(initial_model)
print(aic_initial)
#initial model
aic_final <- AIC(final_model)
print(aic_final)
#final model
predictions <- predict(final_model, type = "response")
#prediction of outcome
predicted_classes <- ifelse(predictions > 0.5, "Yes", "No")
#probabilities to class labels
confusion_matrix <- table(Actual = MD$ReAdmis, Predicted = predicted_classes)
#create confusion matrix
print(confusion_matrix)
#print confusion matrix
TP <- 3322
TN <- 5736
FP <- 595
FN <- 347
#confusion matrix
accuracy <- (TP + TN) / (TP + TN + FP + FN)
#calculate accuracy
print(accuracy)
#print accuracy

