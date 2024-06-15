getwd()
setwd('C:/Users/magen/OneDrive/Desktop/R work/seminar')

library(tidyverse)
library(dplyr)
library(sandwich)
library(car)
library(lmtest)
library(stargazer)
library(AER)
library(plotrix)
library(lfe)
library(lfe)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(broom)


options(scipen=999) # tell R not to use Scientific notation
options(digits = 7) # controls how many digits are printed by default

# call csv file
Mcdonalds_database <- read.csv("Seminar - survey.csv", header = TRUE)
options(na.action=na.exclude)

columns_to_remove <- c("Response.Type", "IP.Address", "Progress", "Response.ID", "Duration..in.seconds.",
                       "Recorded.Date", "Response.ID", "Recipient.Last.Name", "Recipient.First.Name", "Recipient.Email",
                       "External.Data.Reference", "Location.Latitude", "Location.Longitude", "Distribution.Channel",
                       "User.Language")

# Remove specified columns from 'Mcdonalds_database' using column names
Mcdonalds_database <- Mcdonalds_database[, !names(Mcdonalds_database) %in% columns_to_remove]

# Change the names of the columns
new_col_name <- c(
  "Start.Date" = "StartDate",
  "End.Date" = "EndDate",
  "Finished" = "SurveyFinished",
  "מה.הגיל.שלך." = "Age",
  "מה.המין.שלך." = "Sex" ,
  "מאיפה.את.ה.בארץ." = "LivingArea",
  "מה.הסטטוס.המשפחתי.שלך." = "FamilyStatus",
  "מהי.ההכנסה..נטו..ממוצעת.שלך." = "Income",
  "איך.היית.מתאר.ת.את.התפיסה.העצמית.ואת.דימוי.הגוף.הנוכחיים.שלך." = "SelfEsteem",
  "באיזו.מידה.את.ה.חושב.ת.שהסביבה.משפיעה.על.תפיסת.דימוי.הגוף.שלך." = "EnvironmentAndSelfEsteem",
  "האם.את.ה.חושב.ת.שבובות.ברבי.משקפות.במידה.מספקת.מגוון.צורות.וצבעים.של.נשים.אמיתיות." = "IsBarbieVersatile",
  "באיזו.מידה.לדעתך.בובות.הברבי.משפיעות.לטובה.על.התפיסה.החברתית.למודל.יופי." = "IsBarbiePositiveInfluence",
  "האם.היית.שוקל.ת.לרכוש.את.בובות.הברבי.לילדים.שלך...באופן.היפותטי.במידה.ואין.לך.ילדים." = "BuyingBarbie",
  "באיזו.תדירות.את.ה.אוכל.ת.מקדונלד.ס." = "McDonaldsFrequency",
  "איך.היית.מתאר.ת.את.התפיסה.הכללית.שלך.לגבי.בריאות.התפריט.של.מקדונלדס." = "IsMenuHealthy",
  "כמה.קלוריות.את.ה.מעריך.ה.שיש.בהמבורגר.בסיסי..לא.בארוחה..של.מקדונלדס." = "HamburgerCalories",
  "האם.את.ה.מחשיב.ה.כעת.את.מקדונלד.ס.כאופציה.בריאה.יותר.בהשוואה.ללפני.החשיפה.הפרסומת." = "IsHealthyComparedToBefore",
  "האם.סביר.שתבחר.י.במקדולנד.ס.כארוחת.צהריים." = "ChoiceForMeal",
  "כמה.היית.מוכן.ה.לשלם.על.ארוחת.המבורגר.של.מקדונלד.ס." = "WillingnessToPay",
  "McDonald.s" = "McDonaldsAdd",
  "Barbie" = "BarbieAdd" 
)

names(Mcdonalds_database) <- new_col_name[names(Mcdonalds_database)]


# Remove unnecessary columns + Barbie Columns
names(Mcdonalds_database)

columns_to_remove_for_Mc <- c("IsBarbieVersatile", "IsBarbiePositiveInfluence", "BuyingBarbie", "BarbieAdd")

# Remove specified columns from 'Mcdonalds_database' using column names
Mcdonalds_database <- Mcdonalds_database[, !names(Mcdonalds_database) %in% columns_to_remove_for_Mc]

#Remove non-finished answers
Mcdonalds_database <- Mcdonalds_database[Mcdonalds_database$SurveyFinished != FALSE, ]
Mcdonalds_database <- Mcdonalds_database[, !names(Mcdonalds_database) %in% "SurveyFinished"]

# Arrange the data with dummies - Age
Mcdonalds_database $ Age_18_25 <- ifelse(Mcdonalds_database $ Age == "18-25", 1,0)
Mcdonalds_database $ Age_25_30 <- ifelse(Mcdonalds_database $ Age == "25-30", 1,0)
Mcdonalds_database $ Age_30_40 <- ifelse(Mcdonalds_database $ Age == "30-40", 1,0)
Mcdonalds_database $ Age_40_50 <- ifelse(Mcdonalds_database $ Age == "40-50", 1,0)
Mcdonalds_database $ Age_over_50 <- ifelse(Mcdonalds_database $ Age == "מעל 50", 1,0)

category_mapping <- c("מתחת ל-18" = "Under 18", 
                      "18-25" = "18-25", 
                      "25-30" = "25-30", 
                      "30-40" = "30-40", 
                      "40-50" = "40-50", 
                      "מעל 50" = "Over 50")
unique_hebrew_ages <- unique(Mcdonalds_database$Age)
english_levels <- category_mapping[unique_hebrew_ages]
Mcdonalds_database$Age <- factor(Mcdonalds_database$Age, levels = unique_hebrew_ages)
levels(Mcdonalds_database$Age) <- english_levels

Mcdonalds_database$AgeOver40 <- ifelse(Mcdonalds_database$Age_40_50 == 1 | Mcdonalds_database$Age_over_50 == 1, 1, 0)


# Arrange the data with dummies - Sex
Mcdonalds_database $ Male <- ifelse(Mcdonalds_database $ Sex == "גבר", 1,0)

category_mapping <- c("גבר" = "Male", 
                      "אישה" = "Female")
unique_hebrew_sex <- unique(Mcdonalds_database$Sex)
english_levels <- category_mapping[unique_hebrew_sex]
Mcdonalds_database$Sex <- factor(Mcdonalds_database$Sex, levels = unique_hebrew_sex)
levels(Mcdonalds_database$Sex) <- english_levels

# Arrange the data with dummies - LivingArea
Mcdonalds_database $ Gush_Dan <- ifelse(Mcdonalds_database $ LivingArea == "גוש דן", 1,0)
Mcdonalds_database $ Sharon <- ifelse(Mcdonalds_database $ LivingArea == "שרון", 1,0)
Mcdonalds_database $ Shfela <- ifelse(Mcdonalds_database $ LivingArea == "שפלה", 1,0)
Mcdonalds_database $ South <- ifelse(Mcdonalds_database $ LivingArea == "דרום", 1,0)
Mcdonalds_database $ North <- ifelse(Mcdonalds_database $ LivingArea == "צפון", 1,0)

category_mapping <- c("גוש דן" = "Gush_Dan", 
                      "שרון" = "Sharon", 
                      "שפלה" = "Shfela", 
                      "דרום" = "South",
                      "צפון" = "North", 
                      "אני לא גר/ה בישראל" = "Abroad")
unique_hebrew_LivingArea <- unique(Mcdonalds_database$LivingArea)
english_levels <- category_mapping[unique_hebrew_LivingArea]
Mcdonalds_database$LivingArea <- factor(Mcdonalds_database$LivingArea, levels = unique_hebrew_LivingArea)
levels(Mcdonalds_database$LivingArea) <- english_levels

# Arrange the data with dummies - FamilyStatus
Mcdonalds_database $ Divorcee <- ifelse(Mcdonalds_database $ FamilyStatus == "גרוש/ה", 1,0)
Mcdonalds_database $ Married <- ifelse(Mcdonalds_database $ FamilyStatus == "נשוי/אה", 1,0)
Mcdonalds_database $ Single <- ifelse(Mcdonalds_database $ FamilyStatus == "רווק/ה", 1,0)

category_mapping <- c("אלמן/ה" = "Widower", 
                      "גרוש/ה" = "Divorcee", 
                      "נשוי/אה" = "Married", 
                      "רווק/ה" = "Single")
unique_hebrew_FamilyStatus <- unique(Mcdonalds_database$FamilyStatus)
english_levels <- category_mapping[unique_hebrew_FamilyStatus]
Mcdonalds_database$FamilyStatus <- factor(Mcdonalds_database$FamilyStatus, levels = unique_hebrew_FamilyStatus)
levels(Mcdonalds_database$FamilyStatus) <- english_levels


# Arrange the data with dummies - Income
Mcdonalds_database $ under_5K <- ifelse(Mcdonalds_database $ Income == "עד 5,000 ₪", 1,0)
Mcdonalds_database $ Between_5_7_5K <- ifelse(Mcdonalds_database $ Income == "5,000-7,500 ₪", 1,0)
Mcdonalds_database $ Between_7_5_10K <- ifelse(Mcdonalds_database $ Income == "7,500-10,000 ₪", 1,0)
Mcdonalds_database $ Between_10_20K <- ifelse(Mcdonalds_database $ Income == "10,000-20,000 ₪", 1,0)
Mcdonalds_database $ Over_20K <- ifelse(Mcdonalds_database $ Income == "מעל 20,000 ₪", 1,0)

category_mapping <- c("אין לי הכנסה" = "No Income", 
                      "עד 5,000 ₪" = "Under 5K", 
                      "5,000-7,500 ₪" = "5K - 7.5K", 
                      "7,500-10,000 ₪" = "7.5K - 10K", 
                      "10,000-20,000 ₪" = "10K - 20K",
                      "מעל 20,000 ₪" = "Over 20K")
unique_hebrew_Income <- unique(Mcdonalds_database$Income)
english_levels <- category_mapping[unique_hebrew_Income]
Mcdonalds_database$Income <- factor(Mcdonalds_database$Income, levels = unique_hebrew_Income)
levels(Mcdonalds_database$Income) <- english_levels

income_categories <- c("No Income", "Under 5K", "5K - 7.5K", "7.5K - 10K", "10K - 20K", "Over 20K")
Mcdonalds_database$Income <- factor(Mcdonalds_database$Income, levels = income_categories)
Mcdonalds_database$Income <- as.numeric(Mcdonalds_database$Income)

# Arrange the data with dummies - SelfEsteem
category_mapping <- c("שליליים מאוד" = 1, "שליליים" = 2, "ניטראליים" = 3, "חיוביים" = 4, "חיוביים מאוד" = 5)
Mcdonalds_database$SelfEsteem <- as.numeric(factor(Mcdonalds_database$SelfEsteem, levels = names(category_mapping)))

# Arrange the data with dummies - EnvironmentAndSelfEsteem
category_mapping <- c("בכלל לא" = 1, "מעט" = 2, "במתינות" = 3, "במידה רבה" = 4)
Mcdonalds_database$EnvironmentAndSelfEsteem <- as.numeric(factor(Mcdonalds_database$EnvironmentAndSelfEsteem, levels = names(category_mapping)))

# Arrange the data with dummies - McDonaldsFrequency
category_mapping <- c("אף פעם לא" = 1, "לעיתים רחוקות" = 2, "לפעמים" = 3, "בתדירות גבוהה" = 4, "באופן קבוע" = 5)
Mcdonalds_database$McDonaldsFrequency <- as.numeric(factor(Mcdonalds_database$McDonaldsFrequency, levels = names(category_mapping)))

# Arrange the data with dummies - IsMenuHealthy
category_mapping <- c("מאוד לא בריא" = 1, "לא בריא" = 2, "ניטראלי" = 3, "בריא" = 4, "מאוד בריא" = 5)
Mcdonalds_database$IsMenuHealthy <- as.numeric(factor(Mcdonalds_database$IsMenuHealthy, levels = names(category_mapping)))

# Arrange the data with dummies - HamburgerCalories
category_mapping <- c("פחות מ-300" = 1, "300-400" = 2, "500-600" = 3, "600-700" = 4, "יותר מ-700" = 5)
Mcdonalds_database$HamburgerCalories <- as.numeric(factor(Mcdonalds_database$HamburgerCalories, levels = names(category_mapping)))

# Arrange the data with dummies - IsHealthyComparedToBefore
category_mapping <- c("בהחלט לא" = 1, "כנראה שלא" = 2, "ניטראלי" = 3, "כנראה שכן" = 4, "בהחלט כן" = 5)
Mcdonalds_database$IsHealthyComparedToBefore <- as.numeric(factor(Mcdonalds_database$IsHealthyComparedToBefore, levels = names(category_mapping)))

# Arrange the data with dummies - ChoiceForMeal
category_mapping <- c("לא סביר כלל" = 1, "לא סביר" = 2, "ללא שינוי" = 3, "סביר" = 4, "סביר מאוד" = 5)
Mcdonalds_database$ChoiceForMeal <- as.numeric(factor(Mcdonalds_database$ChoiceForMeal, levels = names(category_mapping)))

# Arrange the data with dummies - WillingnessToPay
category_mapping <- c("עד 60 ₪" = 1, "עד 50 ₪" = 2, "עד 40 ₪" = 3)
Mcdonalds_database$WillingnessToPay <- as.numeric(factor(Mcdonalds_database$WillingnessToPay, levels = names(category_mapping)))

# Arrange the data with dummies - McDonaldsAdd
Mcdonalds_database $ McDonaldsAdd_Old<- ifelse(Mcdonalds_database $ McDonaldsAdd == "old", 1,0)
Mcdonalds_database $ McDonaldsAdd_New<- ifelse(Mcdonalds_database $ McDonaldsAdd == "new", 1,0)

# Reorder columns in the database
desired_order <- c(
  "StartDate", "EndDate",
  "Age", "Age_18_25", "Age_25_30", "Age_30_40", "Age_40_50", "Age_over_50", "AgeOver40",
  "Sex", "Male", 
  "LivingArea", "Gush_Dan", "Sharon", "Shfela", "South", "North",
  "FamilyStatus", "Divorcee", "Married", "Single", 
  "Income", "under_5K", "Between_5_7_5K", "Between_7_5_10K", "Between_10_20K", "Over_20K", 
  "SelfEsteem", "EnvironmentAndSelfEsteem", "McDonaldsFrequency", "IsMenuHealthy","McDonaldsAdd_Old",
  "McDonaldsAdd_New", "HamburgerCalories", "IsHealthyComparedToBefore", "ChoiceForMeal", "WillingnessToPay"
)
Mcdonalds_database <- Mcdonalds_database[, c(desired_order, setdiff(names(Mcdonalds_database), desired_order))]

colnames(Mcdonalds_database)

######################################################################################

##### correlation matrix
install.packages("corrplot")
install.packages("reshape2")

library(ggplot2)
library(reshape2)
library(corrplot)


numeric_columns <- sapply(Mcdonalds_database, is.numeric)
database_numeric <- Mcdonalds_database[, numeric_columns]

correlation_matrix <- cor(database_numeric, use = "complete.obs")

print(correlation_matrix)

correlation_long <- melt(correlation_matrix)

ggplot(data = correlation_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Matrix Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))



######################################################################################



# Model 1 - The effect of the new advertisement on the willingness to buy McDonalds
model1 <- lm(ChoiceForMeal ~  McDonaldsAdd_New,  data = Mcdonalds_database)
summary(model1)

add_freq <- table(Mcdonalds_database$ChoiceForMeal, Mcdonalds_database$McDonaldsAdd)
add_freq_df <- as.data.frame(add_freq)
colnames(add_freq_df) <- c("ChoiceForMeal", "McDonaldsAdd", "Frequency")
add_freq_df$McDonaldsAdd <- factor(add_freq_df$McDonaldsAdd, levels = c("old", "new"))


ggplot(add_freq_df, aes(x = factor(ChoiceForMeal), y = Frequency, fill = factor(McDonaldsAdd))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Frequency), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +  # Add labels
  labs(x = "ChoiceForMeal", y = "Frequency", fill = "McDonaldsAdd") +
  ggtitle("Breakdown of ChoiceForMeal by McDonaldsAdd") +
  scale_fill_manual(values = c("darkseagreen3", "lightskyblue")) +  # Set custom colors
  theme_minimal()


# Model 1a - The effect of the new advertisement on the willingness to buy McDonalds when it's binary

Mcdonalds_database_binary <- Mcdonalds_database
Mcdonalds_database_binary$ChoiceForMeal <- ifelse(Mcdonalds_database_binary$ChoiceForMeal %in% c(1, 2), 0,
                                                         ifelse(Mcdonalds_database_binary$ChoiceForMeal %in% c(4, 5), 1, 3))
Mcdonalds_database_binary <- Mcdonalds_database_binary[!Mcdonalds_database_binary$ChoiceForMeal %in% 3, ]


model1a <- lm(ChoiceForMeal ~  McDonaldsAdd_New,data = Mcdonalds_database_binary)
summary(model1a)

add_freq_binary <- table(Mcdonalds_database_binary$ChoiceForMeal, Mcdonalds_database_binary$McDonaldsAdd)
add_freq_binary_df <- as.data.frame(add_freq_binary)
colnames(add_freq_binary_df) <- c("ChoiceForMeal", "McDonaldsAdd", "Frequency")
add_freq_binary_df$McDonaldsAdd <- factor(add_freq_binary_df$McDonaldsAdd, levels = c("old", "new"))


ggplot(add_freq_binary_df, aes(x = factor(ChoiceForMeal), y = Frequency, fill = factor(McDonaldsAdd))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Frequency), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +  # Add labels
  labs(x = "ChoiceForMeal", y = "Frequency", fill = "McDonaldsAdd") +
  ggtitle("Breakdown of ChoiceForMeal by McDonaldsAdd") +
  scale_fill_manual(values = c("darkseagreen3", "lightskyblue")) +  # Set custom colors
  theme_minimal()


#models

models1 <- list(model1, model1a)

stargazer(models1,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", 
          title = "The Effect Of The Advertisement On The Choice For A Meal", style = "apsr", out = "Models1.html")

######################################################################################

# Model 2 - The effect of the new advertisement, age, sex and income on the willingness to buy McDonalds 
model2 <- lm(ChoiceForMeal ~  McDonaldsAdd_New + Male + AgeOver40 + Income,
             data = Mcdonalds_database)
summary(model2)


model2a <- lm(ChoiceForMeal ~  McDonaldsAdd_New + Male + AgeOver40 + Income,
             data = Mcdonalds_database_binary)
summary(model2a)

models2 <- list(model2, model2a)

stargazer(models2,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text",
          title = "The Effect Of The Advertisement, Sex, Age & Income On The Choice For A Meal", style = "apsr", out = "Models2.html")

tidy_model2 <- tidy(model2, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model2, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

######################################################################################

# Model 3 - The effect of the new advertisement, age, sex and frequency and income on the willingness to buy McDonalds 
model3 <- lm(ChoiceForMeal ~  McDonaldsAdd_New + Male + AgeOver40 + Income + McDonaldsFrequency,
             data = Mcdonalds_database)
summary(model3)

model3a <- lm(ChoiceForMeal ~  McDonaldsAdd_New + Male + AgeOver40 + Income + McDonaldsFrequency,
             data = Mcdonalds_database_binary)
summary(model3a)

models3 <- list(model3, model3a)

stargazer(models3,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", 
          title = "The Effect Of The Advertisement, Sex, Age, Income & Frequency On The Choice For A Meal", style = "apsr", out = "Models3.html")


tidy_model3 <- tidy(model3, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model3, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

######################################################################################

# Model 4 - The effect of the sex, Age, frequency and health opinion on the willingness to buy McDonalds 
model4 <- lm(ChoiceForMeal ~ Male + AgeOver40 + McDonaldsFrequency + IsMenuHealthy + McDonaldsAdd_New + AgeOver40*McDonaldsAdd_New ,
             data = Mcdonalds_database)
summary(model4)

model4a <- lm(ChoiceForMeal ~ Male + AgeOver40 +McDonaldsFrequency + IsMenuHealthy + McDonaldsAdd_New,
             data = Mcdonalds_database_binary)
summary(model4a)


tidy_model4 <- tidy(model4, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model4, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()


models4 <- list(model4, model4a)

stargazer(models4,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text",
          title = "The Effect Of The Advertisement, Sex, Helath Opinion & Frequency On The Choice For A Meal", style = "apsr", out = "Models4.html")


modelsbase <- list(model2, model3, model4)

stargazer(modelsbase,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text",
          title = "Comparison Between Different Models", style = "apsr", out = "modelsbase.html")


######################################################################################

# Model 5 - The effect of the living area and the family status opinion on the willingness to buy McDonalds 
model5 <- lm(ChoiceForMeal ~ LivingArea + FamilyStatus + Male,
             data = Mcdonalds_database)
summary(model5)


model5a <- lm(ChoiceForMeal ~ LivingArea + FamilyStatus + Male,
             data = Mcdonalds_database_binary)
summary(model5a)

models5 <- list(model5)

stargazer(models5,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", 
          title = "The Effect Of The Living Areaa And Family Status On The Choice For A Meal", style = "apsr", out = "Models5.html")



tidy_model5 <- tidy(model5, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model5, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

######################################################################################

# Model 6 - The effect of the new advertisement, sex and frequency and health opinion on the burger calories opinion 
model6 <- lm(HamburgerCalories ~ McDonaldsAdd_New + Male + AgeOver40 + McDonaldsFrequency + IsMenuHealthy,
             data = Mcdonalds_database)
summary(model6)

models6 <- list(model6)

stargazer(models6,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", 
          title = "The Effect Of The Advertisement, Age, Sex, Helath Opinion & Frequency On The Calories Opinion", style = "apsr", out = "Models6.html")

tidy_model6 <- tidy(model6, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model6, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

######################################################################################

#Main model
# Model 7 - The effect of the sex, age, self esteem, new advertisement and the relationship between them on the willingness to buy McDonalds 
model7 <- lm(ChoiceForMeal ~  Male + AgeOver40 + SelfEsteem + IsMenuHealthy + McDonaldsFrequency  + McDonaldsAdd_New,
             data = Mcdonalds_database)
summary(model7)

model7a <- lm(ChoiceForMeal ~  Male + AgeOver40 + SelfEsteem + IsMenuHealthy + McDonaldsFrequency + AgeOver40*Male+ McDonaldsFrequency*AgeOver40 + McDonaldsAdd_New + McDonaldsAdd_New*AgeOver40,
             data = Mcdonalds_database)
summary(model7)


models7 <- list(model7, model7a)

stargazer(models7,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text",
          title = "Main Regression", style = "apsr", out = "Models7.html")

tidy_model7 <- tidy(model7a, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model7, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()


######################################################################################

# Model 8 - The effect of the sex, age, new advertisement and the relationship between them on the willingness to pay for a meal 
model8 <- lm(WillingnessToPay ~  Male + AgeOver40 + SelfEsteem + McDonaldsAdd_New,
             data = Mcdonalds_database)
summary(model8)

models8 <- list(model8)

stargazer(models8,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", 
          title = "The Effect On The Willingness To Pay", style = "apsr", out = "Models8.html")



######################################################################################

# Model 9 - The effect of the sex, age, new advertisement and health opinipn on the willingness to pay for a meal 
model9 <- lm(WillingnessToPay ~  Male + AgeOver40 + McDonaldsAdd_New + McDonaldsFrequency + IsMenuHealthy + McDonaldsAdd_New*IsMenuHealthy ,
             data = Mcdonalds_database)
summary(model9)

models9 <- list(model8 ,model9 )

stargazer_output <- stargazer(models9,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text",
          title = "The Effect On The Willingness To Pay", style = "apsr", out = "Models9.html")


tidy_model9 <- tidy(model9, conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model9, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

######################################################################################

# Model 10 - The effect of the sex, age, new advertisement and health opinipn on the Health 
model10 <- lm(IsHealthyComparedToBefore ~ McDonaldsAdd_New + AgeOver40 + McDonaldsFrequency + IsMenuHealthy + McDonaldsAdd_New*IsMenuHealthy ,
             data = Mcdonalds_database)
summary(model10)

models10 <- list(model10 )

stargazer_output <- stargazer(model10,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
                              type = "text", 
                              title = "The Effect On The Health Opinion", style = "apsr", out = "Models10.html")


tidy_model10 <- tidy(model10 , conf.int = TRUE)

# Create a bar graph for the coefficients with confidence intervals
ggplot(tidy_model9, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()


#AAdditional Graps

######################################################################################
# LivingArea by Sex
living_area_sex_freq <- table(Mcdonalds_database$LivingArea, Mcdonalds_database$Sex)
living_area_sex_df <- as.data.frame(living_area_sex_freq)
colnames(living_area_sex_df) <- c("LivingArea", "Sex", "Frequency")

total_living_area <- living_area_sex_df %>%
  group_by(LivingArea) %>%
  summarize(Frequency = sum(Frequency)) %>%
  mutate(Sex = "Total")

combined_living_area_df <- bind_rows(living_area_sex_df, total_living_area)

ggplot(combined_living_area_df, aes(x = factor(LivingArea), y = Frequency, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Frequency), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +  # Add labels
  labs(x = "Living Area", y = "Frequency", fill = "Sex") +
  ggtitle("Breakdown of Living Area by Sex") +
  scale_fill_manual(values = c("lightpink", "lightskyblue", "darkseagreen3")) +  # Set custom colors
  theme_minimal()

######################################################################################
# Age by Sex

age_sex_freq <- table(Mcdonalds_database$Age, Mcdonalds_database$Sex)
age_sex_df <- as.data.frame(age_sex_freq)
colnames(age_sex_df) <- c("Age", "Sex", "Frequency")

total_age <- age_sex_df %>%
  group_by(Age) %>%
  summarize(Frequency = sum(Frequency)) %>%
  mutate(Sex = "Total")

combined_age_df <- bind_rows(age_sex_df, total_age)

ggplot(combined_age_df, aes(x = factor(Age), y = Frequency, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Frequency), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +  # Add labels
  labs(x = "Age", y = "Frequency", fill = "Sex") +
  ggtitle("Breakdown of Age by Sex") +
  scale_fill_manual(values = c("lightpink", "lightskyblue", "darkseagreen3")) +  # Set custom colors
  theme_minimal()

######################################################################################




