getwd()
setwd('C:/Users/ziv-t/Desktop/RStudio/Seminar')

library(readxl)
library(dplyr)
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
install.packages("corrplot")
library(ggplot2)
library(reshape2)
library(corrplot)

options(scipen=999) # tell R not to use Scientific notation
options(digits = 7) # controls how many digits are printed by default


# Read the Excel file
database <- read_excel("Seminar - survey.xlsx")

# Delete irrelevant columns
database <- select(database, -c(StartDate, EndDate, Status, IPAddress, Progress, 
                                `Duration (in seconds)`, RecordedDate, ResponseId, 
                                RecipientLastName, RecipientFirstName, RecipientEmail, 
                                ExternalReference, LocationLongitude, LocationLatitude, DistributionChannel, 
                                UserLanguage))


# Set the second row as the new headers
names(database) <- database[1, ]

# Delete specified columns- Keep only Barbie Q
database <- select(database, -c("באיזו תדירות את/ה אוכל/ת מקדונלד'ס?",
                                "איך היית מתאר/ת את התפיסה הכללית שלך לגבי בריאות התפריט של מקדונלדס?",
                                "כמה קלוריות את/ה מעריך/ה שיש בהמבורגר בסיסי (לא בארוחה) של מקדונלדס?",
                                "האם את/ה מחשיב/ה כעת את מקדונלד'ס כאופציה בריאה יותר בהשוואה ללפני החשיפה הפרסומת?",
                                "האם סביר שתבחר/י במקדולנד'ס כארוחת צהריים?",
                                "כמה היית מוכן/ה לשלם על ארוחת המבורגר של מקדונלד'ס?",
                                "McDonald's"))


# Rename columns
database <- rename(database,
                   Age = "מה הגיל שלך?",
                   Sex = "מה המין שלך?",
                   LivingArea = "מאיפה את/ה בארץ?",
                   FamilyStatus = "מה הסטטוס המשפחתי שלך?",
                   Income = "מהי ההכנסה (נטו) ממוצעת שלך?",
                   SelfEsteem = "איך היית מתאר/ת את התפיסה העצמית ואת דימוי הגוף הנוכחיים שלך?",
                   EnvironmentAndSelfEsteem = "באיזו מידה את/ה חושב/ת שהסביבה משפיעה על תפיסת דימוי הגוף שלך?",
                   IsBarbieVersatile = "האם את/ה חושב/ת שבובות ברבי משקפות במידה מספקת מגוון צורות וצבעים של נשים אמיתיות?",
                   IsBarbiePositiveInfluence = "באיזו מידה לדעתך בובות הברבי משפיעות לטובה על התפיסה החברתית למודל יופי?",
                   BuyingBarbie = "האם היית שוקל/ת לרכוש את בובות הברבי לילדים שלך? (באופן היפותטי במידה ואין לך ילדים)"
)


# Filter rows where "Finished" is "True"
database <- filter(database, Finished == "True")


## Creating new columns 

# Arrange the data with dummies- Age
database $ Age_under_18 <- ifelse(database $ Age == "מתחת ל-18", 1,0)
database $ Age_18_25 <- ifelse(database $ Age == "18-25", 1,0)
database $ Age_25_30 <- ifelse(database $ Age == "25-30", 1,0)
database $ Age_30_40 <- ifelse(database $ Age == "30-40", 1,0)
database $ Age_40_50 <- ifelse(database $ Age == "40-50", 1,0)
database $ Age_over_50 <- ifelse(database $ Age == "מעל 50", 1,0)

# Arrange the data with dummies- Sex
database $ Female <- ifelse(database $ Sex == "אישה", 1,0)

# Arrange the data with dummies- LivingArea
database $ Gush_Dan <- ifelse(database $ LivingArea == "גוש דן", 1,0)
database $ Sharon <- ifelse(database $ LivingArea == "שרון", 1,0)
database $ Shfela <- ifelse(database $ LivingArea == "שפלה", 1,0)
database $ South <- ifelse(database $ LivingArea == "דרום", 1,0)
database $ North <- ifelse(database $ LivingArea == "צפון", 1,0)
database $ Abroad <- ifelse(database $ LivingArea == "אני לא גר/ה בישראל", 1,0)

# Arrange the data with dummies- FamilyStatus
database $ Widower <- ifelse(database $ FamilyStatus == "אלמן/ה", 1,0)
database $ Divorcee <- ifelse(database $ FamilyStatus == "גרוש/ה", 1,0)
database $ Married <- ifelse(database $ FamilyStatus == "נשוי/אה", 1,0)
database $ Single <- ifelse(database $ FamilyStatus == "רווק/ה", 1,0)

# Create a function to map income categories to numerical values
map_income_categories <- function(income_category) {
  if (income_category == "אין לי הכנסה") {
    return(1)
  } else if (income_category == "עד 5,000 ₪") {
    return(2)
  } else if (income_category == "5,000-7,500 ₪") {
    return(3)
  } else if (income_category == "7,500-10,000 ₪") {
    return(4)
  } else if (income_category == "10,000-20,000 ₪") {
    return(5)
  } else if (income_category == "מעל 20,000 ₪") {
    return(6)
  } else {
    return(NA)  # Return NA for any unexpected values
  }
}

# Apply the function to the Income column
database$Income_Category <- sapply(database$Income, map_income_categories)


# Create a function to map self-esteem categories to numerical values
map_self_esteem_categories <- function(self_esteem_category) {
  if (self_esteem_category == "שליליים") {
    return(1)
  } else if (self_esteem_category == "ניטראליים") {
    return(2)
  } else if (self_esteem_category == "חיוביים") {
    return(3)
  } else if (self_esteem_category == "חיוביים מאוד") {
    return(4)
  } else {
    return(NA)  # Return NA for any unexpected values
  }
}

# Apply the function to the Income column
database$SelfEsteem_category <- sapply(database$SelfEsteem, map_self_esteem_categories)


# Create a function to map self-esteem categories to numerical values
map_EnvironmentEsteem <- function(EnvironmentEsteem_category) {
  if (EnvironmentEsteem_category == "בכלל לא") {
    return(1)
  } else if (EnvironmentEsteem_category == "מעט") {
    return(2)
  } else if (EnvironmentEsteem_category == "במתינות") {
    return(3)
  } else if (EnvironmentEsteem_category == "במידה רבה") {
    return(4)
  } else {
    return(NA)  # Return NA for any unexpected values
  }
}

# Apply the function to the Income column
database$EnvironmentEsteem_category <- sapply(database$EnvironmentAndSelfEsteem, map_EnvironmentEsteem)


# Create a function to map self-esteem categories to numerical values
map_Versatile <- function(Versatile_category) {
  if (Versatile_category == "בכלל לא") {
    return(1)
  } else if (Versatile_category == "לא") {
    return(2)
  } else if (Versatile_category == "ניטראלי") {
    return(3)
  } else if (Versatile_category == "כן") {
    return(4)
  }  else if (Versatile_category == "בהחלט כן") {
      return(5)
  } else {
    return(NA)  # Return NA for any unexpected values
  }
}

# Apply the function to the Income column
database$Versatile_category <- sapply(database$IsBarbieVersatile, map_Versatile)


# Create a function to map self-esteem categories to numerical values
map_PositiveInfluence <- function(PositiveInfluence_category) {
  if (PositiveInfluence_category == "בכלל לא") {
    return(1)
  } else if (PositiveInfluence_category == "במידה מועטה") {
    return(2)
  } else if (PositiveInfluence_category == "במתינות") {
    return(3)
  } else if (PositiveInfluence_category == "במידה רבה") {
    return(4)
  }  else if (PositiveInfluence_category == "באופן מוחלט") {
    return(5)
  } else {
    return(NA)  # Return NA for any unexpected values
  }
}

# Apply the function to the Income column
database$PositiveInfluence_category <- sapply(database$IsBarbiePositiveInfluence, map_PositiveInfluence)

# Create a function to map self-esteem categories to numerical values
map_BuyingBarbie <- function(BuyingBarbie_category) {
  if (BuyingBarbie_category == "בכלל לא") {
    return(1)
  } else if (BuyingBarbie_category == "כנראה שלא") {
    return(2)
  } else if (BuyingBarbie_category == "ניטראלי") {
    return(3)
  } else if (BuyingBarbie_category == "כנראה שכן") {
    return(4)
  }  else if (BuyingBarbie_category == "באופן וודאי") {
    return(5)
  } else {
    return(NA)  # Return NA for any unexpected values
  }
}

# Apply the function to the Income column
database$BuyingBarbie_category <- sapply(database$BuyingBarbie, map_BuyingBarbie)

# Creating a dummy column for the Barbie commercial
database$New_Barbie_Commercial <- ifelse(database$Barbie == "new", 1, 0)


# Create a dummy column based on the "Buy Barbie" column
database <- database %>%
  mutate(Buy_Barbie_Dummy = ifelse(`BuyingBarbie_category` %in% 1:3, 0, 1))


##### correlation matrix

# Select only numeric columns
numeric_columns <- sapply(database, is.numeric)
database_numeric <- database[, numeric_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(database_numeric, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Convert the correlation matrix to a long format
correlation_long <- melt(correlation_matrix)

# Plot the heatmap
ggplot(data = correlation_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1)) +
  coord_fixed() +
  ggtitle("Correlation Matrix Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))



##### Regression
# Main regression equations
main_regression <- lm(BuyingBarbie_category ~ Female + Age_over_50 + PositiveInfluence_category + Versatile_category + EnvironmentEsteem_category + SelfEsteem_category + New_Barbie_Commercial, data = database)

stargazer(main_regression,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", omit.stat = c("n"), 
          title = "Main regression", style = "apsr", out = "main.html")



## Regression equations to analysis Barbie survey data

# Influence of initial emotions
reg1 <- lm(BuyingBarbie_category ~ Versatile_category + EnvironmentEsteem_category + SelfEsteem_category, data = database)

stargazer(reg1,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", omit.stat = c("n"), 
          title = "Regression 1", style = "apsr", out = "reg1.html")



# The effect of initial emotions plus the ad they watched
reg2 <- lm(BuyingBarbie_category ~ Versatile_category + EnvironmentEsteem_category + SelfEsteem_category + New_Barbie_Commercial, data = database)

stargazer(reg2,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", omit.stat = c("n"), 
          title = "Regression 2", style = "apsr", out = "reg2.html")


# The effect of belonging to a group of mature women

reg3 <- lm(BuyingBarbie_category ~ SelfEsteem_category + Female + Age_over_50 + New_Barbie_Commercial + Female*Age_over_50, data = database)

stargazer(reg3,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", omit.stat = c("n"), 
          title = "Regression 3", style = "apsr", out = "reg3.html")



# Create the new column "HighIncome"
database$HighIncome <- ifelse(database$Income_Category %in% c(5, 6), 1, 0)

reg4 <- lm(BuyingBarbie_category ~ Versatile_category + EnvironmentEsteem_category + SelfEsteem_category + PositiveInfluence_category + Gush_Dan + Single +  HighIncome + New_Barbie_Commercial, data = database)
summary(reg4)
stargazer(reg4,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", omit.stat = c("n"), 
          title = "Regression 4", style = "apsr", out = "reg4.html")



# Examining the Effect of Advertisement and Initial Perception
#This model examines how the type of advertisement (New/Old) along with initial perceptions of Barbie (versatility, environmental influence, self-esteem) affect the perceived positive influence on body image.

reg5 <- lm(PositiveInfluence_category ~ New_Barbie_Commercial + Versatile_category + EnvironmentEsteem_category + SelfEsteem_category, data = database)

stargazer(reg5,   summary = FALSE, rownames = FALSE, ci.level = 0.95,
          type = "text", omit.stat = c("n"), 
          title = "Regression 5", style = "apsr", out = "reg5.html")


### Graphs of coefficients with confidence intervals
install.packages("broom")
library(broom)

#Regression 1 
tidy_model1 <- tidy(reg1, conf.int = TRUE)

ggplot(tidy_model1, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

#Regression 
tidy_model2 <- tidy(reg2, conf.int = TRUE)

ggplot(tidy_model2, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

#Regression 3
tidy_model3 <- tidy(reg3, conf.int = TRUE)

ggplot(tidy_model3, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

#Regression 4
tidy_model1 <- tidy(reg4, conf.int = TRUE)

ggplot(tidy_model4, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

#Regression 5
tidy_model5 <- tidy(reg1, conf.int = TRUE)

ggplot(tidy_model5, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()

#Main regression
tidy_main_regression <- tidy(main_regression, conf.int = TRUE)

ggplot(tidy_main_regression , aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +  # Flip coordinates for better readability
  labs(x = "Predictor", y = "Estimate", title = "Coefficient Estimates with 95% Confidence Intervals") +
  theme_minimal()