# Clearing Environment
rm(list = ls())
# Working Directory
getwd()
# Setting Working Directory
setwd("D:\\GWR_NFHS_Assam")
# Verifying Working Directory
getwd()

# Packages
library(tidyverse)
library(MASS)
library(glmnet)
library(caret)

# NFHS Dataset Downloaded from NDAP (https://ndap.niti.gov.in/dataset/6822)
# Importing
origin_df <- read.csv("1752321020348461.csv", header = TRUE)
# examination
str(origin_df)
head(origin_df)
summary(origin_df)
colnames(origin_df)


# Removing unwanted columns
columns_remove <- c(
  "Married.Women.Age.Group.15.To.49.Years.Use.Any.Family.Planning.Methods......UOM...Percentage....Scaling.Factor.1",
  "Married.Women.Age.Group.15.To.49.Years.Any.Modern.Family.Planning.Method......UOM...Percentage....Scaling.Factor.1",
  "Average.Out.Of.Pocket.Expenditure.For.Each.Delivery.In.Public.Health.Facility..UOM.INR.IndianRupees....Scaling.Factor.1",
  "Births.Attended.By.Skilled.Health.Personnel......UOM...Percentage....Scaling.Factor.1",
  "Births.Delivered.By.Caesarean.Section......UOM...Percentage....Scaling.Factor.1",
  "Births.In.Private.Health.Facility.Delivered.By.Caesarean.Section......UOM...Percentage....Scaling.Factor.1",
  "Births.In.Public.Health.Facility.Delivered.By.Caesarean.Section......UOM...Percentage....Scaling.Factor.1",
  "Births.In.The.5.Years.Preceding.The.Survey.That.Are.Third.Or.Higher.Order......UOM...Percentage....Scaling.Factor.1",
  "Children.Age.Group.12.To.23.Months.Fully.Vaccinated.Based.On.Information.From.Vaccination.Card.Only......UOM...Percentage....Scaling.Factor.1",
  "Children.Born.At.Home.Who.Were.Taken.To.A.Health.Facility.For.A.Check.Up.Within.24.Hours.Of.Birth......UOM...Percentage....Scaling.Factor.1",
  "Children.Under.Age.5.Years.Whose.Birth.Was.Registered.With.The.Civil.Authority......UOM...Percentage....Scaling.Factor.1",
  "Children.Who.Received.Postnatal.Care.From.A.Doctor.Or.Nurse.Or.Lady.Health.Visitor..Lhv..Or.Auxiliary.Nurse.Midwifery..Anm..Or.Midwife.Or.Other.Health.Personnel.Within.2.Days.Of.Delivery......UOM...Percentage....Scaling.Factor.1",
  "Married.Women.Using.Condom.In.The.Age.Group.Of.15.To.49.Years......UOM...Percentage....Scaling.Factor.1",
  "Current.Users.Ever.Told.About.Side.Effects.Of.Current.Method......UOM...Percentage....Scaling.Factor.1",
  "Deaths.In.The.Last.3.Years.Registered.With.The.Civil.Authority......UOM...Percentage....Scaling.Factor.1",
  "Women.Who.Have.Ever.Undergone.A.Breast.Examination.For.Breast.Cancer......UOM...Percentage....Scaling.Factor.1",
  "Women.Ever.Undergone.A.Screening.Test.For.Cervical.Cancer......UOM...Percentage....Scaling.Factor.1",
  "Women.Ever.Undergone.An.Oral.Cavity.Examination.For.Oral.Cancer......UOM...Percentage....Scaling.Factor.1",
  "Female.Sterilization.Married.Women.15.To.49.Years......UOM...Percentage....Scaling.Factor.1",
  "Health.Worker.Ever.Talked.To.Female.Non.Users.About.Family.Planning......UOM...Percentage....Scaling.Factor.1",
  "Home.Births.That.Were.Conducted.By.Skilled.Health.Personnel......UOM...Percentage....Scaling.Factor.1",
  "Households.Using.Clean.Fuel.For.Cooking......UOM...Percentage....Scaling.Factor.1",
  "Population.And.Household.Profile.Households.With.Any.Usual.Member.Covered.Under.A.Health.Insurance.Or.Financing.Scheme......UOM...Percentage....Scaling.Factor.1",
  "Married.Women.Age.Group.15.To.49.Years.Using.Injectables.As.One.Of.The.Family.Planning.Methods......UOM...Percentage....Scaling.Factor.1",
  "Married.Women.Age.Group.15.To.49.Years.Using.Intrauterine.Device..Iud..Or.Postpartum.Intrauterine.Contraceptive.Device..Ppiud..As.Family.Planning.Methods......UOM...Percentage....Scaling.Factor.1",
  "Institutional.Births.In.Public.Facility......UOM...Percentage....Scaling.Factor.1",
  "Men.Suffering.From.High.Blood.Sugar.Level......UOM...Percentage....Scaling.Factor.1",
  "Men.Suffering.From.High.Or.Very.High.Blood.Sugar.Level......UOM...Percentage....Scaling.Factor.1",
  "Men.Suffering.From.Very.High.Blood.Sugar.Level......UOM...Percentage....Scaling.Factor.1",
  "Elevated.Blood.Pressure.Or.Taking.Medicine.To.Control.Blood.Pressure......UOM...Percentage....Scaling.Factor.1",
  "Men.Mildly.Elevated.Blood.Pressure......UOM...Percentage....Scaling.Factor.1",
  "Moderately.Or.Severely.Elevated.Blood.Pressure......UOM...Percentage....Scaling.Factor.1",
  "Population.Below.Age.15.Years......UOM...Percentage....Scaling.Factor.1",
  "Population.Living.In.Households.That.Use.An.Improved.Sanitation.Facility......UOM...Percentage....Scaling.Factor.1",
  "Population.Living.In.Households.With.An.Improved.Drinking.Water.Source......UOM...Percentage....Scaling.Factor.1",
  "Population.Living.In.Households.With.Electricity......UOM...Percentage....Scaling.Factor.1",
  "Registered.Pregnancies.For.Which.The.Mother.Received.A.Mother.And.Child.Protection.Card..Mcp......UOM...Percentage....Scaling.Factor.1",
  "Population.And.Household.Profile.Sex.Ratio.At.Birth.For.Children.Born.In.The.Last.Five.Years..Females.Per.1.000.Males...UOM.Number...Scaling.Factor.1",
  "Population.And.Household.Profile.Sex.Ratio.Of.The.Total.Population..Females.Per.1.000.Males...UOM.Number...Scaling.Factor.1",
  "Married.Women.Age.Group.15.To.49.Years.Unmet.Need.For.Family.Planning......UOM...Percentage....Scaling.Factor.1",
  "Women.Unmet.Need.For.Spacing......UOM...Percentage....Scaling.Factor.1",
  "Year",
  "Country"
)

# Creating a new dataframe
origin_df_clean <- origin_df %>% dplyr::select(-any_of(columns_remove)) 

# Dimensions check
dim(origin_df_clean)
# 704 districts, 65 variables

# Check for missing values
origin_df_clean %>% summarise(across(everything(), ~ sum(is.na(.))))
anyNA(origin_df_clean)
# Output says missing values in one of the variables
origin_df_clean %>%
  filter(if_any(everything(), is.na))

# Checking which variables have missing values
origin_df_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  mutate(
    short_col = str_trunc(column, width = 60),  # Adjust width as needed
    pct_missing = round(na_count / nrow(origin_df_clean) * 100, 2)
  ) %>%
  dplyr::select(short_col, na_count, pct_missing) %>%
  arrange(desc(na_count)) %>%
  print(n = Inf)

# Removing those variables where missing data percentage is above 20%
# Finding exact column matches with targeted patterns
columns_remove_2 <- names(origin_df_clean)[
  str_detect(names(origin_df_clean), fixed("Children.Age.Group.6.To.8.Months.Receiving.Solid.Or.Semisolid.Food")) |
    str_detect(names(origin_df_clean), fixed("Non.Breastfeeding.Children.Age.Group.6.To.23.Months.Receiving.An.Adequate.Diet")) |
    str_detect(names(origin_df_clean), fixed("Children.With.Diarrhoea.In.The.2.Weeks.Preceding.The.Survey.Taken.To.A.Health.Facility")) |
    str_detect(names(origin_df_clean), fixed("Children.With.Diarrhoea.In.The.2.Weeks.Preceding.The.Survey.Who.Received.Oral.Rehydration.Salts")) |
    str_detect(names(origin_df_clean), fixed("Children.With.Diarrhoea.In.The.2.Weeks.Preceding.The.Survey.Who.Received.Zinc")) |
    str_detect(names(origin_df_clean), fixed("Children.With.Fever.Or.Symptoms.Of.Acute.Respiratory.Infection.In.The.2.Weeks.Preceding.The.Survey"))
]
print (columns_remove_2)
# Removing those columns
origin_df_clean_2 <- origin_df_clean %>%
  dplyr::select(-all_of(columns_remove_2))

# Checking which variables have missing values for new dataframe
origin_df_clean_2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  mutate(
    short_col = str_trunc(column, width = 60),  # Adjust width as needed
    pct_missing = round(na_count / nrow(origin_df_clean) * 100, 2)
  ) %>%
  dplyr::select(short_col, na_count, pct_missing) %>%
  arrange(desc(na_count)) %>%
  print(n = Inf)
# 16 variables have missing values

# Checking rows which do not have complete date (missing values)
origin_df_clean_2 %>%
  filter(if_any(everything(), is.na)) %>%
  nrow()
# 112 districts do not have complete information

# Checking Missing values in districts of Assam if there's any
origin_df_clean_2 %>%
  filter(State == "Assam") %>%
  filter(if_any(everything(), is.na)) %>%
  nrow()
# Districts in Assam have complete values

# dropping all rows in dataframe origin_df_clean_2 with missing values since project is focused on Assam
origin_df_clean_2 <- origin_df_clean_2 %>%
  drop_na()

# Check for Missing Values again
anyNA(origin_df_clean_2)
# NO Missing Values

# Data Types
origin_df_clean_2 %>% summarise(across(everything(), class))

# target variable: Women Age Group 15 To 49 Years Who Are Anaemic (%) (UOM:%(Percentage)), Scaling Factor:1 (Women.Age.Group.15.To.49.Years.Who.Are.Anaemic......UOM...Percentage....Scaling.Factor.1)

# Removing part of variables with '......UOM...Percentage....Scaling.Factor.1'
colnames(origin_df_clean_2) <- gsub("\\.*UOM.*Percentage.*Scaling.*Factor.*1", "", colnames(origin_df_clean_2))

# Removing some more redundant or similar variables to target variable
origin_df_clean_2 <- origin_df_clean_2 %>%
  dplyr::select(
    -Men.Age.Group.15.Years.And.Above.Who.Use.Any.Kind.Of.Tobacco,
    -Men.Age.Group.15.Years.And.Above.Who.Consume.Alcohol,
    -Women.Age.Group.15.To.19.Years.Who.Are.Anaemic,
    -Non.Pregnant.Women.Age.Group.15.To.49.Years.Who.Are.Anaemic,
    -Pregnant.Women.Age.Group.15.To.49.Years.Who.Are.Anaemic
  )

#-------------StepWise Variable Selection---------------------

# Both-Direction Stepwise Selection (https://www.statology.org/stepwise-regression-r/)
# Creating temporary dataframe
temp_df <- origin_df_clean_2 %>% 
  dplyr::select(-State, -District)

# Defining target variable
target_var <- "Women.Age.Group.15.To.49.Years.Who.Are.Anaemic"

# Models with all parameters
res_lm <- lm(as.formula(paste(target_var, "~ .")), data = temp_df)
step_lm <- stepAIC(res_lm, direction = "both", trace = FALSE)
summary(step_lm)

# Extracting coefficients summary
coef_step <- summary(step_lm)$coefficients
# Creating significance labels based on p-values for easier understanding
significance_labels_step <- cut(coef_step[,4], 
                           breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), 
                           labels = c("***", "**", "*", ".", " "))

# Combining with original coefficient matrix
coef_with_sig_step <- cbind(coef_step, Signif = significance_labels_step)

# Updating significance codes to readable symbols
signif_symbols_step <- c("***", "**", "*", ".", " ")
coef_with_sig_step[, "Signif"] <- signif_symbols_step[as.numeric(coef_with_sig_step[, "Signif"])]

# View final table with proper significance stars
print(coef_with_sig_step)

# Adding row names as column for exporting it to CSV
coef_with_sig_step_csv <- cbind(Variable = rownames(coef_with_sig_step), coef_with_sig_step)

# Writing it as a CSV
write.csv(coef_with_sig_step_csv, "stepwise_regression_results.csv", row.names = FALSE)

# Only keeping those variables with p < 0.01
sig_vars_step <- rownames(coef_with_sig_step)[coef_with_sig_step[, "Pr(>|t|)"] < 0.01]
sig_vars_step
# Ensuring that target and geo identifiers are preserved
temp_vars_pre <- c("State", "District", "Women.Age.Group.15.To.49.Years.Who.Are.Anaemic", sig_vars_step)
# Creating new dataframe with target and geo-identifiers and significant variables
origin_df_rg <- origin_df_clean_2[, intersect(temp_vars_pre, colnames(origin_df_clean_2))]
# Exporting to CSV
write.csv(origin_df_rg, "Dataset_with_significant_variables_with_stepwise_regression.csv", row.names = FALSE)

#-----------Elastic Net Regression--------------------
# Creating a new dataframe for Elastic Net Regression
temp_df_enet <- origin_df_clean_2 %>% 
  dplyr::select(-State, -District)
# This is the target variable
target_var <- "Women.Age.Group.15.To.49.Years.Who.Are.Anaemic"
# Target variable Y
y_enet <- temp_df_enet$Women.Age.Group.15.To.49.Years.Who.Are.Anaemic
# Predictor matrix
x_enet <- temp_df_enet %>%
  dplyr::select(-all_of(target_var)) %>%
  as.matrix() 
# data
data_enet <- cbind(x_enet, y_enet)
