set.seed(123)
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
library(corrplot)
library(car)
library(broom)
library(randomForest)
library(Boruta)

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

# Setting up trainControl()
ctrl_enet <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  search = "random",
  verboseIter = TRUE
)

# Elastic Net Model
enet_model <- train(
  x = x_enet,
  y = y_enet,
  method = "glmnet",
  trControl = ctrl_enet,
  tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-2, 2, length = 50)),
)


print(enet_model)
plot(enet_model)


# Extracting out the lamba value
best_lambda <- enet_model$bestTune$lambda
lasso_coef <- coef(enet_model$finalModel, s = best_lambda)

# Converting it to a tidy data frame
coef_df_enet <- as.data.frame(as.matrix(lasso_coef))
coef_df_enet$Variable <- rownames(coef_df_enet)
colnames(coef_df_enet)[1] <- "Coefficient"

# Filter non-zero coefficients (excluding intercept)
selected_vars_enet <- coef_df_enet %>%
  dplyr::filter(Coefficient != 0 & Variable != "(Intercept)") %>%
  dplyr::arrange(desc(abs(Coefficient)))

print(selected_vars_enet)

# Extracting Coefficient values to CSV
write.csv(selected_vars_enet, "Dataset_with_Elastic_Net_Regression_Values.csv", row.names = FALSE)

# top 5 positive coefficients (variables)
top_positive <- selected_vars_enet %>%
  arrange(desc(Coefficient)) %>%
  head(5)
# top 5 negative coefficients (variables)
top_negative <- selected_vars_enet %>%
  arrange(Coefficient) %>%
  head(5)

# Top 10 variables
top_10_vars <- c(top_positive$Variable, top_negative$Variable)

# Subsetting the dataframe now for analysis
origin_df_enet <- origin_df_clean_2 %>%
  dplyr::select(State, District, Women.Age.Group.15.To.49.Years.Who.Are.Anaemic, all_of(top_10_vars))

# Exporting to CSV
write.csv(origin_df_enet, 
          "Dataset_with_Elastic_Net_Regression_Selected_Variables_and_Target_Variable.csv", row.names = FALSE)

# Proceeding with linear regression to eliminate variables based on p-values

# creating temporary dataset without State and District variables
temp_df_enet <- origin_df_enet %>%
  dplyr::select(-State, -District)


lin_reg_enet <- lm(as.formula(paste(target_var, "~ .")), data = temp_df_enet)
lin_reg_enet$coefficients
summary(lin_reg_enet)

# Some variables are signification

# Checking Linear Regression assumptions

# (a) Collinearity
colnames(temp_df_enet)
# Renaming variables in temp_df_enet
colnames(temp_df_enet) <- c(
  "Women_Anaemic_15_49",
  "Women_High_Blood_Sugar",
  "Children_Anaemic_6_59_Months",
  "Women_BMI_Below_Normal",
  "Children_ARI_Last_2_Weeks",
  "Teen_Mothers_15_19",
  "Women_Mild_BP",
  "Women_Moderate_Severe_BP",
  "Children_Severely_Wasted_U5",
  "Children_BCG_Vaccine_12_23_Months",
  "Female_Ever_Schooled_6plus"
)
cor_enet_var <- cor(temp_df_enet)
corrplot(cor_enet_var, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
findCorrelation(cor_enet_var, cutoff = 0.7)
# VIF Check
vif(lin_reg_enet)
# all VIFs < 2.2 - independence assumptions not exactly violated

# (b) Diagnostic Plots
par(mfrow = c(2,2))
plot(lin_reg_enet)
par(mfrow = c(1,1))
# Linearity Data
plot(lin_reg_enet, 1)
# Homogeneity of variance
plot(lin_reg_enet,3)
# Normality of Residuals
plot(lin_reg_enet, 2)
# Outliers
plot(lin_reg_enet, 5)

# Presence of some outliers impacting the assumptions slightly

#-------------Secondary Filter: Random Forest-------------------

# Applying a Second Filter of Variable Importance using Random Forest from the 46 variables chosen by Elastic Net Regression

# Getting all variables selected by ENET
all_enet_vars <- selected_vars_enet$Variable

# Subsettinh original dataframe with State, District, target variable, and all ENET-selected variables
origin_df_enet_rf <- origin_df_clean_2 %>%
  dplyr::select(State, District, Women.Age.Group.15.To.49.Years.Who.Are.Anaemic, all_of(all_enet_vars))

# creating temporary dataset without State and District variables
temp_df_enet_rf <- origin_df_enet_rf %>%
  dplyr::select(-State, -District)

# Random Forest
enet_rf_model <- randomForest::randomForest(Women.Age.Group.15.To.49.Years.Who.Are.Anaemic ~., data = temp_df_enet_rf, importance = TRUE)
# Ten Important variables
variable_importance <- importance(enet_rf_model)
sorted_importance <- data.frame(variable_importance[order(-variable_importance[, 1]), ])
head(sorted_importance, 10)
barplot(sorted_importance[, 1], names.arg = rownames(sorted_importance), las = 2, col = "blue", main = "Variable Importance")

#--------------Secondary Filter: Boruta--------------

boruta_output <- Boruta(Women.Age.Group.15.To.49.Years.Who.Are.Anaemic ~., 
                        data = temp_df_enet_rf, doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")
print(boruta_signif)

# Converting Boruta results to data frame
imp_scores_boruta <- attStats(boruta_output)

# Filtering for Confirmed and Tentative variables
imp_scores_filtered_boruta <- imp_scores[rownames(imp_scores_boruta) %in% boruta_signif, ]

# Ordering by median importance
top10_boruta_vars <- imp_scores_filtered_boruta %>%
  dplyr::arrange(desc(medianImp)) %>%
  head(10)

# Variable Names
top10_var_names_boruta <- rownames(top10_boruta_vars)
print(top10_var_names_boruta)

# top 8 variables are identical in both Boruta and Random Forest ranking
# Boruta being more robust, selecting its top 10 variables by importance
# Getting the final dataset
final_dataset <- origin_df_enet_rf %>% dplyr::select(State, District, Women.Age.Group.15.To.49.Years.Who.Are.Anaemic, all_of(top10_var_names_boruta))
colnames(final_dataset)
# Renaming the columns
colnames(final_dataset) <- c(
  "State",
  "District",
  "Women_Anaemic_15_49",
  "Children_Anaemic_6_59_Months",
  "Women_High_Blood_Sugar",
  "Teen_Mothers_15_19",
  "Women_BMI_Below_Normal",
  "Children_Underweight_U5",
  "Women_Married_Before_18_20_24",
  "Women_Consume_Alcohol_15Plus",
  "Female_Ever_Attended_School_6Plus",
  "Married_Women_Using_Pill_15_49",
  "Women_Literate_15_49"
)

# Proceeding with linear regression to eliminate variables based on p-values

# creating temporary dataset without State and District variables
temp_final_df <- final_dataset %>%
  dplyr::select(-State, -District)


lm_final <- lm(Women_Anaemic_15_49 ~., data = temp_final_df)
summary(lm_final)
# Significant variables according to p-values

# Checking Linear Regression assumptions

# (a) Collinearity
cor_final <- cor(temp_final_df)
corrplot(cor_final, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
findCorrelation(cor_final, cutoff = 0.7)
# Finding the pairs of variables with high correlation
as.data.frame(unique(t(apply(which(abs(cor(temp_final_df)) > 0.7 & abs(cor(temp_final_df)) < 1, arr.ind = TRUE), 1, function(i) sort(colnames(temp_final_df)[i]))))) %>% view()
# VIF Check
vif(lm_final)

# Looking at significance levels from regression, high collinearity values and VIF values, dropping the variables Women_Literate_15_49, Women_Married_Before_18_20_24 as Teen_Mothers_15_19, Female_Ever_Attended_School_6Plus convey same meaning

final_dataset_2 <- final_dataset %>%
  dplyr::select(-Women_Literate_15_49, -Women_Married_Before_18_20_24)

# Exporting to CSV (final dataset)
write.csv(final_dataset_2, 
          "Processed_dataset.csv", row.names = FALSE)

# Running the linear regression again

# creating temporary dataset without State and District variables
temp_final_df_2 <- final_dataset_2 %>%
  dplyr::select(-State, -District)

lm_final_2 <- lm(Women_Anaemic_15_49 ~., data = temp_final_df_2)
summary(lm_final_2)
# Significant variables according to p-values
# Checking Linear Regression assumptions
# (a) Collinearity
cor_final_2 <- cor(temp_final_df_2)
corrplot(cor_final_2, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
findCorrelation(cor_final_2, cutoff = 0.7)
# Finding the pairs of variables with high correlation
as.data.frame(unique(t(apply(which(abs(cor(temp_final_df_2)) > 0.7 & abs(cor(temp_final_df_2)) < 1, arr.ind = TRUE), 1, function(i) sort(colnames(temp_final_df_2)[i]))))) %>% view()
# VIF Check
vif(lm_final_2)

# Diagnostic Checks
# (b) Diagnostic Plots
par(mfrow = c(2,2))
plot(lm_final_2)
par(mfrow = c(1,1))
# Linearity Data
plot(lm_final_2, 1)
# Homogeneity of variance
plot(lm_final_2,3)
# Normality of Residuals
plot(lm_final_2, 2)
# Outliers
plot(lm_final_2, 5)

# Comparing the three linear regression models
#lin_reg_enet
#lm_final
#lm_final_2

# R-Squared Values Check
summary(lin_reg_enet)$adj.r.squared
summary(lm_final)$adj.r.squared
summary(lm_final_2)$adj.r.squared
# AIC/ BIC
AIC(lin_reg_enet, lm_final, lm_final_2)
BIC(lin_reg_enet, lm_final, lm_final_2)

# Model lm_final_2 was selected for Geographically Weighted Regression due to its comparable adjusted R², lowest AIC and BIC, and a simpler, non-collinear structure optimal for localised modelling.

save.image(file = "gwr_nfhs_assam_1.RData")


