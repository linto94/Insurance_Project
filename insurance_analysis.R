# Loading Libraries Needed for Project 
library(tidyverse)
library(readxl)
library(outliers)
library(lubridate)
library(caret)
library(broom)
library(rmarkdown)
library(skimr)
library(SmartEDA)


# Load the datasets
insurance_data <- read_excel("insurance_dataset.xlsx")
View(insurance_data)

#  Automated Data Exploration 
ExpReport(insurance_data,op_file = "EDA_report.html")
skim_without_charts(insurance_data)
str(insurance_data)

# Data Pre_processing 
# Feature Engineering to Extract Age from birth_date Colum

# Extract the Current year and month
current_year <- 2024
current_month <- 6

# Extract the Birth year and month for policyholders
insurance_data$birth_year <- year(insurance_data$birthdate)
insurance_data$birth_month <- month(insurance_data$birthdate) 
or 
#insurance_data <- insurance_data %>%
 # mutate(
    #birth_year <- year(birthdate),
    #birth_month <-month(birthdate))

# calculate the age 
insurance_data <- insurance_data %>%
  mutate(
     age = ifelse(birth_month > current_month, current_year - birth_year - 1, current_year - birth_year)
  )

# to group this ages into interval for the aim of exploration 
age_breaks <- c(21, 31, 41, 51, 61, 71, 75)
age_lables <- c("21-30", "31-40", "41-50", "51-60", "61-70", "71-74")

insurance_data <- insurance_data %>%
  mutate(
    age_group = cut(age, breaks = age_breaks, labels = age_lables, right = FALSE)
  )



# Analysis to answer each key question

# 1. Claim Frequency and Amount Analysis
# A) What are the average claim frequencies and amounts for different demographic groups
demographic_summary <- insurance_data %>% group_by(age_group, gender, marital_status) %>%
  summarise(
    avg_claim_frequency = mean(claim_freq),
    avg_claim_amount = mean(claim_amt)
  ) %>%
  gather(measures, values, c(avg_claim_frequency, avg_claim_amount))

#visuals 
# Age Group
ggplot(data = demographic_summary, aes(x= age_group, y= values, fill = age_group))+
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Average Claim Amount & Claim Frequency based on Age Group",
       x = "Age_Group",
       y = "Value") +
  facet_wrap(~ measures, scales = "free_y") +
  theme_minimal()

# Gender
ggplot(data = demographic_summary, aes(x= gender, y= values, fill = gender))+
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Average Claim Amount & Claim Frequency based on Gender",
       x = "Gender",
       y = "Value") +
  facet_wrap(~ measures, scales = "free_y") +
  theme_minimal()

# Martial Status
ggplot(data = demographic_summary, aes(x= marital_status, y= values, fill = marital_status))+
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Average Claim Amount & Claim Frequency based for bassed on Marital Status",
       x = "Marital Status",
       y = "Value") +
  facet_wrap(~ measures, scales = "free_y") +
  theme_minimal()

# B. specific vehicle characteristics (e.g., make, model, year) 
#that correlate with higher claim frequencies or amounts
# Encode the data to turn the character data type to numeric for correlation
insurance_data_encoded <- insurance_data %>%
  mutate(make_encoded = as.numeric(factor(car_make)),
         model_encoded = as.numeric(factor(car_model)),
         color_encoded = as.numeric(factor(car_color)))
# Select only the needed numeric variables out of the encoded data
numeric_data <- insurance_data_encoded %>% 
  select(claim_amt, claim_freq, car_year, make_encoded, model_encoded, color_encoded)
# calcuate correlation among the variables
cor(numeric_data)
 

# 2. Demo Graphic Analysis 
# A. Distribution of policyholders across different demographic factors
# Age_Group 
insurance_data %>% group_by(age_group) %>%
  summarise(agegroup_distribution = n()) %>%
  ggplot(aes(x=age_group, y=agegroup_distribution, fill=age_group)) +
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Distribution of policyholders based on age_group",
       x= "Age_Group",
       y= "Value") +
  theme_minimal()
# Gender
insurance_data %>% group_by(gender) %>%
  summarise(gender_distribution = n()) %>%
  ggplot(aes(x=gender, y=gender_distribution, fill=gender)) +
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Distribution of policyholders based on gender",
       x= "Gender",
       y= "Value") +
 theme_minimal()

# Marital Status
insurance_data %>% group_by(marital_status) %>%
  summarise(ms_distribution = n()) %>%
  ggplot(aes(x=marital_status, y=ms_distribution, fill=marital_status)) +
  geom_col(position = "dodge", width = 0.4) +
  labs(title = "Distribution of policyholders based on marital status",
       x= "Marital Status",
       y= "Value") +
  theme_minimal()

# B. Trends in car usage and ownership among different demographic groups
# Age_Group
insurance_data %>%
  group_by(age_group, car_use) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = car_use, y = Count, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Car Usage based on Age Group",
       x = "Car Use",
       y = "Value") +
  theme_minimal() +
  coord_flip()

# Gender
  insurance_data %>%
  group_by(gender, car_use) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = car_use, y = Count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Car Usage based on Gender",
       x = "Car Use",
       y = "Value") +
  theme_minimal() +
    coord_flip()
  
  # Marital Status
  insurance_data %>%
    group_by(marital_status, car_use) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ggplot(aes(x = car_use, y = Count, fill = marital_status)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Car Usage based on Marital Status",
         x = "Car Use",
         y = "Value") +
    theme_minimal() +
    coord_flip()


# 3 Geographical Analysis
# A. claim frequencies and amounts vary across different coverage zones?
  insurance_data %>% group_by(coverage_zone) %>%
    summarise(avg_claim_frequency = mean(claim_freq),
              avg_claim_amount = mean(claim_amt)) %>%
    pivot_longer(cols = c(avg_claim_frequency, avg_claim_amount), 
                 names_to = "measures", values_to = "values") %>% 
    ggplot(aes(x=coverage_zone,y=values, fill = coverage_zone)) +
    geom_col(position = "dodge", width = 0.4) +
    labs(title = "Average Claim Amount & Frequenecy for each Coverage Zone",
         x="Coverage_Zone",
         y="Values") +
    facet_wrap(~ measures, scales = "free_y") +
    theme_minimal()
  
  
# 4 Customer Behavior Insights
# A trends or patterns in the behavior of policyholders who have children driving?
 # Marital Status
 insurance_data %>% group_by(kids_driving_ch, marital_status) %>% 
   filter(kids_driving_ch %in% c("One", "Two", "Three")) %>%
   summarise(
     avg_household_income = mean(household_income),
     avg_claim_amount = mean(claim_amt),
     avg_claim_frequecy = mean(claim_freq), .groups = 'drop') %>%
   pivot_longer(cols = c(avg_household_income, avg_claim_amount, avg_claim_frequecy), 
                names_to = "measures", 
                values_to = "values") %>%
   ggplot(aes(x=kids_driving_ch, y=values, fill = marital_status)) +
   geom_col(position = "dodge", width = 0.4) +
   labs(title = "Pattern in Behaviour of Poliyholders with Children Driving 
        based on Marital Status",
        x="Kids Driving",
        y="Values") +
   facet_wrap(~ measures, scales = "free_y") +
   theme_minimal()
  
 # Education
 insurance_data %>% group_by(kids_driving_ch, education) %>% 
   filter(kids_driving_ch %in% c("One", "Two", "Three")) %>%
   summarise(
     avg_household_income = mean(household_income),
     avg_claim_amount = mean(claim_amt),
     avg_claim_frequecy = mean(claim_freq), .groups = 'drop') %>%
   pivot_longer(cols = c(avg_household_income, avg_claim_amount, avg_claim_frequecy), 
                names_to = "measures", 
                values_to = "values") %>%
   ggplot(aes(x=kids_driving_ch, y=values, fill = education)) +
   geom_col(position = "dodge", width = 0.4) +
   labs(title = "Pattern in Behaviour of Poliyholders with Children Driving 
        based on Education level",
        x="Kids Driving",
        y="Values") +
   facet_wrap(~ measures, scales = "free_y") +
   theme_minimal()
 
 # Coverage_Zone
 insurance_data %>% group_by(kids_driving_ch, coverage_zone) %>% 
   filter(kids_driving_ch %in% c("One", "Two", "Three")) %>%
   summarise(
     avg_household_income = mean(household_income),
     avg_claim_amount = mean(claim_amt),
     avg_claim_frequecy = mean(claim_freq), .groups = 'drop') %>%
   pivot_longer(cols = c(avg_household_income, avg_claim_amount, avg_claim_frequecy), 
                names_to = "measures", 
                values_to = "values") %>%
   ggplot(aes(x=kids_driving_ch, y=values, fill = coverage_zone)) +
   geom_col(position = "dodge", width = 0.4) +
   labs(title = "Pattern in Behaviour of Poliyholders with Children Driving 
        based on Coverage Zone",
        x="Kids Driving",
        y="Values") +
   facet_wrap(~ measures, scales = "free_y") +
   theme_minimal()
 
 # Car Use
 insurance_data %>% group_by(kids_driving_ch, car_use) %>% 
   filter(kids_driving_ch %in% c("One", "Two", "Three")) %>%
   summarise(
     avg_household_income = mean(household_income),
     avg_claim_amount = mean(claim_amt),
     avg_claim_frequecy = mean(claim_freq), .groups = 'drop') %>%
   pivot_longer(cols = c(avg_household_income, avg_claim_amount, avg_claim_frequecy), 
                names_to = "measures", 
                values_to = "values") %>%
   ggplot(aes(x=kids_driving_ch, y=values, fill = car_use)) +
   geom_col(position = "dodge", width = 0.4) +
   labs(title = "Pattern in Behaviour of Poliyholders with Children Driving 
        based on Car use",
        x="Kids Driving",
        y="Values") +
   facet_wrap(~ measures, scales = "free_y") +
   theme_minimal()
 
 # B How does the presence of children driving affect the frequency and number of claims?
 insurance_data %>% group_by(kids_driving_ch) %>% 
   filter(kids_driving_ch %in% c("One", "Two", "Three")) %>%
   summarise(
     avg_claim_fre = mean(claim_freq),
     avg_claim_amount = mean(claim_amt),
     avg_household_income = mean(household_income)) %>%
   pivot_longer(cols = -kids_driving_ch, names_to = "measures", 
                values_to = "values") %>%
   ggplot(aes(x=kids_driving_ch, y=values, fill = kids_driving_ch)) +
   geom_col(position = "dodge", width = 0.4) +
   labs(title = "Presence of Children Driving effect on Claim Amount, 
        Frequency and Household Income",
        x="Kids Driving",
        y="Values") +
   facet_wrap(~ measures, scales = "free_y") +
   theme_minimal()



# 5. Risk Assessment 
# A. factors that are most indicative of high-risk policyholders
 
# Define risk profile of policyholders based on claim frequency and claim amount
 insurance_data <- insurance_data %>%
   mutate(risk_profile = ifelse(claim_freq >= 2 | claim_amt > 75000, "High", "Low"))
 
 # Convert risk_profile to a binary variable for logistic regression
 insurance_data <- insurance_data %>%
   mutate(high_risk = ifelse(risk_profile == "High", 1, 0))
 
 # Using Logistic Regression for other features
 logistic_model_1 <- glm(high_risk ~ household_income + education + kids_driving_ch + parent, 
                       data = insurance_data, 
                       family = binomial)
 # Based on Car features
 logistic_model_2 <- glm(high_risk ~ car_use + car_make + car_model + 
                 car_color , data = insurance_data, family = binomial) 
 
 # Based on Demographics
 logistic_model_3 <- glm(high_risk ~ age_group + gender + marital_status + 
                           coverage_zone,
                          data = insurance_data, family = binomial) 
 
 options(max.print = 10000)
 

 # B common characteristics among policyholders who make frequent claims?
 # Define a threshold for what frequent claim is 
insurance_data <- insurance_data %>%
  mutate(claim_freq_status = ifelse(claim_freq >= 2, "Frequent", "Infrequent"))
# Gender
insurance_data %>%
  group_by(claim_freq_status, gender) %>% 
  filter(claim_freq_status == "Frequent") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=claim_freq_status, y=Count, fill = gender)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distrubtion of PolicyHolders that Claim Frequently based on Gender",
       x="Frequent Claimers",
       y="Value") +
  theme_minimal()

# Marital Status
insurance_data %>%
  group_by(claim_freq_status, marital_status) %>% 
  filter(claim_freq_status == "Frequent") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=claim_freq_status, y=Count, fill = marital_status)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distrubtion of PolicyHolders that Claim Frequently based on Mariral Status",
       x="Frequent Claimers",
       y="Value") +
  theme_minimal()

# Car Use
insurance_data %>%
  group_by(claim_freq_status, car_use) %>% 
  filter(claim_freq_status == "Frequent") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=claim_freq_status, y=Count, fill = car_use)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distrubtion of PolicyHolders that Claim Frequently based on Car Use",
       x="Frequent Claimers",
       y="Value") +
  theme_minimal()

# Coverage Zone
insurance_data %>%
  group_by(claim_freq_status, coverage_zone) %>% 
  filter(claim_freq_status == "Frequent") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=claim_freq_status, y=Count, fill = coverage_zone)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distrubtion of PolicyHolders that Claim Frequently based on Coverage Zone",
       x="Frequent Claimers",
       y="Value") +
  theme_minimal()
# Kids Driving
insurance_data %>%
  group_by(claim_freq_status, kids_driving_ch) %>% 
  filter(claim_freq_status == "Frequent") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=claim_freq_status, y=Count, fill = kids_driving_ch)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distrubtion of PolicyHolders that Claim Frequently based on Kids_Driving",
       x="Frequent Claimers",
       y="Value") +
  theme_minimal()

# Convert claim_freq to a binary variable for logistic regression
insurance_data <- insurance_data %>%
  mutate(frequent_claimer = ifelse(claim_freq_status == "Frequent", 1, 0))
# Based on Car Make
logistic_model_4 <- glm(frequent_claimer ~ car_make, 
                        data = insurance_data, family = binomial) 

# Car Color
logistic_model_5 <- glm(frequent_claimer ~ car_color, 
                        data = insurance_data, family = binomial)





# 6. Customer Segmentation 
# A. key characteristics of policyholders with low claim frequencies and high household incomes
# Define what Low Claim Frequency and High House Hold income is 
insurance_data <- insurance_data %>%
  mutate(low_claim_high_income = ifelse(claim_freq < 2 
                                        & household_income >= 198000,"Yes", "No"))

# Gender 
insurance_data %>%
  group_by(low_claim_high_income, gender) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=low_claim_high_income, y=Count, fill = gender)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distribution of Policyholders with Low Claim Frequency 
       and High Household Income based on Gender",
       x="Low Claim & High Income",
       y="Value") +
  theme_minimal()


# Marital Status
insurance_data %>%
  group_by(low_claim_high_income, marital_status) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=low_claim_high_income, y=Count, fill = marital_status)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distribution of Policyholders with Low Claim Frequency and 
       High Household Income based on Marital Status",
       x="Low Claim & High Income",
       y="Value") +
  theme_minimal()

# Car Use
insurance_data %>%
  group_by(low_claim_high_income, car_use) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=low_claim_high_income, y=Count, fill = car_use)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distribution of Policyholders with 
       Low Claim Frequency and High Household Income based on Car Use",
       x="Low Claim & High Income",
       y="Value") +
  theme_minimal()

# Coverage Zone
insurance_data %>%
  group_by(low_claim_high_income, coverage_zone) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=low_claim_high_income, y=Count, fill = coverage_zone)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distribution of Policyholders with Low Claim 
       Frequency and High Household Income based on Coverage Zone",
       x="Low Claim & High Income",
       y="Count") +
  theme_minimal()

# Kids Driving
insurance_data %>%
  group_by(low_claim_high_income, kids_driving_ch) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=low_claim_high_income, y=Count, fill = kids_driving_ch)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distribution of Policyholders with Low Claim 
       Frequency and High Household Income based on Kids Driving",
       x="Low Claim & High Income",
       y="Value") +
  theme_minimal()

# Age Group
insurance_data %>%
  group_by(low_claim_high_income, age_group) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x=low_claim_high_income, y=Count, fill = age_group)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Distribution of Policyholders with Low Claim 
       Frequency and High Household Income based on Age Group",
       x="Low Claim & High Income",
       y="Value") +
  theme_minimal()

# Numeric Variables 
insurance_data %>%
  group_by(low_claim_high_income) %>% 
  filter(low_claim_high_income == "Yes") %>%
  summarise(avg_claim_amount = mean(claim_amt),
            avg_household_income = mean(household_income)) %>%
  pivot_longer(cols = -low_claim_high_income, names_to = "measures", values_to = "values") %>%
  ggplot(aes(x=low_claim_high_income, y=values, fill = measures)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Average Claim Amount and Household 
       Income for Policyholders with Low Claim 
       Frequency and High Household Income",
       x="Low Claim & High Income",
       y="Value") +
  facet_wrap(~ measures)+
  theme_minimal()

# Convert low_claim_high_income to a binary variable for logistic regression
insurance_data <- insurance_data %>%
  mutate(highi_lowc = ifelse(low_claim_high_income == "Yes", 1, 0))

# Based on Car Make
logistic_model_6 <- glm(highi_lowc ~ car_make, 
                               data = insurance_data, family = binomial)


# Based on Car Color
logistic_model_7 <- glm(highi_lowc ~ car_color, 
                                data = insurance_data, family = binomial)


# 7. Premium Optimization 
# Estimate premiums based on claim amounts and frequency 
insurance_data <- insurance_data %>%
    mutate(estimated_premium = 0.1 * claim_amt + 100 * claim_freq)

insurance_data %>% group_by(risk_profile) %>%
  summarise(total_estimated_premium = sum(estimated_premium),
            avg_estimated_premmium = mean(estimated_premium)) %>%
  pivot_longer(cols = -risk_profile, names_to = "measures", values_to = "values") %>%
  ggplot(aes(x=risk_profile, y=values, fill = risk_profile)) +
  geom_col(position = 'dodge', width = 0.4) +
  labs(title = "Estimated Premium based on Risk Profile",
       x="Risk Profile",
       y="Value") +
  facet_wrap(~ measures, scales = "free_y")+
  theme_minimal()




