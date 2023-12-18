library(tidyverse)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(glue)
library(gt)
library(dplyr)
library(summarytools)
library(gtsummary)
library(modelsummary)
library(pROC)
library(broom)


#DATA
broward_clean <- read_csv("allData/BROWARD_CLEAN.csv")
charges <- read_csv("allData/CHARGE_ID.csv")
#mturk_predictions <- read_csv("allData/MTURK_RACE.csv")
demographics <- read_csv("allData/MTURK_RACE_DEMOGRAPHICS.csv")

broward_clean <- broward_clean |>
  rename(charge_degree = `charge_degree (misd/fel)`) |>
  mutate(sex = ifelse(sex == 0, 'male', 'female'))

#Summary Statistics - Table 1.1
mean_age <- mean(broward_clean$age)
mean_juv_fel_count <- mean(broward_clean$juv_fel_count)
mean_juv_misd_count <- mean(broward_clean$juv_misd_count)
mean_priors_count <- mean(broward_clean$priors_count)
mean_COMPAS_score <- mean(broward_clean$compas_decile_score)

sd_age <- sd(broward_clean$age)
sd_jfc <-sd(broward_clean$juv_fel_count)
sd_jmc <- sd(broward_clean$juv_misd_count)
sd_priors <- sd(broward_clean$priors_count)
sd_COMPAS <- sd(broward_clean$compas_decile_score)

summary_data <- data.frame(
  Variable = c("Age", "Juv_Fel_Count", "Juv_Misd_Count", "Priors_Count", "COMPAS_Score"),
  Mean = c(mean_age, mean_juv_fel_count, mean_juv_misd_count, mean_priors_count, mean_COMPAS_score),
  SD = c(sd_age, sd_jfc, sd_jmc, sd_priors, sd_COMPAS)
)

num_observations <- length(na.omit(broward_clean$age))

summary_data <- data.frame(
  Variable = c("Age", "Juvenile Felony Count", "Juvenile Misdemeanor Count", "Priors Count", "COMPAS Score"),
  Mean = c(mean_age, mean_juv_fel_count, mean_juv_misd_count, mean_priors_count, mean_COMPAS_score),
  SD = c(sd_age, sd_jfc, sd_jmc, sd_priors, sd_COMPAS),
  Obs = num_observations
)

#Final Table
summary_table_gt <- summary_data |>
  gt() |>
  tab_spanner(
    label = "Statistics",
    columns = c("Mean", "SD", "Obs")
  ) |>
  fmt_number(
    columns = vars(Mean, SD),
    decimals = 2
  ) |>
  tab_header(
    title = "Defendant Summary Statistics",
    subtitle = "for Broward County Dataset"
  )

summary_table_gt


#Replicate Table 1.2 (Accuracy Rates)
##COMPAS
accuracy_overall <- broward_clean |>
  summarise(accuracy_overall = sum(compas_correct) / n())

accuracy_black <- broward_clean |>
  filter(race == 2)|>
  summarise(accuracy_black = sum(compas_correct) / n())

accuracy_white <- broward_clean |>
  filter(race == 1)|>
  summarise(accuracy_white = sum(compas_correct) / n())

broward_clean <- broward_clean |>
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Hispanic",
    race == 4 ~ "Asian",
    race == 5 ~ "Native American",
    race == 6 ~ "Other"
  ))

accuracies <- broward_clean |>
  group_by(race)|>
  summarize(
    race_accuracy = sum(compas_correct) / n(),
    false_pos = sum(compas_correct == 0 & compas_guess == 1) / (sum(two_year_recid == 0)),
    false_neg = sum(compas_correct == 0 & compas_guess == 0) / (sum(two_year_recid == 1))
  ) |>
  mutate(
    accuracy_overall = accuracy_overall$accuracy_overall
  )|>
  filter(race %in% c("Black", "White"))


#AUC-ROC for Table 1.2
"NEEDS TO BE DONE WITH PREDICTION"

#Finalizing Table 1.2
accuracy_data <- data.frame(
  . = c("Accuracy (overall)", "AUC-ROC (overall)", "Accuracy", "False positive", "False negative"),
  White = c(accuracies$accuracy_overall[2], NA , accuracies$race_accuracy[2], accuracies$false_pos[2], accuracies$false_neg[2]),
  Black = c(accuracies$accuracy_overall[1], NA , accuracies$race_accuracy[1], accuracies$false_pos[1], accuracies$false_neg[1])
)

accuracies_table <- accuracy_data |>
  gt() |>
  tab_spanner(
    label = "Defendant Race",
    columns = c("White", "Black")
  ) |>
  fmt_percent(
    columns = vars(White, Black),
    decimals = 1
  ) |>
  tab_header(
    title = "COMPAS algorithmic predictions from 1000 defendants",
    subtitle = "COMPAS accuracy by defendant race"
  )

accuracies_table


#-------------Estimating Equation----------------------
broward_split <- rsample::initial_split(broward_clean, prop = .8)
broward_train <- training(broward_split)
broward_test <- testing(broward_split)

form7 <- as.formula("two_year_recid ~ age + sex + juv_misd_count + juv_fel_count + priors_count + charge_id + charge_degree")
model7 <- glm(form7, data = broward_train, family = binomial)
summary(model7)

#Making a table
tidy_summary <- tidy(model7)
tidy_summary

model7_table <- tidy_summary |>
  gt() |>
  tab_header(
    title = "Seven Feture Logistic Regression Model Summary",
    subtitle = "Predictors of Recidivism"
  ) %>%
  fmt_number(
    columns = vars(estimate, std.error, statistic, p.value),
    decimals = 3
  ) %>%
  fmt_number(
    columns = vars(estimate),
    suffixing = TRUE
  ) %>%
  fmt_number(
    columns = vars(p.value),
    decimals = 4
  ) %>%
  tab_spanner(
    label = "Coefficient",
    columns = vars(estimate, std.error)
  ) %>%
  tab_spanner(
    label = "Statistical Significance",
    columns = vars(statistic, p.value)
  )

model7_table

#Table 2 - Making predictions-----------------



#ANOVA

#Regression tests 

#LASSO



