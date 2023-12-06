library(tidyverse)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(glue)
library(gt)
library(dplyr)
library(summarytools)
library(gtsummary)

broward_clean <- read_csv("allData/BROWARD_CLEAN_SUBSET.csv")

charges <- read_csv("allData/CHARGE_ID.csv")

mturk_predictions <- read_csv("allData/MTURK_RACE.csv")

demographics <- read_csv("allData/MTURK_RACE_DEMOGRAPHICS.csv")


#defendant statistics
mean_age <- mean(broward_clean$age)
mean_juv_fel_count <- mean(broward_clean$juv_fel_count)
mean_juv_misd_count <- mean(broward_clean$juv_misd_count)
mean_COMPAS_score <- mean(broward_clean$compas_decile_score)

sd_age <- sd(broward_clean$age)
sd_jfc <-sd(broward_clean$juv_fel_count)
sd_jmc <- sd(broward_clean$juv_misd_count)
sd_COMPAS <- sd(broward_clean$compas_decile_score)

summary_data <- data.frame(
  Variable = c("Age", "Juv_Fel_Count", "Juv_Misd_Count", "COMPAS_Score"),
  Mean = c(mean_age, mean_juv_fel_count, mean_juv_misd_count, mean_COMPAS_score),
  SD = c(sd_age, sd_jfc, sd_jmc, sd_COMPAS)
)

num_observations <- length(na.omit(broward_clean$age))

summary_data <- data.frame(
  Variable = c("Age", "Juv. Felony Count", "Juv. Misdemeanor Count", "COMPAS Score"),
  Mean = c(mean_age, mean_juv_fel_count, mean_juv_misd_count, mean_COMPAS_score),
  SD = c(sd_age, sd_jfc, sd_jmc, sd_COMPAS),
  Num_Obs = num_observations
)

summary_table_gt <- summary_data %>%
  gt() %>%
  tab_spanner(
    label = "Statistics",
    columns = c("Mean", "SD", "Num_Obs")
  ) %>%
  fmt_number(
    columns = vars(Mean, SD),
    decimals = 2
  ) %>%
  tab_header(
    title = "Summary Statistics",
    subtitle = "for BROWARD_CLEAN_SUBSET"
  )

summary_table_gt

ggsave("summary_stats.png")
