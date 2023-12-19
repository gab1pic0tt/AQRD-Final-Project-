

#MAKING DEFENDANT SUMMARY STATS TABLE


#LOAD DATA
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
summary_stats_tbl <- summary_data |>
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

summary_stats_tbl
