library(tableone)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

myVars <- c(
  "Age", "Gender", "Education", "Income", "Residential", "Marital",
  "MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3",
  "Digital_T0", "Digital_T1", "Digital_T2", "Digital_T3",
  "Unique_App_Count",
  "Cumulative_frequency_all",
  "Cumulative_frequency_cognitive_capacity",
  "Cumulative_frequency_locomotor_capacity",
  "Cumulative_frequency_medical_service_or_support",
  "Cumulative_frequency_psychological_capacity",
  "Cumulative_duration_all_daily_hours",
  "Cumulative_duration_cognitive_capacity_daily_hours",
  "Cumulative_duration_locomotor_capacity_daily_hours",
  "Cumulative_duration_medical_service_or_support_daily_hours",
  "Cumulative_duration_psychological_capacity_daily_hours"
)

catVars <- c("Age", "Gender", "Education", "Income", "Residential", "Marital")

nonpara1 <- c(
  "MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3",
  "Digital_T0", "Digital_T1", "Digital_T2", "Digital_T3",
  "Unique_App_Count",
  "Cumulative_frequency_all",
  "Cumulative_frequency_cognitive_capacity",
  "Cumulative_frequency_locomotor_capacity",
  "Cumulative_frequency_medical_service_or_support",
  "Cumulative_frequency_psychological_capacity",
  "Cumulative_duration_all_daily_hours",
  "Cumulative_duration_cognitive_capacity_daily_hours",
  "Cumulative_duration_locomotor_capacity_daily_hours",
  "Cumulative_duration_medical_service_or_support_daily_hours",
  "Cumulative_duration_psychological_capacity_daily_hours"
)

tab <- CreateTableOne(
  vars = myVars,
  data = df,
  factorVars = catVars
)

table1 <- print(
  tab,
  nonnormal = nonpara1,
  showAllLevels = TRUE,
  formatOptions = list(big.mark = ",")
)

write.csv(
  table1,
  file = "~/Downloads/table_1_baseline_characteristics.csv"
)
