library(dplyr)

df <- read.csv(
  "~/Downloads/Suppl_data_2_FlowChart.csv",
  stringsAsFactors = FALSE
)

# Participants who completed baseline assessment
before_T0 <- df %>%
  filter(Completed_T0_assessment == "Yes")

# Exclusions before T0 due to post-dropout data restrictions
not_after_drop_out_ids <- before_T0 %>%
  filter(Cannot_use_data_post_dropout == "Cannot") %>%
  pull(Case_ID_with_SN)

not_T0 <- before_T0 %>%
  filter(Case_ID_with_SN %in% not_after_drop_out_ids)

T0 <- before_T0 %>%
  filter(!Case_ID_with_SN %in% not_after_drop_out_ids)

# Follow-up completion counts
t1_counts <- table(T0$Completed_T1_FU)
t2_counts <- table(T0$Completed_T2_FU)
t3_counts <- table(T0$Completed_T3_FU)

# Usage data availability
usage_counts <- table(T0$Usage_data_available)

available_usage <- T0 %>%
  filter(Usage_data_available == "Yes")

usage_270d_counts <- table(available_usage$With_at_least_270d_of_usage_data_for_analysis)

# Print key counts
cat("Completed T0 assessment:", nrow(before_T0), "\n")
cat("Excluded before T0:", nrow(not_T0), "\n")
cat("Included at T0:", nrow(T0), "\n\n")

cat("T1 follow-up counts:\n")
print(t1_counts)
cat("\n")

cat("T2 follow-up counts:\n")
print(t2_counts)
cat("\n")

cat("T3 follow-up counts:\n")
print(t3_counts)
cat("\n")

cat("Usage data availability counts:\n")
print(usage_counts)
cat("\n")

cat("At least 270 days of usage data for analysis:\n")
print(usage_270d_counts)
cat("\n")
