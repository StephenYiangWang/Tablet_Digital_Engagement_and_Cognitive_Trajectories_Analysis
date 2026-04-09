library(nnet)
library(questionr)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

# Create output directory if needed
dir.create(
  "~/Downloads/table_3_output/03_psychological_app_engagement/",
  recursive = TRUE,
  showWarnings = FALSE
)

# Function to add significance symbols
add_p_signif <- function(tab) {
  tab$p_signif <- ifelse(
    tab$p < 0.001, "***",
    ifelse(
      tab$p < 0.01, "**",
      ifelse(
        tab$p < 0.05, "*",
        ifelse(tab$p < 0.1, ".", " ")
      )
    )
  )
  tab
}

# Function to run a multinomial model and save results
run_and_save_multinom <- function(data, ref_class, formula_str, output_name) {
  data$Class <- as.factor(data$Class)
  data$Class <- relevel(data$Class, ref = ref_class)
  
  formula_obj <- as.formula(formula_str)
  
  invisible(
    capture.output(
      model <- multinom(formula_obj, data = data, model = TRUE)
    )
  )
  
  result_tab <- odds.ratio(model)
  result_tab <- add_p_signif(result_tab)
  
  write.csv(
    result_tab,
    file = paste0(
      "~/Downloads/table_3_output/03_psychological_app_engagement/",
      "Class", ref_class, "_",
      output_name,
      ".csv"
    ),
    row.names = FALSE
  )
}

# PAE cumulative duration
run_and_save_multinom(
  df,
  ref_class = "2",
  formula_str = "Class ~ Cumulative_duration_psychological_capacity_daily_hours",
  output_name = "model0_pae_cumulative_duration"
)

run_and_save_multinom(
  df,
  ref_class = "2",
  formula_str = "Class ~ Cumulative_duration_psychological_capacity_daily_hours + Age + Gender + Education + Literacy",
  output_name = "model1_pae_cumulative_duration"
)

run_and_save_multinom(
  df,
  ref_class = "4",
  formula_str = "Class ~ Cumulative_duration_psychological_capacity_daily_hours",
  output_name = "model0_pae_cumulative_duration"
)

run_and_save_multinom(
  df,
  ref_class = "4",
  formula_str = "Class ~ Cumulative_duration_psychological_capacity_daily_hours + Age + Gender + Education + Literacy",
  output_name = "model1_pae_cumulative_duration"
)

# PAE cumulative frequency
run_and_save_multinom(
  df,
  ref_class = "2",
  formula_str = "Class ~ Cumulative_frequency_psychological_capacity",
  output_name = "model0_pae_cumulative_frequency"
)

run_and_save_multinom(
  df,
  ref_class = "2",
  formula_str = "Class ~ Cumulative_frequency_psychological_capacity + Age + Gender + Education + Literacy",
  output_name = "model1_pae_cumulative_frequency"
)

run_and_save_multinom(
  df,
  ref_class = "4",
  formula_str = "Class ~ Cumulative_frequency_psychological_capacity",
  output_name = "model0_pae_cumulative_frequency"
)

run_and_save_multinom(
  df,
  ref_class = "4",
  formula_str = "Class ~ Cumulative_frequency_psychological_capacity + Age + Gender + Education + Literacy",
  output_name = "model1_pae_cumulative_frequency"
)
