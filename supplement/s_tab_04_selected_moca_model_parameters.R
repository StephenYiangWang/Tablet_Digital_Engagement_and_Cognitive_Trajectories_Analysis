library(tidyr)
library(lcmm)
library(stringr)
library(plyr)

df <- read.csv(
  "~/Downloads/Suppl_data_3_Metadata_9MCumulativeUsage.csv",
  stringsAsFactors = FALSE
)

df_1 <- subset(
  df,
  select = c("Case_ID", "MoCA_T0", "MoCA_T1", "MoCA_T2", "MoCA_T3")
)

df_long <- gather(df_1, Time, Scores, -Case_ID)
df_long$Time <- gsub("MoCA_T", "", df_long$Time)

dat <- df_long
dat$Time <- as.numeric(dat$Time)
dat$Time1 <- poly(dat$Time, 2)[, 1]
dat$Time2 <- poly(dat$Time, 2)[, 2]

rep <- 100
set.seed(2002)

dat$Case_ID <- substr(dat$Case_ID, start = 3, stop = nchar(dat$Case_ID))
dat$Case_ID <- as.numeric(dat$Case_ID)

load("~/Downloads/MoCA_100rep_growth_models.RData")

# Extract six best models
e_bmt <- list()
for (i in names(e_bestmodels)[1:6]) {
  model <- get(sub("..$", "", i))[[as.numeric(str_sub(i, start = -1))]]
  invisible(capture.output(
    e_bmt[[i]] <- as.data.frame(
      summarytable(model, which = c("G", "AIC", "BIC", "SABIC", "entropy", "%class"))
    )
  ))
}

e_best_models_table <- plyr::rbind.fill(
  as.data.frame(e_bmt[[1]]),
  as.data.frame(e_bmt[[2]]),
  as.data.frame(e_bmt[[3]]),
  as.data.frame(e_bmt[[4]]),
  as.data.frame(e_bmt[[5]]),
  as.data.frame(e_bmt[[6]])
)

# Selected model
e_chosen_model_name <- "e_randI.4"
e_chosen_model <- get(sub("..$", "", e_chosen_model_name))[[as.numeric(str_sub(e_chosen_model_name, start = -1))]]

summary_output <- capture.output(summary(e_chosen_model))

writeLines(
  summary_output,
  con = "~/Downloads/supplementary_table_4_selected_model_summary.txt"
)
