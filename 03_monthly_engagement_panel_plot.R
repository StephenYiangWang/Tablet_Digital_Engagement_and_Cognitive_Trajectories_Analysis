library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)

df <- read.csv("~/Downloads/Suppl_data_4_Metadata_MonthlyUsage.csv", stringsAsFactors = FALSE)

df$Months_From_Earliest <- as.factor(df$Months_From_Earliest)
df$Class <- as.factor(df$Class)

class_colors <- c("#CA601B", "#1B85B2", "#DD0187", "#037203")
class_labels <- c("Class 1", "Class 2", "Class 3", "Class 4")

make_monthly_plot <- function(data, y_var, panel_title, y_label, label_y) {
  
  summary_df <- data |>
    group_by(Months_From_Earliest, Class) |>
    summarise(
      med = median(.data[[y_var]], na.rm = TRUE),
      Q1 = quantile(.data[[y_var]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[y_var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(data, aes(x = Months_From_Earliest, y = .data[[y_var]], color = Class)) +
    geom_pointrange(
      data = summary_df,
      aes(y = med, ymin = Q1, ymax = Q3),
      position = position_dodge(width = 0.5),
      size = 0.1,
      show.legend = TRUE
    ) +
    geom_line(
      data = summary_df,
      aes(y = med, group = Class)
    ) +
    stat_compare_means(
      aes(group = Class),
      method = "kruskal.test",
      label = "p.signif",
      label.y = label_y,
      show.legend = FALSE
    ) +
    labs(
      x = "Months",
      y = y_label,
      title = panel_title,
      color = "MoCA trajectory classes"
    ) +
    theme_light() +
    scale_color_manual(
      values = class_colors,
      labels = class_labels
    ) +
    scale_x_discrete(
      labels = paste("Month", seq_along(levels(data$Months_From_Earliest)))
    ) +
    theme(
      plot.title = element_text(face = "plain"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x = element_text(margin = margin(t = 20))
    )
}

# TDE plots
df_all <- subset(df, Categories == "ALL")

pa <- make_monthly_plot(
  data = df_all,
  y_var = "Absolute_duration_hours_monthly",
  panel_title = expression(bold("a.") ~ "Absolute monthly duration (TDE)"),
  y_label = "Hours",
  label_y = 55
)

pb <- make_monthly_plot(
  data = df_all,
  y_var = "Cumulative_duration_hours_monthly",
  panel_title = expression(bold("b.") ~ "Cumulative monthly duration (TDE)"),
  y_label = "Hours",
  label_y = 310
)

pc <- make_monthly_plot(
  data = df_all,
  y_var = "absolute_freq_monthly",
  panel_title = expression(bold("c.") ~ "Absolute monthly frequency (TDE)"),
  y_label = "Frequency",
  label_y = 1
)

pd <- make_monthly_plot(
  data = df_all,
  y_var = "cumulative_freq_monthly",
  panel_title = expression(bold("d.") ~ "Cumulative monthly frequency (TDE)"),
  y_label = "Frequency",
  label_y = 0.6
)

# PAE plots
df_psy <- subset(df, Categories == "Psychological_capacity")

pe <- make_monthly_plot(
  data = df_psy,
  y_var = "Absolute_duration_hours_monthly",
  panel_title = expression(bold("e.") ~ "Absolute monthly duration (PAE)"),
  y_label = "Hours",
  label_y = 30
)

pf <- make_monthly_plot(
  data = df_psy,
  y_var = "Cumulative_duration_hours_monthly",
  panel_title = expression(bold("f.") ~ "Cumulative monthly duration (PAE)"),
  y_label = "Hours",
  label_y = 200
)

pg <- make_monthly_plot(
  data = df_psy,
  y_var = "absolute_freq_monthly",
  panel_title = expression(bold("g.") ~ "Absolute monthly frequency (PAE)"),
  y_label = "Frequency",
  label_y = 0.7
)

ph <- make_monthly_plot(
  data = df_psy,
  y_var = "cumulative_freq_monthly",
  panel_title = expression(bold("h.") ~ "Cumulative monthly frequency (PAE)"),
  y_label = "Frequency",
  label_y = 0.45
)

# CAE plots
df_cog <- subset(df, Categories == "Cognitive_capacity")

pi <- make_monthly_plot(
  data = df_cog,
  y_var = "Absolute_duration_hours_monthly",
  panel_title = expression(bold("i.") ~ "Absolute monthly duration (CAE)"),
  y_label = "Hours",
  label_y = 5
)

pj <- make_monthly_plot(
  data = df_cog,
  y_var = "Cumulative_duration_hours_monthly",
  panel_title = expression(bold("j.") ~ "Cumulative monthly duration (CAE)"),
  y_label = "Hours",
  label_y = 50
)

pk <- make_monthly_plot(
  data = df_cog,
  y_var = "absolute_freq_monthly",
  panel_title = expression(bold("k.") ~ "Absolute monthly frequency (CAE)"),
  y_label = "Frequency",
  label_y = 0.55
)

pl <- make_monthly_plot(
  data = df_cog,
  y_var = "cumulative_freq_monthly",
  panel_title = expression(bold("l.") ~ "Cumulative monthly frequency (CAE)"),
  y_label = "Frequency",
  label_y = 0.35
)

# Combine all panels
p <- pa + pb + pc +
     pd + pe + pf +
     pg + ph + pi +
     pj + pk + pl +
  plot_layout(ncol = 3, nrow = 4, guides = "collect") &
  theme(legend.position = "bottom")

print(p)

ggsave(
  filename = "~/Downloads/figure_2_monthly_engagement_panel.png",
  plot = p,
  dpi = 800,
  width = 20,
  height = 16,
  units = "in"
)
