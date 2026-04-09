library(dplyr)
library(ggplot2)

df <- read.csv(
  "~/Downloads/Suppl_data_1_Metadata_apps.csv",
  stringsAsFactors = FALSE
)

df <- df %>%
  group_by(Categories) %>%
  arrange(desc(ALL_Count_App), .by_group = TRUE) %>%
  ungroup()

df$Categories <- factor(df$Categories)

empty_bar <- 0
to_add <- data.frame(matrix(NA, empty_bar * nlevels(df$Categories), ncol(df)))
colnames(to_add) <- colnames(df)
to_add$Categories <- rep(levels(df$Categories), each = empty_bar)

df <- df %>% arrange(Categories)
df$id <- seq_len(nrow(df))
df$perc <- df$ALL_Count_App / 1185 * 100

label_data <- df
number_of_bar <- nrow(label_data)

label_data <- label_data %>%
  mutate(angle = 90 - 360 * (id - 0.5) / number_of_bar)

label_data$hjust <- ifelse(label_data$angle < -90, 1, 0)
label_data$angle <- ifelse(label_data$angle < -90, label_data$angle + 180, label_data$angle)

beautiful_colors <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728",
  "#0ec434", "#228c68", "#8ad8e8", "#235b54",
  "#29bdab", "#3998f5", "#37294f", "#277da7",
  "#3750db", "#f22020", "#991919", "#ffcba5",
  "#e68f66", "#c56133", "#96341c", "#632819",
  "#ffc413", "#f47a22", "#2f2aa0", "#b732cc",
  "#772b9d", "#f07cab", "#d30b94", "#edeff3",
  "#c3a5b4", "#946aa2"
)

p10 <- ggplot(label_data, aes(x = factor(id), y = perc, fill = Categories)) +
  geom_rect(
    aes(
      fill = Googleplay,
      xmin = id - 0.5,
      xmax = id + 0.5,
      ymin = 100,
      ymax = 110
    ),
    color = "black",
    linewidth = 0.01
  ) +
  geom_bar(
    stat = "identity",
    alpha = 0.8,
    position = "dodge",
    width = 0.5
  ) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#9fdce1", linewidth = 0.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "#9fdce1", linewidth = 0.5) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "#9fdce1", linewidth = 0.5) +
  coord_polar(start = 0) +
  scale_fill_manual(values = beautiful_colors) +
  scale_x_discrete(labels = label_data$APP.name) +
  geom_text(
    aes(
      x = id,
      y = 115,
      label = APP.name,
      angle = angle,
      hjust = hjust
    ),
    size = 2,
    fontface = "bold"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  ylim(-100, 190)

print(p10)

ggsave(
  filename = "~/Downloads/supplementary_figure_4_app_user_distribution_circular_plot.jpg",
  plot = p10,
  dpi = 1200,
  width = 12,
  height = 12,
  units = "in"
)
