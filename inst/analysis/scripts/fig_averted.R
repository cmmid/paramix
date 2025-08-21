
library(data.table)
library(ggplot2)
library(ggh4x)
library(patchwork)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("output", "incidence.rds"),
  file.path("figure", "averted.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
ts_dt <- readRDS(.args[2])

joinvars <- c("method", "place", "pathogen")

base_dt <- ts_dt[
  intervention == "none", .SD, .SDcols = -c("intervention", "capita")
][,.(deaths = sum(deaths)), by = c(joinvars)]

int_dt <- ts_dt[intervention != "none", .(
  deaths = sum(deaths), capita = sum(unique(capita))
), by = c(joinvars, "intervention")][
  base_dt, on = c(joinvars)
]

int_dt[, averted_death := i.deaths - deaths]

int_dt$method <- factor(int_dt$method, levels=unique(int_dt$method))
int_dt$intervention <- factor(int_dt$intervention, levels=unique(int_dt$intervention))

# in this model, deaths do not affect dynamics, so the method for aggregating
# death parameter (`method` field) is irrelevant

max_hic <- 1000*max(int_dt[!method=='mean_f' & place == 'GBR',]$averted_death)/max(int_dt[place == 'GBR',]$capita)
max_lmic <- 1000*max(int_dt[!method=='mean_f' & place == 'AFG',]$averted_death)/max(int_dt[place == 'AFG',]$capita)

plot_p <- ggplot(int_dt[!method=='mean_f' & place=='AFG' & pathogen=='FLU']) + aes(
  x = intervention, y = 1000*averted_death/capita, fill = method
) +
  facet_nested(pathogen ~ place, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line"),
    strip.text.y = element_blank()
  ) +
  scale_x_discrete("Vaccination age group", labels = intervention_labels) +
  scale_y_continuous("Deaths averted (per 1000 total population)", limits = c(0, max_lmic)) +
  scale_color_model()

plot_q <- ggplot(int_dt[!method=='mean_f' & place=='GBR' & pathogen=='FLU']) + aes(
  x = intervention, y = 1000*averted_death/capita, fill = method
) +
  facet_nested(pathogen ~ place, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line")
  ) +
  scale_x_discrete("Vaccination age group", labels = intervention_labels) +
  scale_y_continuous("Deaths averted (per 1000 total population)", limits = c(0, max_hic)) +
  scale_color_model()

plot_r <- ggplot(int_dt[!method=='mean_f' & place=='AFG' & pathogen=='SC2']) + aes(
  x = intervention, y = 1000*averted_death/capita, fill = method
) +
  facet_nested(pathogen ~ place, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line"),
    strip.text.y = element_blank(),
    strip.text.x = element_blank()
  ) +
  scale_x_discrete("Vaccination age group", labels = intervention_labels) +
  scale_y_continuous("Deaths averted (per 1000 total population)", limits = c(0, max_lmic)) +
  scale_color_model()

plot_s <- ggplot(int_dt[!method=='mean_f' & place=='GBR' & pathogen=='SC2']) + aes(
  x = intervention, y = 1000*averted_death/capita, fill = method
) +
  facet_nested(pathogen ~ place, scale = "free_y", labeller = labeller(
    pathogen = pathogen_labels, place = iso_labels
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() + theme(
    element_text(size = 16), legend.position = "right",
    panel.spacing.x = unit(1.5, "line"),
    strip.text.x = element_blank()
  ) +
  scale_x_discrete("Vaccination age group", labels = intervention_labels) +
  scale_y_continuous("Deaths averted (per 1000 total population)", limits = c(0, max_hic)) +
  scale_color_model()

plots <- plot_p + plot_q + plot_r + plot_s + plot_layout(nrow = 2, axes = 'collect', guides = 'collect')

tarfile <- tail(.args, 1)

ggsave_opts <- list(
  filename = tarfile, plot = plots,
  width = 25, height = 14, units = "cm", bg = "white"
)

if (tarfile %like% "tiff$") {
  ggsave_opts$compression <- "lzw+p"
  ggsave_opts$dpi <- 600
  hwratio <- ggsave_opts$height / ggsave_opts$width
  ggsave_opts$width <- 19
  ggsave_opts$height <- ggsave_opts$width * hwratio
}

do.call(ggsave, ggsave_opts)
