
require(data.table)
require(paramix)
require(ggplot2)
require(patchwork)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("input", "population.rds"),
  file.path("input", "lex.rds"),
  file.path("input", "disease_pars.rda"),
  file.path("output", "incidence.rds"),
  file.path("figure", "summary.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])
load(.args[4])
inc_dt <- readRDS(.args[5])

model_partition <- c(inc_dt[, unique(model_from)], 101)

inc_dt <- inc_dt[method == "f_mean"][,
  .(value = sum(value), capita = sum(unique(capita))),
  by = .(iso3 = place, pathogen, intervention, time)
]

isoset <- inc_dt[, unique(iso3)]

pop_dt <- readRDS(.args[2])[iso3 %in% isoset]
lex_dt <- readRDS(.args[3])[iso3 %in% isoset]

pop_p <- ggplot(pop_dt) + aes(x = from, y = weight) +
  facet_iso() +
  geom_bar(stat = "identity", fill = "#b2abd2", width = 1) +
  theme_minimal() + theme(
    axis.title.x = element_blank(), axis.text.x = element_blank(),
    panel.spacing.x = unit(1.5, "line")
  ) +
  labs(y = "Population\n(thousands)")

lex_p <- ggplot(lex_dt) + aes(x = age, y = ex) +
  facet_iso() +
  geom_line(color = "#5e3c99", lwd=0.8) +
  theme_minimal() + theme(
    strip.background = element_blank(), strip.text = element_blank(),
    axis.title.x = element_blank(), axis.text.x = element_blank(),
    panel.spacing.x = unit(1.5, "line")
  ) +
  labs(y = "Remaining life expectancy\n(years)")

ifr_dt <- pop_dt[,
  ifr_opts |> lapply(\(fp) parameter_summary(fp, .SD, model_partition)) |>
    rbindlist(idcol = "pathogen"),
  by = iso3
]

ifr_p <- ggplot(ifr_dt) + aes(x, y = value, color = method) +
  facet_iso(rows = vars(pathogen), labeller = labeller(
    iso3 = iso_labels, pathogen = pathogen_labels
  )) +
  geom_line(data = \(dt) subset(dt, method == "f_val"), linetype = "dotted", lwd=0.8) +
  geom_step(data = \(dt) subset(dt, method != "f_val"), lwd=0.7) +
  theme_minimal() + theme(
    strip.background.x = element_blank(), strip.text.x = element_blank(),
    legend.position = "inside", legend.position.inside = c(0.5, 0.5),
    legend.justification = c(0.5, 0.5), legend.direction = "horizontal",
    legend.spacing = unit(0.5, "line"),
    panel.spacing.x = unit(1.5, "line")
  ) + scale_color_manual(
    values=model_assumption_cols, "Approach", labels=model_assumption_labels
  ) +
  scale_x_continuous("Age (years)", breaks = seq(0, 100, by = 10)) +
  scale_y_log10(
    "Infection Fatality Ratio",breaks = 10^c(-6, -4, -2, 0), limits = 10^c(-6, 0)
  )

inc_p <- ggplot(inc_dt[between(time, 0, 7*15)]) + aes(
  x = time, y = value/capita, color = intervention, linetype = pathogen
) + facet_iso() +
  geom_line(lwd=0.7) +
  scale_y_continuous(
    "Infections\n(incidence per capita)", limits = c(0, 0.06)
  ) +
  scale_color_intervention() +
  scale_linetype_pathogen() +
  scale_x_simtime('Simulation time (weeks)') +
  theme_minimal() + theme(
    strip.background = element_blank(), strip.text = element_blank(),
    panel.spacing.x = unit(1.5, "line"),
    legend.position = "inside", legend.position.inside = c(0.4, 1.05),
    legend.justification.inside = c(0.5, 1),
    legend.direction = "horizontal",
    legend.margin = margin(), legend.spacing = unit(0, "line"),
    legend.box.margin = margin()
  )


summary_p <- pop_p + lex_p + ifr_p + inc_p + plot_layout(
  ncol = 1, heights = c(1, 1, 2, 1)
) +  plot_annotation(tag_levels = 'a', tag_prefix = '(',
                     tag_suffix = ')  ')

ggsave(
  tail(.args, 1), summary_p, height = 11, width = 8, units = "in", bg = "white"
)
