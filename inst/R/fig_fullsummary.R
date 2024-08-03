
require(data.table)
require(ggplot2)
require(patchwork)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("input", "population.rds"),
  list.files("input", "^param_.*\\.rda", full.names = TRUE),
  file.path("output", "incidence.rds"),
  file.path("figure", "full_summary.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])

isoset <- .args[-1] |> grep("^.*param_.*\\.rda", x = _, value = TRUE) |>
  gsub("^.+_(.+)_.+\\.rda$", "\\1", x = _) |> unique()

pop_dt <- readRDS(.args[2])[iso3 %in% isoset]

pop_p <- ggplot(pop_dt) + aes(x = from, y = weight) +
  facet_grid(. ~ iso3) +
  geom_bar(stat = 'identity', fill = 'lightsalmon', width = 1) +
  theme_minimal() + xlab('Age [years]') + ylab('Population [1Ks]')

inc_dt <- readRDS(tail(.args, 2)[1])[
  method == "f_mean"
][,
  .(value = sum(value), capita = sum(capita)),
  by = .(place, pathogen, intervention, time)
]

inc_p <- ggplot(inc_dt[between(time, 0, 70)]) + aes(
  x = time, y = value/capita, color = intervention, linetype = pathogen
) + facet_grid(. ~ place) + geom_line() +
  theme_minimal() + scale_x_simtime()

plot8 <- function(dt) {
  inf <- ggplot(dt, aes(x=age, y=infections)) +
    geom_bar(position='dodge', stat='identity', fill='darkorange1') +
    theme_minimal() + xlab('') + ylab('Infections')
  mort <- ggplot(dt, aes(x=age, y=cont_ifr)) +
    geom_line() + theme_minimal() + xlab('') + ylab('IFR \n(log 10 scale)') +
    scale_y_log10()
  ifr <- ggplot(dt, aes(x=age, y=agg_ifr)) +
    geom_line() + theme_minimal() + xlab('') + ylab('Aggregated IFR \n(log 10 scale)') +
    scale_y_log10()
  agg_deaths <- ggplot(dt, aes(x=age, y=agg_d)) +
    geom_bar(position='dodge', stat='identity', fill='red2') +
    theme_minimal() + xlab('') + ylab('Aggregated deaths')
  as_deaths <- ggplot(dt, aes(x=age, y=deaths)) +
    geom_bar(position='dodge', stat='identity', fill='red2') +
    theme_minimal() + xlab('') + ylab('Deaths')
  ex_p <- ggplot(dt, aes(x=age, y=ex)) +
    geom_line() + theme_minimal() + xlab('Age') + ylab('Life expectancy')
  yll_p <- ggplot(dt, aes(x=age, y=yll)) +
    geom_bar(position='dodge', stat='identity', fill='mediumpurple4') +
    theme_minimal() + xlab('Age') + ylab('Years-of-life lost')

  pop + inf + mort + ifr + agg_deaths + as_deaths + ex_p + yll_p +
    plot_layout(nrow=4)
}
