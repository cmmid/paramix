
require(data.table)
require(ggplot2)
require(patchwork)

.args <- if (interactive()) c(
  file.path("figure", "fig_utilities.rda"),
  file.path("input", "population.rds"),
  file.path("figure", "full_summary.png")
) else commandArgs(trailingOnly = TRUE)

load(.args[1])

pop_dt <- readRDS(.args[2])



plot8 <- function(dt){
  pop <- ggplot(dt, aes(x=age, y=pop)) +
    geom_bar(position='dodge', stat='identity', fill='lightsalmon') +
    theme_minimal() + xlab('') + ylab('Population')
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
