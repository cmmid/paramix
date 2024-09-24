
require(data.table)
require(wpp2022)
require(countrycode)

.args <- if (interactive()) c(
  file.path("input", "lex.rds")
) else commandArgs(trailingOnly = TRUE)

data("mxB1")
life_expectancy_dt <- setDT(mxB1)[, .(
  iso3 = countrycode(country_code, "iso3n", "iso3c", warn = FALSE),
  age, mx = `2021`
)][!is.na(iso3)][,
  ax := fifelse(age == 0, 0.2, 0.5)
][,
  qx := fifelse(age == max(age), 1, mx / (1 + mx*(1-ax)))
]

life_expectancy_dt[age == 0, lx := 1000]
life_expectancy_dt[, lx := {
  tmp <- lx
  for (i in 2:.N) {
    tmp[i] <- (1-qx[i-1])*tmp[i-1]
  }
  tmp
}, by = iso3]

life_expectancy_dt[,
  Lx := c(
    tail(lx, -1) + head(ax, -1)*(head(lx, -1) - tail(lx, -1)),
    tail(lx, 1)
  ),
  by = iso3
]

life_expectancy_dt[,
  ex := rev(cumsum(rev(Lx)/1000)),
  by = iso3
]

life_expectancy_dt |> saveRDS(tail(.args, 1))
