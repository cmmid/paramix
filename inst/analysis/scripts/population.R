
require(data.table)
require(wpp2022)
require(countrycode)

.args <- if (interactive()) c(
  file.path("input", "population.rds")
) else commandArgs(trailingOnly = TRUE)

data("popAge1dt")
density_dt <- popAge1dt[
  year == 2021, .(
    iso3 = countrycode(country_code, "iso3n", "iso3c", warn = FALSE),
    from = age, weight = pop
  )
][!is.na(iso3)]

density_dt |> saveRDS(tail(.args, 1))
