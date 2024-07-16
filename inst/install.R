
# TODO convert to pacman?

if (!require(data.table)) {
	install.packages("data.table")
}

if (!require(jsonlite)) {
  install.packages("jsonlite")
}

if (!require(countrycode)) {
  install.packages("countrycode")
}

if (!require(ggplot2)) {
	install.packages("ggplot2")
}

if (!require(wpp2022)) {
  # TODO remotes install wpp2022
}
