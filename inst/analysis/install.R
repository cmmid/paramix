
stopifnot("Must have `renv` installed." = require(renv))

renv::activate()
res <- renv::restore(prompt = FALSE)

print(res)
