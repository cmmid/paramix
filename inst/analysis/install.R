
stopifnot("Must have `renv` installed." = require(renv))

renv::activate()
tryCatch(
  res <- renv::restore(prompt = FALSE),
  error = function(e) {
    warning(e)
    renv::deactivate()
  }
)

print(res)
