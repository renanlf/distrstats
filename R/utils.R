as.dataset <- function(name, values){
  list(
    name = name,
    values = values
  )
}

as.distribution <- function(name, pdf, cdf, nparams, lower, upper){
  list(
    name = name,
    pdf = pdf,
    cdf = cdf,
    nparams = nparams,
    lower = lower,
    upper = upper,
  )
}
