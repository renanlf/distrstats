moment <- function(psi, pdf, par, lower=-Inf,upper=Inf){
  integrate(function(x) { psi(x) * pdf(par,x) }, lower, upper)$value
}

moment.raw <- function(m, pdf, par, lower=-Inf,upper=Inf) {
  moment(psi = function(x) { x^m }, pdf, par, lower, upper)
}

moment.central <- function(m, pdf, par, lower=-Inf,upper=Inf) {
  mu <- moment.raw(m=1, pdf, par, lower, upper)
  moment(psi = function(x) { (x - mu)^m }, pdf, par, lower, upper)
}

moment.incomplete <- function(m, t, pdf, par, lower=-Inf,upper=Inf) {
  moment(psi = function(x) { ifelse(x <= t, x^m, 0) }, pdf, par, lower, upper)
}

moment.standardized <- function(m, pdf, par, lower=-Inf,upper=Inf) {
  mu.m <- moment.central(m, pdf, par, lower, upper)
  sigma.m <- (moment.central(m=2, pdf, par, lower, upper))^(m/2)
}

moment.mean <- function(pdf, par, lower=-Inf,upper=Inf) {
  moment.raw(m = 1, pdf, par, lower, upper)
}

moment.variance <- function(pdf, par, lower=-Inf,upper=Inf) {
  moment.central(m = 2, pdf, par, lower, upper)
}

moment.skewness <- function(pdf, par, lower=-Inf,upper=Inf) {
  moment.standardized(m = 3, pdf, par, lower, upper)
}

moment.kurtosis <- function(pdf, par, lower=-Inf,upper=Inf) {
  moment.standardized(m = 3, pdf, par, lower, upper)
}
