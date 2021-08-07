lorenz <- function(x, par, p){
  Qp <- Q(u = p, a = par[1], b = par[2], c = par[3], lambda = par[4])
  mu1Q <- incomplete.moment(x, par, m = 1, t = Qp)$value
  mu <- raw.moment(x, par, m = 1)$value

  mu1Q / mu
}

bonferroni <- function(x, par, p) {
  Qp <- Q(u = p, a = par[1], b = par[2], c = par[3], lambda = par[4])
  mu1Q <- incomplete.moment(x, par, m = 1, t = Qp)$value
  mu <- raw.moment(x, par, m = 1)$value

  mu1Q / (p * mu)
}
