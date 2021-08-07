mle.run <- function(par,
                    data,
                    distribution,
                    ...) {

  pdf <- distribution$pdf
  cdf <- distribution$cdf

  likelihood <- function(par, x){
    if(sum(par < 0) > 0) { return(NaN) }

    -sum(log(pdf(par, x)))
  }

  mle = optim(par = starts
              , fn = likelihood
              , x = data
              , ...
  )

  # Statistics
  ll <- likelihood(mle$par, data)
  AIC <- -2 * ll +        2          * length(mle$par)
  BIC <- -2 * ll + log(length(data)) * length(mle$par)

  if(hessian){
    Erro <- sqrt(diag(solve(mle$hessian)))
  } else {
    Erro <- NaN
  }

  A = ad.test(x = data
              , null = cdf
              , par = mle$par
              , estimated = TRUE
  )

  W = cvm.test(x = data
               , null = cdf
               , par = mle$par
               , estimated = TRUE
  )

  return(list(
    par            = mle$par,
    convergence    = mle$convergence,
    value          = mle$value,
    hessian        = mle$hessian,
    Erro           = Erro,
    AIC            = AIC,
    BIC            = BIC,
    A              = A$statistic,
    W              = W$statistic
  ))
}

best.fits <- function(datasets, distributions, criteria='A', nrep = 100, verbose=FALSE, ...){
  for(dataset in datasets){

    if(verbose) { print(paste('Running for dataset',dataset$name)) }

    for(distribution in distributions){

      if(verbose) { print(paste('--Running distribution',distribution$name)) }

      for(i in 1:nrep){
        if(verbose) { print(paste('----Running replica', i)) }
      }
    }
  }
}
