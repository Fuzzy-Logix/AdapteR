## This file is in most parts automatically generated.
## Manual changes need to be commited to git and then reintroduced after regenerating the file.


#' @export
setGeneric('rbinom', function(n, size, prob) { standardGeneric('rbinom') })
setMethod('rbinom', signature(n='integer'),
       function(n, size, prob)
               stats::rbinom(n, size, prob))
setMethod('rbinom', signature(n='FLSimpleVector'),
       function(n, size, prob)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rbinom'
  n@select@variables[[1]] <- paste0('FLSimBinomial(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    prob,',',size,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rbeta', function(n, shape1, shape2, ncp=0) { standardGeneric('rbeta') })
setMethod('rbeta', signature(n='integer'),
       function(n, shape1, shape2, ncp=0)
               stats::rbeta(n, shape1, shape2, ncp))
setMethod('rbeta', signature(n='FLSimpleVector'),
       function(n, shape1, shape2, ncp=0)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rbeta'
  n@select@variables[[1]] <- paste0('FLSimBeta(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    0,',',0,',',shap1,',',shape2,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rbradford', function(n, lowbd, uppbd, shape) { standardGeneric('rbradford') })
setMethod('rbradford', signature(n='FLSimpleVector'),
       function(n, lowbd, uppbd, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rbradford'
  n@select@variables[[1]] <- paste0('FLSimBradford(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowbd,',',uppbd,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rburr', function(n, shape1, shape2, rate=1, scale=1/rate) { standardGeneric('rburr') })
setMethod('rburr', signature(n='integer'),
       function(n, shape1, shape2, rate=1, scale=1/rate)
               actuar::rburr(n, shape1, shape2, rate, scale))
setMethod('rburr', signature(n='FLSimpleVector'),
       function(n, shape1, shape2, rate=1, scale=1/rate)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rburr'
  n@select@variables[[1]] <- paste0('FLSimBurr(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowbd=0,',',scale,',',shape1,',',shape2,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rcauchy', function(n, location=0, scale=1) { standardGeneric('rcauchy') })
setMethod('rcauchy', signature(n='integer'),
       function(n, location=0, scale=1)
               stats::rcauchy(n, location, scale))
setMethod('rcauchy', signature(n='FLSimpleVector'),
       function(n, location=0, scale=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rcauchy'
  n@select@variables[[1]] <- paste0('FLSimCauchy(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',', scale,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rchi', function(n, lowBD, scale, df) { standardGeneric('rchi') })
setMethod('rchi', signature(n='FLSimpleVector'),
       function(n, lowBD, scale, df)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rchi'
  n@select@variables[[1]] <- paste0('FLSimChi(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowBD,',',scale,',',df,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rchisq', function(n, df, ncp=0) { standardGeneric('rchisq') })
setMethod('rchisq', signature(n='integer'),
       function(n, df, ncp=0)
               stats::rchisq(n, df, ncp))
setMethod('rchisq', signature(n='FLSimpleVector'),
       function(n, df, ncp=0)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rchisq'
  n@select@variables[[1]] <- paste0('FLSimChiSq(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    df,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rcosine', function(n, location, scale) { standardGeneric('rcosine') })
setMethod('rcosine', signature(n='FLSimpleVector'),
       function(n, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rcosine'
  n@select@variables[[1]] <- paste0('FLSimCosine(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',scale,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rdoublegamma', function(n, location, scale, shape) { standardGeneric('rdoublegamma') })
setMethod('rdoublegamma', signature(n='FLSimpleVector'),
       function(n, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rdoublegamma'
  n@select@variables[[1]] <- paste0('FLSimDoubleGamma(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rdoubleweibull', function(n, location, scale, shape) { standardGeneric('rdoubleweibull') })
setMethod('rdoubleweibull', signature(n='FLSimpleVector'),
       function(n, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rdoubleweibull'
  n@select@variables[[1]] <- paste0('FLSimDoubleWeibull(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rerlang', function(n, lowBD, scale, shape) { standardGeneric('rerlang') })
setMethod('rerlang', signature(n='FLSimpleVector'),
       function(n, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rerlang'
  n@select@variables[[1]] <- paste0('FLSimErlang(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowBD,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rexp', function(n, rate=1) { standardGeneric('rexp') })
setMethod('rexp', signature(n='integer'),
       function(n, rate=1)
               stats::rexp(n, rate))
setMethod('rexp', signature(n='FLSimpleVector'),
       function(n, rate=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rexp'
  n@select@variables[[1]] <- paste0('FLSimExp(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    0,',',rate,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rextremeLB', function(n, lowBD, scale, shape) { standardGeneric('rextremeLB') })
setMethod('rextremeLB', signature(n='FLSimpleVector'),
       function(n, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rextremeLB'
  n@select@variables[[1]] <- paste0('FLSimExtremeLB(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowBD,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rfisk', function(n, lowBD, scale, shape) { standardGeneric('rfisk') })
setMethod('rfisk', signature(n='FLSimpleVector'),
       function(n, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rfisk'
  n@select@variables[[1]] <- paste0('FLSimFisk(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowBD,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rfoldnormal', function(n, mean, sd) { standardGeneric('rfoldnormal') })
setMethod('rfoldnormal', signature(n='FLSimpleVector'),
       function(n, mean, sd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rfoldnormal'
  n@select@variables[[1]] <- paste0('FLSimFoldedNormal(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    mean,',',sd,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rgamma', function(n, shape, rate=1, scale=1/rate) { standardGeneric('rgamma') })
setMethod('rgamma', signature(n='integer'),
       function(n, shape, rate=1, scale=1/rate)
               stats::rgamma(n, shape, rate, scale))
setMethod('rgamma', signature(n='FLSimpleVector'),
       function(n, shape, rate=1, scale=1/rate)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rgamma'
  n@select@variables[[1]] <- paste0('FLSimGamma(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    0,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rglogis', function(n, location=0, scale=1, shape=1) { standardGeneric('rglogis') })
setMethod('rglogis', signature(n='integer'),
       function(n, location=0, scale=1, shape=1)
               glogis::rglogis(n, location, scale, shape))
setMethod('rglogis', signature(n='FLSimpleVector'),
       function(n, location=0, scale=1, shape=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rglogis'
  n@select@variables[[1]] <- paste0('FLSimGenLogistic(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rgeom', function(n, prob) { standardGeneric('rgeom') })
setMethod('rgeom', signature(n='integer'),
       function(n, prob)
               stats::rgeom(n, prob))
setMethod('rgeom', signature(n='FLSimpleVector'),
       function(n, prob)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rgeom'
  n@select@variables[[1]] <- paste0('FLSimGeometric(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    prob,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rgumbel', function(n, mu=0, sigma=1) { standardGeneric('rgumbel') })
setMethod('rgumbel', signature(n='integer'),
       function(n, mu=0, sigma=1)
               extraDistr::rgumbel(n, mu, sigma))
setMethod('rgumbel', signature(n='FLSimpleVector'),
       function(n, mu=0, sigma=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rgumbel'
  n@select@variables[[1]] <- paste0('FLSimGumbel(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    mu,',',sigma,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rHyperbolicSecant', function(n, location, scale) { standardGeneric('rHyperbolicSecant') })
setMethod('rHyperbolicSecant', signature(n='FLSimpleVector'),
       function(n, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rHyperbolicSecant'
  n@select@variables[[1]] <- paste0('FLSimHypSecant(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',scale,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rinvgamma', function(n, alpha, beta=1) { standardGeneric('rinvgamma') })
setMethod('rinvgamma', signature(n='integer'),
       function(n, alpha, beta=1)
               extraDistr::rinvgamma(n, alpha, beta))
setMethod('rinvgamma', signature(n='FLSimpleVector'),
       function(n, alpha, beta=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rinvgamma'
  n@select@variables[[1]] <- paste0('FLSimInvGamma(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    beta,',',alpha,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rInvNorm', function(n, mu, lambda) { standardGeneric('rInvNorm') })
setMethod('rInvNorm', signature(n='FLSimpleVector'),
       function(n, mu, lambda)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rInvNorm'
  n@select@variables[[1]] <- paste0('FLSimInvNormal(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    mu,',',lambda,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rlaplace', function(n, mu=0, sigma=1) { standardGeneric('rlaplace') })
setMethod('rlaplace', signature(n='integer'),
       function(n, mu=0, sigma=1)
               extraDistr::rlaplace(n, mu, sigma))
setMethod('rlaplace', signature(n='FLSimpleVector'),
       function(n, mu=0, sigma=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlaplace'
  n@select@variables[[1]] <- paste0('FLSimLaplace(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    mu,',',sigma,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rlgser', function(n, theta) { standardGeneric('rlgser') })
setMethod('rlgser', signature(n='integer'),
       function(n, theta)
               extraDistr::rlgser(n, theta))
setMethod('rlgser', signature(n='FLSimpleVector'),
       function(n, theta)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlgser'
  n@select@variables[[1]] <- paste0('FLSimLaplace(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    theta,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rlogis', function(n, location=0, scale=1) { standardGeneric('rlogis') })
setMethod('rlogis', signature(n='integer'),
       function(n, location=0, scale=1)
               stats::rlogis(n, location, scale))
setMethod('rlogis', signature(n='FLSimpleVector'),
       function(n, location=0, scale=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlogis'
  n@select@variables[[1]] <- paste0('FLSimLogistic(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',scale,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rlnorm', function(n, meanlog=0, sdlog=1) { standardGeneric('rlnorm') })
setMethod('rlnorm', signature(n='integer'),
       function(n, meanlog=0, sdlog=1)
               stats::rlnorm(n, meanlog, sdlog))
setMethod('rlnorm', signature(n='FLSimpleVector'),
       function(n, meanlog=0, sdlog=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlnorm'
  n@select@variables[[1]] <- paste0('FLSimLogNormal(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    meanlog,',',sdlog,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rmaxwell', function(n, rate) { standardGeneric('rmaxwell') })
setMethod('rmaxwell', signature(n='integer'),
       function(n, rate)
               VGAM::rmaxwell(n, rate))
setMethod('rmaxwell', signature(n='FLSimpleVector'),
       function(n, rate)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rmaxwell'
  n@select@variables[[1]] <- paste0('FLSimMaxwell(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    a,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rnbinom', function(n, size, prob, mu) { standardGeneric('rnbinom') })
setMethod('rnbinom', signature(n='integer'),
       function(n, size, prob, mu)
               stats::rnbinom(n, size, prob, mu))
setMethod('rnbinom', signature(n='FLSimpleVector'),
       function(n, size, prob, mu)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rnbinom'
  n@select@variables[[1]] <- paste0('FLSimNegBinomial(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    prob,',',size,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rnorm', function(n, mean=0, sd=1) { standardGeneric('rnorm') })
setMethod('rnorm', signature(n='integer'),
       function(n, mean=0, sd=1)
               stats::rnorm(n, mean, sd))
setMethod('rnorm', signature(n='FLSimpleVector'),
       function(n, mean=0, sd=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rnorm'
  n@select@variables[[1]] <- paste0('FLSimNormal(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    mean,',',sd,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rpareto', function(n, a=1, b=1) { standardGeneric('rpareto') })
setMethod('rpareto', signature(n='integer'),
       function(n, a=1, b=1)
               extraDistr::rpareto(n, a, b))
setMethod('rpareto', signature(n='FLSimpleVector'),
       function(n, a=1, b=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rpareto'
  n@select@variables[[1]] <- paste0('FLSimPareto(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    b,',',a,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rpois', function(n, lambda) { standardGeneric('rpois') })
setMethod('rpois', signature(n='integer'),
       function(n, lambda)
               stats::rpois(n, lambda))
setMethod('rpois', signature(n='FLSimpleVector'),
       function(n, lambda)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rpois'
  n@select@variables[[1]] <- paste0('FLSimPoisson(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lambda,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rplcon', function(n, xmin, alpha) { standardGeneric('rplcon') })
setMethod('rplcon', signature(n='integer'),
       function(n, xmin, alpha)
               poweRlaw::rplcon(n, xmin, alpha))
setMethod('rplcon', signature(n='FLSimpleVector'),
       function(n, xmin, alpha)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rplcon'
  n@select@variables[[1]] <- paste0('FLSimPower(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    alpha) ##??,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rrayleigh', function(n, sigma=1) { standardGeneric('rrayleigh') })
setMethod('rrayleigh', signature(n='integer'),
       function(n, sigma=1)
               extraDistr::rrayleigh(n, sigma))
setMethod('rrayleigh', signature(n='FLSimpleVector'),
       function(n, sigma=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rrayleigh'
  n@select@variables[[1]] <- paste0('FLSimRayleigh(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    sigma,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rreciprocal', function(n, lowbd, upbd) { standardGeneric('rreciprocal') })
setMethod('rreciprocal', signature(n='FLSimpleVector'),
       function(n, lowbd, upbd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rreciprocal'
  n@select@variables[[1]] <- paste0('FLSimReciprocal(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    lowbd,',',upbd,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rsemicircular', function(n, location, radius) { standardGeneric('rsemicircular') })
setMethod('rsemicircular', signature(n='FLSimpleVector'),
       function(n, location, radius)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rsemicircular'
  n@select@variables[[1]] <- paste0('FLSimSemicircular(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location,',',radius,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rt', function(n, df, ncp) { standardGeneric('rt') })
setMethod('rt', signature(n='integer'),
       function(n, df, ncp)
               stats::rt(n, df, ncp))
setMethod('rt', signature(n='FLSimpleVector'),
       function(n, df, ncp)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rt'
  n@select@variables[[1]] <- paste0('FLSimStudentsT(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    0,',',1,',',df,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rtrbeta', function(n, shape1, shape2, shape3, rate=1, scale=1/rate) { standardGeneric('rtrbeta') })
setMethod('rtrbeta', signature(n='integer'),
       function(n, shape1, shape2, shape3, rate=1, scale=1/rate)
               actuar::rtrbeta(n, shape1, shape2, shape3, rate, scale))
setMethod('rtrbeta', signature(n='FLSimpleVector'),
       function(n, shape1, shape2, shape3, rate=1, scale=1/rate)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rtrbeta'
  n@select@variables[[1]] <- paste0('FLSimTransBeta(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    scale,',',shape1,',',shape2,',',shape3,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rtriang', function(n, a=-1, b=1, c=(a + b)/2) { standardGeneric('rtriang') })
setMethod('rtriang', signature(n='integer'),
       function(n, a=-1, b=1, c=(a + b)/2)
               extraDistr::rtriang(n, a, b, c))
setMethod('rtriang', signature(n='FLSimpleVector'),
       function(n, a=-1, b=1, c=(a + b)/2)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rtriang'
  n@select@variables[[1]] <- paste0('FLSimTriangular(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    a,',',b,',',c,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('runif', function(n, min=0, max=1) { standardGeneric('runif') })
setMethod('runif', signature(n='integer'),
       function(n, min=0, max=1)
               stats::runif(n, min, max))
setMethod('runif', signature(n='FLSimpleVector'),
       function(n, min=0, max=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_runif'
  n@select@variables[[1]] <- paste0('FLSimUniform(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    min,',',max,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('rweibull', function(n, shape, scale=1) { standardGeneric('rweibull') })
setMethod('rweibull', signature(n='integer'),
       function(n, shape, scale=1)
               stats::rweibull(n, shape, scale))
setMethod('rweibull', signature(n='FLSimpleVector'),
       function(n, shape, scale=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rweibull'
  n@select@variables[[1]] <- paste0('FLSimWeibull(',
                                    n@select@variables[[1]],'+',round(runif(1)*10000),',',
                                    location=0,',',scale,',',shape,')')
  n@type <- 'numeric'
  n
})


#' @export
setGeneric('pbinom', function(q, size, prob, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pbinom') })
setMethod('pbinom', signature(q='integer'),
       function(q, size, prob, lower.tail=TRUE, log.p=FALSE)
               stats::pbinom(q, size, prob, lower.tail, log.p))
setMethod('pbinom', signature(q='FLSimpleVector'),
       function(q, size, prob, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pbinom'
  n@select@variables[[1]] <- paste0('FLCDFBinomial(',
                                    n@select@variables[[1]],',',
                                    prob,',',size,')')
  n
})


#' @export
setGeneric('pbeta', function(q, shape1, shape2, ncp=0, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pbeta') })
setMethod('pbeta', signature(q='integer'),
       function(q, shape1, shape2, ncp=0, lower.tail=TRUE, log.p=FALSE)
               stats::pbeta(q, shape1, shape2, ncp, lower.tail, log.p))
setMethod('pbeta', signature(q='FLSimpleVector'),
       function(q, shape1, shape2, ncp=0, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pbeta'
  n@select@variables[[1]] <- paste0('FLCDFBeta(',
                                    n@select@variables[[1]],',',
                                    0,',',0,',',shap1,',',shape2,')')
  n
})


#' @export
setGeneric('pbradford', function(q, lowbd, uppbd, shape) { standardGeneric('pbradford') })
setMethod('pbradford', signature(q='FLSimpleVector'),
       function(q, lowbd, uppbd, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pbradford'
  n@select@variables[[1]] <- paste0('FLCDFBradford(',
                                    n@select@variables[[1]],',',
                                    lowbd,',',uppbd,',',shape,')')
  n
})


#' @export
setGeneric('pburr', function(q, shape1, shape2, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pburr') })
setMethod('pburr', signature(q='integer'),
       function(q, shape1, shape2, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
               actuar::pburr(q, shape1, shape2, rate, scale, lower.tail, log.p))
setMethod('pburr', signature(q='FLSimpleVector'),
       function(q, shape1, shape2, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pburr'
  n@select@variables[[1]] <- paste0('FLCDFBurr(',
                                    n@select@variables[[1]],',',
                                    lowbd=0,',',scale,',',shape1,',',shape2,')')
  n
})


#' @export
setGeneric('pcauchy', function(q, location=0, scale=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pcauchy') })
setMethod('pcauchy', signature(q='integer'),
       function(q, location=0, scale=1, lower.tail=TRUE, log.p=FALSE)
               stats::pcauchy(q, location, scale, lower.tail, log.p))
setMethod('pcauchy', signature(q='FLSimpleVector'),
       function(q, location=0, scale=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pcauchy'
  n@select@variables[[1]] <- paste0('FLCDFCauchy(',
                                    n@select@variables[[1]],',',
                                    location,',', scale,')')
  n
})


#' @export
setGeneric('pchi', function(q, lowBD, scale, df) { standardGeneric('pchi') })
setMethod('pchi', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, df)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pchi'
  n@select@variables[[1]] <- paste0('FLCDFChi(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',df,')')
  n
})


#' @export
setGeneric('pchisq', function(q, df, ncp=0, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pchisq') })
setMethod('pchisq', signature(q='integer'),
       function(q, df, ncp=0, lower.tail=TRUE, log.p=FALSE)
               stats::pchisq(q, df, ncp, lower.tail, log.p))
setMethod('pchisq', signature(q='FLSimpleVector'),
       function(q, df, ncp=0, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pchisq'
  n@select@variables[[1]] <- paste0('FLCDFChiSq(',
                                    n@select@variables[[1]],',',
                                    df,')')
  n
})


#' @export
setGeneric('pcosine', function(q, location, scale) { standardGeneric('pcosine') })
setMethod('pcosine', signature(q='FLSimpleVector'),
       function(q, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pcosine'
  n@select@variables[[1]] <- paste0('FLCDFCosine(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('pdoublegamma', function(q, location, scale, shape) { standardGeneric('pdoublegamma') })
setMethod('pdoublegamma', signature(q='FLSimpleVector'),
       function(q, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pdoublegamma'
  n@select@variables[[1]] <- paste0('FLCDFDoubleGamma(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('pdoubleweibull', function(q, location, scale, shape) { standardGeneric('pdoubleweibull') })
setMethod('pdoubleweibull', signature(q='FLSimpleVector'),
       function(q, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pdoubleweibull'
  n@select@variables[[1]] <- paste0('FLCDFDoubleWeibull(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('perlang', function(q, lowBD, scale, shape) { standardGeneric('perlang') })
setMethod('perlang', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_perlang'
  n@select@variables[[1]] <- paste0('FLCDFErlang(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('pexp', function(q, rate=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pexp') })
setMethod('pexp', signature(q='integer'),
       function(q, rate=1, lower.tail=TRUE, log.p=FALSE)
               stats::pexp(q, rate, lower.tail, log.p))
setMethod('pexp', signature(q='FLSimpleVector'),
       function(q, rate=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pexp'
  n@select@variables[[1]] <- paste0('FLCDFExp(',
                                    n@select@variables[[1]],',',
                                    0,',',rate,')')
  n
})


#' @export
setGeneric('pextremeLB', function(q, lowBD, scale, shape) { standardGeneric('pextremeLB') })
setMethod('pextremeLB', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pextremeLB'
  n@select@variables[[1]] <- paste0('FLCDFExtremeLB(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('pfisk', function(q, lowBD, scale, shape) { standardGeneric('pfisk') })
setMethod('pfisk', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pfisk'
  n@select@variables[[1]] <- paste0('FLCDFFisk(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('pfoldnormal', function(q, mean, sd) { standardGeneric('pfoldnormal') })
setMethod('pfoldnormal', signature(q='FLSimpleVector'),
       function(q, mean, sd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pfoldnormal'
  n@select@variables[[1]] <- paste0('FLCDFFoldedNormal(',
                                    n@select@variables[[1]],',',
                                    mean,',',sd,')')
  n
})


#' @export
setGeneric('pgamma', function(q, shape, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pgamma') })
setMethod('pgamma', signature(q='integer'),
       function(q, shape, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
               stats::pgamma(q, shape, rate, scale, lower.tail, log.p))
setMethod('pgamma', signature(q='FLSimpleVector'),
       function(q, shape, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pgamma'
  n@select@variables[[1]] <- paste0('FLCDFGamma(',
                                    n@select@variables[[1]],',',
                                    0,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('pglogis', function(q, location=0, scale=1, shape=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pglogis') })
setMethod('pglogis', signature(q='integer'),
       function(q, location=0, scale=1, shape=1, lower.tail=TRUE, log.p=FALSE)
               glogis::pglogis(q, location, scale, shape, lower.tail, log.p))
setMethod('pglogis', signature(q='FLSimpleVector'),
       function(q, location=0, scale=1, shape=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pglogis'
  n@select@variables[[1]] <- paste0('FLCDFGenLogistic(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('pgeom', function(q, prob, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pgeom') })
setMethod('pgeom', signature(q='integer'),
       function(q, prob, lower.tail=TRUE, log.p=FALSE)
               stats::pgeom(q, prob, lower.tail, log.p))
setMethod('pgeom', signature(q='FLSimpleVector'),
       function(q, prob, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pgeom'
  n@select@variables[[1]] <- paste0('FLCDFGeometric(',
                                    n@select@variables[[1]],',',
                                    prob,')')
  n
})


#' @export
setGeneric('pgumbel', function(q, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pgumbel') })
setMethod('pgumbel', signature(q='integer'),
       function(q, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::pgumbel(q, mu, sigma, lower.tail, log.p))
setMethod('pgumbel', signature(q='FLSimpleVector'),
       function(q, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pgumbel'
  n@select@variables[[1]] <- paste0('FLCDFGumbel(',
                                    n@select@variables[[1]],',',
                                    mu,',',sigma,')')
  n
})


#' @export
setGeneric('pHyperbolicSecant', function(q, location, scale) { standardGeneric('pHyperbolicSecant') })
setMethod('pHyperbolicSecant', signature(q='FLSimpleVector'),
       function(q, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pHyperbolicSecant'
  n@select@variables[[1]] <- paste0('FLCDFHypSecant(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('pinvgamma', function(q, alpha, beta=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pinvgamma') })
setMethod('pinvgamma', signature(q='integer'),
       function(q, alpha, beta=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::pinvgamma(q, alpha, beta, lower.tail, log.p))
setMethod('pinvgamma', signature(q='FLSimpleVector'),
       function(q, alpha, beta=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pinvgamma'
  n@select@variables[[1]] <- paste0('FLCDFInvGamma(',
                                    n@select@variables[[1]],',',
                                    beta,',',alpha,')')
  n
})


#' @export
setGeneric('pInvNorm', function(q, mu, lambda) { standardGeneric('pInvNorm') })
setMethod('pInvNorm', signature(q='FLSimpleVector'),
       function(q, mu, lambda)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pInvNorm'
  n@select@variables[[1]] <- paste0('FLCDFInvNormal(',
                                    n@select@variables[[1]],',',
                                    mu,',',lambda,')')
  n
})


#' @export
setGeneric('plaplace', function(q, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('plaplace') })
setMethod('plaplace', signature(q='integer'),
       function(q, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::plaplace(q, mu, sigma, lower.tail, log.p))
setMethod('plaplace', signature(q='FLSimpleVector'),
       function(q, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_plaplace'
  n@select@variables[[1]] <- paste0('FLCDFLaplace(',
                                    n@select@variables[[1]],',',
                                    mu,',',sigma,')')
  n
})


#' @export
setGeneric('plgser', function(q, theta, lower.tail=TRUE, log.p=FALSE) { standardGeneric('plgser') })
setMethod('plgser', signature(q='integer'),
       function(q, theta, lower.tail=TRUE, log.p=FALSE)
               extraDistr::plgser(q, theta, lower.tail, log.p))
setMethod('plgser', signature(q='FLSimpleVector'),
       function(q, theta, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_plgser'
  n@select@variables[[1]] <- paste0('FLCDFLaplace(',
                                    n@select@variables[[1]],',',
                                    theta,')')
  n
})


#' @export
setGeneric('rlogis', function(n, location=0, scale=1) { standardGeneric('rlogis') })
setMethod('rlogis', signature(n='integer'),
       function(n, location=0, scale=1)
               stats::rlogis(n, location, scale))
setMethod('rlogis', signature(n='FLSimpleVector'),
       function(n, location=0, scale=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlogis'
  n@select@variables[[1]] <- paste0('FLCDFLogistic(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('plnorm', function(q, meanlog=0, sdlog=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('plnorm') })
setMethod('plnorm', signature(q='integer'),
       function(q, meanlog=0, sdlog=1, lower.tail=TRUE, log.p=FALSE)
               stats::plnorm(q, meanlog, sdlog, lower.tail, log.p))
setMethod('plnorm', signature(q='FLSimpleVector'),
       function(q, meanlog=0, sdlog=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_plnorm'
  n@select@variables[[1]] <- paste0('FLCDFLogNormal(',
                                    n@select@variables[[1]],',',
                                    meanlog,',',sdlog,')')
  n
})


#' @export
setGeneric('pmaxwell', function(q, rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pmaxwell') })
setMethod('pmaxwell', signature(q='integer'),
       function(q, rate, lower.tail=TRUE, log.p=FALSE)
               VGAM::pmaxwell(q, rate, lower.tail, log.p))
setMethod('pmaxwell', signature(q='FLSimpleVector'),
       function(q, rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pmaxwell'
  n@select@variables[[1]] <- paste0('FLCDFMaxwell(',
                                    n@select@variables[[1]],',',
                                    a,')')
  n
})


#' @export
setGeneric('pnbinom', function(q, size, prob, mu, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pnbinom') })
setMethod('pnbinom', signature(q='integer'),
       function(q, size, prob, mu, lower.tail=TRUE, log.p=FALSE)
               stats::pnbinom(q, size, prob, mu, lower.tail, log.p))
setMethod('pnbinom', signature(q='FLSimpleVector'),
       function(q, size, prob, mu, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pnbinom'
  n@select@variables[[1]] <- paste0('FLCDFNegBinomial(',
                                    n@select@variables[[1]],',',
                                    prob,',',size,')')
  n
})


#' @export
setGeneric('pnorm', function(q, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pnorm') })
setMethod('pnorm', signature(q='integer'),
       function(q, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
               stats::pnorm(q, mean, sd, lower.tail, log.p))
setMethod('pnorm', signature(q='FLSimpleVector'),
       function(q, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pnorm'
  n@select@variables[[1]] <- paste0('FLCDFNormal(',
                                    n@select@variables[[1]],',',
                                    mean,',',sd,')')
  n
})


#' @export
setGeneric('ppareto', function(q, a=1, b=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('ppareto') })
setMethod('ppareto', signature(q='integer'),
       function(q, a=1, b=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::ppareto(q, a, b, lower.tail, log.p))
setMethod('ppareto', signature(q='FLSimpleVector'),
       function(q, a=1, b=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_ppareto'
  n@select@variables[[1]] <- paste0('FLCDFPareto(',
                                    n@select@variables[[1]],',',
                                    b,',',a,')')
  n
})


#' @export
setGeneric('ppois', function(q, lambda, lower.tail=TRUE, log.p=FALSE) { standardGeneric('ppois') })
setMethod('ppois', signature(q='integer'),
       function(q, lambda, lower.tail=TRUE, log.p=FALSE)
               stats::ppois(q, lambda, lower.tail, log.p))
setMethod('ppois', signature(q='FLSimpleVector'),
       function(q, lambda, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_ppois'
  n@select@variables[[1]] <- paste0('FLCDFPoisson(',
                                    n@select@variables[[1]],',',
                                    lambda,')')
  n
})


#' @export
setGeneric('pplcon', function(q, xmin, alpha, lower.tail=TRUE) { standardGeneric('pplcon') })
setMethod('pplcon', signature(q='integer'),
       function(q, xmin, alpha, lower.tail=TRUE)
               poweRlaw::pplcon(q, xmin, alpha, lower.tail))
setMethod('pplcon', signature(q='FLSimpleVector'),
       function(q, xmin, alpha, lower.tail=TRUE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pplcon'
  n@select@variables[[1]] <- paste0('FLCDFPower(',
                                    n@select@variables[[1]],',',
                                    alpha) ##??,')')
  n
})


#' @export
setGeneric('prayleigh', function(q, sigma=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('prayleigh') })
setMethod('prayleigh', signature(q='integer'),
       function(q, sigma=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::prayleigh(q, sigma, lower.tail, log.p))
setMethod('prayleigh', signature(q='FLSimpleVector'),
       function(q, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_prayleigh'
  n@select@variables[[1]] <- paste0('FLCDFRayleigh(',
                                    n@select@variables[[1]],',',
                                    sigma,')')
  n
})


#' @export
setGeneric('preciprocal', function(q, lowbd, upbd) { standardGeneric('preciprocal') })
setMethod('preciprocal', signature(q='FLSimpleVector'),
       function(q, lowbd, upbd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_preciprocal'
  n@select@variables[[1]] <- paste0('FLCDFReciprocal(',
                                    n@select@variables[[1]],',',
                                    lowbd,',',upbd,')')
  n
})


#' @export
setGeneric('psemicircular', function(q, location, radius) { standardGeneric('psemicircular') })
setMethod('psemicircular', signature(q='FLSimpleVector'),
       function(q, location, radius)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_psemicircular'
  n@select@variables[[1]] <- paste0('FLCDFSemicircular(',
                                    n@select@variables[[1]],',',
                                    location,',',radius,')')
  n
})


#' @export
setGeneric('pt', function(q, df, ncp, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pt') })
setMethod('pt', signature(q='integer'),
       function(q, df, ncp, lower.tail=TRUE, log.p=FALSE)
               stats::pt(q, df, ncp, lower.tail, log.p))
setMethod('pt', signature(q='FLSimpleVector'),
       function(q, df, ncp, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pt'
  n@select@variables[[1]] <- paste0('FLCDFStudentsT(',
                                    n@select@variables[[1]],',',
                                    0,',',1,',',df,')')
  n
})


#' @export
setGeneric('ptrbeta', function(q, shape1, shape2, shape3, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('ptrbeta') })
setMethod('ptrbeta', signature(q='integer'),
       function(q, shape1, shape2, shape3, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
               actuar::ptrbeta(q, shape1, shape2, shape3, rate, scale, lower.tail, log.p))
setMethod('ptrbeta', signature(q='FLSimpleVector'),
       function(q, shape1, shape2, shape3, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_ptrbeta'
  n@select@variables[[1]] <- paste0('FLCDFTransBeta(',
                                    n@select@variables[[1]],',',
                                    scale,',',shape1,',',shape2,',',shape3,')')
  n
})


#' @export
setGeneric('ptriang', function(q, a=-1, b=1, c=(a + b)/2, lower.tail=TRUE, log.p=FALSE) { standardGeneric('ptriang') })
setMethod('ptriang', signature(q='integer'),
       function(q, a=-1, b=1, c=(a + b)/2, lower.tail=TRUE, log.p=FALSE)
               extraDistr::ptriang(q, a, b, c, lower.tail, log.p))
setMethod('ptriang', signature(q='FLSimpleVector'),
       function(q, a=-1, b=1, c=(a + b)/2, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_ptriang'
  n@select@variables[[1]] <- paste0('FLCDFTriangular(',
                                    n@select@variables[[1]],',',
                                    a,',',b,',',c,')')
  n
})


#' @export
setGeneric('punif', function(q, min=0, max=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('punif') })
setMethod('punif', signature(q='integer'),
       function(q, min=0, max=1, lower.tail=TRUE, log.p=FALSE)
               stats::punif(q, min, max, lower.tail, log.p))
setMethod('punif', signature(q='FLSimpleVector'),
       function(q, min=0, max=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_punif'
  n@select@variables[[1]] <- paste0('FLCDFUniform(',
                                    n@select@variables[[1]],',',
                                    min,',',max,')')
  n
})


#' @export
setGeneric('pweibull', function(q, shape, scale=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('pweibull') })
setMethod('pweibull', signature(q='integer'),
       function(q, shape, scale=1, lower.tail=TRUE, log.p=FALSE)
               stats::pweibull(q, shape, scale, lower.tail, log.p))
setMethod('pweibull', signature(q='FLSimpleVector'),
       function(q, shape, scale=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_pweibull'
  n@select@variables[[1]] <- paste0('FLCDFWeibull(',
                                    n@select@variables[[1]],',',
                                    location=0,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('dbinom', function(x, size, prob, log=FALSE) { standardGeneric('dbinom') })
setMethod('dbinom', signature(x='integer'),
       function(x, size, prob, log=FALSE)
               stats::dbinom(x, size, prob, log))
setMethod('dbinom', signature(x='FLSimpleVector'),
       function(x, size, prob, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dbinom'
  n@select@variables[[1]] <- paste0('FLPDFBinomial(',
                                    n@select@variables[[1]],',',
                                    prob,',',size,')')
  n
})


#' @export
setGeneric('dbeta', function(x, shape1, shape2, ncp=0, log=FALSE) { standardGeneric('dbeta') })
setMethod('dbeta', signature(x='integer'),
       function(x, shape1, shape2, ncp=0, log=FALSE)
               stats::dbeta(x, shape1, shape2, ncp, log))
setMethod('dbeta', signature(x='FLSimpleVector'),
       function(x, shape1, shape2, ncp=0, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dbeta'
  n@select@variables[[1]] <- paste0('FLPDFBeta(',
                                    n@select@variables[[1]],',',
                                    0,',',0,',',shap1,',',shape2,')')
  n
})


#' @export
setGeneric('dbradford', function(q, lowbd, uppbd, shape) { standardGeneric('dbradford') })
setMethod('dbradford', signature(q='FLSimpleVector'),
       function(q, lowbd, uppbd, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dbradford'
  n@select@variables[[1]] <- paste0('FLPDFBradford(',
                                    n@select@variables[[1]],',',
                                    lowbd,',',uppbd,',',shape,')')
  n
})


#' @export
setGeneric('dburr', function(x, shape1, shape2, rate=1, scale=1/rate, log=FALSE) { standardGeneric('dburr') })
setMethod('dburr', signature(x='integer'),
       function(x, shape1, shape2, rate=1, scale=1/rate, log=FALSE)
               actuar::dburr(x, shape1, shape2, rate, scale, log))
setMethod('dburr', signature(x='FLSimpleVector'),
       function(x, shape1, shape2, rate=1, scale=1/rate, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dburr'
  n@select@variables[[1]] <- paste0('FLPDFBurr(',
                                    n@select@variables[[1]],',',
                                    lowbd=0,',',scale,',',shape1,',',shape2,')')
  n
})


#' @export
setGeneric('dcauchy', function(x, location=0, scale=1, log=FALSE) { standardGeneric('dcauchy') })
setMethod('dcauchy', signature(x='integer'),
       function(x, location=0, scale=1, log=FALSE)
               stats::dcauchy(x, location, scale, log))
setMethod('dcauchy', signature(x='FLSimpleVector'),
       function(x, location=0, scale=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dcauchy'
  n@select@variables[[1]] <- paste0('FLPDFCauchy(',
                                    n@select@variables[[1]],',',
                                    location,',', scale,')')
  n
})


#' @export
setGeneric('dchi', function(q, lowBD, scale, df) { standardGeneric('dchi') })
setMethod('dchi', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, df)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dchi'
  n@select@variables[[1]] <- paste0('FLPDFChi(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',df,')')
  n
})


#' @export
setGeneric('dchisq', function(x, df, ncp=0, log=FALSE) { standardGeneric('dchisq') })
setMethod('dchisq', signature(x='integer'),
       function(x, df, ncp=0, log=FALSE)
               stats::dchisq(x, df, ncp, log))
setMethod('dchisq', signature(x='FLSimpleVector'),
       function(x, df, ncp=0, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dchisq'
  n@select@variables[[1]] <- paste0('FLPDFChiSq(',
                                    n@select@variables[[1]],',',
                                    df,')')
  n
})


#' @export
setGeneric('dcosine', function(q, location, scale) { standardGeneric('dcosine') })
setMethod('dcosine', signature(q='FLSimpleVector'),
       function(q, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dcosine'
  n@select@variables[[1]] <- paste0('FLPDFCosine(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('ddoublegamma', function(q, location, scale, shape) { standardGeneric('ddoublegamma') })
setMethod('ddoublegamma', signature(q='FLSimpleVector'),
       function(q, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_ddoublegamma'
  n@select@variables[[1]] <- paste0('FLPDFDoubleGamma(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('ddoubleweibull', function(q, location, scale, shape) { standardGeneric('ddoubleweibull') })
setMethod('ddoubleweibull', signature(q='FLSimpleVector'),
       function(q, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_ddoubleweibull'
  n@select@variables[[1]] <- paste0('FLPDFDoubleWeibull(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('derlang', function(q, lowBD, scale, shape) { standardGeneric('derlang') })
setMethod('derlang', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_derlang'
  n@select@variables[[1]] <- paste0('FLPDFErlang(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('dexp', function(x, rate=1, log=FALSE) { standardGeneric('dexp') })
setMethod('dexp', signature(x='integer'),
       function(x, rate=1, log=FALSE)
               stats::dexp(x, rate, log))
setMethod('dexp', signature(x='FLSimpleVector'),
       function(x, rate=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dexp'
  n@select@variables[[1]] <- paste0('FLPDFExp(',
                                    n@select@variables[[1]],',',
                                    0,',',rate,')')
  n
})


#' @export
setGeneric('dextremeLB', function(q, lowBD, scale, shape) { standardGeneric('dextremeLB') })
setMethod('dextremeLB', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dextremeLB'
  n@select@variables[[1]] <- paste0('FLPDFExtremeLB(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('dfisk', function(q, lowBD, scale, shape) { standardGeneric('dfisk') })
setMethod('dfisk', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dfisk'
  n@select@variables[[1]] <- paste0('FLPDFFisk(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('dfoldnormal', function(q, mean, sd) { standardGeneric('dfoldnormal') })
setMethod('dfoldnormal', signature(q='FLSimpleVector'),
       function(q, mean, sd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dfoldnormal'
  n@select@variables[[1]] <- paste0('FLPDFFoldedNormal(',
                                    n@select@variables[[1]],',',
                                    mean,',',sd,')')
  n
})


#' @export
setGeneric('dgamma', function(x, shape, rate=1, scale=1/rate, log=FALSE) { standardGeneric('dgamma') })
setMethod('dgamma', signature(x='integer'),
       function(x, shape, rate=1, scale=1/rate, log=FALSE)
               stats::dgamma(x, shape, rate, scale, log))
setMethod('dgamma', signature(x='FLSimpleVector'),
       function(x, shape, rate=1, scale=1/rate, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dgamma'
  n@select@variables[[1]] <- paste0('FLPDFGamma(',
                                    n@select@variables[[1]],',',
                                    0,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('dglogis', function(x, location=0, scale=1, shape=1, log=FALSE) { standardGeneric('dglogis') })
setMethod('dglogis', signature(x='integer'),
       function(x, location=0, scale=1, shape=1, log=FALSE)
               glogis::dglogis(x, location, scale, shape, log))
setMethod('dglogis', signature(x='FLSimpleVector'),
       function(x, location=0, scale=1, shape=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dglogis'
  n@select@variables[[1]] <- paste0('FLPDFGenLogistic(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('dgeom', function(x, prob, log=FALSE) { standardGeneric('dgeom') })
setMethod('dgeom', signature(x='integer'),
       function(x, prob, log=FALSE)
               stats::dgeom(x, prob, log))
setMethod('dgeom', signature(x='FLSimpleVector'),
       function(x, prob, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dgeom'
  n@select@variables[[1]] <- paste0('FLPDFGeometric(',
                                    n@select@variables[[1]],',',
                                    prob,')')
  n
})


#' @export
setGeneric('dgumbel', function(x, mu=0, sigma=1, log=FALSE) { standardGeneric('dgumbel') })
setMethod('dgumbel', signature(x='integer'),
       function(x, mu=0, sigma=1, log=FALSE)
               extraDistr::dgumbel(x, mu, sigma, log))
setMethod('dgumbel', signature(x='FLSimpleVector'),
       function(x, mu=0, sigma=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dgumbel'
  n@select@variables[[1]] <- paste0('FLPDFGumbel(',
                                    n@select@variables[[1]],',',
                                    mu,',',sigma,')')
  n
})


#' @export
setGeneric('dHyperbolicSecant', function(q, location, scale) { standardGeneric('dHyperbolicSecant') })
setMethod('dHyperbolicSecant', signature(q='FLSimpleVector'),
       function(q, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dHyperbolicSecant'
  n@select@variables[[1]] <- paste0('FLPDFHypSecant(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('dinvgamma', function(x, alpha, beta=1, log=FALSE) { standardGeneric('dinvgamma') })
setMethod('dinvgamma', signature(x='integer'),
       function(x, alpha, beta=1, log=FALSE)
               extraDistr::dinvgamma(x, alpha, beta, log))
setMethod('dinvgamma', signature(x='FLSimpleVector'),
       function(x, alpha, beta=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dinvgamma'
  n@select@variables[[1]] <- paste0('FLPDFInvGamma(',
                                    n@select@variables[[1]],',',
                                    beta,',',alpha,')')
  n
})


#' @export
setGeneric('dInvNorm', function(q, mu, lambda) { standardGeneric('dInvNorm') })
setMethod('dInvNorm', signature(q='FLSimpleVector'),
       function(q, mu, lambda)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dInvNorm'
  n@select@variables[[1]] <- paste0('FLPDFInvNormal(',
                                    n@select@variables[[1]],',',
                                    mu,',',lambda,')')
  n
})


#' @export
setGeneric('dlaplace', function(x, mu=0, sigma=1, log=FALSE) { standardGeneric('dlaplace') })
setMethod('dlaplace', signature(x='integer'),
       function(x, mu=0, sigma=1, log=FALSE)
               extraDistr::dlaplace(x, mu, sigma, log))
setMethod('dlaplace', signature(x='FLSimpleVector'),
       function(x, mu=0, sigma=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dlaplace'
  n@select@variables[[1]] <- paste0('FLPDFLaplace(',
                                    n@select@variables[[1]],',',
                                    mu,',',sigma,')')
  n
})


#' @export
setGeneric('dlgser', function(x, theta, log=FALSE) { standardGeneric('dlgser') })
setMethod('dlgser', signature(x='integer'),
       function(x, theta, log=FALSE)
               extraDistr::dlgser(x, theta, log))
setMethod('dlgser', signature(x='FLSimpleVector'),
       function(x, theta, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dlgser'
  n@select@variables[[1]] <- paste0('FLPDFLaplace(',
                                    n@select@variables[[1]],',',
                                    theta,')')
  n
})


#' @export
setGeneric('rlogis', function(n, location=0, scale=1) { standardGeneric('rlogis') })
setMethod('rlogis', signature(n='integer'),
       function(n, location=0, scale=1)
               stats::rlogis(n, location, scale))
setMethod('rlogis', signature(n='FLSimpleVector'),
       function(n, location=0, scale=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlogis'
  n@select@variables[[1]] <- paste0('FLPDFLogistic(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('dlnorm', function(x, meanlog=0, sdlog=1, log=FALSE) { standardGeneric('dlnorm') })
setMethod('dlnorm', signature(x='integer'),
       function(x, meanlog=0, sdlog=1, log=FALSE)
               stats::dlnorm(x, meanlog, sdlog, log))
setMethod('dlnorm', signature(x='FLSimpleVector'),
       function(x, meanlog=0, sdlog=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dlnorm'
  n@select@variables[[1]] <- paste0('FLPDFLogNormal(',
                                    n@select@variables[[1]],',',
                                    meanlog,',',sdlog,')')
  n
})


#' @export
setGeneric('dmaxwell', function(x, rate, log=FALSE) { standardGeneric('dmaxwell') })
setMethod('dmaxwell', signature(x='integer'),
       function(x, rate, log=FALSE)
               VGAM::dmaxwell(x, rate, log))
setMethod('dmaxwell', signature(x='FLSimpleVector'),
       function(x, rate, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dmaxwell'
  n@select@variables[[1]] <- paste0('FLPDFMaxwell(',
                                    n@select@variables[[1]],',',
                                    a,')')
  n
})


#' @export
setGeneric('dnbinom', function(x, size, prob, mu, log=FALSE) { standardGeneric('dnbinom') })
setMethod('dnbinom', signature(x='integer'),
       function(x, size, prob, mu, log=FALSE)
               stats::dnbinom(x, size, prob, mu, log))
setMethod('dnbinom', signature(x='FLSimpleVector'),
       function(x, size, prob, mu, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dnbinom'
  n@select@variables[[1]] <- paste0('FLPDFNegBinomial(',
                                    n@select@variables[[1]],',',
                                    prob,',',size,')')
  n
})


#' @export
setGeneric('dnorm', function(x, mean=0, sd=1, log=FALSE) { standardGeneric('dnorm') })
setMethod('dnorm', signature(x='integer'),
       function(x, mean=0, sd=1, log=FALSE)
               stats::dnorm(x, mean, sd, log))
setMethod('dnorm', signature(x='FLSimpleVector'),
       function(x, mean=0, sd=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dnorm'
  n@select@variables[[1]] <- paste0('FLPDFNormal(',
                                    n@select@variables[[1]],',',
                                    mean,',',sd,')')
  n
})


#' @export
setGeneric('dpareto', function(x, a=1, b=1, log=FALSE) { standardGeneric('dpareto') })
setMethod('dpareto', signature(x='integer'),
       function(x, a=1, b=1, log=FALSE)
               extraDistr::dpareto(x, a, b, log))
setMethod('dpareto', signature(x='FLSimpleVector'),
       function(x, a=1, b=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dpareto'
  n@select@variables[[1]] <- paste0('FLPDFPareto(',
                                    n@select@variables[[1]],',',
                                    b,',',a,')')
  n
})


#' @export
setGeneric('dpois', function(x, lambda, log=FALSE) { standardGeneric('dpois') })
setMethod('dpois', signature(x='integer'),
       function(x, lambda, log=FALSE)
               stats::dpois(x, lambda, log))
setMethod('dpois', signature(x='FLSimpleVector'),
       function(x, lambda, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dpois'
  n@select@variables[[1]] <- paste0('FLPDFPoisson(',
                                    n@select@variables[[1]],',',
                                    lambda,')')
  n
})


#' @export
setGeneric('drayleigh', function(x, sigma=1, log=FALSE) { standardGeneric('drayleigh') })
setMethod('drayleigh', signature(x='integer'),
       function(x, sigma=1, log=FALSE)
               extraDistr::drayleigh(x, sigma, log))
setMethod('drayleigh', signature(x='FLSimpleVector'),
       function(x, sigma=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_drayleigh'
  n@select@variables[[1]] <- paste0('FLPDFRayleigh(',
                                    n@select@variables[[1]],',',
                                    sigma,')')
  n
})


#' @export
setGeneric('dreciprocal', function(q, lowbd, upbd) { standardGeneric('dreciprocal') })
setMethod('dreciprocal', signature(q='FLSimpleVector'),
       function(q, lowbd, upbd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dreciprocal'
  n@select@variables[[1]] <- paste0('FLPDFReciprocal(',
                                    n@select@variables[[1]],',',
                                    lowbd,',',upbd,')')
  n
})


#' @export
setGeneric('dsemicircular', function(q, location, radius) { standardGeneric('dsemicircular') })
setMethod('dsemicircular', signature(q='FLSimpleVector'),
       function(q, location, radius)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dsemicircular'
  n@select@variables[[1]] <- paste0('FLPDFSemicircular(',
                                    n@select@variables[[1]],',',
                                    location,',',radius,')')
  n
})


#' @export
setGeneric('dt', function(x, df, ncp, log=FALSE) { standardGeneric('dt') })
setMethod('dt', signature(x='integer'),
       function(x, df, ncp, log=FALSE)
               stats::dt(x, df, ncp, log))
setMethod('dt', signature(x='FLSimpleVector'),
       function(x, df, ncp, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dt'
  n@select@variables[[1]] <- paste0('FLPDFStudentsT(',
                                    n@select@variables[[1]],',',
                                    0,',',1,',',df,')')
  n
})


#' @export
setGeneric('dtrbeta', function(x, shape1, shape2, shape3, rate=1, scale=1/rate, log=FALSE) { standardGeneric('dtrbeta') })
setMethod('dtrbeta', signature(x='integer'),
       function(x, shape1, shape2, shape3, rate=1, scale=1/rate, log=FALSE)
               actuar::dtrbeta(x, shape1, shape2, shape3, rate, scale, log))
setMethod('dtrbeta', signature(x='FLSimpleVector'),
       function(x, shape1, shape2, shape3, rate=1, scale=1/rate, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dtrbeta'
  n@select@variables[[1]] <- paste0('FLPDFTransBeta(',
                                    n@select@variables[[1]],',',
                                    scale,',',shape1,',',shape2,',',shape3,')')
  n
})


#' @export
setGeneric('dtriang', function(x, a=-1, b=1, c=(a + b)/2, log=FALSE) { standardGeneric('dtriang') })
setMethod('dtriang', signature(x='integer'),
       function(x, a=-1, b=1, c=(a + b)/2, log=FALSE)
               extraDistr::dtriang(x, a, b, c, log))
setMethod('dtriang', signature(x='FLSimpleVector'),
       function(x, a=-1, b=1, c=(a + b)/2, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dtriang'
  n@select@variables[[1]] <- paste0('FLPDFTriangular(',
                                    n@select@variables[[1]],',',
                                    a,',',b,',',c,')')
  n
})


#' @export
setGeneric('dunif', function(x, min=0, max=1, log=FALSE) { standardGeneric('dunif') })
setMethod('dunif', signature(x='integer'),
       function(x, min=0, max=1, log=FALSE)
               stats::dunif(x, min, max, log))
setMethod('dunif', signature(x='FLSimpleVector'),
       function(x, min=0, max=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dunif'
  n@select@variables[[1]] <- paste0('FLPDFUniform(',
                                    n@select@variables[[1]],',',
                                    min,',',max,')')
  n
})


#' @export
setGeneric('dweibull', function(x, shape, scale=1, log=FALSE) { standardGeneric('dweibull') })
setMethod('dweibull', signature(x='integer'),
       function(x, shape, scale=1, log=FALSE)
               stats::dweibull(x, shape, scale, log))
setMethod('dweibull', signature(x='FLSimpleVector'),
       function(x, shape, scale=1, log=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_dweibull'
  n@select@variables[[1]] <- paste0('FLPDFWeibull(',
                                    n@select@variables[[1]],',',
                                    location=0,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qbinom', function(p, size, prob, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qbinom') })
setMethod('qbinom', signature(p='integer'),
       function(p, size, prob, lower.tail=TRUE, log.p=FALSE)
               stats::qbinom(p, size, prob, lower.tail, log.p))
setMethod('qbinom', signature(p='FLSimpleVector'),
       function(p, size, prob, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qbinom'
  n@select@variables[[1]] <- paste0('FLInvCDFBinomial(',
                                    n@select@variables[[1]],',',
                                    prob,',',size,')')
  n
})


#' @export
setGeneric('qbeta', function(p, shape1, shape2, ncp=0, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qbeta') })
setMethod('qbeta', signature(p='integer'),
       function(p, shape1, shape2, ncp=0, lower.tail=TRUE, log.p=FALSE)
               stats::qbeta(p, shape1, shape2, ncp, lower.tail, log.p))
setMethod('qbeta', signature(p='FLSimpleVector'),
       function(p, shape1, shape2, ncp=0, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qbeta'
  n@select@variables[[1]] <- paste0('FLInvCDFBeta(',
                                    n@select@variables[[1]],',',
                                    0,',',0,',',shap1,',',shape2,')')
  n
})


#' @export
setGeneric('qbradford', function(q, lowbd, uppbd, shape) { standardGeneric('qbradford') })
setMethod('qbradford', signature(q='FLSimpleVector'),
       function(q, lowbd, uppbd, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qbradford'
  n@select@variables[[1]] <- paste0('FLInvCDFBradford(',
                                    n@select@variables[[1]],',',
                                    lowbd,',',uppbd,',',shape,')')
  n
})


#' @export
setGeneric('qburr', function(p, shape1, shape2, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qburr') })
setMethod('qburr', signature(p='integer'),
       function(p, shape1, shape2, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
               actuar::qburr(p, shape1, shape2, rate, scale, lower.tail, log.p))
setMethod('qburr', signature(p='FLSimpleVector'),
       function(p, shape1, shape2, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qburr'
  n@select@variables[[1]] <- paste0('FLInvCDFBurr(',
                                    n@select@variables[[1]],',',
                                    lowbd=0,',',scale,',',shape1,',',shape2,')')
  n
})


#' @export
setGeneric('qcauchy', function(p, location=0, scale=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qcauchy') })
setMethod('qcauchy', signature(p='integer'),
       function(p, location=0, scale=1, lower.tail=TRUE, log.p=FALSE)
               stats::qcauchy(p, location, scale, lower.tail, log.p))
setMethod('qcauchy', signature(p='FLSimpleVector'),
       function(p, location=0, scale=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qcauchy'
  n@select@variables[[1]] <- paste0('FLInvCDFCauchy(',
                                    n@select@variables[[1]],',',
                                    location,',', scale,')')
  n
})


#' @export
setGeneric('qchi', function(q, lowBD, scale, df) { standardGeneric('qchi') })
setMethod('qchi', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, df)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qchi'
  n@select@variables[[1]] <- paste0('FLInvCDFChi(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',df,')')
  n
})


#' @export
setGeneric('qchisq', function(p, df, ncp=0, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qchisq') })
setMethod('qchisq', signature(p='integer'),
       function(p, df, ncp=0, lower.tail=TRUE, log.p=FALSE)
               stats::qchisq(p, df, ncp, lower.tail, log.p))
setMethod('qchisq', signature(p='FLSimpleVector'),
       function(p, df, ncp=0, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qchisq'
  n@select@variables[[1]] <- paste0('FLInvCDFChiSq(',
                                    n@select@variables[[1]],',',
                                    df,')')
  n
})


#' @export
setGeneric('qcosine', function(q, location, scale) { standardGeneric('qcosine') })
setMethod('qcosine', signature(q='FLSimpleVector'),
       function(q, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qcosine'
  n@select@variables[[1]] <- paste0('FLInvCDFCosine(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('qdoublegamma', function(q, location, scale, shape) { standardGeneric('qdoublegamma') })
setMethod('qdoublegamma', signature(q='FLSimpleVector'),
       function(q, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qdoublegamma'
  n@select@variables[[1]] <- paste0('FLInvCDFDoubleGamma(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qdoubleweibull', function(q, location, scale, shape) { standardGeneric('qdoubleweibull') })
setMethod('qdoubleweibull', signature(q='FLSimpleVector'),
       function(q, location, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qdoubleweibull'
  n@select@variables[[1]] <- paste0('FLInvCDFDoubleWeibull(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qerlang', function(q, lowBD, scale, shape) { standardGeneric('qerlang') })
setMethod('qerlang', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qerlang'
  n@select@variables[[1]] <- paste0('FLInvCDFErlang(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qexp', function(p, rate=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qexp') })
setMethod('qexp', signature(p='integer'),
       function(p, rate=1, lower.tail=TRUE, log.p=FALSE)
               stats::qexp(p, rate, lower.tail, log.p))
setMethod('qexp', signature(p='FLSimpleVector'),
       function(p, rate=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qexp'
  n@select@variables[[1]] <- paste0('FLInvCDFExp(',
                                    n@select@variables[[1]],',',
                                    0,',',rate,')')
  n
})


#' @export
setGeneric('qextremeLB', function(q, lowBD, scale, shape) { standardGeneric('qextremeLB') })
setMethod('qextremeLB', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qextremeLB'
  n@select@variables[[1]] <- paste0('FLInvCDFExtremeLB(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qfisk', function(q, lowBD, scale, shape) { standardGeneric('qfisk') })
setMethod('qfisk', signature(q='FLSimpleVector'),
       function(q, lowBD, scale, shape)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qfisk'
  n@select@variables[[1]] <- paste0('FLInvCDFFisk(',
                                    n@select@variables[[1]],',',
                                    lowBD,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qfoldnormal', function(q, mean, sd) { standardGeneric('qfoldnormal') })
setMethod('qfoldnormal', signature(q='FLSimpleVector'),
       function(q, mean, sd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qfoldnormal'
  n@select@variables[[1]] <- paste0('FLInvCDFFoldedNormal(',
                                    n@select@variables[[1]],',',
                                    mean,',',sd,')')
  n
})


#' @export
setGeneric('qgamma', function(p, shape, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qgamma') })
setMethod('qgamma', signature(p='integer'),
       function(p, shape, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
               stats::qgamma(p, shape, rate, scale, lower.tail, log.p))
setMethod('qgamma', signature(p='FLSimpleVector'),
       function(p, shape, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qgamma'
  n@select@variables[[1]] <- paste0('FLInvCDFGamma(',
                                    n@select@variables[[1]],',',
                                    0,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qglogis', function(p, location=0, scale=1, shape=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qglogis') })
setMethod('qglogis', signature(p='integer'),
       function(p, location=0, scale=1, shape=1, lower.tail=TRUE, log.p=FALSE)
               glogis::qglogis(p, location, scale, shape, lower.tail, log.p))
setMethod('qglogis', signature(p='FLSimpleVector'),
       function(p, location=0, scale=1, shape=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qglogis'
  n@select@variables[[1]] <- paste0('FLInvCDFGenLogistic(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,',',shape,')')
  n
})


#' @export
setGeneric('qgeom', function(p, prob, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qgeom') })
setMethod('qgeom', signature(p='integer'),
       function(p, prob, lower.tail=TRUE, log.p=FALSE)
               stats::qgeom(p, prob, lower.tail, log.p))
setMethod('qgeom', signature(p='FLSimpleVector'),
       function(p, prob, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qgeom'
  n@select@variables[[1]] <- paste0('FLInvCDFGeometric(',
                                    n@select@variables[[1]],',',
                                    prob,')')
  n
})


#' @export
setGeneric('qgumbel', function(p, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qgumbel') })
setMethod('qgumbel', signature(p='integer'),
       function(p, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qgumbel(p, mu, sigma, lower.tail, log.p))
setMethod('qgumbel', signature(p='FLSimpleVector'),
       function(p, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qgumbel'
  n@select@variables[[1]] <- paste0('FLInvCDFGumbel(',
                                    n@select@variables[[1]],',',
                                    mu,',',sigma,')')
  n
})


#' @export
setGeneric('qHyperbolicSecant', function(q, location, scale) { standardGeneric('qHyperbolicSecant') })
setMethod('qHyperbolicSecant', signature(q='FLSimpleVector'),
       function(q, location, scale)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qHyperbolicSecant'
  n@select@variables[[1]] <- paste0('FLInvCDFHypSecant(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('qinvgamma', function(p, alpha, beta=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qinvgamma') })
setMethod('qinvgamma', signature(p='integer'),
       function(p, alpha, beta=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qinvgamma(p, alpha, beta, lower.tail, log.p))
setMethod('qinvgamma', signature(p='FLSimpleVector'),
       function(p, alpha, beta=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qinvgamma'
  n@select@variables[[1]] <- paste0('FLInvCDFInvGamma(',
                                    n@select@variables[[1]],',',
                                    beta,',',alpha,')')
  n
})


#' @export
setGeneric('qInvNorm', function(q, mu, lambda) { standardGeneric('qInvNorm') })
setMethod('qInvNorm', signature(q='FLSimpleVector'),
       function(q, mu, lambda)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qInvNorm'
  n@select@variables[[1]] <- paste0('FLInvCDFInvNormal(',
                                    n@select@variables[[1]],',',
                                    mu,',',lambda,')')
  n
})


#' @export
setGeneric('qlaplace', function(p, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qlaplace') })
setMethod('qlaplace', signature(p='integer'),
       function(p, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qlaplace(p, mu, sigma, lower.tail, log.p))
setMethod('qlaplace', signature(p='FLSimpleVector'),
       function(p, mu=0, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qlaplace'
  n@select@variables[[1]] <- paste0('FLInvCDFLaplace(',
                                    n@select@variables[[1]],',',
                                    mu,',',sigma,')')
  n
})


#' @export
setGeneric('qlgser', function(p, theta, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qlgser') })
setMethod('qlgser', signature(p='integer'),
       function(p, theta, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qlgser(p, theta, lower.tail, log.p))
setMethod('qlgser', signature(p='FLSimpleVector'),
       function(p, theta, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qlgser'
  n@select@variables[[1]] <- paste0('FLInvCDFLaplace(',
                                    n@select@variables[[1]],',',
                                    theta,')')
  n
})


#' @export
setGeneric('rlogis', function(n, location=0, scale=1) { standardGeneric('rlogis') })
setMethod('rlogis', signature(n='integer'),
       function(n, location=0, scale=1)
               stats::rlogis(n, location, scale))
setMethod('rlogis', signature(n='FLSimpleVector'),
       function(n, location=0, scale=1)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_rlogis'
  n@select@variables[[1]] <- paste0('FLInvCDFLogistic(',
                                    n@select@variables[[1]],',',
                                    location,',',scale,')')
  n
})


#' @export
setGeneric('qlnorm', function(p, meanlog=0, sdlog=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qlnorm') })
setMethod('qlnorm', signature(p='integer'),
       function(p, meanlog=0, sdlog=1, lower.tail=TRUE, log.p=FALSE)
               stats::qlnorm(p, meanlog, sdlog, lower.tail, log.p))
setMethod('qlnorm', signature(p='FLSimpleVector'),
       function(p, meanlog=0, sdlog=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qlnorm'
  n@select@variables[[1]] <- paste0('FLInvCDFLogNormal(',
                                    n@select@variables[[1]],',',
                                    meanlog,',',sdlog,')')
  n
})


#' @export
setGeneric('qmaxwell', function(p, rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qmaxwell') })
setMethod('qmaxwell', signature(p='integer'),
       function(p, rate, lower.tail=TRUE, log.p=FALSE)
               VGAM::qmaxwell(p, rate, lower.tail, log.p))
setMethod('qmaxwell', signature(p='FLSimpleVector'),
       function(p, rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qmaxwell'
  n@select@variables[[1]] <- paste0('FLInvCDFMaxwell(',
                                    n@select@variables[[1]],',',
                                    a,')')
  n
})


#' @export
setGeneric('qnbinom', function(p, size, prob, mu, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qnbinom') })
setMethod('qnbinom', signature(p='integer'),
       function(p, size, prob, mu, lower.tail=TRUE, log.p=FALSE)
               stats::qnbinom(p, size, prob, mu, lower.tail, log.p))
setMethod('qnbinom', signature(p='FLSimpleVector'),
       function(p, size, prob, mu, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qnbinom'
  n@select@variables[[1]] <- paste0('FLInvCDFNegBinomial(',
                                    n@select@variables[[1]],',',
                                    prob,',',size,')')
  n
})


#' @export
setGeneric('qnorm', function(p, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qnorm') })
setMethod('qnorm', signature(p='integer'),
       function(p, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
               stats::qnorm(p, mean, sd, lower.tail, log.p))
setMethod('qnorm', signature(p='FLSimpleVector'),
       function(p, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qnorm'
  n@select@variables[[1]] <- paste0('FLInvCDFNormal(',
                                    n@select@variables[[1]],',',
                                    mean,',',sd,')')
  n
})


#' @export
setGeneric('qpareto', function(p, a=1, b=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qpareto') })
setMethod('qpareto', signature(p='integer'),
       function(p, a=1, b=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qpareto(p, a, b, lower.tail, log.p))
setMethod('qpareto', signature(p='FLSimpleVector'),
       function(p, a=1, b=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qpareto'
  n@select@variables[[1]] <- paste0('FLInvCDFPareto(',
                                    n@select@variables[[1]],',',
                                    b,',',a,')')
  n
})


#' @export
setGeneric('qpois', function(p, lambda, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qpois') })
setMethod('qpois', signature(p='integer'),
       function(p, lambda, lower.tail=TRUE, log.p=FALSE)
               stats::qpois(p, lambda, lower.tail, log.p))
setMethod('qpois', signature(p='FLSimpleVector'),
       function(p, lambda, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qpois'
  n@select@variables[[1]] <- paste0('FLInvCDFPoisson(',
                                    n@select@variables[[1]],',',
                                    lambda,')')
  n
})


#' @export
setGeneric('qrayleigh', function(p, sigma=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qrayleigh') })
setMethod('qrayleigh', signature(p='integer'),
       function(p, sigma=1, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qrayleigh(p, sigma, lower.tail, log.p))
setMethod('qrayleigh', signature(p='FLSimpleVector'),
       function(p, sigma=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qrayleigh'
  n@select@variables[[1]] <- paste0('FLInvCDFRayleigh(',
                                    n@select@variables[[1]],',',
                                    sigma,')')
  n
})


#' @export
setGeneric('qreciprocal', function(q, lowbd, upbd) { standardGeneric('qreciprocal') })
setMethod('qreciprocal', signature(q='FLSimpleVector'),
       function(q, lowbd, upbd)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qreciprocal'
  n@select@variables[[1]] <- paste0('FLInvCDFReciprocal(',
                                    n@select@variables[[1]],',',
                                    lowbd,',',upbd,')')
  n
})


#' @export
setGeneric('qsemicircular', function(q, location, radius) { standardGeneric('qsemicircular') })
setMethod('qsemicircular', signature(q='FLSimpleVector'),
       function(q, location, radius)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qsemicircular'
  n@select@variables[[1]] <- paste0('FLInvCDFSemicircular(',
                                    n@select@variables[[1]],',',
                                    location,',',radius,')')
  n
})


#' @export
setGeneric('qt', function(p, df, ncp, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qt') })
setMethod('qt', signature(p='integer'),
       function(p, df, ncp, lower.tail=TRUE, log.p=FALSE)
               stats::qt(p, df, ncp, lower.tail, log.p))
setMethod('qt', signature(p='FLSimpleVector'),
       function(p, df, ncp, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qt'
  n@select@variables[[1]] <- paste0('FLInvCDFStudentsT(',
                                    n@select@variables[[1]],',',
                                    0,',',1,',',df,')')
  n
})


#' @export
setGeneric('qtrbeta', function(p, shape1, shape2, shape3, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qtrbeta') })
setMethod('qtrbeta', signature(p='integer'),
       function(p, shape1, shape2, shape3, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
               actuar::qtrbeta(p, shape1, shape2, shape3, rate, scale, lower.tail, log.p))
setMethod('qtrbeta', signature(p='FLSimpleVector'),
       function(p, shape1, shape2, shape3, rate=1, scale=1/rate, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qtrbeta'
  n@select@variables[[1]] <- paste0('FLInvCDFTransBeta(',
                                    n@select@variables[[1]],',',
                                    scale,',',shape1,',',shape2,',',shape3,')')
  n
})


#' @export
setGeneric('qtriang', function(p, a=-1, b=1, c=(a + b)/2, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qtriang') })
setMethod('qtriang', signature(p='integer'),
       function(p, a=-1, b=1, c=(a + b)/2, lower.tail=TRUE, log.p=FALSE)
               extraDistr::qtriang(p, a, b, c, lower.tail, log.p))
setMethod('qtriang', signature(p='FLSimpleVector'),
       function(p, a=-1, b=1, c=(a + b)/2, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qtriang'
  n@select@variables[[1]] <- paste0('FLInvCDFTriangular(',
                                    n@select@variables[[1]],',',
                                    a,',',b,',',c,')')
  n
})


#' @export
setGeneric('qunif', function(p, min=0, max=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qunif') })
setMethod('qunif', signature(p='integer'),
       function(p, min=0, max=1, lower.tail=TRUE, log.p=FALSE)
               stats::qunif(p, min, max, lower.tail, log.p))
setMethod('qunif', signature(p='FLSimpleVector'),
       function(p, min=0, max=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qunif'
  n@select@variables[[1]] <- paste0('FLInvCDFUniform(',
                                    n@select@variables[[1]],',',
                                    min,',',max,')')
  n
})


#' @export
setGeneric('qweibull', function(p, shape, scale=1, lower.tail=TRUE, log.p=FALSE) { standardGeneric('qweibull') })
setMethod('qweibull', signature(p='integer'),
       function(p, shape, scale=1, lower.tail=TRUE, log.p=FALSE)
               stats::qweibull(p, shape, scale, lower.tail, log.p))
setMethod('qweibull', signature(p='FLSimpleVector'),
       function(p, shape, scale=1, lower.tail=TRUE, log.p=FALSE)
{
  names(n@select@variables)[[1]] <- n@dimColumns[[2]] <- 'r_qweibull'
  n@select@variables[[1]] <- paste0('FLInvCDFWeibull(',
                                    n@select@variables[[1]],',',
                                    location=0,',',scale,',',shape,')')
  n
})
