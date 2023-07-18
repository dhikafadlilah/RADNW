MLE = function(n, mean, sd)
{
  x = rnorm(n, mean, sd)
  LL = function(mu, sigma2)
  {
    n/2*log(2*pi)+n/2*log(sigma2)+sum((x-mu)^2)/(2*sigma2)
  }
  summary(mle(LL, start = list(mu = mean, sigma2=(sd^2))))
}
