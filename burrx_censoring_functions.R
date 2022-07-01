rburrxtype1 = function(n, theta, C)
{
  # Generating a type 1 censored random sample of burrxs with theta.
  # C is either a scalar representing a common censoring time, or a vector of length
  # n representing individual censoring times.
  
  # Error checking for C.
  if ((length(C) > 1) && (length(C) != n))
  {
    stop("C must be a scalar or have length equal to n.")
  }
  if (min(C) <=0 )
  {
    stop("C must be greater than 0.")
  }
  
  # Generate a burrx(theta) sample.
  x = rburrx(n,theta)
  
  # Make C a vector if not already.
  if (length(C)==1)
  {
    C = rep(C,n)
  }
  
  # Compare each x to each C, and if x <= C then we have a failure
  # so t = x, and delta = 1.  Otherwise, it is censored, and t = C and
  # delta = 0.
  t = vector(mode="numeric", length=n)
  for (i in 1:n)
  {
    t[i] = min(x[i],C[i])
  }
  delta = as.numeric(x <= C)
  
  ans = data.frame(t=t, delta=delta)
  return(ans)
}

rburrxtype2 = function(n, theta, r)
{
  # Generating a type 2 censored random sample of burrx with parameter theta.
  # r is the number of failures.
  
  # Error checking on r.
  if ((r < 1) || (r > n))
  {
    stop("r must be between 1 and n.")
  }
  
  # If r is not an integer, round it down.
  r = floor(r)
  # Generate a sample of burrx parameter theta.
  x = rburrx(n,theta)
  
  # Sort x, then let t = the first r values of x and x[r] for the last
  # (n-r) values of x.  delta will be 1 for the first r values, and 0 otherwise.
  x = sort(x)
  
  t = vector(mode="numeric", length=n)
  delta = vector(mode="numeric", length=n)
  t[1:r] = x[1:r]
  delta[1:r] = 1
  
  if (r < n)
  {
    t[(r+1):n] = x[r]
    delta[(r+1):n] = 0
  }
  
  ans = data.frame(t=t, delta=delta)
  return(ans)
}




