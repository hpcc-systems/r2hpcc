#
r2hpcc.NVL <- function(arg1, arg2 = "")
{
  if (is.null(arg1))
  {
    resp <- arg2
  }
  else
  {
    resp <-arg1
  }
  resp
}