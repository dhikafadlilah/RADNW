golden<-function(f, a, b, tol=0.0000001)
{
  ratio<-2/(sqrt(5)+1)
  x1<- b - ratio*(b-a)
  x2<- a - ratio*(b-a)
  f1<- f(x1)
  f2<- f(x2)
  while(abs(b-a)>tol){
    if (f2>f1){
      b<- x2
      x2<- x1
      f2<- f1
      x1<- b - ratio * (b - a)
      f1 <- f(x1)
    } else {
      a<- x1
      x1<- x2
      f1<- f2
      x2<- a+ ratio * (b - a)
      f2<- f(x2)
    }
  }
  return((a + b) / 2)
}
