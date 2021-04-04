fn = function(x){
  x = #la funcion#
}
x = seq(from=0, to=1, by= 0.01)
x
y = fn(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

FalsaPos = function(fn,p0,p1,Tol, N){
  if(fn(p0)*fn(p1) < 0){
    i=2
    q0 = fn(p0)
    q1 = fn(p1)
    while(i <= N) {
      p = p1 - q1*(p1-p0)/(q1-q0)
      if (abs(p - p1) < Tol ){
        return(paste('el método funciono despues de',i,'intento, con resultado:',p))
      } 
      i = i+1
      q = fn(p)
      if(q * q1 < 0){
        p0 = p1
        q0 = q1
      } 
      p1 = p
      q1 = q
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
FalsaPos(fn,1.7,1.8, 10^-8, 100)