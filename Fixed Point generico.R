f1 = function(x){
  x = x - x^3 - 4*x^2 + 10}
x = seq(from=1, to=2, by= 0.1)
x
y = f1(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1, col = "red") #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)


PuntoFijo = function(fn,p0,Tol, N){
  i = 1
  while(i <= N) {
    p = fn(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(f1,1.36555, 10^-2, 10000)