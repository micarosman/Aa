fn = function(x){
  f = #la funcion#
  f}
x = 0:5
x
y = fn(x)
y
J = c(x,y)
dim(J) = c(6,2)
plot(x,y, lines(x,y))
abline(h=0)

Raiz_Biseccion = function(fn,a,b, Tol, N){
  i = 1
  FA = fn(a)
  while(i <= N){
    p = a + (b-a)/2
    FP = fn (p)
    if (FP == 0 | (b-a)/2 < Tol){ #para la barra es Alt + 124
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    }
    i = i + 1
    if (FP * FA > 0){
      a = p
      FA = FP
    } else {
      b = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Raiz_Biseccion(fn, 0, 3, 0.00001, 100)