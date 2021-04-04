fn = function(x){
  x = #la funcion#
}
ffn = expression(#la funcion#)
dfn = D(ffn, "x")
dfn
Dfn = function(x){
  dfn = cos(x) + exp(-x) }
x = seq(from=0, to=1, by= 0.01)
x
y = fn(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(fn,p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - fn(p0)/Dfn(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(fn, 0.58,10^-2, 100)