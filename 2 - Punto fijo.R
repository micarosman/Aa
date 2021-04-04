##Punto fijo

#x = g1(x) = x - x^3 - 4*x^2 + 10----
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


PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f1(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(1.36555, 10^-2, 10000)

#x = g2(x) = (10/x - 4x)^1/2----
f2 = function(x){
  x = (10/x - 4*x)^(1/2)}
x = seq(from=1, to=2, by= 0.1)
x
y = f2(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1) 
abline(h=0)
abline(v=0)

PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f2(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(1.36555, 10^-1, 10000)


#x = g3(x) = 1/2*(10-x^3)^1/2----
f3 = function(x){
  x = (1/2)*(10-x^3)^(1/2)}
x = -3:2
x
y = f3(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1) #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)
PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f3(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(1.36555, 10^-2, 100)

#x = g4(x) = (10/(4+x))^1/2----
f4 = function(x){
  x = (10/x - 4*x)^(1/2)}
x = -3:2
x
y = f4(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1) #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)
PuntoFijo = function(f4, p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f4(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(f4, 1.36555, 10^-2, 100)

#x = g5(x) = x - (x^3 + 4*X^2 - 10)/(3*x^2 + 8*x)----
f5 = function(x){
  xx = x - (x^3 + 4*x^2 - 10)/(3*x^2 + 8*x)}
x = -3:2
x
y = f5(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1, col = "red") #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)
PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f5(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(1.36555, 10^-2, 100)



##reescribir como problemas de punto fijo----

#cos(x) = sqrt(x)---- 
# x = g(x) = x - cos(x) + sqrt(x)
f6 = function(x){
  x = x - cos(x) + sqrt(x)} 
x = 0:2
x
y = f6(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1, col = "red") #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)
PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f6(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(0.6417, 10^-1, 10000)


#2 + cos(e^x - 2) = e^x----
# x = g(x) = x - 2 - cos(exp(x)-2) + exp(x)
f7 = function(x){
  x = x - 2 - cos(exp(x)-2) + exp(x)} 
x = 0:2
x
y = f7(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1, col = "red") #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)
PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f7(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(1, 10^-1, 10000)

#x^3 - 7*x^2 + 14*x - 6 = 0----
# x = g(x) = x - x^3 + 7*x^2 - 14*x + 6
f8 = function(x){
  x = x - x^3 + 7*x^2 - 14*x + 6} 
x = 0:2
x
y = f8(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(a=0, b=1, col = "red") #y = x con la forma de y = a + bx
abline(h=0)
abline(v=0)
PuntoFijo = function(p0,Tol, N){
  i = 1
  while(i <= N) {
    p = f8(p0)
    if (abs(p-p0) < Tol){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
PuntoFijo(0.58, 10^-1, 1)

