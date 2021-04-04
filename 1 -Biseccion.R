##Biseccion

#cos (x) = sqrt(x)----
funcion1 = function(x){
  f = cos(x) - sqrt(x)
  f}
x = 0:5
x
y = funcion1(x)
y
J = c(x,y)
dim(J) = c(6,2)
plot(x,y, lines(x,y))
abline(h=0)
Raiz_Biseccion = function(a,b, Tol, N){
  i = 1
  FA = funcion1(a)
  while(i <= N){
    p = a + (b-a)/2
    FP = funcion1 (p)
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
Raiz_Biseccion(0, 3, 0.00001, 100)
#x^3 + 4*x^2 - 10 = 0----
funcion2 = function(x){
  f = x^3 + 4*x^2 - 10
  f}
x = -5:5
x
y = funcion2(x)
y
H = c(x,y)
dim(H) = c(11,2)
plot(x,y, lines(x,y))
abline(h=0)
Raiz_Biseccion = function(a,b, Tol, N){
  i = 1
  FA = funcion2(a)
  while(i <= N){
    p = a + (b-a)/2
    FP = funcion2 (p)
    if (FP == 0 | (b-a)/2 < Tol){ 
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
Raiz_Biseccion(-5, 2, 0.00001, 100)
#2+ cos(e^x - 2) = e^x----
funcion3 = function(x){
  f = 2 + cos(exp(x) - 2)- exp(x)
  f}
x = -3:3
x
y = funcion3(x)
y
K = c(x,y)
dim(K) = c(7,2)
plot(x,y, lines(x,y))
abline(h=0)
Raiz_Biseccion = function(a,b, Tol, N){
  i = 1
  FA = funcion3(a)
  while(i <= N){
    p = a + (b-a)/2
    FP = funcion3 (p)
    if (FP == 0 | (b-a)/2 < Tol){ 
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
Raiz_Biseccion(-1, 2, 0.00001, 100)
#x^3 - 7*x^2 + 14*x - 6 = 0----
funcion4 = function(x){
  f = x^3 - 7*x^2 + 14*x - 6
  f}
x = -1:5
x
y = funcion4(x)
y
M = c(x,y)
dim(M) = c(7,2)
plot(x,y, lines(x,y))
abline(h=0)
Raiz_Biseccion = function(a,b, Tol, N){
  i = 1
  FA = funcion4(a)
  while(i <= N){
    p = a + (b-a)/2
    FP = funcion4 (p)
    if (FP == 0 | (b-a)/2 < Tol){ 
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
Raiz_Biseccion(-1, 2, 0.00001, 100)
Raiz_Biseccion(2, 3.1, 0.00001, 100) 
Raiz_Biseccion(3.1, 5, 0.00001, 100)