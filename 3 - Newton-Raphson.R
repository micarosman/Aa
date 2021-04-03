##Newton-Raphson

#e^x + 2^-x + 2*cos(x) - 6 = 0 x E [1;2] ----
f1 = function(x){
  x = exp(x) + 2^-x + 2*cos(x) - 6
}
ff1 = expression(exp(x) + 2^-x + 2*cos(x) - 6)
df1 = D(ff1, "x")
df1
Df1 = function(x){
  df1 = exp(x) - 2^-x * log(2) - 2 * sin(x) }
x = 1:2
x
y = f1(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f1(p0)/Df1(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1.82,10^-2, 100)
#ln(x-1) + cos(x-1) = 0 x E [1.3;2]----
f2 = function(x){
  x = log10(x-1) + cos(x-1)
}
ff2 = expression(log10(x-1) + cos(x-1))
df2 = D(ff2, "x")
df2
Df2 = function(x){
  df2 = (1/(x-1) - sin(x-1)) }
x = 1.3:2
x
y = f2(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f2(p0)/Df2(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1.3,10^-2, 100)
#2*x*cos(2*x) - (x-2)^2 = 0 x E [2;3] ^ x E [3;4]----
f3 = function(x){
  x = 2*x*cos(2*x) - (x-2)^2
}
ff3 = expression(2*x*cos(2*x) - (x-2)^2)
df3 = D(ff3, "x")
df3 
Df3 = function(x){
  df3 = 2 * cos(2*x) - 2 * (x - 2) + 4 * x * sin(2*x) 
}
x = seq(from = 2, to= 3, by = 0.01)
x
y = f3(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f3(p0)/Df3(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(2.4,10^-1, 100)

x = 3:4
x
y = f3(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Newton(3.7,10^-1, 10000)
#(x-2)^2 - ln(x) = 0 x E [1;2] ^ x E [e; 4]---- ###revisar!! ----
f4 = function(x){
  x = (x-2)^2 - log10(x)
}
ff4 = expression((x-2)^2 - log10(x))
df4 = D(ff4, "x")
df4 
Df4 = function(x){
  df4 = 2 * (x - 2) - 1/(x * log(10)) }
x = 1:2
x
y = f4(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f4(p0)/Df4(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    }else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1,10^-3, 100)

x = exp(1):4
x
y = f4(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Newton(exp(1),10^-2, 10000)
#e^x - 3*x^2 = 0 x E [0;1] ^ x E [3;5]----
f5 = function(x){
  x = exp(x) - 3*x^2
}
ff5 = expression(exp(x) - 3*x^2)
df5 = D(ff5, "x")
df5 
Df5 = function(x){
  df5 = exp(x) - 3 * (2 * x)}
x = seq(from=0, to=1, by= 0.01)
x
y = f5(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f5(p0)/Df5(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1,10^-2, 100)

x = seq(from=3, to=5, by= 0.01)
x
y = f5(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Newton(5,10^-2, 100)
#sin(x) - e^-x = 0 x E [0;1] ^ x E [3;4] ^ x E [6;7]----
f6 = function(x){
  x = sin(x) - exp(-x)
}
ff6 = expression(sin(x) - exp(-x))
df6 = D(ff6, "x")
df6 
Df6 = function(x){
  df6 = cos(x) + exp(-x) }
x = seq(from=0, to=1, by= 0.01)
x
y = f6(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f6(p0)/Df6(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(0,10^-2, 100)

x = seq(from=3, to=4, by= 0.01)
x
y = f6(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Newton(3,10^-2, 100)

x = seq(from=6, to=7, by= 0.01)
x
y = f6(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Newton(7,10^-2, 100)
#cos(x) = sqrt(x)----
f7 = function(x){
  x = cos(x) - sqrt(x)
}
ff7 = expression(cos(x) - sqrt(x))
df7 = D(ff7, "x")
df7 
Df7 = function(x){
  df7 = -(0.5/sqrt(x) + sin(x)) }
x = seq(from=0, to=1, by= 0.01)
x
y = f7(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f7(p0)/Df7(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(0.5,10^-1, 100)
#2 + cos(e^x - 2) = e^x----
f8 = function(x){
  x = 2 + cos(exp(x) - 2) - exp(x)
}
ff8 = expression(2 + cos(exp(x) - 2) - exp(x))
df8 = D(ff8, "x")
df8 
Df8 = function(x){
  df7 = -((1 + sin(exp(x) - 2)) * exp(x)) }
x = seq(from=0, to=1, by= 0.01)
x
y = f8(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f8(p0)/Df8(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1,10^-2, 100)
#x^3 - 7*x^2 + 14*x - 6 = 0----
f9 = function(x){
  x = x^3 - 7*x^2 + 14*x - 6
}
ff9 = expression(x^3 - 7*x^2 + 14*x - 6)
df9 = D(ff9, "x")
df9 
Df9 = function(x){
  df9 = 14 + x * (3 * x - 14) }
x = seq(from=0, to=1, by= 0.01)
x
y = f9(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f9(p0)/Df9(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1,10^-1, 200)
#cos(x) = x----
f10 = function(x){
  x = cos(x) - x
}
ff10 = expression(cos(x) - x)
df10 = D(ff10, "x")
df10
Df10 = function(x){
  df10 = -(1 + sin(x)) }
x = seq(from=0, to=1, by= 0.01)
x
y = f10(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f10(p0)/Df10(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(1,10^-2, 10000)
#-x^3 - cos(x) = 0----
f11 = function(x){
  x = -x^3 - cos(x)
}
ff11 = expression(-x^3 - cos(x))
df11 = D(ff11, "x")
df11
Df11 = function(x){
  df11 = sin(x) - 3 * x^2 }
x = seq(from=-1, to=1, by= 0.01)
x
y = f11(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Newton = function(p0,Tol, N){
  i=1
  while(i <= N) {
    p = p0 - f11(p0)/Df11(p0)
    if (abs(p - p0) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Newton(-1,10^-1, 10000)
