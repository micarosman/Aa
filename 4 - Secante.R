##Secante (y comparacion con Newton)

#e^x + 2^-x + 2*cos(x) - 6 = 0 x E [1;2] ----
f1 = function(x){
  x = exp(x) + 2^-x + 2*cos(x) - 6
}
x = 1:2
x
y = f1(x)
y
z = function(x)
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f1(p0)
  q1 = f1(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f1(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(1,2, 10^-2, 100)
#Newton(1.82,10^-2, 100)
#[1] "el método funciono despues de 1 intento, con resultado: 1.82945804108693"
#Secante(1,2, 10^-2, 100)
#"el método funciono despues de 5 intento, con resultado: 1.82933117293153"

#ln(x-1) + cos(x-1) = 0 x E [1.3;2]---- ver!!! ----
f2 = function(x){
  x = log10(x-1) + cos(x-1)
}
x = 1.3:2
x
y = f2(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f2(p0)
  q1 = f2(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f2(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(1.9,2,10^-2, 100)
#Newton(1.3,10^-2, 100)
#"el método funciono despues de 4 intento, con resultado: 1.10868036376373"
#Secante(1.9,2,10^-2, 100)
#"el método funciono despues de 5 intento, con resultado: 2.83843824179344"

#2*x*cos(2*x) - (x-2)^2 = 0 x E [2;3] ^ x E [3;4]---- ver!!! ----
f3 = function(x){
  x = 2*x*cos(2*x) - (x-2)^2
}
x = seq(from = 2, to= 3, by = 0.01)
x
y = f3(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f3(p0)
  q1 = f3(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f3(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(2,3,10^-1, 100)
# Newton(2.4,10^-1, 100)
# "el método funciono despues de 1 intento, con resultado: 2.42551928434498"
#Secante(2,3,10^-1, 100)
#"el método funciono despues de 3 intento, con resultado: 2.37314878342558"

x = 3:4
x
y = f3(x)
y
  K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Secante(3,4,10^-1, 10000)
#Newton(3.7,10^-1, 10000)
# "el método funciono despues de 1 intento, con resultado: 3.66703924054236"
#Secante(3,4,10^-1, 10000)
# "el método funciono despues de 4 intento, con resultado: 3.73192046127779"

#(x-2)^2 - ln(x) = 0 x E [1;2] ^ x E [e; 4]----
f4 = function(x){
  x = (x-2)^2 -log10(x)
}
x = 1:2
x
y = f4(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f4(p0)
  q1 = f4(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f4(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(1,2, 10^-3, 100)
#Newton(1,10^-3, 100)
#"el método funciono despues de 4 intento, con resultado: 1.56041049616681"
#Secante(1,2, 10^-3, 100)
#"el método funciono despues de 7 intento, con resultado: 1.56041251743393"

x = exp(1):4
x
y = f4(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Secante(exp(1),4, 10^-2, 10000)
#Newton(exp(1),10^-2, 10000)
#"el método funciono despues de 2 intento, con resultado: 2.6506683721065"
#Secante(exp(1),4, 10^-2, 10000)
# "el método funciono despues de 5 intento, con resultado: 2.65066664137801"

#e^x - 3*x^2 = 0 x E [0;1] ^ x E [3;5]----
f5 = function(x){
  x = exp(x) - 3*x^2
}
x = seq(from=0, to=1, by= 0.01)
x
y = f5(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f5(p0)
  q1 = f5(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f5(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(0,1, 10^-2, 100)
#Newton(0,10^-2, 100)
#"el método funciono despues de 2 intento, con resultado: 0.910017665783406"
#Secante(0,1, 10^-2, 100)
#"el método funciono despues de 4 intento, con resultado: 0.910623538896086"

x = seq(from=3, to=5, by= 0.01)
x
y = f5(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Secante(3,5, 10^-2, 100)
#Newton(5,10^-2, 100)
#"el método funciono despues de 5 intento, con resultado: 3.73308078919076"
#Secante(3,5, 10^-2, 100)
#"el método funciono despues de 9 intento, con resultado: 3.73307383612786"

#sin(x) - e^-x = 0 x E [0;1] ^ x E [3;4] ^ x E [6;7]----
f6 = function(x){
  x = sin(x) - exp(-x)
}
x = seq(from=0, to=1, by= 0.01)
x
y = f6(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f6(p0)
  q1 = f6(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f6(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(0,1, 10^-2, 100)
#Newton(0,10^-2, 100)
#"el método funciono despues de 3 intento, con resultado: 0.588529412626355"
#Secante(0,1, 10^-2, 100)
#"el método funciono despues de 5 intento, con resultado: 0.588538358017851"

x = seq(from=3, to=4, by= 0.01)
x
y = f6(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Secante(3,4, 10^-2, 100)
#Newton(3,10^-2, 100)
#"el método funciono despues de 2 intento, con resultado: 3.09636396089665"
#Secante(3,4, 10^-2, 100)
#"el método funciono despues de 4 intento, con resultado: 3.09636350534766"

x = seq(from=6, to=7, by= 0.01)
x
y = f6(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)
Secante(6,7, 10^-2, 100)
#Newton(7,10^-2, 100)
#"el método funciono despues de 3 intento, con resultado: 6.28504927021944"
#Secante(6,7, 10^-2, 100)
#"el método funciono despues de 4 intento, con resultado: 6.28504936779047"

#cos(x) = sqrt(x)----
f7 = function(x){
  x = cos(x) - sqrt(x)
}
x = seq(from=0, to=1, by= 0.01)
x
y = f7(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f7(p0)
  q1 = f7(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f7(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(0,1, 10^-1, 100)
#Newton(0.5,10^-1, 100)
#"el método funciono despues de 2 intento, con resultado: 0.641714866792004"
#Secante(0,1, 10^-1, 100)
#"el método funciono despues de 3 intento, con resultado: 0.643753386653444"

#2 + cos(e^x - 2) = e^x----
f8 = function(x){
  x = 2 + cos(exp(x) - 2) - exp(x)
}
x = seq(from=0, to=1, by= 0.01)
x
y = f8(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f8(p0)
  q1 = f8(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f8(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(0,2, 10^-2, 100)
#Newton(1,10^-2, 100)
#"el método funciono despues de 1 intento, con resultado: 1.00768903625654"
#Secante(0,2, 10^-2, 100)
# "el método funciono despues de 7 intento, con resultado: 1.00798433628262"

#x^3 - 7*x^2 + 14*x - 6 = 0----
f9 = function(x){
  x = x^3 - 7*x^2 + 14*x - 6
}
x = seq(from=0, to=1, by= 0.01)
x
y = f9(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f9(p0)
  q1 = f9(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f9(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(0,1, 10^-1, 200)
#Newton(1,10^-1, 200)
#"el método funciono despues de 3 intento, con resultado: 0.584730165689234"
#Secante(0,1, 10^-1, 200)
#"el método funciono despues de 4 intento, con resultado: 0.596123473519421"

#cos(x) = x----
f10 = function(x){
  x = cos(x) - x
}
x = seq(from=0, to=1, by= 0.01)
x
y = f10(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f10(p0)
  q1 = f10(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f10(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(0,2, 10^-2, 10000)
#Newton(1,10^-2, 10000)
#"el método funciono despues de 3 intento, con resultado: 0.739085133385284"
#Secante(0,2, 10^-2, 10000)
#"el método funciono despues de 5 intento, con resultado: 0.739081136054205"

#-x^3 - cos(x) = 0----
f11 = function(x){
  x = -x^3 - cos(x)
}
x = seq(from=-1, to=1, by= 0.01)
x
y = f11(x)
y
K = c(x,y)
plot(x,y, lines(x,y))
abline(h=0)
abline(v=0)

Secante = function(p0,p1,Tol, N){
  i=2
  q0 = f11(p0)
  q1 = f11(p1)
  while(i <= N) {
    p = p1 - q1*(p1-p0)/(q1-q0)
    if (abs(p - p1) < Tol ){
      return(paste('el método funciono despues de',i,'intento, con resultado:',p))
    } else {
      i = i+1
      p0 = p1
      q0 = q1
      p1 = p
      q1 = f11(p)
      
    } 
  }
  return(paste('el metodo no funciono despues de', N, 'iteraciones, con resultado aproximado:', p))
}
Secante(-1,0, 10^-1, 10000)
#Newton(-1,10^-1, 10000)
#"el método funciono despues de 2 intento, con resultado: -0.865684163176082"
#Secante(-1,0, 10^-1, 10000)
# "el método funciono despues de 5 intento, con resultado: -0.847783769432569"