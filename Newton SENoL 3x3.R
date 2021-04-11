##Newton SENoL 3x3
#Fi, fi
Norma <- function(y, metodo){
  if (metodo==2)
  {
    return(sqrt(sum(y^2)))
    
  }
  if (metodo==Inf){
    return(max(abs(y)))
  }
  return("El metodo debe ser 2 o inf")
}


NewtonSENoL = function(n, x, Tol, N){
  #paso 1
  k = 1
  #paso 2
  while (k <= N){
  #paso 3
    fx = Fx(x)
  J = Jacobiano(x[1], x[2], x[3])
  #paso 4
  y = solve(J) %*% (-fx)
  #paso 5
  x = x + t(y)
  #paso 6
  if (Norma(y, 2) < Tol){
    return(x)
  }
  k = k + 1
  }
  return ("maximo de iteraciones excedido, el metodo wasn't successful")
}


f1 = function(x1, x2, x3){
  3*x1 -cos(x2*x3) - (1/2)
}
df1 = expression(3*x1 -cos(x2*x3) - (1/2))
df11 = D(df1, "x1")
df11
df12 = D(df1, "x2")
df12
df13 = D(df1, "x3")
df13
Df11 = function(x1,x2,x3){
  Df11 = 3
}
Df12 = function(x1,x2,x3){
  Df12 = sin(x2 * x3) * x3
}
Df13 = function(x1,x2,x3){
  Df13 = sin(x2 * x3) * x2
}

f2 = function(x1, x2, x3){
  x1^2 - 81*(x2 + 0.1)^2 + sin(x3) + 1.06
}

df2 = expression(x1^2 - 81*(x2 + 0.1)^2 + sin(x3) + 1.06)
df21 = D(df2, "x1")
df21
df22 = D(df2, "x2")
df22
df23 = D(df2, "x3")
df23
Df21 = function(x1,x2,x3){
  Df21 = 2 * x1
}
Df22 = function(x1,x2,x3){
  Df22 = -(81 * (2 * (x2 + 0.1)))
}
Df23 = function(x1,x2,x3){
  Df23 = cos(x3)
}

f3 = function(x1, x2, x3){
  exp(-x1*x2) + 20*x3 + (10*pi - 3)/3
}

df3 = expression(exp(-x1*x2) + 20*x3 + (10*pi - 3)/3)
df31 = D(df3, "x1")
df31
df32 = D(df3, "x2")
df32
df33 = D(df3, "x3")
df33
Df31 = function(x1,x2,x3){
  Df21 = -(exp(-x1 * x2) * x2)
}
Df32 = function(x1,x2,x3){
  Df32 = -(exp(-x1 * x2) * x1)
}
Df33 = function(x1,x2,x3){
  Df33 = 20
}

Jacobiano <- function(x1,x2,x3){
  col1 <- 
    c(Df11(x1,x2,x3),Df12(x1,x2,x3),Df13(x1,x2,x3))
  
  col2 <- 
    c(Df21(x1,x2,x3),Df22(x1,x2,x3),Df23(x1,x2,x3))
  
  col3 <-
    c(Df31(x1,x2,x3),Df32(x1,x2,x3),Df33(x1,x2,x3))
  
  J <- rbind(col1,col2, col3) #con esta ultima armamos la matrix ampliada
  return(J)
}

Fx <- function(x){
  Fx <- rbind(f1(x[1],x[2],x[3]), f2(x[1],x[2],x[3]), f3(x[1],x[2],x[3]))
  return(Fx)
}

x <- c(0.1, 0.1, -0.1)
n=3

x1 = NewtonSENoL(n, x, 10^-6,100)[1]
x2 = NewtonSENoL(n, x, 10^-6,100)[2]
x3 = NewtonSENoL(n, x, 10^-16,10000)[3]
#imprimo resultados
f1(x1, x2, x3)
f2(x1, x2, x3)
f3(x1, x2, x3)

