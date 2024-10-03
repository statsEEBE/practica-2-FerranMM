#Codigo para problema 2

mis_dades <- iris
mis_dades

x<-mis_dades$Petal.Length
x

y<-mis_dades$Sepal.Length
y

plot(x,y)

x_bar<- mean(x)
x_bar

y_bar<- mean(y)
y_bar

m<-sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
m

b<- y_bar- m*x_bar
b

m*1.5+b

#Predicción sobre las observaciones
y_pred<-m*x+b

plot(x,y)
lines(x,y_pred)

#COEFICIENTE DE CORRELACIÓN

R_sq<- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
R_sq

#(~)Alt Gr+ 4+ espacio
mod<-lm(y~x)
mod
summary(mod)

cor.test(x,y)




####### Pregunta 1 (A=0.1923(R^2), B=0.4385(R), C=m*variacio(x), D=55.54)

sqrt(0.1923)


m<-0.06576
b<-11.79755
x<-647.1

x<- 0:700
y_pred<-m*x+b
plot(x,y_pred,ylim=c(0,120))
