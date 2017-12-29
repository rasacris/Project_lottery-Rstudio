# LIBRERIAS Y DATOS
# -----------------------------------------------------
library(MASS);library(neuralnet); library(ggplot2);library(rpart);library(rpart.plot);library(ggplot2);library(scatterplot3d);library(neuralnet);library(NeuralNetTools);library(nnet)

set.seed(65)
lote_v0 <- lote

datos    <- lote_v0
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]

# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]


# FORMULA
# -----------------------------------------------------
nms  <- names(train_nrm)
frml <- as.formula(paste("Número ~", paste(nms[!nms %in% "Número"], collapse = " + ")))


# MODELO
# -----------------------------------------------------
#a string containing the algorithm type to calculate the neural network. 
#The following types are possible: 'backprop', 'rprop+', 'rprop-', 'sag', or 'slr'. 'backprop' refers to backpropagation, 
#'rprop+' and 'rprop-' refer to the resilient backpropagation with and without weight backtracking,
#while 'sag' and 'slr' induce the usage of the modified globally convergent algorithm (grprop). See Details for more information.
modelo.nn <- neuralnet(frml,
                       data          = train_nrm,
                       learningrate = 0.8, # coeficientedeAprendizaje
                       hidden        = c(3), # ver Notas para detalle 
                       threshold     = 0.02,
                       lifesign = "full",
                       stepmax=2e05,
                       algorithm     = "rprop+"
)
#El parametro hidden = c(7,5) especifica una primera capa oculta con 7 neuronas y una segunda capa oculta con 5 neuronas.

# PREDICCION
# -----------------------------------------------------
pr.nn  <- compute(modelo.nn,within(test_nrm,rm(Número)))

# se transoforma el valor escalar al valor nominal original
Número.predict<-pr.nn$net.result*(max(datos$Número)-min(datos$Número))+min(datos$Número)
digits=2
Número.real<-(test_nrm$Número)*(max(datos$Número)-min(datos$Número))+min(datos$Número)

dataPredict <- as.data.frame(Número.real,Número.predict)

plot(lote_v0,Número.predict,col='red',main='Año vs NúmeroPredecido',pch=1,cex=1.5)

legend('bottomright',legend='NN',pch=6,col='red', bty='n')
#corrplot(M, method="circle")
# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
(se.nn <- sum((Número.real - Número.predict)^2)/221)

#GRAFICOS
# -------
# Errores
qplot(x=Número.real, y=Número.predict, geom=c("point","smooth"), method="lm", 
      main=paste("Real Vs Prediccion. Summa de Error Cuadratico=", round((se.nn/100),2)))
# Red
plot(modelo.nn,rep="best")
plotnet(modelo.nn)


arbol <- function(data){
  
  train01 <- 0.9
  test01 <- 1-train
  modelo01 <- sample(1:nrow(data),round(train01*nrow(data)),replace=T)
  train01 <- data[modelo01,]
  test01 <- data[-modelo01,]
  
  arbolEj5 <-rpart(Número ~.,data=train01,cp=.01,minbucket=1)
  print(arbolEj5)
  rpart.plot(arbolEj5, box.palette="auto",
             branch.lty=3, shadow.col="gray")
}
arbol(lote_v0)
