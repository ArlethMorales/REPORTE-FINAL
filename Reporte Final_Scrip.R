## Comenzamos llamando a la librer�a
library(MASS)

## Elecci�n de la matriz de datos
Ha<-as.data.frame(Halcones)

## Exploraci�n de la matriz
dim(Ha)
colnames(Ha)
str(Ha)

# Variables tipo n�mericas y de caracteres.

## Detecci�n de valores NULOS (NA)
anyNA(Ha)

### RESULTADOS

## Se define la matriz de datos y la variable respuesta con las 
## clasificaciones.
x<-Ha[,5:11]
y<-Ha[,4] 

# Se opt� por trabajar con 8 variables que son: Especies (que es 
# la variable y), Wing, Weight, Culmen, Hallux, Tail, StandardTail, 
# Tarsus. 

## Definir como n y p el n�mero de species y variables
n<-nrow(x)
p<-ncol(x)


## Se aplica el An�lisis discriminante lineal (LDA)

modelo_lda <- lda(formula = y ~ Wing + Weight +
                    Culmen + Hallux + Tail +
                    StandardTail + Tarsus, data = x)

# Desp�es de haber obtenido las funciones discriminantes, ya se puede 
# clasificar un nuevo Halc�n dependiendo sus medidas.

# Gr�fico

plot(modelo_lda, main = "Gr�fico de discriminantes lineales")

## Ponemos un ejemplo con los siguientes datos:

# Wing = 250, Weight = 715, Culmen = 18.2, Hallux = 11
# Tail = 218, StandardTail = 176, Tarsus = 67.8

## NUEVAS OBSERVACIONES

nuevas_observaciones <- data.frame(Wing = 250, Weight = 715,
                                   Culmen = 18.2, Hallux = 11, Tail = 218,
                                   StandardTail = 176, Tarsus = 67.8)
predict(object = modelo_lda, newdata = nuevas_observaciones)


# El resultado muestra que, de acuerdo con la funci�n discriminante,
# la probabilidad posterior de que un Halc�n pertenezca a la especie
# SS es de 99.9%

## Evaluaci�n de los errores de clasificaci�n.
predicciones <- predict(object = modelo_lda, newdata = Ha[, 5:11],
                        method = "predictive")
table(Ha$Species, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(Ha$Species != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")

# La precisi�n de clasficaci�n del modelo discriminante es del 98.62% ya
# que solo tiene 1.38% de error de clasificaci�n
