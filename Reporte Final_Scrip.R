## Comenzamos llamando a la librería
library(MASS)

## Elección de la matriz de datos
Ha<-as.data.frame(Halcones)

## Exploración de la matriz
dim(Ha)
colnames(Ha)
str(Ha)

# Variables tipo númericas y de caracteres.

## Detección de valores NULOS (NA)
anyNA(Ha)

### RESULTADOS

## Se define la matriz de datos y la variable respuesta con las 
## clasificaciones.
x<-Ha[,5:11]
y<-Ha[,4] 

# Se optó por trabajar con 8 variables que son: Especies (que es 
# la variable y), Wing, Weight, Culmen, Hallux, Tail, StandardTail, 
# Tarsus. 

## Definir como n y p el número de species y variables
n<-nrow(x)
p<-ncol(x)


## Se aplica el Análisis discriminante lineal (LDA)

modelo_lda <- lda(formula = y ~ Wing + Weight +
                    Culmen + Hallux + Tail +
                    StandardTail + Tarsus, data = x)

# Despúes de haber obtenido las funciones discriminantes, ya se puede 
# clasificar un nuevo Halcón dependiendo sus medidas.

# Gráfico

plot(modelo_lda, main = "Gráfico de discriminantes lineales")

## Ponemos un ejemplo con los siguientes datos:

# Wing = 250, Weight = 715, Culmen = 18.2, Hallux = 11
# Tail = 218, StandardTail = 176, Tarsus = 67.8

## NUEVAS OBSERVACIONES

nuevas_observaciones <- data.frame(Wing = 250, Weight = 715,
                                   Culmen = 18.2, Hallux = 11, Tail = 218,
                                   StandardTail = 176, Tarsus = 67.8)
predict(object = modelo_lda, newdata = nuevas_observaciones)


# El resultado muestra que, de acuerdo con la función discriminante,
# la probabilidad posterior de que un Halcón pertenezca a la especie
# SS es de 99.9%

## Evaluación de los errores de clasificación.
predicciones <- predict(object = modelo_lda, newdata = Ha[, 5:11],
                        method = "predictive")
table(Ha$Species, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(Ha$Species != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")

# La precisión de clasficación del modelo discriminante es del 98.62% ya
# que solo tiene 1.38% de error de clasificación
