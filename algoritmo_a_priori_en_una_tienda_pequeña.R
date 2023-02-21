# Librerias

library(arules) # Paquete para hacer las reglas de asociación
library(plyr) # Paquete para dar formato de transacción
library(arulesViz) # Paquete para crear un grafo interactivo

# Cargar el archivo

Datos_a_priori<-read.csv('Datos_Compras.csv',sep=',',dec='.',header=TRUE) 

str(Datos_a_priori) # Ver la estructura interna del archivo

# Preparar el archivo

Lista_productos<-ddply(Datos_a_priori,c('Transaccion'),function(df1)paste(df1$Producto,collapse = ','))

Lista_productos$Transaccion<-NULL # Quitar la columna transacción

write.csv(Lista_productos,'Lista_productos.csv',quote = FALSE,row.names = FALSE) # Guardar productos en csv

transacciones<-read.transactions('Lista_productos.csv',format='basket',sep=',',header = TRUE)

inspect(transacciones)

itemFrequencyPlot(transacciones,topN=10,type='absolute') # Graficar los productos que mas se vendieron

# Reglas de asociación

reglas<-apriori(transacciones,parameter = list(supp=0.1,conf=0.7,minlen=2)) # Hace la regla de asociación

reglas<-sort(reglas,by='confidence',decreasing = TRUE)

str(reglas)

inspect(reglas) # Observar que reglas 

duplicated(reglas) # Buscar reglas duplicadas

redundantes<-is.redundant(reglas)
redundantes

which(is.redundant(reglas)) 

reglas_podadas<-reglas[!redundantes] # Quitar reglas redundates

inspect(reglas_podadas)

# Grafo interactivo

plot(reglas_podadas,method = 'graph',engine = 'interactive',shading = 'confidence')

# Agregar indicativos de leverage y conviction
quality(reglas_podadas)<-cbind(
  quality(reglas_podadas),
  leverage=interestMeasure(reglas_podadas,measure = 'leverage',transactions = transacciones),
  conviction=interestMeasure(reglas_podadas,measure='conviction',transactions = transacciones)
  ) 

inspect(reglas_podadas,by='confidence')

productos_frecuentes<-data.frame(Productos=labels(reglas_podadas),Indicador=reglas_podadas @quality)

write.csv(productos_frecuentes,'productos_frecuentes.csv') # Guardar la información