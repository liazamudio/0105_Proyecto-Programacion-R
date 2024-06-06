# En este proyecto se analizarán los precios de algunas FIBRAS que cotizan en la BMV.
# Se considerarán primero las mas conocidas (con mas tiempo en el mercado, preferentemente 5 años o mas).
# Se obtienen los precios de sus titulos en el sitio Investing.

# Instalamos paquetes
install.packages("dplyr")       # Para manipulación de datos
install.packages("ggplot2")     # Para generar gráficos
install.packages("corrplot")    # Para hacer correlaciones
install.packages("reshape2")    # Utilizado en aplicaciones como el heatmap
install.packages("quantmod")    # Para el análisis de series de tiempo financieras,

# Cargamos paquetes
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(quantmod)

##### Obtención de los datos a analizar

# Definimos el directorio de trabajo
setwd("d:/Alex/OneDrive/devs/github/portfolio/proyectos/15-prog-and-stat-r/dataset/")

# Leemos el dataset que vamos a utilizar
fibras2014a2024 <- read.csv("fibras2014-2024.csv")
fibras2014a2024                               # Visualizamos la tabla
# print(tail(fibras2014a2024))                # Visualizamos las ultimas filas del dataframe

# Exploramos algunas características
dim(fibras2014a2024)                          # Calculamos las dimensiones de la tabla
# class(fibras2014a2024)                      # Objeto de la tabla, con esto verificamos que sea un dataframe

##### Limpiamos el dataframe

# Eliminamos las filas con registros con antigüedad mayor a 5 años (60 meses)
ult_registro <- nrow(fibras2014a2024)                           # Determinamos los limites del nuevo dataframe.
ult_registro                                                    # Último registro
nvo_prim_reg <- ult_registro-60                                 # 60 es el numero total de meses considerado, 12 repartidos en 5 años.
nvo_prim_reg                                                    # Nuevo primer registro
fibras_ult_5a <- fibras2014a2024[nvo_prim_reg:ult_registro, ]   # Creación del nuevo dataframe con los limites establecidos
fibras_ult_5a

# Eliminamos columnas con menos de 5 años de historial, es caso de que las haya.      # Aqui se puede emplear complete.cases y na.omit (sesión 2 ejemplo 6)
# fibras_mayor_5a <- Filter(function(x) !any(is.na(x)),fibras_ult_5a)
# fibras_mayor_5a

# Eliminamos la columna de Fecha
# fibras_mayor_5a_sf <- fibras_mayor_5a %>% select(-Fecha)      # Habilitar si aplca la eliminacion de columnas con menos de 5 años de historial
fibras_mayor_5a_sf <- fibras_ult_5a %>% select(-Fecha)          # Eliminamos la columna de fecha
fibras_mayor_5a_sf


########## Análisis técnico

# 5 años

# Variación total de precios
var_5a <- sapply(fibras_mayor_5a_sf, function(col) col[length(col)] - col[1])   # Variación de precios entre el primer y el ultimo registro
por_var_5a <- round((var_5a / fibras_mayor_5a_sf[1,] * 100), digits = 2)        # Determinamos el porcentaje de variación
# por_var_5a
t_por_var_5a <- as.data.frame(t(por_var_5a))                                    # Trasponemos el dataframe generado
t_por_var_5a$ticker <- row.names(t_por_var_5a)                                  # Copiamos el índice existente (nombre del ticker) a una columna adicional
row_index <- row(t_por_var_5a)                                                  # Generamos un indice con el numero de renglón
tn_por_var_5a <- cbind(row_index, t_por_var_5a)                                 # Combinamos el dataframe de los numeros de indice y el de los porcentajes
rownames(tn_por_var_5a) <- tn_por_var_5a$"1"                                    # Agisnamos los valores de la columna "1" como indice
tn_por_var_5a <- subset(tn_por_var_5a, select = -c(1, 2))                       # Eliminamos las columnas innecesarias
tn_por_var_5a <- tn_por_var_5a[, c("ticker", setdiff(names(tn_por_var_5a), "ticker"))]   # Movemos la columna de ticker a la primera posición
names(tn_por_var_5a)[names(tn_por_var_5a) == "61"] <- "cinco"                   # Renombramos la columna de los resultados
tn_por_var_5a                                                                   # Imprimimos el resultado sin ordenar
maj_ren_5a <- tn_por_var_5a
maj_ren_5a <- maj_ren_5a[order(maj_ren_5a$'cinco', decreasing = TRUE), ]        # Ordenar por mayor rendimiento promedio mensual
rownames(maj_ren_5a) <- NULL                                                    # resetear index
# maj_ren_5a                                                                    # Imprimimos el resultado ordenado

ggplot(tn_por_var_5a, aes(x = cinco, y = ticker)) +                                # Graficamos
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Rendimiento acumulados de las FIBRAS en 5 años",
       x = "Rendimiento acumulado",
       y = "Ticker") +
  theme_minimal()

# Rendimiento promedio mensual
ren_prom_men_5a <- tn_por_var_5a
ren_prom_men_5a$cinco <- round((((1 + (ren_prom_men_5a$cinco/100))^(1/60) - 1)*100), digits = 4)   # Calculamos el rendimiento promedio mensual
# ren_prom_men_5a                                                               # Imprimimos el resultado sin ordenar
rpm_ren_5a <- ren_prom_men_5a
rpm_ren_5a <- rpm_ren_5a[order(rpm_ren_5a$cinco, decreasing = TRUE), ]          # Ordenar por mayor rendimiento promedio mensual
# rpm_ren_5a                                                                    # Imprimimos el resultado ordenado

ggplot(ren_prom_men_5a, aes(x = cinco, y = ticker)) +                                # Graficamos
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Rendimiento promedio mensual de las FIBRAS en 5 años",
       x = "Rendimiento promedio mensual",
       y = "Ticker") +
  theme_minimal()

# Riesgo promedio mensual
rie_fib_5a <- round((apply(fibras_mayor_5a_sf, 2, sd)), digits = 4)             # Obtenemos la desviación estandar(riesgo) de cada columna (FIBRA)
rie_fib_5a = as.data.frame(rie_fib_5a)                                          # Lo convertimos a un dataframe
# rie_fib_5a
# rie_fib_5a <- as.data.frame(t(rie_fib_5a))                                    # Descomentar si es necesario
t_rie_fib_5as = rie_fib_5a                                                      # Dejar como comentario cuando no se requiera
t_rie_fib_5as$ticker <- row.names(t_rie_fib_5as)                                # Copiamos el índice existente (nombre del ticker) a una columna adicional
row_index_trie5a <- row(t_rie_fib_5as)                                          # Generamos un indice con el numero de renglón
t_rie_fib_5as <- cbind(row_index_trie5a, t_rie_fib_5as)                         # Combinamos el dataframe de los numeros de indice y el de los porcentajes
rownames(t_rie_fib_5as) <- t_rie_fib_5as$"1"                                    # Asignamos los valores de la columna "1" como indice
t_rie_fib_5as <- subset(t_rie_fib_5as, select = -c(1, 2))                       # Eliminamos las columnas innecesarias
t_rie_fib_5as <- t_rie_fib_5as[, c("ticker", setdiff(names(t_rie_fib_5as), "ticker"))]    # Movemos la columna de ticker a la primera posición
colnames(t_rie_fib_5as) <- c('ticker', 'cinco')                                # Renombramos las columnas
# t_rie_fib_5as                                                                 # Imprimimos el resultado sin ordenar
men_rie_5a <- t_rie_fib_5as
men_rie_5a <- men_rie_5a[order(men_rie_5a$'cinco', decreasing = FALSE), ]      # Ordenar por menor riesgo promedio mensual
# men_rie_5a                                                                    # Imprimimos el resultado ordenado

ggplot(t_rie_fib_5as, aes(x = cinco, y = ticker)) +                                # Graficamos
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Riesgo promedio mensual de las FIBRAS en 5 años",
       x = "Riesgo promedio mensual",
       y = "Ticker") +
  theme_minimal()

# Unimos los dataframe de rendimientos y riesgos de los últimos 5 años
analyt_5a <- merge(ren_prom_men_5a, t_rie_fib_5as, by ='ticker')                # Unión de dataframes
colnames(analyt_5a) <- c('Ticker', 'Rendimiento', 'Riesgo')                     # Renombramos las columnas
may_ren_5a <- analyt_5a
may_ren_5a <- may_ren_5a[order(may_ren_5a$'Rendimiento', decreasing = TRUE), ]  # ordenar por mayor rendimiento
men_rie_5a <- analyt_5a
men_rie_5a <- men_rie_5a[order(men_rie_5a$'Riesgo', decreasing = FALSE), ]      # Ordenar por menor riesgo


# 2 años

fibras_2a <- tail(fibras_mayor_5a_sf, 25)                                       # Extraemos los registros de los últimos dos años
# fibras_2a

# Variación total de precios
var_2a <- sapply(fibras_2a, function(col) col[length(col)] - col[1])            # Variación de precios entre el primer y el ultimo registro
por_var_2a <- round((var_2a / fibras_2a[1,] * 100), digits = 2)                 # Determinamos el porcentaje de variación
# por_var_2a
t_por_var_2a <- as.data.frame(t(por_var_2a))                                    # Trasponemos el dataframe generado
t_por_var_2a$ticker <- row.names(t_por_var_2a)                                  # Copiamos el índice existente (nombre del ticker) a una columna adicional
row_index <- row(t_por_var_2a)                                                  # Generamos un indice con el numero de renglón
tn_por_var_2a <- cbind(row_index, t_por_var_2a)                                 # Combinamos el dataframe de los numeros de indice y el de los porcentajes
rownames(tn_por_var_2a) <- tn_por_var_2a$"1"                                    # Agisnamos los valores de la columna "1" como indice
tn_por_var_2a <- subset(tn_por_var_2a, select = -c(1, 2))                       # Eliminamos las columnas innecesarias
tn_por_var_2a <- tn_por_var_2a[, c("ticker", setdiff(names(tn_por_var_2a), "ticker"))]   # Movemos la columna de ticker a la primera posición
tn_por_var_2a
names(tn_por_var_2a)[names(tn_por_var_2a) == "97"] <- "dos"                     # Renombramos la columna de los resultados
# tn_por_var_2a                                                                 # Imprimimos el resultado sin ordenar
myr_ren_2a <- tn_por_var_2a
myr_ren_2a <- myr_ren_2a[order(myr_ren_2a$dos, decreasing = TRUE), ]            # Ordenar por mayor rendimiento promedio mensual
# myr_ren_2a                                                                    # Imprimimos el resultado ordenado

ggplot(myr_ren_2a, aes(x = dos, y = ticker)) +                                  # Graficamos
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Rendimiento total de las FIBRAS en 2 años",
       x = "Rendimiento total",
       y = "Ticker") +
  theme_minimal()

# Rendimiento promedio mensual
ren_prom_men_2a <- tn_por_var_2a
ren_prom_men_2a$dos <- round((((1 + (ren_prom_men_2a$dos/100))^(1/24) - 1)*100), digits = 4)   # Calculamos el rendimiento promedio mensual
# ren_prom_men_2a                                                               # Imprimimos el resultado sin ordenar
men_rie_2a <- ren_prom_men_2a
men_rie_2a <- men_rie_2a[order(men_rie_2a$dos, decreasing = TRUE), ]      # Ordenar por mayor rendimiento promedio mensual
men_rie_2a                                                                      # Imprimimos el resultado ordenado

ggplot(ren_prom_men_2a, aes(x = dos, y = ticker)) +                                  # Graficamos
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Rendimiento promedio mensual de las FIBRAS en 2 años",
       x = "Rendimiento promedio mensual",
       y = "Ticker") +
  theme_minimal()

# Riesgo promedio mensual
rie_fib_2a = round((apply(fibras_2a, 2, sd)), digits = 4)
rie_fib_2a = as.data.frame(rie_fib_2a)
# t_rie_fib_2a <- as.data.frame(t(rie_fib_2a))                                  # Trasponemos el dataframe generado
# t_rie_fib_2a <- as.data.frame(t(t_rie_fib_2a))                                # descomentar estas dos líneas si es necesario
t_rie_fib_2a = rie_fib_2a                                                       # Dejar como comentariosi no se requiere
t_rie_fib_2a$ticker <- row.names(t_rie_fib_2a)                                  # Copiamos el índice existente (nombre del ticker) a una columna adicional
row_index_trie2a <- row(t_rie_fib_2a)                                           # Generamos un indice con el numero de renglón
t_rie_fib_2a <- cbind(row_index_trie2a, t_rie_fib_2a)                           # Combinamos el dataframe de los numeros de indice y el de los porcentajes
rownames(t_rie_fib_2a) <- t_rie_fib_2a$"1"                                      # Agisnamos los valores de la columna "1" como indice
t_rie_fib_2a <- subset(t_rie_fib_2a, select = -c(1, 2))                         # Eliminamos las columnas innecesarias
t_rie_fib_2a <- t_rie_fib_2a[, c("ticker", setdiff(names(t_rie_fib_2a), "ticker"))]   # Movemos la columna de ticker a la primera posición
colnames(t_rie_fib_2a) <- c('ticker', 'dos')                                    # Renombramos las columnas
# t_rie_fib_2a                                                                  # Imprimimos el resultado sin ordenar
men_rie_2a <- t_rie_fib_2a
men_rie_2a <- men_rie_2a[order(men_rie_2a$dos, decreasing = FALSE), ]      # Ordenar por menor riesgo promedio mensual
men_rie_2a                                                                      # Imprimimos el resultado ordenado

ggplot(men_rie_2a, aes(x = dos, y = ticker)) +                                  # Graficamos
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Riesgo promedio mensual de las FIBRAS en 2 años",
       x = "Riesgo promedio mensual",
       y = "Ticker") +
  theme_minimal()

# Unimos los dataframe de rendimientos y riesgos de los últimos dos años
analyt_2a <- merge(ren_prom_men_2a, t_rie_fib_2a, by ='ticker')                 # Unión de dataframes
colnames(analyt_2a) <- c('Ticker', 'Rendimiento', 'Riesgo')                     # Renombramos las columnas

may_ren_2a <- analyt_2a
may_ren_2a <- may_ren_2a[order(may_ren_2a$'Rendimiento', decreasing = TRUE), ]  # ordenar por mayor rendimiento
men_rie_2a <- analyt_2a
men_rie_2a <- men_rie_2a[order(men_rie_2a$'Riesgo', decreasing = FALSE), ]      # Ordenar por menor riesgo


# Comparativas

# Rendimientos acumulados en 5 y 2 años 
ren_fib_52 <- merge(tn_por_var_5a, tn_por_var_2a, by = 'ticker')                # Unión de los dataframes tn_por_var_5a, tn_por_var_2a
mej_ren_5a <- ren_fib_52
mej_ren_5a <- mej_ren_5a[order(mej_ren_5a$cinco, decreasing = TRUE), ]
mej_ren_5a                                                                      # Mejores rendimientos en 5 años
mej_ren_2a <- ren_fib_52
mej_ren_2a <- mej_ren_2a[order(mej_ren_2a$dos, decreasing = TRUE), ]
mej_ren_2a                                                                      # Mejores rendimientos en 2 años
# Rendimientos promedio mensuales en 5 y 2 años
rpm_fib_52 <- merge(ren_prom_men_5a, ren_prom_men_2a, by = 'ticker')            # Unión de los dataframes ren_prom_men_5a, ren_prom_men_2a
mej_rpm_5a <- rpm_fib_52
mej_rpm_5a <- mej_rpm_5a[order(mej_rpm_5a$cinco, decreasing = TRUE), ]
mej_rpm_5a                                                                      # Mejores rendimientos en 5 años 
mej_rpm_2a <- rpm_fib_52
mej_rpm_2a <- mej_rpm_2a[order(mej_rpm_2a$dos, decreasing = TRUE), ]
mej_rpm_2a                                                                      # Mejores rendimientos en 2 años
# Riesgos promedio mensuales en 5 y 2 años
rie_fib_52 <- merge(t_rie_fib_5as, t_rie_fib_2a, by = 'ticker')                 # Unión de los dataframes t_rie_fib_5as, t_rie_fib_2a
men_rie_5a <- rie_fib_52
men_rie_5a <- men_rie_5a[order(men_rie_5a$cinco, decreasing = FALSE), ]
men_rie_5a                                                                      # Menor riesgo promedio mensual en 5 años 
men_rie_2a <- rie_fib_52
men_rie_2a <- men_rie_2a[order(men_rie_2a$dos, decreasing = FALSE), ]
men_rie_2a                                                                      # Menor riesgo promedio mensual en 2 años 

# Determinamos las fibras con mayor rendimiento y menor riesgo en 5 y 2 años
# Hacemos los cálculos
indice_may_ren_5a <- which.max(ren_fib_52$cinco)     # Con mas rendimiento en 5 años
fila_may_ren_5a <- ren_fib_52[indice_may_ren_5a, ]
indice_may_ren_2a <- which.max(ren_fib_52$dos)     # Con mas rendimiento en 2 años
fila_may_ren_2a <- ren_fib_52[indice_may_ren_2a, ]
indice_men_rie_5a <- which.min(rie_fib_52$cinco)     # Con menos riesgo promedio en 5 años
fila_men_rie_5a <- rie_fib_52[indice_men_rie_5a, ]
indice_men_rie_2a <- which.min(rie_fib_52$dos)     # Con menos rieso promedio en 2 años
fila_men_rie_2a <- rie_fib_52[indice_men_rie_2a, ]
# Imprimimos los resultados finales
print("Fibra com más rendimientos en 5 años")
print(fila_may_ren_5a)
print("Fibra com más rendimientos en 2 años")
print(fila_may_ren_2a)
print("Fibra con menos riesgo promedio mensual en 5 años")
print(fila_men_rie_5a)
print("Fibra con menos riesgo promedio mensual en 2 años")
print(fila_men_rie_2a)

# ----- Si queda mas tiempo, realizar las de 3 y 1 año. -----


# MEDIDAS DE TENDENCIA DE LAS FIBRAS del dataframe fibras_mayor_5a_sf

# Determinamos las medidas de tendencia
# Costo promedio - Trasponer después
cost_prom_5a <- round((colMeans(fibras_mayor_5a_sf)), digits = 4)   

# Mediana en el costo - Trasponer después
mediana_cost_5a <- sapply(fibras_mayor_5a_sf, function(col) if(is.numeric(col)) median(col, na.rm = TRUE) else NA)  

# La moda en los costos de los tickers - Trasponer después
# Función para calcular la moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda_cost_5a <- sapply(fibras_mayor_5a_sf, moda)

mtc_fibras <- rbind(cost_prom_5a, mediana_cost_5a, moda_cost_5a)
mtc_fibras

t_mtc_fibras <- as.data.frame(t(mtc_fibras))                                        # Trasponemos el dataframe generado
t_mtc_fibras$ticker <- row.names(t_mtc_fibras)                                      # Copiamos el índice existente (nombre del ticker) a una columna adicional
row_index_tmtc <- row(t_mtc_fibras)                                                 # Generamos un indice con el numero de renglón
t_mtc_fibras <- cbind(row_index_tmtc, t_mtc_fibras)                                 # Combinamos el dataframe de los numeros de indice y el de los porcentajes
rownames(t_mtc_fibras) <- t_mtc_fibras$"1"                                          # Agisnamos los valores de la columna "1" como indice
t_mtc_fibras <- subset(t_mtc_fibras, select = -c(1, 2, 3, 4))                       # Eliminamos las columnas innecesarias
t_mtc_fibras <- t_mtc_fibras[, c("ticker", setdiff(names(t_mtc_fibras), "ticker"))] # Movemos la columna de ticker a la primera posición
colnames(t_mtc_fibras) <- c('Ticker', 'Costo promedio', 'Mediana', 'Moda')          # Renombramos las columnas

# Imprimimos las medidas de tendencia
print("Medidas de tendencia de las FIBRAS los últimos 5 años")
print(t_mtc_fibras)



############################################################################################################
# CARACTERISTICAS DEL DATAFRAME fibras_mayor_5a_sf

# str(fibras_mayor_5a_sf)
print("Resumen del dataset de FIBRASV")
summary(fibras_mayor_5a_sf)

# head(fibras_mayor_5a_sf)
View(fibras_mayor_5a_sf)      # Visualizar el dataframe fibras_mayor_5a_sf


# GRÁFICOS

# fibras_mayor_5a
# names(fibras_mayor_5a)
fibras_mayor_5a_sf
# names(fibras_mayor_5a_sf)                   # para visualizar el nombre de las columnas

# Gráfico de la Fibra FNOVA17

fibras_mayor_5a_sf$period <- 1:nrow(fibras_mayor_5a_sf)                         # Se le agrega un índice graficable

ggplot(fibras_mayor_5a_sf, aes(x=period, y=FNOVA17)) +                          # FNOVA17
  geom_line() +                                       # Línea de gráfico
  geom_point()                                        # Tipo de geometría,
  theme_gray() +                                      # Temas (inteta cambiarlo)
  labs(title = "Evolución del precio de FNOVA17",
    x = "Mes",
    y = "Precio") +
  theme_minimal()

  
ggplot(fibras_mayor_5a_sf, aes(x=period, y=FINN13)) +                           # FINN13
  geom_line() +                                       # Línea de gráfico
  geom_point()                                        # Tipo de geometría,
  theme_gray() +                                      # Temas (inteta cambiarlo)
  labs(title = "Evolución del precio de FINN13",
    x = "Mes",
    y = "Precio") +
  theme_minimal()
  

# CORRELACIONES Y MAPAS DE CALOR
corre_fibras <- cor(fibras_mayor_5a_sf)       # Generamos las correlaciones
# corre_fibras                                # Correlaciones (solo números)
corr_plot <- corrplot(corre_fibras, method = "circle")     # Visaulizamos la matriz de correlación
corr_plot
# plot(corre_fibras)

cor_melt <- melt(corre_fibras)                # Transformar la matriz de correlación a un formato largo
# cor_melt

# Crear el mapa de calor con ggplot2
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlación") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  labs(title = "Mapa de Calor de Correlaciones",
       x = "Fibras",
       y = "Fibras")

# ANÁLISIS DE UNA FIBRA Y EL TIEMPO Y DE DOS FIBRAS CORRELACIONADAS CON LÍNEAS DE TENDENCIA

(my_scatplot <- ggplot(fibras_mayor_5a_sf, aes(x = period, y = FNOVA17)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)) +
  xlab("period") + ylab("FNOVA17")

(my_scatplot <- ggplot(fibras_mayor_5a_sf, aes(x = FCFE18, y = FNOVA17)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)) +
  xlab("FCFE18") + ylab("FNOVA17")

(my_scatplot <- ggplot(fibras_mayor_5a_sf, aes(x = FIBRAPL14, y = FNOVA17)) +
  geom_point() +
  geom_smooth(method = "lm", se = T)) +
  xlab("FIBRAPL14") + ylab("FNOVA17")

############################################################################################################
# REGRESIÓN LINEAL Y CLASIFICACIÓN

# Regresión linea - Se ejemplificará con FNOVA17

attach(fibras_mayor_5a_sf)                                          # Definimos el dataframe con el que trabajaremos

plot(period, FNOVA17, xlab = "Meses", ylab = "FNOVA17", pch = 16)   # Hacemos una grafica SENCILLA de dispersion previa

# Creación de nuestro modelo
evo_FNOVA17 <- lm(FNOVA17~period)                                   # Ajustamos un modelo de regresión lineal simple
summary(evo_FNOVA17)                                                # Obtenemos un resumen de nuestro modelo

################ PENDIENTE ##################

# Gráfica con la recta de regresión estimada, con algunas ecuaciones y residuales gráficamente - Pendiente de personalización

plot(period, FNOVA17, xlab = "Meses", ylab = "FNOVA17", pch = 16)   # Gráfica de dispersión
abline(lsfit(period, FNOVA17))                                      # Trazamos la recta de regresión estimada
# Pendiente de personalización
mtext(expression(paste('Modelo de regresión lineal simple:',
                       ' ',
                       y[i] == beta[0] + beta[1]*x[i] + e[i])),
      side = 3, adj=1, font = 2)

# Recta de regresión
text(x = 25, y =30, expression(paste('Recta de regresión:',
                                        ' ',
                                        y[i] == beta[0] + beta[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada
text(x = 63, y = 23, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == hat(beta)[0] + hat(beta)[1]*x[i])),
     adj = 1, font = 2)

# Recta de regresión estimada
text(x = 62, y = 21, expression(paste('Recta estimada:',
                                        ' ',
                                        hat(y)[i] == 149.74770 + 0.25924*x[i])),
     adj = 1, font = 2)

# Residuales
points(189, 215, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 189 # Valor y sobre la recta estimada
lines(c(189, 189), c(198.7441, 215), col = "red")

points(173, 166, pch=16, col = "red") # Punto muestral
149.74770 + 0.25924 * 173 # Valor y sobre la recta estimada
lines(c(173, 173), c(166, 194.5962), col = "red")

##############################

# Análisis de varianza
anova(evo_FNOVA17)

# Diagnósticos de regresión
par(mfrow = c(2,2))
plot(evo_FNOVA17)
# dev.off()


# Regresión lineal múltiple

evo_FNOVA_PL_MQ <- lm(period~FNOVA17 + FIBRAPL14 + FIBRAMQ12)     # Ajustamos el modelo con tres FIBRAS
summary(evo_FNOVA_PL_MQ)                                          # Obtenemos el resumen

# Análisis de covarianza con relación a FUNO11

Mfull <- lm(period~FNOVA17 + FIBRAPL14 + FIBRAMQ12 + FUNO11 + FNOVA17:FUNO11 + FIBRAPL14:FUNO11 + FIBRAMQ12:FUNO11)
summary(Mfull)

anova(evo_FNOVA_PL_MQ, Mfull)

# Graficamos el modelo 1
pairs(~FNOVA17 + FIBRAPL14 + FIBRAMQ12, data = fibras_mayor_5a_sf, gap = 0.4, cex.labels = 1.5)

# Gráficas de residuales estandarizados contra cada predictor.
evo_FNOVA_PL_MQ <- lm(period~FNOVA17 + FIBRAPL14 + FIBRAMQ12)
summary(evo_FNOVA_PL_MQ)
StanRes1 <- rstandard(evo_FNOVA_PL_MQ)
par(mfrow = c(2, 2))
plot(FNOVA17, StanRes1, ylab = "Residuales Estandarizados")
plot(FIBRAPL14, StanRes1, ylab = "Residuales Estandarizados")
plot(FIBRAMQ12, StanRes1, ylab = "Residuales Estandarizados")
# dev.off()

############################################################################################################
# SERIES DE TIEMPO

# fibras_may_5a_sf = fibras_mayor_5a_sf
# class (fibras_may_5a_sf)
class(fibras_mayor_5a_sf)
# start(fibras_may_5a_sf); end(fibras_may_5a_sf); frequency(fibras_may_5a_sf)
# summary(fibras_may_5a_sf)
summary(fibras_mayor_5a_sf)

# Extraemos los datos de una FIBRA
FNOVA17_5a <- fibras_ult_5a[, c('Fecha', 'FNOVA17')]                    # Extraemos los datos de FNOVA17
# FNOVA17_5a                                                            # Verificamos el dataframe extraido, asi como sus fechas
FNOVA17_5a_ts <- ts(FNOVA17_5a$FNOVA17, start=c(2019,5), frequency =12) # Convertimos la cilmna d elos precios de FNOVA 17 a una serie temporal
# FNOVA17_5a_ts                                                           # Revisamos la serie temporal
class(FNOVA17_5a_ts)                                                      # Confirmamos su clase
summary(FNOVA17_5a_ts)

plot(FNOVA17_5a_ts, ylab = "Precio", xlab="Tiempo",
    main="Evolución del precio los ultimos 5 años",
    sub="FIBRA FNOVA17_5a")


layout(1:2)
plot(aggregate(FNOVA17_5a_ts), xlab = "Tiempo",
     main = "Evolución del precio los ultimos 5 años",
     sub = "FIBRA FNOVA17_5a")

boxplot(FNOVA17_5a_ts ~ cycle(FNOVA17_5a_ts),
        xlab = "Tiempo",
        sub="FIBRA FNOVA17_5a",
        main="Evolución del precio los ultimos 5 años")
dev.off()


# Series de tiempo multiple

# FNOVA17_5a_ts                                                                 # FNOVA17
# fibras_ult_5a
FIBRAPL14_5a <- fibras_ult_5a[, c('Fecha', 'FIBRAPL14')]                        # FIBRAPL14
FIBRAPL14_5a_ts <- ts(FIBRAPL14_5a$FIBRAPL14, start=c(2019,5), frequency =12)
# FIBRAPL14_5a_ts
FIBRAMQ12_5a <- fibras_ult_5a[, c('Fecha', 'FIBRAMQ12')]                        # FIBRAMQ12
FIBRAMQ12_5a_ts <- ts(FIBRAMQ12_5a$FIBRAMQ12, start=c(2019,5), frequency =12)
# FIBRAMQ12_5a_ts
FUNO11_5a <- fibras_ult_5a[, c('Fecha', 'FUNO11')]                              # FUNO11
FUNO11_5a_ts <- ts(FUNO11_5a$FUNO11, start=c(2019,5), frequency =12)
# FUNO11_5a_ts

plot(cbind(FNOVA17_5a_ts, FIBRAPL14_5a_ts, FIBRAMQ12_5a_ts, FUNO11_5a_ts), 
     main = "Precios de FNOVA17, FIBRAPL14, FIBRAMQ12 y FUNO11", 
     xlab = "Tiempo",
     sub = "Mayo 2019 - Mayo 2024")

# Serie de precios globales, expresadas como anomalías de las medias mensuales
# FNOVA17_5a_ts
Global.annual <- aggregate(FNOVA17_5a_ts, FUN = mean)
plot(FNOVA17_5a_ts, xlab = "Tiempo", ylab = "Precios", main = "Serie de precios global",
     sub = "Serie mensual: Mayo 2019 a Mayo 2024")
plot(Global.annual, xlab = "Tiempo", ylab = "Precios", main = "Serie de precios Global",
     sub = "Serie anual de precios medios: 2019 a 2024")

# Serie de precios globales con línea de tendencia
New.series <- window(FNOVA17_5a_ts, start = c(2019, 5), end = c(2024, 5)) 
New.time <- time(New.series)
plot(New.series, xlab = "Tiempo", ylab = "Precios", main = "Serie de precios Global de FNOVA17",
     sub = "Serie mensual: Mayo 2019 a Mayo 2024"); abline(reg = lm(New.series ~ New.time))

# Descomposición de series
Elec.decom.A <- decompose(FNOVA17_5a_ts)                          # Modelo Aditivo
plot(Elec.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de precios de FNOVA17")
# Componentes
Tendencia <- Elec.decom.A$trend
Estacionalidad <- Elec.decom.A$seasonal
Aleatorio <- Elec.decom.A$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de Precios de FNOVA17", 
        ylab = "Precio", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

Tendencia[20] + Estacionalidad[20] + Aleatorio[20]
FNOVA17_5a_ts[20]

# Modelo AR(4) ajustado a la serie de precios globales mensuales
mean(FNOVA17_5a_ts)
Global.ar <- ar(FNOVA17_5a_ts, method = "mle")
Global.ar$order
Global.ar$ar
acf(Global.ar$res[-(1:Global.ar$order)], lag = 60, main = "")
title(main = "Correlograma de la serie de residuales",
      sub = "Modelo AR(4) ajustado a la serie de precios globales mensuales")


# Predicción - sESIÓN 6, EJEMPLO 2, LINEA 216

# FNOVA17_5a_ts
plot(FNOVA17_5a_ts, xlab = "", ylab = "")                 # Grafica de FNOVA17
title(main = "Serie de precio de FNOVA17",
      xlab = "Tiempo",
      ylab = "Precio")

plot(log(FNOVA17_5a_ts), xlab = "", ylab = "")            # Log de lso precios de FNOVA17
title(main = "Serie-log de precios de FNOVA17",
      xlab = "Tiempo",
      ylab = "Log de Precio de FNOVA17")

# Obtenemos el lm
Time <- 1:length(FNOVA17_5a_ts)
Imth <- cycle(FNOVA17_5a_ts)
FNOVA17_lm <- lm(log(FNOVA17_5a_ts) ~ Time + I(Time^2) + factor(Imth))

# Visualizamos el Correlograma de la serie de residuales del modelo de regresión
acf(resid(FNOVA17_lm), main = "")
title(main = "Correlograma de la serie de residuales del modelo de regresión",
      sub = "Serie de precios de FNOVA17")

plot(resid(FNOVA17_lm), type = "l", main = "", xlab = "", ylab = "")
title(main = "Serie de residuales del modelo de regresión ajustado",
      sub = "Serie de precios de FNOVA17",
      xlab = "Tiempo",
      ylab = "Residuales")                                

# Un poco pesimista este  modelo, e inexacto porque los residuales aún quedarán autocorrelaciones 
# estadísticamente diferentes de cero.

# Código para encontrar un modelo ARMA(p, q) considerando el AIC (Akaike Information Criterion).
# El ajuste se lleva a cabo para los residuales del ajuste anterior.

best.order <- c(0, 0, 0)
best.aic <- Inf
for(i in 0:2)for(j in 0:2){
  model <- arima(resid(FNOVA17_lm), order = c(i, 0, j))
  fit.aic <- AIC(model)
  if(fit.aic < best.aic){
    best.order <- c(i, 0, j)
    best.arma <- arima(resid(FNOVA17_lm), order = best.order)
    best.aic <- fit.aic
  }
}

best.order

acf(resid(best.arma), main = "")
title(main = "Serie de residuales del modelo ARMA(2, 0) ajustado",
      sub = "Serie de residuales del modelo de regresión ajustado a los datos de FNOVA17")

#### Las siguientes predicciones aún pueden ser mejoradas con un modelo "más adecuado"

new.time <- seq(length(FNOVA17_5a_ts)+1, length = 24)
new.data <- data.frame(Time = new.time, Imth = rep(1:12, 2))
predict.lm <- predict(FNOVA17_lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 24)
FNOVA17_pred <- ts(exp(predict.lm + predict.arma$pred), start = 2024.5, freq = 12)

# Gráfico de predicción                                                         # Predicción a 24 meses
ts.plot(cbind(FNOVA17_5a_ts, FNOVA17_pred), lty = 1:2,
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Producción de electricidad",
        main = "Predicción de los datos de precios de FNOVA17",
        sub = "Predicción de 24 meses")

# Gráfico de serie diferenciada de precios de FNOVA
plot(diff(FNOVA17_5a_ts), xlab = "", ylab = "")
title(main = "Serie Diferenciada de Precio de FNOVA",
      xlab = "Tiempo", ylab = "Dif Serie",
      sub = "Gráfica de la serie diferenciada de primer órden")

#Gráfica de la serie log-transformada diferenciada de primer órden
plot(diff(log(FNOVA17_5a_ts)), xlab = "", ylab = "")
title(main = "Serie de log dif de precios de FNOVA",
      xlab = "Tiempo", ylab = "Dif log-Serie",
      sub = "Gráfica de la serie log-transformada diferenciada de primer órden")

# Simulación con la función arima.sim
FNOVA17_ima <- arima(FNOVA17_5a_ts, order = c(0, 1, 1))
FNOVA17_ima

# Autocorrelaciones para los Residuales del Ajuste
acf(resid(FNOVA17_ima), main = "")
title(main = "Autocorrelaciones para los Residuales del Ajuste",
      sub = expression(x[t]==x[t-1]+w[t]-0.33*w[t-1]))

FNOVA17_2025 <- predict(FNOVA17_ima, n.ahead = 12)
sum(FNOVA17_2025$pred)

# Procedimiento de ajuste
FNOVA17_AR <- arima(log(FNOVA17_5a_ts), order = c(1, 1, 0), 
                 seas = list(order = c(1, 0, 0), 12))
FNOVA17_MA <- arima(log(FNOVA17_5a_ts), order = c(0, 1, 1),
                 seas = list(order = c(0, 0, 1), 12))
AIC(FNOVA17_AR)
AIC(FNOVA17_MA)

# simulamos datos de un modelo ARIMA(1, 1, 1)
set.seed(1)
x <- w <- rnorm(60)
for(i in 3:60) x[i] <- 0.5*x[i-1] + x[i-1] - 0.5*x[i-2] + w[i] + 0.3*w[i-1]

###

plot(x, type = "l", 
     main = "Serie simulada de un modelo ARIMA(1, 1, 1)",
     xlab = "Tiempo",
     ylab = expression(x[t]),
     sub = expression(x[t] == 0.5*x[t-1] + x[t-1] - 0.5*x[t-2] + w[t] + 0.3*w[t-1]))

arima(x, order = c(1, 1, 1))

# Simulación con la función arima.sim
x <- arima.sim(model = list(order = c(1, 1, 1), ar = 0.5, ma = 0.3), n = 1000)
arima(x, order = c(1, 1, 1))


# Función para buscar un "buen" modelo (no basarse únicamente en los resultados de aplicar la función)
get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)){
  best.aic <- 1e8
  n <- length(x.ts)
  for(p in 0:maxord[1])for(d in 0:maxord[2])for(q in 0:maxord[3])
    for(P in 0:maxord[4])for(D in 0:maxord[5])for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p, d, q),
                   seas = list(order = c(P, D, Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2*fit$loglik + (log(n) + 1)*length(fit$coef)
      if(fit.aic < best.aic){
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p, d, q, P, D, Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

FNOVA17_best_arima <- get.best.arima(FNOVA17_5a_ts),
                                  maxord = c(2, 2, 2, 2, 2, 2))

FNOVA17_best_fit <- FNOVA17_best_arima[[2]]     # Modelo
best.arima.elec[[3]]                            # Tipo de modelo (órdenes)
FNOVA17_best_fit
FNOVA17_best_arima[[1]]                         # AIC

# Pendiente por corregir
################################################


############################################################################################################


############################################################################################################

  
  
  










