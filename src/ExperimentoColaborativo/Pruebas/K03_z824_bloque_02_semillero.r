# Acá evaluar y escribir recomendación de recursos

# Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")

#-----------------------------------CONFIGURAR PARÁMETROS-------------------------------------------#
# Defino los parametros de la corrida, en una lista, la variable global  PARAM
PARAM <- list()

# Nombre del experimento
PARAM$experimento <- "HT824_EC02_semillero" #Random seeds

# Path donde se aloja el dataset
PARAM$input$dataset <- "./datasets/competencia_03_baseline.csv.gz"

# Meses donde se entrena el modelo
PARAM$input$training <- c(202012, 202101, 202102, 202103, 202104, 202105)

# Mes donde aplico el modelo
PARAM$input$future <- c(202107)

# Defino parámetros fijos obtenidos en la Optimización Bayesiana 
# Parámetro variable
semillas <- c(
  777787, 274837, 874807, 674831, 974821,
  197599, 769207, 269333, 228337, 481849,
  390113, 346369, 367873, 722081, 229267
) # cambiar semillas, (760097, 552207, 339383, 779399, 680047)


# Parámetros fijos
PARAM$finalmodel$num_iterations <- 644
PARAM$finalmodel$learning_rate <- 0.0374189805229533
PARAM$finalmodel$feature_fraction <- 0.830155881111937
PARAM$finalmodel$min_data_in_leaf <- 32688
PARAM$finalmodel$num_leaves <- 908
PARAM$finalmodel$max_bin <- 31

#---------------------------------CARGAR DATOS---------------------------------------------#
# Aqui empieza el programa que voy a ejecutar para cada semilla
# Directorio de origen
setwd("~/buckets/b1/")

# Cargo el conjunto de datos
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# Configuro la variable target como binaria
# El criterio: POS = { BAJA+1, BAJA+2 }, NEG {CONTINUA}
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#-----------------------------------SELECCIONAR DATOS-------------------------------------------#
# Campos a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

# Establezco qué datos usaré para entrenar
# Creo columna train con valor cero en todas sus filas
dataset[, train := 0L]

# Asigno un 1 a todas las filas correspondiente al foto_mes configurado en los parámetros de entrada
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#---------------------------------CREAR DIRECTORIOS---------------------------------------------#
# Creo carpeta donde guardar los experimentos en caso de que no exista
dir.create("./exp/", showWarnings = FALSE)

# Creo carpeta donde guardar este experimento en caso de que no exista
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory de este experimento
setwd(paste0("./exp/", PARAM$experimento, "/"))


#----------------------------------CONFIGURAR DATOS DE ENTRADA----------------------------------#
# Dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#----------------------------------ITERACIÓN----------------------------------#

# Obtengo los datos a predecir
dapply <- dataset[foto_mes == PARAM$input$future]

# Selecciono columna con numero de cliente y foto mes en df para guardar las predicciones
predicciones <- dapply[, list(numero_de_cliente, foto_mes)]

for (semilla in semillas) {
  #----------------------------------CONFIGURAR MODELO--------------------------------------------#
  # Utilizo los parámetros configurados al inicio para el modelo

  modelo <- lgb.train(
  data = dtrain,
  param = list(
      objective = "binary",
      max_bin = PARAM$finalmodel$max_bin,
      learning_rate = PARAM$finalmodel$learning_rate,
      num_iterations = PARAM$finalmodel$num_iterations,
      num_leaves = PARAM$finalmodel$num_leaves,
      min_data_in_leaf = PARAM$finalmodel$min_data_in_leaf,
      feature_fraction = PARAM$finalmodel$feature_fraction,
      seed = semilla 
  )
  )

  #----------------------------------PERSISTIR IMPORTANCIA DE VARIABLES---------------------------------#

  # Calculo la importancia de variables del modelo
  tb_importancia <- as.data.table(lgb.importance(modelo))

  # Configuro nombre del archivo
  archivo_importancia <- paste0("impo_", semilla, ".txt")

  # Guardo en el archivo 
  fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
  )

  #----------------------------------PREDECIR SOBRE MES DE INTERÉS---------------------------------#
  # Aplico el modelo a los nuevos datos
  prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # Agrego columna con las predicciones de cada semilla
  col_name <- paste0("semilla_", semilla)
  predicciones[, (col_name) := prediccion] 
}

#-------------------------------PERSISTO SALIDA CON LAS PREDICCIONES DE CADA SEMILLA------------------------------#

# Guardo el archivo
archivo_salida <- paste0(PARAM$experimento, "_predicciones_semillas.csv")
fwrite(predicciones, file = archivo_salida, sep = ",")