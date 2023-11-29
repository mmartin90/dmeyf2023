# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA8240_fe_reduc_semill"

setwd("~/buckets/b1")

PARAM$input$dataset <- "./datasets/competencia_03_reduc.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(
  202012, 202101, 202102, 202103, 202104, 202105
)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

semillas <- c(
  777787, 274837, 874807, 674831, 974821,
  197599, 769207, 269333, 228337, 481849,
  390113, 346369, 367873, 722081, 229267,
  760097, 552207, 339383, 779399, 680047
)

# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# Selecciono columna con numero de cliente y foto mes en df para guardar las predicciones
predicciones <- dapply[, list(numero_de_cliente, foto_mes)]

# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo


# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


for (semilla in semillas) {

  PARAM$finalmodel$semilla <- semilla

  # hiperparametros intencionalmente NO optimos
  PARAM$finalmodel$num_iterations <- 209
  PARAM$finalmodel$learning_rate <- 0.142209129117921
  PARAM$finalmodel$feature_fraction <- 0.802505482533752
  PARAM$finalmodel$min_data_in_leaf <- 35295
  PARAM$finalmodel$num_leaves <- 1021

  # Hiperparametros FIJOS de  lightgbm
  PARAM$finalmodel$lgb_basicos <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO

    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0

    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

    extra_trees = TRUE, # Magic Sauce

    seed = PARAM$finalmodel$semilla
  )



  # dejo los datos en el formato que necesita LightGBM
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
    label = dataset[train == 1L, clase01]
  )


  # genero el modelo
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
    PARAM$finalmodel$optim)

  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <- "impo.txt"

  fwrite(tb_importancia,
    file = archivo_importancia,
    sep = "\t"
  )

  #--------------------------------------

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
 
  # Agrego columna con las predicciones de cada semilla
  col_name <- paste0("semilla_", semilla)
  predicciones[, (col_name) := prediccion] 
  cat("\n\nSemilla número", semilla , "hora:", Sys.time(), "\n")
 
}

archivo_salida <- paste0(PARAM$experimento, "_predicciones_semillas.csv")
fwrite(predicciones, file = archivo_salida, sep = ",")

#-----------------------------------------------GENERO ENSEMBLE---------------------------------------------------#

# Calcular el promedio de las predicciones (probas) de los 100 modelos ejecutados (excluyo cols "numero_de_cliente" y "foto_mes")
predicciones$proba_ensemble <- rowMeans(predicciones[, .SD, .SDcols = -(1:2)])
setorder(predicciones, -proba_ensemble)


# Genero archivos variando la cantidad de estímulos
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  predicciones[, Predicted := 0L]
  predicciones[1:envios, Predicted := 1L]

  fwrite(predicciones[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
