# Este script genera clustering en bajas y los graficos de las variables

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")
require("ggplot2")
require("RColorBrewer")
require("progress")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$dataset_clusters <- "./exp/RF-CLUST0001/RF-CLUST0001_competencia_02_b2_clusters.csv.gz"
PARAM$plot_output <- "competencia_02_b2_clusters_variables.pdf"
PARAM$experimento <- "RF-CLUST0002"
PARAM$undersampling <- 0.1
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("~/buckets/b1/")
set.seed(501593)

print("Cargando dataset")
# cargo el dataset
dataset <- fread(PARAM$dataset) 
dataset_clusters <- fread(PARAM$dataset_clusters)

print("Haciendo transformaciones")
dataset[dataset_clusters, on = "numero_de_cliente", cluster := i.cluster]

# clientes unicos
unique_continua_clients <- unique(dataset[is.na(cluster), ], by = "numero_de_cliente")
unique_continua_clients[, azar := runif(nrow(unique_continua_clients)),]

dataset[
  unique_continua_clients,
  on = "numero_de_cliente",
  azar := i.azar
]
dataset[
  is.na(cluster) &
    (azar <= PARAM$undersampling),
  cluster := -1L
]
dataset <- dataset[!is.na(cluster), ]

# ordeno el dataset
dataset <- dataset[order(numero_de_cliente, foto_mes), ]

# obtengo la fecha de ultima foto para armar la columna lag
dataset[, last_seen := max(foto_mes), by = numero_de_cliente]

dataset[, lag :=
          (as.integer(foto_mes / 100) - as.integer(last_seen / 100)) * 12 +
          (as.integer(foto_mes) %% 100) - (as.integer(last_seen) %% 100), ]


# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria", "cluster")
)

# Use the RColorBrewer package to choose a palette
len_clusters <- length(unique(dataset$cluster))
dataset[, cluster_name := ifelse(cluster == -1, "CONTINUA", as.character(cluster))]

print("Creando plots")
pb <- progress_bar$new(
  format = "  creando plots [:bar] :percent eta: :eta",
  total = length(campos_buenos), clear = FALSE, width = 60)

options(warn = -1)
print("Cluster sizes")
print(dataset[, .(cluster_name, count = .N), by = cluster_name])

pdf(PARAM$plot_output)
for (campo in campos_buenos) {
  plt <- ggplot(dataset, aes(x = lag, !!sym(campo), group = cluster_name, colour = cluster_name,)) +
    geom_smooth() +
    ggtitle(paste(campo, " by cluster")) +
    scale_fill_brewer(name = "Cluster", palette="Dark2")
  print(plt)
  pb$tick()
}
dev.off()