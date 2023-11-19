# # Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")
require(ggplot2)

#-----------------------------------CARGO DATOS DE GANANCIAS----------------------------------#
setwd("G:/Mi unidad/Maestria Data Mining/2023_DMEyF/exp/HT823_EC")

bloque_01 <- fread("./exp_HT824_EC01_ganancia_HT824_EC01_ganancia_ganancias_semillerio.csv")
bloque_02 <- fread("./exp_HT824_EC02_ganancia_HT824_EC02_ganancia_ganancias_semillerio.csv")
bloque_03 <- fread("./exp_HT824_EC03_ganancia_HT824_EC03_ganancia_ganancias_semillerio.csv")
Prom_bloques <- fread("./exp_HT824_EC04_ganancia_HT824_EC04_ganancia_ganancias_semillerio.csv")

# Gráfico de densidad
density_plot <- ggplot() +
  geom_density(data = bloque_01, aes(x = ganancia, fill = "bloque_01"), alpha = 0.2) +
  geom_density(data = bloque_02, aes(x = ganancia, fill = "bloque_02"), alpha = 0.2) +
  geom_density(data = bloque_03, aes(x = ganancia, fill = "bloque_03"), alpha = 0.2) +
  geom_density(data = Prom_bloques, aes(x = ganancia, fill = "Prom_bloques"), alpha = 0.2) +
  labs(title = "Distribución de Ganancias",
       x = "Ganancia",
       y = "Densidad") +
  theme_minimal() +
  scale_color_manual(values = c("bloque_01" = "blue", 
                                "bloque_02" = "green",
                                "bloque_03" = "red",
                                "Prom_bloques" = "yellow"))

ggsave("distribucion_densidad_vs.png", density_plot, width = 8, height = 5, units = "in", bg = "white")
print(density_plot)

# Gráfico de densidad
density_plot_2 <- ggplot() +
  geom_density(data = bloque_01, aes(x = ganancia, fill = "bloque_01"), alpha = 0.2) +
  geom_density(data = bloque_02, aes(x = ganancia, fill = "bloque_02"), alpha = 0.2) +
  labs(title = "Distribución de Ganancias",
       x = "Ganancia",
       y = "Densidad") +
  theme_minimal() +
  scale_color_manual(values = c("bloque_01" = "blue", 
                                "bloque_02" = "green"))
print(density_plot_2)


library(tidyverse)

bloque_01 <- bloque_01 %>% rename(bloque_01 = ganancia)
bloque_02 <- bloque_02 %>% rename(bloque_02 = ganancia)
bloque_03 <- bloque_03 %>% rename(bloque_03 = ganancia)
Prom_bloques <- Prom_bloques %>% rename(Prom_bloques = ganancia)

tabla <- bloque_01 %>% 
  full_join(bloque_02) %>% 
  full_join(bloque_03) %>% 
  full_join(Prom_bloques) %>% 
  #filter(!is.na(baseline) & !is.na(bagging) & !is.na(goss)) %>% 
  pivot_longer(cols = c(bloque_01,bloque_02,bloque_03,Prom_bloques),
               names_to = "modelo",
               values_to = "ganancia") %>% 
  mutate(ganancia = ganancia/1000000)

summary <- tabla %>% 
  group_by(modelo,envios) %>% 
  summarize(media = mean(ganancia),
            sd = sd(ganancia),
            median = median(ganancia)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(n = 20)

### Grafico curvas por envios

graf_envios <- ggplot(tabla, aes(x = envios, y = ganancia, colour = modelo)) +
  geom_smooth() + 
  labs(title = "Evolución de la ganancia por envío",
       x = "Envíos",
       y = "Ganancia") +
  theme_minimal()
graf_envios


graf_envios_zoom <- ggplot(tabla, aes(x = envios, y = ganancia, colour = modelo)) +
  geom_smooth() +  theme_minimal() +
  xlim(8000, 15000) +
  ylim(108, 115)

graf_envios_zoom

ggsave("Curva_ganancia_por_envíos.png", graf_envios, width = 8, height = 5, units = "in", bg = "white")





