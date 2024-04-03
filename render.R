library(dplyr) #Data manipulación
library(tidyr) #Función separate para las fechas

condition = 1
# 0 = Local
# 1 = Api

if (condition == 0) {
  # Datos para la Tabla
  df <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>%
    mutate(Departamento = as.character(Departamento)) %>% 
    select(Departamento) %>% 
    distinct()
  
  Casos <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
    filter(Departamento != "Sin identificar") %>% 
    mutate(Departamento = as.character(Departamento)) %>% 
    select(Departamento) %>% 
    table() %>% 
    data.frame() %>% 
    rename(Departamento = ".", Casos = Freq) %>% 
    mutate(Departamento = as.character(Departamento))
  
  Recuperados <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
    mutate(Departamento = as.character(Departamento)) %>% 
    filter(Recuperados >= 1) %>% 
    select(Departamento) %>% 
    table() %>% 
    data.frame() %>% 
    rename(Departamento = ".", Recuperados = Freq) %>% 
    mutate(Departamento = as.character(Departamento))
  
  Fallecidos <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
    mutate(Departamento = as.character(Departamento)) %>% 
    filter(Fallecidos >= 1) %>% 
    select(Departamento) %>% 
    table() %>% 
    data.frame() %>% 
    rename(Departamento = ".", Fallecidos = Freq) %>% 
    mutate(Departamento = as.character(Departamento))
  
  data <- left_join(df, Casos, By = "Departamento")
  data <- left_join(data, Recuperados, By = "Departamento")
  data <- left_join(data, Fallecidos, By = "Departamento")
  data <- data %>% 
    arrange(desc(Casos), Departamento)
  data$Casos[is.na(data$Casos)] <- 0
  data$Recuperados[is.na(data$Recuperados)] <- 0
  data$Fallecidos[is.na(data$Fallecidos)] <- 0
  saveRDS(data, "staying/DataTable.rds")
  
  ### Casos confirmados
  data <- readRDS("staying/DataTable.rds") %>% 
    select(Casos) %>% 
    summarise_each(funs(sum(., na.rm = TRUE))) %>% 
    mutate(a = substring(Casos, 1, 1)) %>% 
    mutate(b = substring(Casos, 2, 2)) %>% 
    mutate(c = substring(Casos, 3, 3)) %>% 
    mutate(d = substring(Casos, 4, 4)) %>% 
    mutate(e = substring(Casos, 5, 5)) %>% 
    mutate(f = substring(Casos, 6, 6)) %>% 
    mutate(Casos_Text = paste0(a, b, c, ".", d, e, f)) %>% 
    select(Casos, Casos_Text)
  saveRDS(data, "staying/CasosConfirmados.rds")
  
  ### Casos recuperados
  data <- readRDS("staying/DataTable.rds") %>% 
    select(Recuperados) %>% 
    summarise_each(funs(sum(., na.rm = TRUE))) %>% 
    mutate(a = substring(Recuperados, 1, 1)) %>% 
    mutate(b = substring(Recuperados, 2, 2)) %>% 
    mutate(c = substring(Recuperados, 3, 3)) %>% 
    mutate(d = substring(Recuperados, 4, 4)) %>% 
    mutate(e = substring(Recuperados, 5, 5)) %>% 
    mutate(f = substring(Recuperados, 6, 6)) %>% 
    mutate(Recuperados_Text = paste0(a, b, c, ".", d, e, f)) %>% 
    select(Recuperados, Recuperados_Text)
  saveRDS(data, "staying/CasosRecuperados.rds")
  
  ### Casos fallecidos
  data <- readRDS("staying/DataTable.rds") %>% 
    select(Fallecidos) %>% 
    summarise_each(funs(sum(., na.rm = TRUE))) %>% 
    mutate(a = substring(Fallecidos, 1, 1)) %>% 
    mutate(b = substring(Fallecidos, 2, 2)) %>% 
    mutate(c = substring(Fallecidos, 3, 3)) %>% 
    mutate(d = substring(Fallecidos, 4, 4)) %>% 
    mutate(e = substring(Fallecidos, 5, 5)) %>% 
    mutate(Fallecidos_Text = paste0(a, b, ".", c, d, e)) %>% 
    select(Fallecidos, Fallecidos_Text)
  saveRDS(data, "staying/CasosFallecidos.rds")
  
  
} else if (condition  == 1) {
  ### Casos confirmados
  data <- read.csv("staying/data_api.csv")
  data <- nrow(data)
  data <- data.frame(data)
  data <- data %>% 
    mutate(a = substring(data, 1, 1)) %>% 
    mutate(b = substring(data, 2, 2)) %>% 
    mutate(c = substring(data, 3, 3)) %>% 
    mutate(d = substring(data, 4, 4)) %>% 
    mutate(e = substring(data, 5, 5)) %>% 
    mutate(f = substring(data, 6, 6)) %>% 
    mutate(Casos_Text = paste0(a, b, c, ".", d, e, f)) %>% 
    select(Casos = data, Casos_Text)
  saveRDS(data, "staying/CasosConfirmados.rds")
  
  ### Casos recuperados
  data <- read.csv("staying/data_api.csv") %>% 
    filter(atenci_n == "Recuperado")
  data <- nrow(data)
  data <- data.frame(data)
  data <- data %>% 
    mutate(a = substring(data, 1, 1)) %>% 
    mutate(b = substring(data, 2, 2)) %>% 
    mutate(c = substring(data, 3, 3)) %>% 
    mutate(d = substring(data, 4, 4)) %>% 
    mutate(e = substring(data, 5, 5)) %>% 
    mutate(f = substring(data, 6, 6)) %>% 
    mutate(Recuperados_Text = paste0(a, b, c, ".", d, e, f)) %>% 
    select(Recuperados = data, Recuperados_Text)
  saveRDS(data, "staying/CasosRecuperados.rds")
  
  ### Casos fallecidos
  data <- read.csv("staying/data_api.csv") %>% 
    filter(atenci_n == "Fallecido")
  data <- nrow(data)
  data <- data.frame(data)
  data <- data %>% 
    mutate(a = substring(data, 1, 1)) %>% 
    mutate(b = substring(data, 2, 2)) %>% 
    mutate(c = substring(data, 3, 3)) %>% 
    mutate(d = substring(data, 4, 4)) %>% 
    mutate(e = substring(data, 5, 5)) %>% 
    mutate(f = substring(data, 6, 6)) %>% 
    mutate(Fallecidos_Text = paste0(a, b, ".", c, d, e)) %>% 
    select(Fallecidos = data, Fallecidos_Text)
  saveRDS(data, "staying/CasosFallecidos.rds")
}





#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña home
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

# Casos por día
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  mutate(Fecha = as.character(Fecha))
data <- separate(data =  data, 
                 col  =  Fecha,  
                 into =  c("M", "D", "A"), 
                 sep  =  "/")
data <- data %>% 
  select(M, D) %>% 
  table() %>% 
  data.frame() %>% 
  mutate(D = as.character(D)) %>% 
  mutate(M = as.character(M)) %>% 
  mutate(D = as.numeric(D)) %>% 
  mutate(M = as.numeric(M)) %>% 
  arrange(M) %>% 
  arrange(D) %>% 
  mutate(labels = ifelse(M == 3, "Marzo",
                         ifelse(M == 4, "Abril",
                                ifelse(M == 5, "Mayo",
                                       ifelse(M == 6, "Junio",
                                              ifelse(M == 7, "Julio",
                                                     ifelse(M == 8, "Agosto",
                                                            ifelse(M == 9, "Septiembre",
                                                                   ifelse(M == 10, "Octubre",
                                                                          ifelse(M == 11, "Noviembre", 
                                                                                 ifelse(M == 12, "Diciembre", "na"))))))))))) %>% 
  mutate(labels = paste0(labels, " - ", D))
data$labels <- factor(data$labels, levels = data[["labels"]])
saveRDS(data, "staying/CasosNuevos.rds")

### Casos totales por día
data <- readRDS("staying/CasosNuevos.rds") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  arrange(M)
result <- data.frame("Freq" = numeric(nrow(data)), stringsAsFactors = FALSE)
for (i in 1:nrow(data)) {
  colSum_ <- data[1:i, ]
  colSum_ <- colSum_ %>%
    select(Freq) %>%
    summarise(Freq = sum(Freq)) %>% 
    mutate(Freq = as.numeric(Freq))
  result[i, "Freq"] <- colSum_[1, "Freq"]
}
data <- data %>% 
  select(labels) %>% 
  bind_cols(result)
saveRDS(data, "staying/HistoricoDeCasos.rds")

### Género
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  rename(Gender = Género) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/Gender.rds")

### Lugar de atención
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  filter(Atención != "Fallecido") %>% 
  filter(Atención != "Recuperado") %>% 
  mutate(Atención = as.character(Atención)) %>% 
  select(Atención) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  arrange(desc(Freq))
data$labels <- factor(data$labels, levels = data[["labels"]])
saveRDS(data, "staying/LugarAtención.rds")

### Rangos de edad
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  select(Edad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)

saveRDS(data, "staying/RangoEdad.rds")





#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña Departamento
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

### Data para el Mapa
#Lat_Lon <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
#  filter(Departamento != "Sin identificar") %>% 
#  mutate(Ciudad = as.character(Ciudad)) %>% 
#  select(Ciudad, Latitud, Longitud) %>% 
#  distinct()
#data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
#  filter(Departamento != "Sin identificar") %>% 
#  mutate(Ciudad = as.character(Ciudad)) %>% 
#  select(Ciudad) %>% 
#  table() %>% 
#  data.frame() %>% 
#  rename(Ciudad = ".", Casos = Freq) %>% 
#  mutate(Ciudad = as.character(Ciudad))

#data <- left_join(data, Lat_Lon, by = "Ciudad") %>% 
#  mutate(text = ifelse(Casos > 1, "Casos", "Caso")) %>% 
#  mutate(popup = paste0(Ciudad, ": ", Casos, " ", text)) %>%
#  select(Ciudad, Latitud, Longitud, popup, Casos)
#saveRDS(data, "staying/DataMaps.rds")

# Datos para la Tabla
df <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>%
  mutate(Departamento = as.character(Departamento)) %>% 
  select(Departamento) %>% 
  distinct()

Casos <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Departamento != "Sin identificar") %>% 
  mutate(Departamento = as.character(Departamento)) %>% 
  select(Departamento) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Departamento = ".", Casos = Freq) %>% 
  mutate(Departamento = as.character(Departamento))

Recuperados <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  mutate(Departamento = as.character(Departamento)) %>% 
  filter(Recuperados >= 1) %>% 
  select(Departamento) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Departamento = ".", Recuperados = Freq) %>% 
  mutate(Departamento = as.character(Departamento))

Fallecidos <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  mutate(Departamento = as.character(Departamento)) %>% 
  filter(Fallecidos >= 1) %>% 
  select(Departamento) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Departamento = ".", Fallecidos = Freq) %>% 
  mutate(Departamento = as.character(Departamento))

data <- left_join(df, Casos, By = "Departamento")
data <- left_join(data, Recuperados, By = "Departamento")
data <- left_join(data, Fallecidos, By = "Departamento")
data <- data %>% 
  arrange(desc(Casos), Departamento)
data$Casos[is.na(data$Casos)] <- 0
data$Recuperados[is.na(data$Recuperados)] <- 0
data$Fallecidos[is.na(data$Fallecidos)] <- 0
saveRDS(data, "staying/DataTable.rds")





#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña Ciudad o Municipio
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

# Datos para la Tabla por Ciudad o Municipio
df <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>%
  mutate(Ciudad = as.character(Ciudad)) %>% 
  select(Ciudad) %>% 
  distinct()

Casos <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Ciudad != "Sin identificar") %>% 
  mutate(Ciudad = as.character(Ciudad)) %>% 
  select(Ciudad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Ciudad = ".", Casos = Freq) %>% 
  mutate(Ciudad = as.character(Ciudad))

Recuperados <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  mutate(Ciudad = as.character(Ciudad)) %>% 
  filter(Recuperados >= 1) %>% 
  select(Ciudad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Ciudad = ".", Recuperados = Freq) %>% 
  mutate(Ciudad = as.character(Ciudad))

Fallecidos <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  mutate(Ciudad = as.character(Ciudad)) %>% 
  filter(Fallecidos >= 1) %>% 
  select(Ciudad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Ciudad = ".", Fallecidos = Freq) %>% 
  mutate(Ciudad = as.character(Ciudad))

data <- left_join(df, Casos, By = "Ciudad")
data <- left_join(data, Recuperados, By = "Ciudad")
data <- left_join(data, Fallecidos, By = "Ciudad")
data <- data %>% 
  arrange(desc(Casos), Ciudad)
data$Casos[is.na(data$Casos)] <- 0
data$Recuperados[is.na(data$Recuperados)] <- 0
data$Fallecidos[is.na(data$Fallecidos)] <- 0
saveRDS(data, "staying/Data_Table_Ciudad.rds")





#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña Hospitalizadas
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

# Casos por día
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  mutate(Fecha = as.character(Fecha))
data <- separate(data =  data, 
                 col  =  Fecha,  
                 into =  c("M", "D", "A"), 
                 sep  =  "/")
data <- data %>% 
  select(M, D) %>% 
  table() %>% 
  data.frame() %>% 
  mutate(D = as.character(D)) %>% 
  mutate(M = as.character(M)) %>% 
  mutate(D = as.numeric(D)) %>% 
  mutate(M = as.numeric(M)) %>% 
  arrange(M) %>% 
  arrange(D) %>% 
  mutate(labels = ifelse(M == 3, "Marzo",
                         ifelse(M == 4, "Abril",
                                ifelse(M == 5, "Mayo",
                                       ifelse(M == 6, "Junio",
                                              ifelse(M == 7, "Julio",
                                                     ifelse(M == 8, "Agosto",
                                                            ifelse(M == 9, "Septiembre",
                                                                   ifelse(M == 10, "Octubre",
                                                                          ifelse(M == 11, "Noviembre", 
                                                                                 ifelse(M == 12, "Diciembre", "na"))))))))))) %>% 
  mutate(labels = paste0(labels, " - ", D))
data$labels <- factor(data$labels, levels = data[["labels"]])
saveRDS(data, "staying/CasosNuevosHospitalizadas.rds")

### Casos totales por día
data <- readRDS("staying/CasosNuevosHospitalizadas.rds") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  arrange(M)
result <- data.frame("Freq" = numeric(nrow(data)), stringsAsFactors = FALSE)
for (i in 1:nrow(data)) {
  colSum_ <- data[1:i, ]
  colSum_ <- colSum_ %>%
    select(Freq) %>%
    summarise(Freq = sum(Freq)) %>% 
    mutate(Freq = as.numeric(Freq))
  result[i, "Freq"] <- colSum_[1, "Freq"]
}
data <- data %>% 
  select(labels) %>% 
  bind_cols(result)
saveRDS(data, "staying/HistoricoDeCasosHospitalizadas.rds")

### Género
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  rename(Gender = Género) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/GenderHospitalizadas.rds")

### Rangos de edad
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  select(Edad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)

saveRDS(data, "staying/RangoEdadHospitalizadas.rds")




#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña Hospitalizadas en UCI
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

# Casos por día
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital UCI")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  mutate(Fecha = as.character(Fecha))
data <- separate(data =  data, 
                 col  =  Fecha,  
                 into =  c("M", "D", "A"), 
                 sep  =  "/")
data <- data %>% 
  select(M, D) %>% 
  table() %>% 
  data.frame() %>% 
  mutate(D = as.character(D)) %>% 
  mutate(M = as.character(M)) %>% 
  mutate(D = as.numeric(D)) %>% 
  mutate(M = as.numeric(M)) %>% 
  arrange(M) %>% 
  arrange(D) %>% 
  mutate(labels = ifelse(M == 3, "Marzo",
                         ifelse(M == 4, "Abril",
                                ifelse(M == 5, "Mayo",
                                       ifelse(M == 6, "Junio",
                                              ifelse(M == 7, "Julio",
                                                     ifelse(M == 8, "Agosto",
                                                            ifelse(M == 9, "Septiembre",
                                                                   ifelse(M == 10, "Octubre",
                                                                          ifelse(M == 11, "Noviembre", 
                                                                                 ifelse(M == 12, "Diciembre", "na"))))))))))) %>% 
  mutate(labels = paste0(labels, " - ", D))
data$labels <- factor(data$labels, levels = data[["labels"]])
saveRDS(data, "staying/CasosNuevosHospitalizadasUCI.rds")

### Casos totales por día
data <- readRDS("staying/CasosNuevosHospitalizadasUCI.rds") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  arrange(M)
result <- data.frame("Freq" = numeric(nrow(data)), stringsAsFactors = FALSE)
for (i in 1:nrow(data)) {
  colSum_ <- data[1:i, ]
  colSum_ <- colSum_ %>%
    select(Freq) %>%
    summarise(Freq = sum(Freq)) %>% 
    mutate(Freq = as.numeric(Freq))
  result[i, "Freq"] <- colSum_[1, "Freq"]
}
data <- data %>% 
  select(labels) %>% 
  bind_cols(result)
saveRDS(data, "staying/HistoricoDeCasosHospitalizadasUCI.rds")

### Género
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital UCI")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  rename(Gender = Género) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/GenderHospitalizadasUCI.rds")

### Rangos de edad
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital UCI")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  select(Edad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)

saveRDS(data, "staying/RangoEdadHospitalizadasUCI.rds")

# Datos para la Tabla por Ciudad o Municipio en Hospitalización en UCI
df <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>%
  mutate(Ciudad = as.character(Ciudad)) %>% 
  select(Ciudad) %>% 
  distinct()
UCI <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Hospital UCI") %>% 
  mutate(Ciudad = as.character(Ciudad)) %>% 
  select(Ciudad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(Ciudad = ".", Camas = Freq) %>% 
  mutate(Ciudad = as.character(Ciudad))
data <- left_join(df, UCI, By = "Ciudad")
data <- data %>% 
  arrange(desc(Camas), Ciudad) %>% 
  filter(Camas > 0)
saveRDS(data, "staying/Data_Table_Camas_UCI.rds")





#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña En casa
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

# Casos por día
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Casa")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  mutate(Fecha = as.character(Fecha))
data <- separate(data =  data, 
                 col  =  Fecha,  
                 into =  c("M", "D", "A"), 
                 sep  =  "/")
data <- data %>% 
  select(M, D) %>% 
  table() %>% 
  data.frame() %>% 
  mutate(D = as.character(D)) %>% 
  mutate(M = as.character(M)) %>% 
  mutate(D = as.numeric(D)) %>% 
  mutate(M = as.numeric(M)) %>% 
  arrange(M) %>% 
  arrange(D) %>% 
  mutate(labels = ifelse(M == 3, "Marzo",
                         ifelse(M == 4, "Abril",
                                ifelse(M == 5, "Mayo",
                                       ifelse(M == 6, "Junio",
                                              ifelse(M == 7, "Julio",
                                                     ifelse(M == 8, "Agosto",
                                                            ifelse(M == 9, "Septiembre",
                                                                   ifelse(M == 10, "Octubre",
                                                                          ifelse(M == 11, "Noviembre", 
                                                                                 ifelse(M == 12, "Diciembre", "na"))))))))))) %>% 
  mutate(labels = paste0(labels, " - ", D))
data$labels <- factor(data$labels, levels = data[["labels"]])
saveRDS(data, "staying/CasosNuevosCasa.rds")

### Casos totales por día
data <- readRDS("staying/CasosNuevosCasa.rds") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  arrange(M)
result <- data.frame("Freq" = numeric(nrow(data)), stringsAsFactors = FALSE)
for (i in 1:nrow(data)) {
  colSum_ <- data[1:i, ]
  colSum_ <- colSum_ %>%
    select(Freq) %>%
    summarise(Freq = sum(Freq)) %>% 
    mutate(Freq = as.numeric(Freq))
  result[i, "Freq"] <- colSum_[1, "Freq"]
}
data <- data %>% 
  select(labels) %>% 
  bind_cols(result)
saveRDS(data, "staying/HistoricoDeCasosCasa.rds")

### Género
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Casa")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  rename(Gender = Género) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/GenderCasa.rds")

### Rangos de edad
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Atención == "Casa")

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  select(Edad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)

saveRDS(data, "staying/RangoEdadCasa.rds")





#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña fallecidas
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------

# Casos por día
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Fallecidos == 1)

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  mutate(Fecha = as.character(Fecha))
data <- separate(data =  data, 
                 col  =  Fecha,  
                 into =  c("M", "D", "A"), 
                 sep  =  "/")
data <- data %>% 
  select(M, D) %>% 
  table() %>% 
  data.frame() %>% 
  mutate(D = as.character(D)) %>% 
  mutate(M = as.character(M)) %>% 
  mutate(D = as.numeric(D)) %>% 
  mutate(M = as.numeric(M)) %>% 
  arrange(M) %>% 
  arrange(D) %>% 
  mutate(labels = ifelse(M == 3, "Marzo",
                         ifelse(M == 4, "Abril",
                                ifelse(M == 5, "Mayo",
                                       ifelse(M == 6, "Junio",
                                              ifelse(M == 7, "Julio",
                                                     ifelse(M == 8, "Agosto",
                                                            ifelse(M == 9, "Septiembre",
                                                                   ifelse(M == 10, "Octubre",
                                                                          ifelse(M == 11, "Noviembre", 
                                                                                 ifelse(M == 12, "Diciembre", "na"))))))))))) %>% 
  mutate(labels = paste0(labels, " - ", D))
data$labels <- factor(data$labels, levels = data[["labels"]])
saveRDS(data, "staying/CasosNuevosfallecidas.rds")

### Casos totales por día
data <- readRDS("staying/CasosNuevosfallecidas.rds") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  arrange(M)
result <- data.frame("Freq" = numeric(nrow(data)), stringsAsFactors = FALSE)
for (i in 1:nrow(data)) {
  colSum_ <- data[1:i, ]
  colSum_ <- colSum_ %>%
    select(Freq) %>%
    summarise(Freq = sum(Freq)) %>% 
    mutate(Freq = as.numeric(Freq))
  result[i, "Freq"] <- colSum_[1, "Freq"]
}
data <- data %>% 
  select(labels) %>% 
  bind_cols(result)
saveRDS(data, "staying/HistoricoDeCasosfallecidas.rds")

### Fallecidos - Departamento
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Fallecidos == 1)

data <- data %>% 
  select(Departamento) %>%
  table() %>% 
  data.frame() %>% 
  rename(Departamento = ".", Casos = Freq) %>% 
  mutate(Departamento = as.character(Departamento)) %>% 
  filter(Casos > 0) %>% 
  arrange(desc(Casos), Departamento) 
saveRDS(data, "staying/FallecidosDepartamento.rds")

### Género
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Fallecidos == 1)

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  rename(Gender = Género) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/Genderfallecidas.rds")

### Rangos de edad
data <- read.csv("staying/data-ins.csv", encoding = "UTF-8") %>% 
  filter(Fallecidos == 1)

data <- data %>% 
  filter(Departamento != "Sin identificar") %>% 
  select(Edad) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)

saveRDS(data, "staying/RangoEdadfallecidas.rds")










#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Pestaña Casos de Bogotá
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------
# Limpieza de los datos
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8") %>%
  mutate(Localidad.de.residencia = trimws(Localidad.de.residencia)) %>% 
  mutate(Ubicacion = trimws(Ubicacion)) %>% 
  mutate(Estado = trimws(Estado)) %>% 
  mutate(Ubicacion = ifelse(Ubicacion == "Estudio", "Casa", Ubicacion)) %>% 
  mutate(Estado = ifelse(Estado == "Estudio", "Moderado", Estado))
write.csv(data, "staying/data_api_bog.csv", row.names = FALSE)

### Casos confirmados
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")
data <- nrow(data)
data <- data.frame(data)
data <- data %>% 
  mutate(a = substring(data, 1, 1)) %>% 
  mutate(b = substring(data, 2, 2)) %>% 
  mutate(c = substring(data, 3, 3)) %>% 
  mutate(d = substring(data, 4, 4)) %>% 
  mutate(e = substring(data, 5, 5)) %>% 
  mutate(Casos_Text = paste0(a, b, ".", c, d, e)) %>% 
  select(Casos = data, Casos_Text)
saveRDS(data, "staying/CasosConfirmadosBog.rds")

### Casos recuperados
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8") %>% 
  filter(Estado == "Recuperado")
data <- nrow(data)
data <- data.frame(data)
data <- data %>% 
  mutate(a = substring(data, 1, 1)) %>% 
  mutate(b = substring(data, 2, 2)) %>% 
  mutate(c = substring(data, 3, 3)) %>% 
  mutate(d = substring(data, 4, 4)) %>% 
  mutate(e = substring(data, 5, 5)) %>% 
  mutate(Recuperados_Text = paste0(a, ".", b, c, d)) %>% 
  select(Recuperados = data, Recuperados_Text)
saveRDS(data, "staying/CasosRecuperadosBog.rds")

### Casos fallecidos
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8") %>% 
  filter(Estado == "Fallecido")
data <- nrow(data)
data <- data.frame(data)
data <- data %>% 
  mutate(a = substring(data, 1, 1)) %>% 
  mutate(b = substring(data, 2, 2)) %>% 
  mutate(c = substring(data, 3, 3)) %>% 
  mutate(d = substring(data, 4, 4)) %>% 
  mutate(e = substring(data, 5, 5)) %>% 
  mutate(Fallecidos_Text = paste0(a, b, c, d)) %>% 
  select(Fallecidos = data, Fallecidos_Text)
saveRDS(data, "staying/CasosFallecidosBog.rds")

### Género
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>% 
  rename(Gender = Sexo) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/GenderBog.rds")


### Rangos de edad
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>% 
  mutate(edad_ = ifelse(Edad > 0 & Edad <= 9, "0 a 9", 
                               ifelse(Edad >= 10 & Edad <= 19, "10 a 19", 
                                      ifelse(Edad >= 20 & Edad <= 29, "20 a 29",  
                                             ifelse(Edad >= 30 & Edad <= 39, "30 a 39",
                                                    ifelse(Edad >= 40 & Edad <= 49, "40 a 49",
                                                           ifelse(Edad >= 50 & Edad <= 59, "50 a 59", 
                                                                  ifelse(Edad >= 60 & Edad <= 69, "60 a 69", 
                                                                         ifelse(Edad >= 70 & Edad <= 79, "70 a 79",
                                                                                ifelse(Edad >= 80 & Edad <= 89, "80 a 89", 
                                                                                       ifelse(Edad >= 90 & Edad <= 99, "90 a 99",
                                                                                              ifelse(Edad >= 100 & Edad <= 109, "100 a 109", "NA")))))))))))) %>% 
  select(edad_) %>%
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)
  saveRDS(data, "staying/RangoEdadBog.rds")

### Localidad de residencia
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")
  
data <- data %>% 
  select(Localidad.de.residencia) %>%
  table() %>% 
  data.frame() %>% 
  rename(Localidad = ".", Casos = Freq) %>% 
  mutate(Localidad = as.character(Localidad)) %>% 
  filter(Localidad != "Sin Dato") %>% 
  filter(Localidad != "Fuera de Bogotá") %>% 
  arrange(desc(Casos), Localidad) 
saveRDS(data, "staying/LocalidadBog.rds")

### Ubicación
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>% 
  mutate(Ubicacion = as.character(Ubicacion)) %>% 
  select(Ubicacion) %>%
  mutate(Ubicacion = ifelse(Ubicacion == "fallecido", "Fallecido", 
                                   ifelse(Ubicacion == "casa", "Casa",
                                          ifelse(Ubicacion == "hospital UCI", "Hospital UCI", 
                                                 ifelse(Ubicacion == "hospital", "Hospital", Ubicacion))))) %>% 
  filter(Ubicacion != "Fallecido No aplica No causa Directa") %>%
  filter(Ubicacion != "Fallecido  No aplica  No causa Directa") %>%
  filter(Ubicacion != "Fallecido No aplica  No causa Directa") %>%
  filter(Ubicacion != "Fallecido  No aplica No causa Directa") %>% 
  filter(Ubicacion != "Fallecido (No aplica") %>%
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(labels != "Fallecido")
saveRDS(data, "staying/UbicacionBog.rds")

### Estado
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>%
  mutate(Estado = as.character(Estado)) %>% 
  select(Estado) %>%
  filter(Estado != "fallecido") %>% 
  filter(Estado != "Fallecido") %>% 
  filter(Estado != "No causa Directa)") %>%
  filter(Estado != "Fallecido No aplica No causa Directa") %>%
  filter(Estado != "Fallecido  No aplica  No causa Directa") %>%
  filter(Estado != "Fallecido No aplica  No causa Directa") %>%
  filter(Estado != "Fallecido  No aplica No causa Directa") %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  arrange(desc(Freq), labels) 
saveRDS(data, "staying/EstadoBog.rds")

### Fallecidos por Localidad de residencia
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Estado == "Fallecido") %>% 
  select(Localidad.de.residencia) %>%
  table() %>% 
  data.frame() %>% 
  rename(Localidad = ".", Casos = Freq) %>% 
  mutate(Localidad = as.character(Localidad)) %>% 
  filter(Localidad != "Sin Dato") %>% 
  filter(Localidad != "Fuera de Bogotá") %>% 
  filter(Casos > 0) %>% 
  arrange(desc(Casos), Localidad) 
saveRDS(data, "staying/LocalidadFallecidosBog.rds")

### Fallecidos por Género
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Estado == "Fallecido") %>% 
  rename(Gender = Sexo) %>% 
  mutate(Gender = as.character(Gender)) %>% 
  select(Gender) %>%
  mutate(Gender = ifelse(Gender == "F", "Femenino",
                         ifelse(Gender == "M", "Masculino", "NA"))) %>% 
  table() %>% 
  data.frame() %>% 
  rename(labels = ".")
saveRDS(data, "staying/GenderFallecidosBog.rds")


### Fallecidos por Rangos de edad
data <- read.csv("staying/data_api_bog.csv", encoding = "UTF-8")

data <- data %>% 
  filter(Estado == "Fallecido") %>% 
  mutate(edad_ = ifelse(Edad > 0 & Edad <= 9, "0 a 9", 
                        ifelse(Edad >= 10 & Edad <= 19, "10 a 19", 
                               ifelse(Edad >= 20 & Edad <= 29, "20 a 29",  
                                      ifelse(Edad >= 30 & Edad <= 39, "30 a 39",
                                             ifelse(Edad >= 40 & Edad <= 49, "40 a 49",
                                                    ifelse(Edad >= 50 & Edad <= 59, "50 a 59", 
                                                           ifelse(Edad >= 60 & Edad <= 69, "60 a 69", 
                                                                  ifelse(Edad >= 70 & Edad <= 79, "70 a 79",
                                                                         ifelse(Edad >= 80 & Edad <= 89, "80 a 89", 
                                                                                ifelse(Edad >= 90 & Edad <= 99, "90 a 99",
                                                                                       ifelse(Edad >= 100 & Edad <= 109, "100 a 109", "NA")))))))))))) %>% 
  select(edad_) %>%
  table() %>% 
  data.frame() %>% 
  rename(labels = ".") %>% 
  mutate(labels = as.character(labels)) %>% 
  filter(Freq > 0) %>% 
  mutate(orden_labels = ifelse(labels == "0 a 9", 1, 
                               ifelse(labels == "10 a 19", 2, 
                                      ifelse(labels == "20 a 29", 3, 
                                             ifelse(labels == "30 a 39", 4, 
                                                    ifelse(labels == "40 a 49", 5, 
                                                           ifelse(labels == "50 a 59", 6, 
                                                                  ifelse(labels == "60 a 69", 7, 
                                                                         ifelse(labels == "70 a 79", 8, 
                                                                                ifelse(labels == "80 a 89", 9, 
                                                                                       ifelse(labels == "90 a 99", 10, 
                                                                                              ifelse(labels == "100 a 109", 11, 12)))))))))))) %>% 
  arrange(orden_labels)
saveRDS(data, "staying/RangoEdadFallecidosBog.rds")


#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# Render
#---------------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------
rmarkdown::render("index.Rmd")

















