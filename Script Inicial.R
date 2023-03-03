library(sf)
library(raster)
library(terra)
library(tidyverse)
library(geobr)

## Leitura de dados e primeiros ajustes

# municipios da região metropolitana do Rio de Janeiro 

rj <- geobr::read_municipality(code_muni = "all", simplified = F)
rj <- rj %>% 
  filter(abbrev_state == "RJ")

metro <- c("Belford Roxo", "Cachoeiras De Macacu", "Duque De Caxias", "Guapimirim", "Itaboraí", "Itaguaí", "Japeri", "Magé", "Maricá", "Mesquita", "Nilópolis", "Niterói", "Nova Iguaçu", "Paracambi", "Petrópolis", "Queimados", "Rio Bonito", "São Gonçalo", "São João De Meriti", "Seropédica", "Tanguá", "Rio De Janeiro")

metro <- rj %>% 
  filter(name_muni %in% metro)


# Dados SIGEF
# Fiz o Download no site https://certificacao.incra.gov.br/csv_shp/export_shp.py
# "Imovel certificado sigef privado". Rio de Janeiro. Este dado refere-se às certificações feitas pela norma 3
# O arquivo "Imovel certificado SNCI privado" refere-se às certificações feitas pelas normas 1 e 2. Devemos decidir sobre isso. Aqui consta a análise somente com a norma 3


sigef_3 <- read_sf("SIGEF/Norma 3/Sigef Privado_RJ.shp")

# transformando dados sigef em CRS wgs84

sigef_3 <- st_transform(sigef_3, 4326)

# Existe o dado de SIGEF norma 1 e 2
#sigef_1_2 <- read_sf("SIGEF/Norma 1 2/Imóvel certificado SNCI Privado_RJ.shp")


# Dados Mapbiomas
# fiz o download atraves deste link e armazenei no local
# https://storage.googleapis.com/mapbiomas-public/brasil/collection-7/lclu/coverage/brasil_coverage_2021.tif

BR <- terra::rast("Mapbiomas/brasil_coverage_2021.tif")

# transformando dados mapbiomas em CRS wgs84 (igual dados sigef)
crs(BR) <- crs(sigef_3)


# Filtrando dados espaciais de SIGEF para a região metropolitana do Rio de Janeiro

metro_df <- metro %>% 
  st_drop_geometry() %>% 
  as_tibble()

metro_cod <- metro_df %>% 
  select(code_muni) %>% 
  pull()

sigef_3 <- sigef_3 %>% 
  filter(municipio_ %in% metro_cod)

# Adicionando o nome dos municípios aos dados espaciais SIGEF

sigef_3 <- sigef_3 %>% 
  mutate(name_muni = municipio_)

for (i in metro_cod){
  sigef_3 <- sigef_3 %>% 
    mutate(name_muni = gsub(i, metro_df[metro_df$code_muni == i,"name_muni"], name_muni))
}

# Selecionando os dados espaciais do Mapbiomas que ocorrem na região metropolitana do Rio de Janeiro

RJ_raster <- crop(BR, ext(sigef_3))

plot(RJ_raster)

# Extraindo os dados de Mapbiomas para cada polygono SIGEF
# 1. identificar as classes de cobertura. 1 = floresta; 2 = não floresta
# 2. filtrar (crop) e mascarar (mask) a área do poligono (propriedade) no raster mapbiomas
# 3. projetar o raster do polígono (albers)
# 4. Reclassificar os pixels

# Matrix de reclassificação
unique(values(RJ_raster$brasil_coverage_2021)) %>% sort()
is <- c(3, 4, 5, 49, 0, 9, 11, 15, 21, 23, 24, 25, 29, 30, 31, 32, 33, 39, 41)
becomes <- c(1, 1, 1, 1, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0)
reclass_matrix <- matrix(c(is, becomes), ncol = 2)

# Projeção albers para calculo de área

albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"

# Criando lista para armazenar o resultado

poly <- list()

for(i in 1:nrow(sigef_3)){
  message(i)
  # i = 499
  poly.i <-  terra::crop(RJ_raster, sigef_3[i,]) %>% 
    terra::mask(sigef_3[i,])
 
  
  poly[[i]] <- poly.i %>% 
    terra::project(albers, method = "near") %>% 
    terra::classify(rcl = reclass_matrix )
}

# Calculando a área de florestas

list_results <- list()
for (i in 1:length(poly)) {
  list_results [[i]] <- as.data.frame(poly[[i]]) %>% 
    group_by(brasil_coverage_2021) %>% 
    summarise(pixel = n()) %>% 
    mutate(area_m2 = pixel * res(poly[[i]])[1] * res(poly[[i]])[2])
}

list_results[[3]]

# Inserindo o calculo de área na tabela sigef


sigef_3 <- sigef_3 %>% 
  mutate(area_for_m2 = NA)

for(i in 1:nrow(sigef_3)){
  #i = 3
  # i = 499
  if(nrow(list_results[[i]]) == 2){
    df.i <- list_results[[i]] %>% 
      as_tibble()
  sigef_3$area_for_m2[i] <- df.i[df.i$brasil_coverage_2021 == 1, "area_m2"] %>% pull()} else {
    sigef_3$area_for_m2[i] = 0
  }
}


# Tabela Final
colnames(sigef_3)
sigef_3

# Transformando os dados em dataframe

st_geometry(sigef_3) <- NULL
class(sigef_3)


write_csv(sigef_3, "Resultados/tabela_final_sigef.csv")
# rm(list = ls())

library(tmap)
RJ_map <- tm_shape(RJ_raster)+
  tm_raster(title = "")+
  tm_layout(main.title = "Classes Mapbiomas 2021")
tmap_save(RJ_map, "Resultados/mapa_rj.png")  


# para fazer os próximos mapas, rodei novamente o script até iniciar o for do raster. Então, rodei apenas 1 i (499), para gerar o poly.i. Alguma coisa no restante do script não permitir ter poly.i sem rodar tudo novamente


poly_map <- tm_shape(poly.i)+
  tm_raster(title = "")+
  tm_layout(main.title = "Classes Propriedade")
tmap_save(poly_map, "Resultados/propriedade.png")  

poly_reclass <- tm_shape(poly.i)+
  tm_raster(title = "")+
  tm_layout(main.title = "Propriedade Classificada",
            )
tmap_save(poly_reclass, "Resultados/propriedade_classificada.png")  
