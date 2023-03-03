install.packages("rgdal", dependencies = T)
library(rgdal)
library(sf)
library(raster)
library(terra)
library(tidyverse)
library(geobr)
# RJ <- untar(tarfile = "FBDS/RJ.tar")
# BRoxo <- read_sf("FBDS/BELFORD_ROXO/USO/RJ_3300456_USO.shp")
# CMacacu <- read_sf("FBDS/CACHOEIRAS_DE_MACACU/USO/RJ_3300803_USO.shp")
# DCaxias <- read_sf("FBDS/DUQUE_DE_CAXIAS/USO/RJ_3301702_USO.shp")
# Guapimirim <- read_sf("FBDS/GUAPIMIRIM/USO/RJ_3301850_USO.shp")
# Itaborai <- read_sf("FBDS/ITABORAI/USO/RJ_3301900_USO.shp")
# Itaguai <- read_sf("FBDS/ITAGUAI/USO/RJ_3302007_USO.shp")
# Japeri <- read_sf("FBDS/JAPERI/USO/RJ_3302270_USO.shp")
# Mage <- read_sf("FBDS/MAGE/USO/RJ_3302502_USO.shp")
# Marica <- read_sf("FBDS/MARICA/USO/RJ_3302700_USO.shp")
# Mesquita <- read_sf("FBDS/MESQUITA/USO/RJ_3302858_USO.shp")
# Nilopolis <- read_sf("FBDS/NILOPOLIS/USO/RJ_3303203_USO.shp")
# Niteroi <- read_sf("FBDS/NITEROI/USO/RJ_3303302_USO.shp")
# NIguacu <- read_sf("FBDS/NOVA_IGUACU/USO/RJ_3303500_USO.shp")
# Paracambi <- read_sf("FBDS/PARACAMBI/USO/RJ_3303609_USO.shp")
# Petropolis <- read_sf("FBDS/PETROPOLIS/USO/RJ_3303906_USO.shp")
# Queimados <- read_sf("FBDS/QUEIMADOS/USO/RJ_3304144_USO.shp")
# RBonito <- read_sf("FBDS/RIO_BONITO/USO/RJ_3304300_USO.shp")
# SGoncalo <- read_sf("FBDS/SAO_GONCALO/USO/RJ_3304904_USO.shp")
# SMeriti <- read_sf("FBDS/SAO_JOAO_DE_MERITI/USO/RJ_3305109_USO.shp")
# Seropedica <- read_sf("FBDS/SEROPEDICA/USO/RJ_3305554_USO.shp")
# Tangua <- read_sf("FBDS/TANGUA/USO/RJ_3305752_USO.shp")
# Rio <- read_sf("FBDS/RIO_DE_JANEIRO/USO/RJ_3304557_USO.shp")
# metro <- rbind(BRoxo, CMacacu, DCaxias, Guapimirim, Itaborai, Itaguai, Japeri, Mage, Marica, Mesquita, Nilopolis, Niteroi, NIguacu, Paracambi, Petropolis, Queimados, RBonito, SGoncalo, SMeriti, Seropedica, Tangua, Rio)


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
# "Imovel certificado sigef privado". RIo de Janeiro. Este dado refere-se às certificações feitas pela norma 3
# O arquivo "Imovel certificado SNCI privado" refere-se às certificações feitas pelas normas 1 e 2. Devemos decidir sobre isso. Aqui consta a análise somente com a norma 3


sigef_3 <- read_sf("SIGEF/Norma 3/Sigef Privado_RJ.shp")


# Aqui é uma dúvida que tenho. Temos que projetar o raster no mesmo CRS de SIGEF, porém CRS em SIRGAS não fornece a resolução em metros, e isso esta confundindo o calculo da área. Será que projetamos para Albers?

utm23s <- "+init=epsg:31983"
sf::st_transform(sigef_3, CRS = utm23s)


#sigef_1_2 <- read_sf("SIGEF/Norma 1 2/Imóvel certificado SNCI Privado_RJ.shp")
sf_use_s2(T)


# Dados Mapbiomas
# fiz o download atraves deste link e armazenei no local
# https://storage.googleapis.com/mapbiomas-public/brasil/collection-7/lclu/coverage/brasil_coverage_2021.tif

BR <- terra::rast("Mapbiomas/brasil_coverage_2021.tif")
albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs"
raster::projectRaster(BR, crs = albers)
raster::projection(BR)
crs(BR) <- albers
BR
b <- rast(ncols=94, nrows=124, xmin=-944881, xmax=935118, ymin=4664377, ymax=7144377, crs=albers)
terra::project(BR, y = b)


plot(BR)
values(BR$brasil_coverage_2021)
crs(BR) <- "+proj=longlat +datum=WGS84 +no_defs"
raster:projection(BR)

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


raster::projectRaster(BR, crs = utm23s)

values(BR)

RJ_raster <- crop(BR, ext(sigef_3))

plot(RJ_raster)

# Extraindo os dados de Mapbiomas para cada polygono SIGEF


unique(values(RJ_raster$brasil_coverage_2021)) %>% sort()



# Matrix de reclassificação

is <- c(3, 4, 5, 0, 9, 11, 15, 21, 23, 24, 25, 29, 30, 31, 32, 33, 39, 41, 49)
becomes <- c(1, 1, 1, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0)
reclass_matrix <- matrix(c(is, becomes), ncol = 2)

RJ_raster <- rast(RJ_raster)
RJ_raster <- terra::classify(RJ_raster, rcl = reclass_matrix )
RJ_raster$brasil_coverage_2021
plot(RJ_raster$brasil_coverage_2021)

poly <- list()

for(i in 1:nrow(sigef_3)){
  message(i)
  poly[i] <- crop(RJ_raster, sigef_3[i,]) %>% 
    mask(sigef_3[i,])
}

plot(poly[[3]])


# Calculando a área de florestas

list_results <- list()
for (i in 1:length(poly)) {
  list_results [[i]] <- as.data.frame(poly[[i]]) %>% 
    group_by(brasil_coverage_2021) %>% 
    summarise(pixel = n()) %>% 
    mutate(area = pixel *30 *30)
}

list_results[[3]]

sigef_3 <- sigef_3 %>% 
  mutate(area_for = NA)

for(i in 1:nrow(sigef_3)){
  #i = 3
  if(nrow(list_results[[i]]) == 2){
    df.i <- list_results[[i]] %>% 
      as_tibble()
  sigef_3$area_for[i] <- df.i[df.i$brasil_coverage_2021 == 1, "area"] %>% pull()} else {
    sigef_3$area_for[i] = 0
  }
}






# rm(list = ls())
