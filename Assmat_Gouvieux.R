# Assmat Gouvieux Cartographie

# Library -----------
# lire
library(readxl)
library(writexl)
library(DBI)
library(arrow)
library(duckdb)
# manipuler
library(tidyverse)
library(dplyr)
# carto
library(leaflet)
library(ggplot2)
library(tidygeocoder)
library(ggspatial)
library(sf)

# Autre potentielle : https://www.insee.fr/fr/statistiques/2011101?geo=COM-60282&utm
# Prix de l'immobilier : https://explore.data.gouv.fr/fr/immobilier?onglet=carte&filtre=tous&lat=49.18981&lng=2.42004&zoom=13.38&level=section&code


# Data -----------

data_ass_mat <- read_excel("data/LISTE-ASS.MAT-.xlsx")


# View(data_ass_mat_raw)
# 60270 Gouvieux


# Géocodage ---------

data_ass_mat <- data_ass_mat |> 
  mutate(full_address = paste(ADRESSE, "60270 Gouvieux, France"))

data_ass_mat_geocod <- data_ass_mat |> 
  geocode(address = full_address, method = "osm", lat = lat, long = lon, timeout = 10)

#  check
# head(data_ass_mat_geocod)
write_xlsx(data_ass_mat_geocod, "data/data_ass_mat_geocod.xlsx")


# Conversion pour l'affichage ------------

data_ass_mat_sf <- data_ass_mat_geocod |> 
  filter(!is.na(lat) & !is.na(lon)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)





# Convertir
df_export <- data_ass_mat_sf |> 
  mutate(geometry = st_as_text(geometry))
write_xlsx(df_export, "data_ass_mat_sf_export.xlsx")


#  Leaflet ------
leaflet(data_ass_mat_geocod) |> 
  addTiles() |> 
  addMarkers(
    popup = ~paste0("<strong>", PRENOMS, " ", NOMS, "</strong><br>",
                    "Adresse : ", ADRESSE, "<br>",
                    "Tél : ", TEL, "<br>",
                    "Email : ", MAIL)
  )








#########################################

# Donnée pour les carreaux -----------

library(DBI)
library(duckdb)

# Connexion DuckDB (sans ouvrir un fichier)
con <- dbConnect(duckdb::duckdb())


# Lire CSV et écrire directement en .parquet sans tout charger
dbExecute(con, "
  COPY (SELECT * FROM 'data/datacarroyage2020.csv')
  TO 'data/.parquet' (FORMAT 'parquet')")



# Savoir quelles colonnes je prends
cols <- dbGetQuery(con, "
  SELECT * FROM read_csv_auto('data/datacarroyage2020.csv') LIMIT 5
")

df <- tbl(con, "read_csv_auto('data/datacarroyage2020.csv')") |> 
  filter(lcog_geo == '60282') |> 
  collect()


# View(df)

df_geo <- df |> 
  mutate(
    resolution = str_extract(Idcar_200m, "RES\\d+m") %>% str_remove("RES"),   
    y = str_extract(Idcar_200m, "N\\d+") %>% str_remove("N") %>% as.numeric, 
    x = str_extract(Idcar_200m, "E\\d+") %>% str_remove("E") %>% as.numeric   
  )

df_geo_sf <- df_geo |> 
  st_as_sf(coords = c("x", "y"), crs = 3035)

df_geo_wgs84 <- st_transform(df_geo_sf, crs = 4326)

coords <- st_coordinates(df_geo_wgs84)
df_geo_def <- df_geo_wgs84 |> 
  mutate(longitude = coords[,1], latitude = coords[,2])



# export
write_xlsx(df_geo_def, "data/df_geo_def.xlsx")


# Exemple de valeurs représentatives
valeurs_legende <- c(1, 5, 12)
rayons_legende <- sqrt(valeurs_legende) * 2 + 3  
couleur_legende <- "#31688e"  
espace_vertical <- 40  

# Construction de la légende HTML avec cercles bien espacés
html_legende <- paste0(
  "<div style='font-weight:bold; margin-bottom:4px;'>Nombre d'enfants de 0 à 3 ans<br/>(par carré de 200 mètres)</div>",
  "<svg width='120' height='", length(rayons_legende) * espace_vertical, "'>",
  paste0(
    mapply(function(r, v, i) {
      y_pos <- i * espace_vertical - 10  # Décalage vertical progressif
      paste0(
        "<circle cx='20' cy='", y_pos, "' r='", round(r), 
        "' fill='", couleur_legende, "' fill-opacity='0.7' stroke='black' stroke-width='0.5'/>",
        "<text x='45' y='", y_pos + 4, "' font-size='12'>", v, "</text>"
      )
    }, rayons_legende, valeurs_legende, seq_along(rayons_legende)),
    collapse = ""
  ),
  "</svg>"
)


# Carte leaflet avec cercles
leaflet(df_geo_def) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    radius = ~sqrt(Ind_0_3)*2 + 3,  
    color = "#1f78b4",  # couleur fixe ou définie plus haut
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0("Ind_0_3 : ", Ind_0_3)
  ) %>%
  addControl(html = html_legende, position = "bottomright")

