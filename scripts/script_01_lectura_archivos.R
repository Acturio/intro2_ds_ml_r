# install.packages("pacman")

library(readr)
library(readxl)
library(jsonlite)
library(DBI)
library(RJDBC)
library(RSQLite)
library(dbplyr)

# Alternativamente: instala librerías si no existen y las carga a la sesión

pacman::p_load(
 readr,
 readxl,
 jsonlite,
 RSQLite
)


# Lectura desde .csv
base <- read.csv("data/ames.csv")
head(base, 10)

# Lectura desde .csv (PRO)
tidy <- read_csv("data/ames.csv")
head(tidy, 2)

# Lectura desde .txt
ames_txt <- read_delim("data/ames.txt", delim = ";", col_names = T)
head(ames_txt, 2)

# Lectura desde .xlsx
### Ejercicio: Escribir el código para leer datos a partir de un archivo de excel (xlsx)

ames_xlsx <- read_xlsx(
 "data/ames.xlsx", 
 sheet = 1, 
 na = c("", "NA", "*", "N/A", "vacío", "#", "desconocido")
 #col_types = "ccccdcccDccci"
 )


# Lectura desde .json
base_json <- jsonlite::fromJSON("data/ames.json")
head(base_json, 2)

lista <- list(a = 1:5, x = 5, texto = "Hola Mundo!")
saveRDS(lista, "data/lista.rds")

# Lectura desde .rds
lista <- readRDS("data/lista.rds")
base_rds <- readRDS("data/ames.rds")



# Lectura desde base de datos

con <- src_memdb()

copy_to(con, storms, overwrite = T)
copy_to(con, mtcars, overwrite = T)

tbl_storms <- tbl(con, "storms")
tbl_storms

tbl_mtcars <- tbl(con, "mtcars")
tbl_mtcars















