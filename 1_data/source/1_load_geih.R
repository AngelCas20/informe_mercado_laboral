cat("\f")
rm(list=ls())
library(pacman)
p_load(tidyverse,
       janitor,
       data.table,
       dtplyr,
       rio)

### List files
files = list.files(path = "1_data/input/",full.names = T)

### function to read in geih
read_geih = function(x){
  
  ### Files
  monthly_files = list.files(path = x,full.names = T)
  
  ### Load CGN
  cgn = fread(file = monthly_files %>% str_subset(pattern = "generales")) %>% clean_names()
  
  ### Ocupados
  ocu = fread(file = monthly_files %>% str_subset(pattern = "Ocu")) %>% clean_names()
  
  ### deso
  deso = fread(file = monthly_files %>% str_subset(pattern = "No ocu")) %>% clean_names()
 
  ### Ft
  fuerza = fread(file = monthly_files %>% str_subset(pattern = "Fuerza")) %>% clean_names()
  
  ### GEIH
  geih = left_join(x = cgn,y = ocu,by=c("directorio","secuencia_p","orden")) %>% 
    left_join(x = .,y = deso,by=c("directorio","secuencia_p","orden")) %>% 
    left_join(x = .,y = fuerza,by=c("directorio","secuencia_p","orden")) 
  
  ### clean
  geih=geih %>% select(-contains(".x.x"),-contains(".y.y"),-contains(".y"))
  
  ### return
  return(geih)
  
}

### load
db = map(.x = files,.f = read_geih,.progress = T) %>% rbindlist(use.names = T,fill = T)

### export
export(db,"1_data/output/geih_2023.rds")
