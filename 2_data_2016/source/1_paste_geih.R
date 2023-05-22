cat("\f")
rm(list=ls())
library(pacman)
p_load(tidyverse,
       janitor,
       data.table,
       dtplyr,
       rio)

### List files
files = list.files(path = "2_data_2016/input/",full.names = T)

### function to read in geih
read_geih = function(x){
  
  ### Files
  monthly_files = list.files(path = x,full.names = T)
  
  ### Load CGN
  cgn_cab = fread(file = monthly_files %>% str_subset(pattern = "Cabecera - Caract")) %>% clean_names() %>% mutate(periodo=2016,clase=1)
  cgn_resto =fread(file = monthly_files %>% str_subset(pattern = "Resto - Caract")) %>% clean_names()%>% mutate(periodo=2016,clase=2)
  
  cgn = bind_rows(cgn_cab,cgn_resto)
  
   ### Ocupados
  ocu_cab = fread(file = monthly_files %>% str_subset(pattern = "Cabecera - Ocupados")) %>% clean_names()%>% mutate(periodo=2016,clase=1) %>% mutate_at(.vars = c("rama2d","rama4d","rama4dp8","rama2dp8"),.funs = as.numeric)
  ocu_resto = fread(file = monthly_files %>% str_subset(pattern = "Resto - Ocupados")) %>% clean_names()%>% mutate(periodo=2016,clase=2)%>% mutate_at(.vars = c("rama2d","rama4d","rama4dp8","rama2dp8"),.funs = as.numeric)
  
  ocu = bind_rows(ocu_cab,ocu_resto)
  
  ### deso
  deso_cab = fread(file = monthly_files %>% str_subset(pattern = "Cabecera - Desocupados")) %>% clean_names() %>% mutate(periodo=2016,clase=1)
  deso_resto =  fread(file = monthly_files %>% str_subset(pattern = "Resto - Desocupados")) %>% clean_names()%>% mutate(periodo=2016,clase=2)
  
  deso = bind_rows(deso_cab,deso_resto)
  
  ### Ft
  fuerza_cab = fread(file = monthly_files %>% str_subset(pattern = "Cabecera - Fuerza ")) %>% clean_names()%>% mutate(periodo=2016,clase=1)
  fuerza_resto =  fread(file = monthly_files %>% str_subset(pattern = "Resto - Fuerza de trabajo")) %>% clean_names()%>% mutate(periodo=2016,clase=2)
  
  fuerza = bind_rows(fuerza_cab,fuerza_resto)
  
  ### GEIH
  geih = left_join(x = cgn,y = ocu,by=c("directorio","secuencia_p","orden")) %>% 
    left_join(x = .,y = deso,by=c("directorio","secuencia_p","orden")) %>% 
    left_join(x = .,y = fuerza,by=c("directorio","secuencia_p","orden")) 
  
  ### clean
  geih=geih %>% select(-contains(".x.x"),-contains(".y.y"),-contains(".y"))
  
  ### return
  return(geih)
  
}

### Map
list_geih = map(.x = files,.f = read_geih,.progress = T) %>% rbindlist(use.names = T,fill = T)

### export
export(list_geih,"2_data_2016/output/geih_2016.rds")
