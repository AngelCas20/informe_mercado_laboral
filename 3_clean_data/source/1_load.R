cat("\f")
rm(list=ls())
library(pacman)
p_load(tidyverse,
       janitor,
       data.table,
       dtplyr,
       rio)

### load data
geih_2016 = import("2_data_2016/output/geih_2016.rds") %>% select(directorio,secuencia_p,orden,sexo=p6020,edad=p6040,esc=p6210,clase=clase.x,dpto=dpto.x,salud=p6090,pension=p6920,fex_2011=fex_c_2011.x,ft,ocu=oci,deso=dsi,tiene_contrato=p6440,contrato_verbal_escrito=p6450,contrato_def_indef=p6460,horas_semanales=p6800,dur_desempleo=p7250,subsidio_desempleo=p9460,inglabo) %>% tibble() %>% mutate(pet=ifelse(edad>10,1,0),year=2016)


### load data
geih_2023 = import("1_data/output/geih_2023.rds")%>% select(mes=mes.x,directorio,secuencia_p,orden,sexo=p3271,edad=p6040,esc=p3042,clase=clase.x,dpto=dpto.x,salud=p6090,pension=p6920,pet,fex_2018=fex_c18.x,ft=ft.x,ocu=oci,deso=dsi,tiene_contrato=p6440,contrato_verbal_escrito=p6450,contrato_def_indef=p6460,horas_semanales=p6800,dur_desempleo=p7250,subsidio_desempleo=p9460,inglabo)%>% tibble() %>% mutate(year=2023,fex_2018=fex_2018/3)
geih_2023 = geih_2023 %>% subset(mes==3)


### Correct Expansion Factor
geih_2016$fex_2011 = gsub(",","\\.",geih_2016$fex_2011) %>% as.numeric()/3
geih_2016$fex_2011 %>% sum()


### Clean Variables
geih_2016=geih_2016 %>% 
  mutate(sexo=case_when(sexo==1~"Hombre",
                        sexo==2~"Mujer"),
         clase=case_when(clase==1~"Urbano",
                         clase==2~"Rural"),
         esc=case_when(esc==1 ~"Ninguno",
                       esc==2 ~"Preescolar",
                       esc==3 ~"Primaria",
                       esc==4 ~"Secundaria",
                       esc==5 ~"Media",
                       esc==6 ~"Superior"),
         salud=case_when(salud==1 ~"Si",
                         salud==2~"No"),
         grupo_etario = case_when(edad<15~"15<",
                                  between(edad,15,19)~"15-19",
                                  between(edad,20,24)~"20-24",
                                  between(edad,25,29)~"25-29",
                                  between(edad,30,34)~"30-34",
                                  between(edad,35,39)~"35-39",
                                  between(edad,40,44)~"40-44",
                                  between(edad,45,49)~"45-49",
                                  between(edad,50,54)~"50-54",
                                  between(edad,55,59)~"55-59",
                                  between(edad,60,65)~"60-65",
                                  edad>65 ~"65+"),
         pension=case_when(pension==1 ~"Cotiza Pensión",
                           pension==2 ~"No Cotiza Pensión",
                           pension==3 ~"Pensionado"),
         tiene_contrato=case_when(tiene_contrato==1 ~"Si",
                                  tiene_contrato==2 ~"No"),
         contrato_verbal_escrito=case_when(contrato_verbal_escrito==1 ~"Si",
                                           contrato_verbal_escrito==2~"No",
                                           contrato_verbal_escrito==3~"Ns/Nr"),
         contrato_def_indef=case_when(contrato_def_indef==1 ~"Termino Indefinido",
                                      contrato_def_indef==2 ~"Termino Fijo",
                                      contrato_def_indef==3 ~"Ns/Nr"),
         subsidio_desempleo=case_when(subsidio_desempleo=="1" ~"Si",
                                      subsidio_desempleo=="2"~"No",
                                      subsidio_desempleo=="9" ~"Ns/Nr")) %>% 
  mutate_at(.vars = c("sexo","clase","esc","salud","grupo_etario","pension","contrato_verbal_escrito","contrato_def_indef","tiene_contrato","subsidio_desempleo"),.funs = as_factor)

#======#
# 2016 #
#======#

### TBP
((geih_2016$pet*geih_2016$fex_2011) %>% sum())/ geih_2016$fex_2011 %>% sum()

### TGP
(((geih_2016$ft * geih_2016$fex_2011)) %>% sum(na.rm = T))/  (((geih_2016$pet * geih_2016$fex_2011)) %>% sum(na.rm = T))

### T Desemplo
(geih_2016$deso*geih_2016$fex_2011) %>% sum(na.rm = T) /(((geih_2016$ft * geih_2016$fex_2011)) %>% sum(na.rm = T))

### T Ocupacion
(geih_2016$ocu*geih_2016$fex_2011) %>% sum(na.rm = T) /(((geih_2016$ft * geih_2016$fex_2011)) %>% sum(na.rm = T))

### Por sexo
fct_count(f = geih_2016$sexo,prop = T,sort = T)

### INgresos por sexo
geih_2016 %>% group_by(sexo) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2011),
                                           ingresos_median=median(inglabo,na.rm=T),
                                           ingresos_sd=sd(inglabo,na.rm=T))

### Ingresos por Nivel Eductivo
geih_2016 %>% group_by(esc) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2011),
                                           ingresos_median=median(inglabo,na.rm=T),
                                          ingresos_sd=sd(inglabo,na.rm=T))

### iNgresos por nivel educativo y urbano/rural
geih_2016 %>% group_by(sexo,clase) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2011),
                                          ingresos_median=median(inglabo,na.rm=T),
                                          ingresos_sd=sd(inglabo,na.rm=T))

### Ingresos por grupo etario y sexo
geih_2016 %>% group_by(sexo,grupo_etario) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2011),
                                           ingresos_median=median(inglabo,na.rm=T),
                                           ingresos_sd=sd(inglabo,na.rm=T)) %>% subset(sexo=="Mujer")

### Seguro de Salud
fct_count(geih_2016$salud,prop = T)

### SUbset ocupados
a=geih_2016 %>% subset(ocu==1)

### Cotiza Pension
fct_count(a$pension,prop = T)

### Tiene contrato
fct_count(a$tiene_contrato,prop = T)

### Contrato Formal
a_contrato = a %>% subset(tiene_contrato=="Si")
fct_count(a_contrato$contrato_def_indef,prop = T)

### Contrato Verba no Verbal
fct_count(a_contrato$contrato_verbal_escrito,prop = T)

### Horas trabajadas semanalmente
geih_2016 %>% 
  group_by(sexo) %>% 
  summarise(horas_promedio=mean(horas_semanales,na.rm = T),
            horas_mediana=median(horas_semanales,na.rm = T),
            horas_sd=sd(horas_semanales,na.rm = T))

### Horas trabajadas semanalmente
geih_2016 %>% 
  group_by(sexo,clase) %>% 
  summarise(horas_promedio=mean(horas_semanales,na.rm = T),
            horas_mediana=median(horas_semanales,na.rm = T),
            horas_sd=sd(horas_semanales,na.rm = T))

### TIempo Sin trabajar meses
geih_2016 %>% 
  group_by(sexo) %>% 
  summarise(dur_desempleo_promedio=mean(dur_desempleo,na.rm = T),
            dur_desempleo_mediana=median(dur_desempleo,na.rm = T),
            dur_desempleo_sd=sd(dur_desempleo,na.rm = T)) %>% 
  mutate_if(is.numeric,function(x) x/4)

### Subsidio
b_deso = geih_2016  %>% subset(deso==1)
fct_count(b_deso$subsidio_desempleo,prop = T)


#======#
# 2023#
#=====#

geih_2023= geih_2023%>% 
  mutate(sexo=case_when(sexo==1~"Hombre",
                        sexo==2~"Mujer"),
         clase=case_when(clase==1~"Urbano",
                         clase==2~"Rural"),
         esc=case_when(esc==1 ~"Ninguno",
                       esc==2 ~"Preescolar",
                       esc==3 ~"Primaria",
                       esc==4 ~"Secundaria",
                       esc==5 ~"Media",
                       esc==6 ~"Superior"),
         salud=case_when(salud==1 ~"Si",
                         salud==2~"No"),
         grupo_etario = case_when(edad<15~"15<",
                                  between(edad,15,19)~"15-19",
                                  between(edad,20,24)~"20-24",
                                  between(edad,25,29)~"25-29",
                                  between(edad,30,34)~"30-34",
                                  between(edad,35,39)~"35-39",
                                  between(edad,40,44)~"40-44",
                                  between(edad,45,49)~"45-49",
                                  between(edad,50,54)~"50-54",
                                  between(edad,55,59)~"55-59",
                                  between(edad,60,65)~"60-65",
                                  edad>65 ~"65+"),
         pension=case_when(pension==1 ~"Cotiza Pensión",
                           pension==2 ~"No Cotiza Pensión",
                           pension==3 ~"Pensionado"),
         tiene_contrato=case_when(tiene_contrato==1 ~"Si",
                                  tiene_contrato==2 ~"No"),
         contrato_verbal_escrito=case_when(contrato_verbal_escrito==1 ~"Si",
                                           contrato_verbal_escrito==2~"No",
                                           contrato_verbal_escrito==3~"Ns/Nr"),
         contrato_def_indef=case_when(contrato_def_indef==1 ~"Termino Indefinido",
                                      contrato_def_indef==2 ~"Termino Fijo",
                                      contrato_def_indef==3 ~"Ns/Nr"),
         subsidio_desempleo=case_when(subsidio_desempleo=="1" ~"Si",
                                      subsidio_desempleo=="2"~"No",
                                      subsidio_desempleo=="9" ~"Ns/Nr")) %>% 
  mutate_at(.vars = c("sexo","clase","esc","salud","grupo_etario","pension","contrato_verbal_escrito","contrato_def_indef","tiene_contrato","subsidio_desempleo"),.funs = as_factor)


### TBP
((geih_2023$pet*geih_2023$fex_2018) %>% sum(na.rm=T))/ geih_2023$fex_2018 %>% sum()

### TGP
(((geih_2023$ft * geih_2023$fex_2018)) %>% sum(na.rm = T))/  (((geih_2023$pet * geih_2023$fex_2018)) %>% sum(na.rm = T))

### T Desemplo
(geih_2023$deso*geih_2023$fex_2018) %>% sum(na.rm = T) /(((geih_2023$ft * geih_2023$fex_2018)) %>% sum(na.rm = T))

### T Ocupacion
(geih_2023$ocu*geih_2023$fex_2018) %>% sum(na.rm = T) /(((geih_2023$ft * geih_2023$fex_2018)) %>% sum(na.rm = T))

### Por sexo
fct_count(f = geih_2023$sexo,prop = T,sort = T)

### INgresos por sexo
geih_2023 %>% group_by(sexo) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2018),
                                           ingresos_median=median(inglabo,na.rm=T),
                                           ingresos_sd=sd(inglabo,na.rm=T))

### Ingresos por Nivel Eductivo
geih_2023 %>% group_by(esc) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2018),
                                          ingresos_median=median(inglabo,na.rm=T),
                                          ingresos_sd=sd(inglabo,na.rm=T))

### iNgresos por nivel educativo y urbano/rural
geih_2023 %>% group_by(sexo,clase) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2018),
                                                 ingresos_median=median(inglabo,na.rm=T),
                                                 ingresos_sd=sd(inglabo,na.rm=T))

### Ingresos por grupo etario y sexo
geih_2023 %>% group_by(sexo,grupo_etario) %>% summarise(ingresos_mean=weighted.mean(inglabo,na.rm = T,w=fex_2018),
                                                        ingresos_median=median(inglabo,na.rm=T),
                                                        ingresos_sd=sd(inglabo,na.rm=T)) %>% subset(sexo=="Mujer")

### Seguro de Salud
fct_count(geih_2023$salud,prop = T)

### SUbset ocupados
a=geih_2023 %>% subset(ocu==1)

### Cotiza Pension
fct_count(a$pension,prop = T)

### Tiene contrato
fct_count(a$tiene_contrato,prop = T)

### Contrato Formal
a_contrato = a %>% subset(tiene_contrato=="Si")
fct_count(a_contrato$contrato_def_indef,prop = T)

### Contrato Verba no Verbal
fct_count(a_contrato$contrato_verbal_escrito,prop = T)

### Horas trabajadas semanalmente
geih_2023 %>% 
  group_by(sexo) %>% 
  summarise(horas_promedio=mean(horas_semanales,na.rm = T),
            horas_mediana=median(horas_semanales,na.rm = T),
            horas_sd=sd(horas_semanales,na.rm = T))

### Horas trabajadas semanalmente
geih_2023 %>% 
  group_by(sexo,clase) %>% 
  summarise(horas_promedio=mean(horas_semanales,na.rm = T),
            horas_mediana=median(horas_semanales,na.rm = T),
            horas_sd=sd(horas_semanales,na.rm = T))

### TIempo Sin trabajar meses
geih_2023 %>% 
  group_by(sexo) %>% 
  summarise(dur_desempleo_promedio=mean(dur_desempleo,na.rm = T),
            dur_desempleo_mediana=median(dur_desempleo,na.rm = T),
            dur_desempleo_sd=sd(dur_desempleo,na.rm = T)) %>% 
  mutate_if(is.numeric,function(x) x/4)

### Subsidio
b_deso = geih_2023  %>% subset(deso==1)
fct_count(b_deso$subsidio_desempleo,prop = T)

#===============#
# Create Df with Mesures


