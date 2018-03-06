#################
# MAPAS COL2018 #
#################

  #1. Instalar y cargar paquetes R
  #2. Alistamiento datos
  #3. Mapas

######################## PAQUETES ############################

rm(list = ls()) #Limpiar ambiente R
library(devtools) #Instalar paquete de desarrollo antes de comenzar
install_github("espanta/lubripack") #Instalar lubripack para instalar y cargar multiples paquetes
lubripack::lubripack("readxl","broom","maps","sp","tidyverse","maptools","RColorBrewer")

######################## ALISTAMIENTO ########################

options(encoding = "UTF-8")

#######################
# MAPA DEPARTAMENTAL  #
#######################

#Mapa departamental:
md <- readShapeSpatial("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Maps/Mapas Colombia/COL_adm1.shp")
md <- tidy(md)

###################
# CENSO ELECTORAL #
###################

#Censo electoral por departamento 2018
censo <- read_excel("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Datos/Registraduria/censo_electoral.xlsx", sheet = "Sheet1")
censo <- select(censo, id, votantes)
censo <- filter(censo, id!="NA")
censo <- arrange(censo, id)

#Combinacion
md_censo <- merge(md, censo, by = "id")

#Mapa:
ggplot(data=md_censo, aes(x=long, y=lat, group = group, fill = votantes)) +
  geom_polygon(color = "grey60") +
  labs(x="",y="", title = "Colombia", subtitle = "Censo electoral 2018", caption = "Fuente: Calculos propios con base en datos de la Registraduria", fill = "") +
  scale_x_continuous(limits=c(-80,-65))+
  scale_y_continuous(limits=c(-4.5,13)) +
  theme(legend.position = "bottom", legend.text = element_blank(), panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_fill_distiller(palette = "Spectral", direction = -1)
  
############################################
# 1era y 2da vuelta 2014 por departamentos #
############################################

#Datos panel CEDE a nivel departamental
pres_1era_d <- read_excel("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Datos/Panel electoral CEDE/1era_vuelta_2014_depto.xlsx")
pres_2da_d <- read_excel("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Datos/Panel electoral CEDE/2da_vuelta_2014_depto.xlsx")
pres_1era_d <- pres_1era_d %>% filter(candidato!="VOTOS NULOS" & candidato!="TARJETAS N0 MARCADAS" & candidato!="VOTOS EN BLANCO")
pres_2da_d <- pres_2da_d %>% filter(candidato!="VOTOS NULOS" & candidato!="TARJETAS N0 MARCADAS" & candidato!="VOTOS EN BLANCO")

list <- c("pres_1era","pres_2da") 

pres_1era_d$candidato[pres_1era_d$candidato == "LOPEZ"] <- "López"
pres_1era_d$candidato[pres_1era_d$candidato == "PENALOSA"] <- "Peñalosa"
pres_1era_d$candidato[pres_1era_d$candidato == "RAMIREZ"] <- "Ramirez"
pres_1era_d$candidato[pres_1era_d$candidato == "SANTOS"] <- "Santos"
pres_1era_d$candidato[pres_1era_d$candidato == "ZULUAGA"] <- "Zuluaga"
pres_2da_d$candidato[pres_2da_d$candidato == "SANTOS"] <- "Santos"
pres_2da_d$candidato[pres_2da_d$candidato == "ZULUAGA"] <- "Zuluaga"

#Merge
md_1era <- merge(md, pres_1era_d, by = "id")
md_2da <- merge(md, pres_2da_d, by = "id")

#Primera vuelta:
ggplot(data=md_1era, aes(x=long, y=lat, group = group, fill = votos)) +
  geom_polygon(color = "grey60") +
  labs(x="",y="", title = "Primera vuelta en 2014", subtitle = "Densidad de votación por candidato", caption = "Fuente: Calculos propios con base en datos panel CEDE", fill = "") +
  scale_x_continuous(limits=c(-80,-65))+
  scale_y_continuous(limits=c(-4.5,13)) +
  theme(legend.position = "bottom", legend.text = element_blank(), panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  facet_wrap(~candidato, nrow = 1)

#Segunda vuelta:
ggplot(data=md_2da, aes(x=long, y=lat, group = group, fill = votos)) +
  geom_polygon(color = "grey60") +
  labs(x="",y="", title = "Segunda vuelta en 2014", subtitle = "Densidad de votación por candidato", caption = "Fuente: Calculos propios con base en datos panel CEDE", fill = "") +
  scale_x_continuous(limits=c(-80,-65))+
  scale_y_continuous(limits=c(-4.5,13)) +
  theme(legend.position = "bottom", legend.text = element_blank(), panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  facet_wrap(~candidato, nrow = 1)

#Ramirez
ggplot(data=filter(md_1era, candidato=="Ramirez"), aes(x=long, y=lat, group = group, fill = votos)) +
  geom_polygon(color = "grey60") +
  labs(x="",y="", title = "Marta Lucía Ramirez", subtitle = "1era vuelta 2014, densidad de votos", caption = "Fuente: Calculos propios con base en datos panel CEDE", fill = "") +
  scale_x_continuous(limits=c(-80,-65))+
  scale_y_continuous(limits=c(-4.5,12)) +
  theme(legend.position = "bottom", legend.text = element_blank(), panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette = "Spectral", direction = -1) 

##################
# MAPA MUNICIPAL #
##################
load("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Maps/Municipios_GIS.RData")
mun <- tidy(Municipios)

----------------
#Test
g <- data.frame(id = unique(mun[ , c("id")]))
g[ , "x"] <- runif(nrow(g), 0, 1)

test <- merge(mun, g, by = "id")

ggplot(data=test, aes(x=long, y=lat, group = group, fill = x)) +
  geom_polygon(color = "black") + 
  labs(x="", y = "") +
  scale_x_continuous(limits=c(-2300000,-750000))+
  scale_y_continuous(limits=c(3000000, 4900000)) +
  theme(legend.position = "none", legend.text = element_blank(), panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette = "Spectral", direction = -1) 
----------------
  
#Codigos DANE: Sacar codigos de la lista y fusionarlos de nuevo con ID
cods <- as.data.frame(Municipios)
cods <- select(cods, OBJECTID, CODANE2, CodigoDane, MPIO_CNMBR)
colnames(cods)[1] <- "id"

mm <- merge(mun, cods, by = "id")
colnames(mm)[colnames(mm)=="id"] <- "id2"
colnames(mm)[colnames(mm)=="CODANE2"] <- "id"

##############################
# 1era vuelta por municipios #
##############################

pres_1era_mun <- read_excel("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Datos/Panel electoral CEDE/1era_vuelta_2014_mun.xlsx")
colnames(pres_1era_mun)[1] <- "id"
colnames(pres_1era_mun)[3] <- "candidato"
pres_1era_mun <- pres_1era_mun %>% group_by(municipio) %>% mutate(tot_mun = sum(votos))
pres_1era_mun <- pres_1era_mun %>% group_by(municipio) %>% mutate(p_mun = 100*votos/tot_mun)

#Ej: Clara Lopez
cl <- filter(pres_1era_mun, candidato == "LOPEZ")
cl <- cl %>% group_by(id) %>% summarise(votos = sum(votos), tot_mun, p_mun)
cl <- merge(mm, cl, by = "id")

#Mapa Clara Lopez
ggplot(data=cl, aes(x=long, y=lat, group=group, fill=p_mun)) +
  geom_polygon() + 
  scale_x_continuous(limits=c(-2300000,-750000))+
  scale_y_continuous(limits=c(3000000, 4900000)) +
  labs(x="", y="", title="Clara López en 1era vuelta 2014", subtitle="% votos validos por municipio", caption="Fuente: Panel electoral CEDE") +
  theme(legend.position="right", legend.title=element_blank(), panel.background=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette="YlOrRd", direction = 1) 

########################
# 2da vuelta municipal #
########################

pres_2da_mun <- read_excel("C:/Users/Nelson Amaya/Google Drive/Projects/Modelo COL2018/Datos/Panel electoral CEDE/2da_vuelta_2014_mun.xlsx")
colnames(pres_2da_mun)[1] <- "id"
colnames(pres_2da_mun)[3] <- "candidato"

jms <- filter(pres_2da_mun, candidato == "SANTOS")
jms <- jms %>% group_by(id) %>% summarise(votos = sum(votos))
jms <- merge(mm, jms, by = "id")

ggplot(data=jms, aes(x=long, y=lat, group = group, fill = votos)) +
  geom_polygon() + 
  scale_x_continuous(limits=c(-2300000,-750000))+
  scale_y_continuous(limits=c(3000000, 4900000)) +
  labs(x="", y = "", title = "Juan M. Santos en 2da vuelta", subtitle = "Densidad de votos por municipio", caption = "Fuente: Panel electoral CEDE") +
  theme(legend.position = "none", legend.text = element_blank(), panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  scale_fill_distiller(palette = "Oranges", direction = 1) 