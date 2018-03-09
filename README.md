# Elecciones presidenciales Colombia 2018 #

Vea las tendencias en la intencion de voto aca: [Sites Using React](https://nelsonamayad.shinyapps.io/col2018_tend/)

Repositorio abierto de análsis pre-electoral para las presidenciales de Colombia en 2018 con:

- Datos básicos de las encuestas
- Datos básicos de las encuestas que se hicieron para las elecciones 2014 y el plebiscito 2016
- Resultados electorales y pre-electorales (consultas, candidaturas por firmas, etc.)
- Fichas técnicas
- Más cosas, si el tiempo lo permite

Si usa R, importe las encuestas 2018 en 3 pasos:

- install.packages("RCurl")   #Instale RCurl
- library(RCurl)              #Cargue RCurl
- encuestas2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"))
