# Elecciones presidenciales Colombia 2018 #

Repositorio abierto de datos y algo de análsis pre-electoral para las presidenciales de Colombia en 2018.

Si quiere ver cómo va la intención de voto para cada candidato, [entre acá.](https://nelsonamayad.shinyapps.io/col2018_tend/)

Si quiere analizar los datos, en las carpetas arriba puede encontrar:

- Datos básicos de las encuestas y de los resultados electorales fáciles de usar (en CSV)
- Datos básicos de las encuestas que se hicieron para las elecciones 2014 y el plebiscito 2016
- Resultados electorales y pre-electorales (consultas, candidaturas por firmas, etc.)
- Fichas técnicas de las encuestas que se consiguen
- Más cosas, si el tiempo lo permite

Si usa R, importe las encuestas 2018 en 3 pasos:

- install.packages("RCurl")   #Instale RCurl
- library(RCurl)              #Cargue RCurl
- encuestas2018 <- read.csv(text=getURL("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv"))
