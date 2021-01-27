library(tidyverse)
library(extrafont)
library(scales)
library(plotly)
library(htmlwidgets)
library(showtext)
library(tint)
library(rgdal)
library(rgeos)
library(ggiraph)
library(miniUI)
library(units)
library(reactable)
library(lubridate)
library(zoo)
library(leaflet)
library("Cairo")
library(htmltools)
library(rcartocolor)


censoagebsonora <- read_csv("datos/censoagebsonora.csv", 
                            col_types = cols(POBTOT = col_double(),
                                             POBFEM = col_double(),
                                             POBMAS = col_double(),
                                             P_18YMAS = col_double(),
                                             P_18YMAS_F = col_double(),
                                             P_18YMAS_M = col_double(),
                                             P_60YMAS = col_double(),
                                             P_60YMAS_F = col_double(),
                                             P_60YMAS_M = col_double()))
censoagebsonora [is.na(censoagebsonora)] <- 0
hmoageb <- censoagebsonora %>% 
  filter (MUN=="030", LOC=="0001", NOM_LOC=="Hermosillo") %>% group_by(NOM_LOC, AGEB) %>% 
  summarise (POBTOT=sum(POBTOT),POBFEM=sum(POBFEM),POBMAS=sum(POBMAS),P_18YMAS=sum(P_18YMAS),P_18YMAS_F=sum(P_18YMAS_F), P_18YMAS_M=sum(P_18YMAS_M),P_60YMAS=sum(P_60YMAS),  P_60YMAS_F=sum(P_60YMAS_F), P_60YMAS_M=sum(P_60YMAS_M))
hmoageb [is.na(hmoageb )] <- 0

hmotot <- hmoageb %>% 
  group_by(NOM_LOC) %>% 
  summarise(HPOBTOT=sum(POBTOT), HPOBFEM=sum(POBFEM), HPOBMAS=sum(POBMAS), HP_18YMAS=sum(P_18YMAS), HP_18YMAS_F=sum(P_18YMAS_F), HP_18YMAS_M=sum(P_18YMAS_M), HP_60YMAS=sum(P_60YMAS), HP_60YMAS_F=sum(P_60YMAS_F), HP_60YMAS_M=sum(P_60YMAS_M))

hmoageb <- hmoageb %>% left_join(hmotot, by = "NOM_LOC") 
#hmoageb[hmoageb == 0] <- NA
hmoageb <- hmoageb %>% mutate(ppobtot=round(POBTOT*1000/HPOBTOT,1), pp18=round(P_18YMAS*1000/HP_18YMAS,1), pp60=round(P_60YMAS*1000/HP_60YMAS,1)) %>% rename(CVE_AGEB=AGEB)

quantile(hmoageb$ppobtot, seq(0,1, 0.05), na.rm=TRUE)
pMuyalto <- quantile(hmoageb$ppobtot, 0.80, na.rm=TRUE)
pAlto <- quantile(hmoageb$ppobtot, 0.60, na.rm=TRUE)
pMedio <- quantile(hmoageb$ppobtot, 0.40, na.rm=TRUE)
pBajo <- quantile(hmoageb$ppobtot, 0.20, na.rm=TRUE)

jMuyalto <- quantile(hmoageb$pp18, 0.80, na.rm=TRUE)
jAlto <- quantile(hmoageb$pp18, 0.60, na.rm=TRUE)
jMedio <- quantile(hmoageb$pp18, 0.40, na.rm=TRUE)
jBajo <- quantile(hmoageb$pp18, 0.20, na.rm=TRUE)

sMuyalto <- quantile(hmoageb$pp60, 0.80, na.rm=TRUE)
sAlto <- quantile(hmoageb$pp60, 0.60, na.rm=TRUE)
sMedio <- quantile(hmoageb$pp60, 0.40, na.rm=TRUE)
sBajo <- quantile(hmoageb$pp60, 0.20, na.rm=TRUE)

hmoageb <- hmoageb %>% mutate(ptot=if_else(ppobtot>(round(pMuyalto,0)),5, 
                                                 if_else(ppobtot>(round(pAlto,0)),4, 
                                                         if_else(ppobtot>(round(pMedio,0)),3,
                                                                 if_else(ppobtot>(round(pBajo,0)),2,1))))) %>%
  mutate(p18=if_else(pp18>(round(pMuyalto,0)),5, 
                      if_else(pp18>(round(pAlto,0)),4, 
                              if_else(pp18>(round(pMedio,0)),3,
                                      if_else(pp18>(round(pBajo,0)),2,1))))) %>% 
  mutate(p60=if_else(pp60>(round(pMuyalto,0)),5, 
                     if_else(pp60>(round(pAlto,0)),4, 
                             if_else(pp60>(round(pMedio,0)),3,
                                     if_else(pp60>(round(pBajo,0)),2,1)))))
  

capa_munison <- readOGR("Shapes", layer="HERMOSILLOCD",  encoding = "UTF-8", use_iconv=TRUE)

capa_munison <- capa_munison %>%  merge(hmoageb)

incipal <-  colorFactor(c("#58BCBC", "#01A2AC", "#01787E", "#005155", "black"), levels= c("1","2","3","4","5"), na.color ="#e8e6e6")

labs <- c("0.0 - 0.20","0.20 - 0.80", "0.80 - 1.94", "1.94 - 3.10" , "+3.10")

popup <- paste0(
  "<b>", as.character(capa_munison$NOMMAY), "</b>",     "<br>",                     
  "<b>", "AGEB: ",        "</b>",   as.character(capa_munison$CVE_AGEB)   ,      "<br>",
  "<b>", "Población total: ",           "</b>",   as.character(capa_munison$POBTOT),      "<br>", 
  "<b>", "Mayores de 18: ",           "</b>",   capa_munison$P_18YMAS ,      "<br>", 
  "<b>", "Mayores de 60: ",           "</b>",   capa_munison$P_60YMAS,      "<br>")  %>% lapply(htmltools::HTML)




mapahmo <- leaflet(capa_munison) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addLayersControl( 
    baseGroups = c("POBLACIÓN TOTAL", "MAYORES DE 18 AÑOS", "MAYORES DE 60 AÑOS"), 
    options = layersControlOptions(collapsed = FALSE, position = "topright")) %>% 
  addPolygons(data= capa_munison,
              stroke= TRUE,
              weight=1.2,                   
              opacity=1,
              fillColor = ~incipal(capa_munison$ptot),
              color= "white",
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "top",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "11px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "POBLACIÓN TOTAL") %>%
  addPolygons(data= capa_munison,
              stroke= TRUE,
              weight=1.2,                   
              opacity=1,
              fillColor = ~incipal(capa_munison$p18),
              color= "white",
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "top",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "11px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "MAYORES DE 18 AÑOS") %>%
  addPolygons(data= capa_munison,
              stroke= TRUE,
              weight=1.2,                   
              opacity=1,
              fillColor = ~incipal(capa_munison$p60),
              color= "white",
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              highlightOptions = highlightOptions(color = "black", 
                                                  weight = 1.2,
                                                  bringToFront = TRUE),
              label=popup, 
              labelOptions = labelOptions(noHide = F, direction = "top",
                                          style = list(
                                            "color" = "black",
                                            "font-family" = "Lato",
                                            "font-style" = "regular",
                                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                                            "font-size" = "11px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          )),
              group= "MAYORES DE 60 AÑOS") %>%

  addLegend(position = "topright", pal = incipal, values = ~capa_munison$ptot, opacity=1, group= "POBLACIÓN TOTAL", 
            labFormat = function(type, cuts, p) {  
              paste0(labs)} ,
            title = "Residentes por 1000 habitantes", na.label = "N/A") 

mapahmo

