library(tidyverse)
library(extrafont)
library(scales)
library(plotly)
library(htmlwidgets)
library(showtext)
library(tint)
library(rgdal)
library(rgeos)
library(miniUI)
library(zoo)
library(leaflet)
library("Cairo")
library(htmltools)

censoagebsonora <- read_csv("datos/censoagebsonora.csv", 
                            col_types = cols(POBTOT = col_double(),
                                             POBFEM = col_double(),
                                             POBMAS = col_double(),
                                             P_18YMAS = col_double(),
                                             P_60YMAS = col_double(),
                                             P_5YMAS = col_double(),
                                             P_15YMAS = col_double(),
                                             P_12YMAS = col_double(),
                                             PSIND_LIM = col_double(),
                                             P15YM_AN = col_double(),
                                             P15YM_AN = col_double(),
                                             PEA = col_double(),
                                             PAFIL_OTRAI = col_double(),
                                             P12YM_SOLT = col_double(),
                                             PCATOLICA = col_double(),
                                             PDER_SS = col_double(),
                                             VPH_C_ELEC = col_double(),
                                             VPH_AGUADV = col_double(),
                                             VPH_AUTOM = col_double(),
                                             VPH_BICI = col_double(),
                                             VIVTOT = col_double(),
                                             TVIVHAB = col_double(),
                                             VIVPAR_HAB = col_double(),
                                             PROM_OCUP = col_double(),
                                             VPH_2YMASD = col_double(),
                                             VPH_INTER = col_double(),
                                             VPH_SINTIC = col_double()))
censoagebsonora [is.na(censoagebsonora)] <- 0
hmomza <- censoagebsonora %>% 
  select(ENTIDAD,NOM_ENT,MUN,NOM_MUN,LOC,NOM_LOC,AGEB,MZA, POBTOT, POBFEM, POBMAS, P_18YMAS, P_60YMAS, P_5YMAS, P_15YMAS, P_12YMAS, P15YM_AN, P15YM_AN, 
         PEA, PDER_SS, P12YM_SOLT, PCATOLICA, PDER_SS, VPH_C_ELEC,VPH_AGUADV, VPH_AUTOM, VPH_BICI, VIVTOT, TVIVHAB, VIVPAR_HAB,
         PROM_OCUP, VPH_2YMASD, VPH_INTER, VPH_SINTIC) %>% 
  unite(CVEGEO,c(ENTIDAD,MUN,LOC,AGEB,MZA),  sep = "", remove = TRUE) %>% 
  filter (NOM_LOC=="Hermosillo")
hmomza [is.na(hmomza )] <- 0 
hmomza <- hmomza %>% filter(POBTOT!=0)
hmomza <- hmomza %>% mutate(mujeres=round(POBFEM*100/POBTOT,1), hombres=round(POBMAS*100/POBTOT,1), 
                            P60=round(P_60YMAS*100/POBTOT,1), P18=round(P_18YMAS*100/POBTOT,1), lyr=round(P15YM_AN*100/P_15YMAS,1), pea=round(PEA*100/P_12YMAS,1), salud=round(PDER_SS*100/POBTOT,1),
                            solt=round(P12YM_SOLT*100/P_12YMAS,1), cato=round(PCATOLICA*100/POBTOT,1), auto=round(VPH_AUTOM*100/VIVPAR_HAB,1), 
                            bici=round(VPH_BICI*100/VIVPAR_HAB,1), TIC=round(VPH_SINTIC*100/VIVPAR_HAB,1),
                            v2rec=round(VPH_2YMASD*100/VIVPAR_HAB,1), vinter=round(VPH_INTER*100/VIVPAR_HAB,1))
hmomza <- hmomza %>% mutate(rec=if_else(v2rec>90,5, 
                           if_else(v2rec>75,4, 
                           if_else(v2rec>50,3,
                           if_else(v2rec>25,2,1)))),
                           inter=if_else(vinter>90,5, 
                            if_else(vinter>75,4, 
                            if_else(vinter>50,3,
                            if_else(vinter>25,2,1)))),
                           mujerp=if_else(mujeres>90,5, 
                                         if_else(mujeres>75,4, 
                                                 if_else(mujeres>50,3,
                                                         if_else(mujeres>25,2,1)))),
                           hombrep=if_else(hombres>90,5, 
                                         if_else(hombres>75,4, 
                                                 if_else(hombres>50,3,
                                                         if_else(hombres>25,2,1)))),
                           P60p=if_else(P60>90,5, 
                                          if_else(P60>75,4, 
                                                  if_else(P60>50,3,
                                                          if_else(P60>25,2,1)))),
                           P18p=if_else(P18>90,5, 
                                       if_else(P18>75,4, 
                                               if_else(P18>50,3,
                                                       if_else(P18>25,2,1)))),
                           lyrp=if_else(lyr>90,5, 
                                       if_else(lyr>75,4, 
                                               if_else(lyr>50,3,
                                                       if_else(lyr>25,2,1)))),
                           peap=if_else(pea>90,5, 
                                       if_else(pea>75,4, 
                                               if_else(pea>50,3,
                                                       if_else(pea>25,2,1)))),
                           saludp=if_else(salud>90,5, 
                                       if_else(salud>75,4, 
                                               if_else(salud>50,3,
                                                       if_else(salud>25,2,1)))),
                           soltp=if_else(solt>90,5, 
                                         if_else(solt>75,4, 
                                                 if_else(solt>50,3,
                                                         if_else(solt>25,2,1)))),
                           catop=if_else(cato>90,5, 
                                        if_else(cato>75,4, 
                                                if_else(cato>50,3,
                                                        if_else(cato>25,2,1)))),
                           autop=if_else(auto>90,5, 
                                         if_else(auto>75,4, 
                                                 if_else(auto>50,3,
                                                         if_else(auto>25,2,1)))),
                           bicip=if_else(bici>90,5, 
                                         if_else(bici>75,4, 
                                                 if_else(bici>50,3,
                                                         if_else(bici>25,2,1)))),
                           TICp=if_else(TIC>90,5, 
                                         if_else(TIC>75,4, 
                                                 if_else(TIC>50,3,
                                                         if_else(TIC>25,2,1)))))

capa_mza<- readOGR("Shapes", layer="HMOMZA",  encoding = "UTF-8", use_iconv=TRUE)

capa_mza <- capa_mza %>%  merge(hmomza)

incipal <-  colorFactor(c("#58BCBC", "#01A2AC", "#01787E", "#005155", "black"), levels= c("1","2","3","4","5"), na.color ="#e8e6e6")

labs <- c("0% - 25%","25% - 50%", "50% - 75%", "75% - 90%" , "90%-100%")

popup <- paste0(
  "<b>", as.character(capa_mza$CVEGEO), "</b>",     "<br>",
  "<b>", "Población total: ",           "</b>",   as.character(capa_mza$POBTOT),      "<br>",
  "<b>", "Población sexo femenino: ",           "</b>",   as.character(capa_mza$POBFEM),      "<br>",
  "<b>", "Población sexo masculino: ",           "</b>",   as.character(capa_mza$POBMAS),      "<br>",
  "<b>", "Mayores de 18: ",           "</b>",   capa_mza$P_18YMAS ,      "<br>", 
  "<b>", "Mayores de 60: ",           "</b>",   capa_mza$P_60YMAS,      "<br>",
  "<b>", "Promedio de ocupantes en viviendas particulares habitadas: ",           "</b>",   capa_mza$PROM_OCUP,      "<br>")  %>% lapply(htmltools::HTML)




mapahmo <- leaflet(capa_mza) %>% 
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addLayersControl( 
    baseGroups = c("% POBLACIÓN FEMENINA","% POBLACIÓN MASCULINA", "% POBLACIÓN MAYOR DE 60 AÑOS", "% POBLACIÓN MAYOR DE 18 AÑOS",
                   "% POBLACIÓN MAYOR DE 15 AÑOS ANALFABETA", "% POBLACIÓN ECONÓMICAMENTE ACTIVA", "% POBLACIÓN AFILIADA A SERVICIOS DE SALUD", 
                   "% POBLACIÓN RELIGIÓN CATÓLICA", "% POBLACIÓN MAYOR DE 12 AÑOS SOLTERA",
                   "% VIVIENDAS PARTICULARES HABITADAS QUE DISPONEN DE AUTO O CAMIONETA", "% VIVIENDAS PARTICULARES HABITADAS CON BICICLETA COMO MEDIO DE TRANSPORTE", 
                   "% VIVIENDAS PARTICULARES HABITADAS CON INTERNET", "% VIVIENDAS PARTICULARES HABITADAS SIN TIC's", 
                   "% VIVIENDAS PARTICULARES HABITADAS CON 2 DORMITORIOS O MÁS"), 
    options = layersControlOptions(collapsed = TRUE, position = "topleft")) %>% 
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$inter),
              color= "white",
              fillOpacity = 1,
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
              group= "% VIVIENDAS PARTICULARES HABITADAS CON INTERNET") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$rec),
              color= "white",
              fillOpacity = 1,
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
              group= "% VIVIENDAS HABITADAS CON 2 DORMITORIOS O MÁS") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$mujerp),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN FEMENINA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$hombrep),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN MASCULINA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$P60p),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN MAYOR DE 60 AÑOS") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$P18p),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN MAYOR DE 18 AÑOS") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$lyrp),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN MAYOR DE 15 AÑOS ANALFABETA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$peap),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN ECONÓMICAMENTE ACTIVA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$saludp),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN AFILIADA A SERVICIOS DE SALUD") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$catop),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN RELIGIÓN CATÓLICA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$soltp),
              color= "white",
              fillOpacity = 1,
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
              group= "% POBLACIÓN MAYOR DE 12 AÑOS SOLTERA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$autop),
              color= "white",
              fillOpacity = 1,
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
              group="% VIVIENDAS PARTICULARES HABITADAS QUE DISPONEN DE AUTO O CAMIONETA") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$bicip),
              color= "white",
              fillOpacity = 1,
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
              group= "% VIVIENDAS PARTICULARES HABITADAS CON BICICLETA COMO MEDIO DE TRANSPORTE") %>%
  addPolygons(data= capa_mza,
              stroke= TRUE,
              weight=0.2,                   
              opacity=1,
              fillColor = ~incipal(capa_mza$TICp),
              color= "white",
              fillOpacity = 1,
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
              group= "% VIVIENDAS PARTICULARES HABITADAS SIN TIC's") %>%
  addLegend(position = "topright", pal = incipal, values = ~capa_mza$inter, opacity=1, group= "% VIVIENDAS PARTICULARES HABITADAS CON INTERNET", 
            labFormat = function(type, cuts, p) {  
              paste0(labs)} ,
            title = "Respecto al total de la manzana", na.label = "N/A") 

save_html(mapahmo,"index.html", background = "white", libdir = "lib", lang = "en")
saveWidget(mapahmo,"index.html", selfcontained = F, libdir = "lib")

