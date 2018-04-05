library(rgdal)   #para shapefiles
library(ggmap)   #plotea mapas en esto
library(ggplot2) #libreria grafica
library(plyr)    # manejo de datos
library(dplyr)   #para manejar como BD los dataframe
library(RColorBrewer)

setwd("~/Documents/Maestria/Visualizacion/TP1")

#Leo el Shapefile
dpto.shape <- readOGR(dsn="shapes",layer="departamentos")
dpto.shape@data$id <- rownames(dpto.shape@data)
#asigno la proyeccion al shapefile ( porque no la tiene ...)
proj4string(dpto.shape) <- CRS("+proj=longlat +datum=WGS84")
dpto.shape <- spTransform(dpto.shape, CRS("+proj=longlat +datum=WGS84"))
dpto.shape.df <- fortify(dpto.shape)

#data.frame para armar la tabla de conversion de deptos
dpto.id=na.omit(data.frame(id=rownames(dpto.shape@data),prov=dpto.shape$PROVINCIA,dpto=dpto.shape$DEPARTAMEN))

#Leo el Shapefile
prov.shape <- readOGR(dsn="shapes",layer="argentina")
#asigno la proyeccion al shapefile ( porque no la tiene ...)
proj4string(prov.shape) <- CRS("+proj=longlat +datum=WGS84")
prov.shape <- spTransform(prov.shape, CRS("+proj=longlat +datum=WGS84"))
prov.shape.df <- fortify(prov.shape)


#Leo los datos
incendios.df <- read.csv("enunciado/focosincendio-v10.csv",header=T,sep = ";",dec = ",")

#Agrupo los datos de incendios para tener "cant. de focos" y sumatoria de "area afectada"
incendios.dpto.r <- incendios.df %>% group_by(provincia_id,provincia,departamento_id,departamento) %>% summarize(n = n(),cnt.focos=sum(cant_focos),area.afectada=sum(sup_afectada))

incendios.dpto.id=data.frame(
  prov=chartr("ÁÉÍÓÚÑ", "AEIOUN",toupper(incendios.dpto.r$provincia)),
  dpto=chartr("ÁÉÍÓÚÑ", "AEIOUN",toupper(incendios.dpto.r$departamento))
)

dptos.transf=join(incendios.dpto.id, dpto.id, by = c("prov","dpto"), type = "left", match = "all")

incendios.dpto.r$id=dptos.transf$id



data.shape <- data.frame(id=rownames(dpto.shape@data), id=dpto.shape@data$id, stringsAsFactors=F)

df=join(as.data.frame(incendios.dpto.r),data.shape,by="id")


dpto.areas=join(dpto.shape.df,df,by="id")

dpto.areas=na.omit(dpto.areas)

#Ploteo las areas afectadas
areas = ggplot() +
  labs(title="Superficie afectada por incendios forestales \n Argentina (2011-2016) ", x="Longitud", y = "Latitud")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=12, face='bold'),
        axis.title.x = element_text(size=10,color='black',face="plain"),
        axis.title.y = element_text(size=10,color='black',face="plain"),
        panel.border = element_rect(colour = "black", fill=NA, size=3))+
  geom_polygon(data = dpto.shape.df, aes(x = long, y = lat,  group=group ),color = 'grey50',fill="white", size = .2,alpha=1) +
  geom_polygon(data = dpto.areas, aes(x = long, y = lat,  group=id,fill=area.afectada),color="grey60",size = .2,alpha=1)+
  
  scale_fill_gradient(name = "Hectareas", trans = "sqrt",
                      low = "grey90",high = "darkred",
                      limits=c(0,2500),
                      breaks = c(0,5,30,250,2500),
                      labels = c(0,5,30,250,2500))+
  guides(fill = guide_colorbar(barheight = unit(8, "cm"),barwidth = unit(0.6, "cm"),title.hjust=0.5,ticks=TRUE,nbin=20,draw.ulim=TRUE,draw.llim=TRUE))+
  
  geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .5,alpha=1) +
  coord_map()

areas

#Ploteo los focos de incendio
focos = ggplot() +
  labs(title="Cantidad de focos de incendios forestales \n Argentina (2011-2016) ", x="Longitud", y = "Latitud")+
  theme(plot.title = element_text(hjust = 0.5),
        title =element_text(size=12, face='bold'),
        axis.title.x = element_text(size=12,color='black',face="plain"),
        axis.title.y = element_text(size=12,color='black',face="plain"),
        panel.border = element_rect(colour = "black", fill=NA, size=3))+
  geom_polygon(data = dpto.shape.df, aes(x = long, y = lat,  group=group ),color = 'grey50',fill=NA, size = .2,alpha=1) +
  geom_polygon(data = dpto.areas, aes(x = long, y = lat,  group=group,fill=cnt.focos),color="grey60",size = .2,alpha=1)+
  
  scale_fill_gradient(name = "Focos", trans = "log",na.value = "white",
                      low = "white",high = "darkred",
                      limits=c(0.1,550),
                      breaks = c(0.1,1,3,7,550),
                      labels = c(0,1,3,7,550))+
  guides(fill = guide_colorbar(barheight = unit(8, "cm"),barwidth = unit(0.6, "cm"),title.hjust=0.5,ticks=TRUE,nbin=20,draw.ulim=TRUE,draw.llim=TRUE))+
  
  geom_polygon(data = prov.shape.df, aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = .5,alpha=1) +
  coord_map()

focos





