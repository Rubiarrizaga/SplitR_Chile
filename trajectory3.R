setwd("C:/Users/Usuario/Desktop/trjectory_3")

######HYSPLIT Backtrajectory Analysis######

#install.packages("xlsx",dependencies = T)
#install.packages("readxl",dependencies = T)
#install_github("rich-iannone/splitr")
#install.packages("readxlsx",dependencies = T)



library(sf)
library(readxl)
#library(readxlsx)
library(xlsx)
library(splitr)
library(lubridate)
library(plyr)
library(foreign)
#library(devtools)

#Prevent timing out
getOption('timeout')
options(timeout=10000)

#Tabla excel con los datos de los incendios
data=read.xlsx("pruebainc201hc.xlsx",sheetName = "pruebainc200hc")
IDF=data$Nombre.IF;IDF
LAT=data$LAT;LAT
LON=data$LON;LON
DUR=as.numeric(data$X.Duración..horas.);DUR
INI=data$Inicio;INI
EXT=data$Extinción;EXT

#Variables para agregar
RegI=data$Región
ComI=data$Comuna
CausagI=data$Causa.general
CausaeI=data$Causa.específica
Pino0a10=data$Pino.0.a.10
Pino11a17=data$Pino.11.a.17
Pino18omas=data$Pino.18.o.más
Eucalipto=data$Eucalipto
Otrasplantaciones=data$Otras.plantaciones
Subtotalplantaciones=data$Subtotal.plantaciones	
Arbolado=data$Arbolado	
Matorral=data$Matorral	
Pastizal=data$Pastizal	
Subtotalvegetacionnatural=data$Subtotal.vegetación.natural	
Agricola=data$Agrícola	
Desechos=data$Desechos	
Subtotalotrassuperficies=data$Subtotal.otras.superficies
Superficietotal=data$Superficie.total
Hora_I=data$Hora_I
Hora_F=data$Hora_F
Temperatura=data$Temperatura
Humedad=data$Humedad	
Exposicion=data$Exposición	
Direccion=data$Dirección.viento 
Velocidad=data$Velocidad.viento 
Topografia=data$Topografía	
Pendiente=data$Pendiente






#Probando los datos
is.na(EXT[974])
EXT[3]

#Conteo de incendios con datos suficientes
row_data=nrow(data)

#HYSPLIT
incendio_logical=c()
for(i in 1:row_data){
  incendio_logical=c(incendio_logical, is.na(LAT[i])|is.na(LON[i])|is.na(DUR[i])|is.na(INI[i])|is.na(EXT[i]))
}
table(incendio_logical)

mat = matrix(ncol = 43, nrow = 0)
df=data.frame(mat)


for(i in 1:row_data){
  if(incendio_logical[i]==FALSE){
    trajectory_model <-hysplit_trajectory(
      lat = LAT[i],                  
      lon = LON[i],
      height = 30,
      duration = DUR[i],
      met_type = "gdas1",
      model_height= 500,
      direction	= "forward",
      days = seq(
        lubridate::ymd(INI[i]),
        lubridate::ymd(EXT[i]),
        by = "1 day"
      ),
      daily_hours = c(0,8,16)
    )
    trajectory_model_complete=trajectory_model
    filas=nrow(trajectory_model_complete)
  }   
  else {
    vacio = matrix(ncol = 13, nrow = 0)
    trajectory_model_complete=data.frame(vacio)
    filas=1
    
    lat=LAT[i]
    lon=LON[i]
    
    
    if(is.null(DUR[i])==FALSE){
      duration=DUR[i]
    }
    else {
      duration="-"
    }
    
    if(is.null(INI[i])==FALSE){
      ini=INI[i]
    }
    else {
      ini="-"
    }
    
    if(is.null(EXT[i])==FALSE){
      ext=EXT[i]
    }
    else {
      ext="-"
    } 
    
    
    
    
    trajectory_model=data.frame(t(c("-","-","-","-","-","-","-","-",lat,lon, "-", "-")))  
    
    trajectory_model_complete=rbind(trajectory_model_complete,trajectory_model)
    colnames(trajectory_model_complete)=c("run", "receptor", "hour_along", "traj_dt", "lat", "lon", "height", "traj_dt_i", "lat_i", "lon_i", "height_i", "pressure")
    
  }
  IDF_dataframe=rep(IDF[i],filas)
  trajectory_model_complete$IF=IDF_dataframe
  
  REG_dataframe=rep(RegI[i],filas)
  trajectory_model_complete$REGION=REG_dataframe
  
  COMU_dataframe=rep(ComI[i],filas)
  trajectory_model_complete$COMUNA=COMU_dataframe
  
  CAUSAG_dataframe=rep(CausagI[i],filas)
  trajectory_model_complete$CausaG=CAUSAG_dataframe
  
  CAUSAE_dataframe=rep(CausaeI[i],filas)
  trajectory_model_complete$CausaE=CAUSAE_dataframe
  
  PINO0_dataframe=rep(Pino0a10[i],filas)
  trajectory_model_complete$Pino0a10=PINO0_dataframe
  
  PINO11_dataframe=rep(Pino11a17[i],filas)
  trajectory_model_complete$Pino11a17=PINO11_dataframe
  
  PINO18_dataframe=rep(Pino18omas[i],filas)
  trajectory_model_complete$Pino18omas=PINO18_dataframe
  
  Eucalipto_dataframe=rep(Eucalipto[i],filas)
  trajectory_model_complete$Eucalipto=Eucalipto_dataframe
  
  Otrasplantaciones_dataframe=rep(Otrasplantaciones[i],filas)
  trajectory_model_complete$Otrasplantaciones=Otrasplantaciones_dataframe
  
  Subtotalplantaciones_dataframe=rep(Subtotalplantaciones[i],filas)
  trajectory_model_complete$Subtotalplantaciones=Subtotalplantaciones_dataframe
  
  Arbolado_dataframe=rep(Arbolado[i],filas)
  trajectory_model_complete$Arbolado=Arbolado_dataframe
  
  Matorral_dataframe=rep(Matorral[i],filas)
  trajectory_model_complete$Matorral=Matorral_dataframe
  
  Pastizal_dataframe=rep(Pastizal[i],filas)
  trajectory_model_complete$Pastizal=Pastizal_dataframe
  
  INICIO_dataframe=rep(INI[i],filas)
  trajectory_model_complete$Inicio=INICIO_dataframe
  
  EXTINCION_dataframe=rep(EXT[i],filas)
  trajectory_model_complete$Extincion=EXTINCION_dataframe
  
  SubVeg_dataframe=rep(Subtotalvegetacionnatural[i],filas)
  trajectory_model_complete$Subtotalvegetacionnatural=SubVeg_dataframe
  
  AGR_dataframe=rep(Agricola[i],filas)
  trajectory_model_complete$Agricola=AGR_dataframe
  
  DES_dataframe=rep(Desechos[i],filas)
  trajectory_model_complete$Desechos=DES_dataframe
  
  SUBOS_dataframe=rep(Subtotalotrassuperficies[i],filas)
  trajectory_model_complete$Subtotalotrassuperficies=SUBOS_dataframe
  
  SUPTOT_dataframe=rep(Superficietotal[i],filas)
  trajectory_model_complete$Superficietotal=SUPTOT_dataframe
  
  
  HORAINI_dataframe=rep(Hora_I[i],filas)
  trajectory_model_complete$Hora_I=HORAINI_dataframe
  
  HORAFIN_dataframe=rep(Hora_F[i],filas)
  trajectory_model_complete$Hora_F=HORAFIN_dataframe
  
  if(is.null(DUR[i])==FALSE){
    dur=DUR[i]
  }
  else {
    dur="-"
  } 
  
  DURACION_dataframe=rep(dur,filas)
  trajectory_model_complete$DUR=DURACION_dataframe
  
  TEMP_dataframe=rep(Temperatura[i],filas)
  trajectory_model_complete$Temperatura=TEMP_dataframe
  
  
  HUMD_dataframe=rep(Humedad[i],filas)
  trajectory_model_complete$Humedad=HUMD_dataframe
  
  EXPO_dataframe=rep(Exposicion[i],filas)
  trajectory_model_complete$Exposicion=	EXPO_dataframe
  
  DIREC_dataframe=rep(Direccion[i],filas)
  trajectory_model_complete$Direccion= DIREC_dataframe
  
  VEL_dataframe=rep(Velocidad[i],filas)
  trajectory_model_complete$Velocidad=VEL_dataframe
  
  TOPO_dataframe=rep(Topografia[i],filas)
  trajectory_model_complete$Topografia=	TOPO_dataframe
  
  PEND_dataframe=rep(Pendiente[i],filas)
  trajectory_model_complete$Pendiente=PEND_dataframe
  
  
  
  # trajectory_model_complete <-trajectory_model[complete.cases(trajectory_model), ]
  
  
  
  print(i) 
  
  trajectory_model_complete = trajectory_model_complete[ , c(13	,14	,15	,9	,10	,42	,33	,43	,34	,20	,21	,22	,23	,24	,25	,26	,27	,28	,29	,30	,31	,32	,35	,36	,37	,38	,39	,40	,41	,16	,17	,18	,19	,1	,2	,3	,4	,5	,6	,7	,8	,11	,12)]
  df=rbind(df,trajectory_model_complete)
  #colnames(trajectory_model_complete)
  print(i/147*100)
}


df=data.frame(df)
write.xlsx2(df, file="res2.xlsx", sheetName = "Sheet1", col.names = TRUE,
            row.names = FALSE, append = FALSE, showNA = TRUE, password = NULL)






##Nombrar comunas a puntos #
# Leer los datos de puntos desde el archivo XLSX
datos_puntos <- read_excel("res200hc.xlsx")
datos_puntos2=na.omit(datos_puntos)
incendio_logical2=c()
for(i in 1:nrow(df)){
  if(df$lat[i]=="-"){
    incendio_logical2=c(incendio_logical2,i)
  }
}


datos.vacio=df[incendio_logical2,]
mat.vacios = matrix(ncol = 21, nrow = nrow(datos.vacio))
df.vacios=data.frame(mat.vacios)
df.vacios2=cbind(datos.vacio,df.vacios)

distancia<-function(p_x,p_y,p_x_1,p_y_1){
  
  # Definir las coordenadas de los puntos (en radianes)
  lat1_rad <- (p_x * pi) / 180
  lon1_rad <- (p_y * pi) / 180
  lat2_rad <- (p_x_1 * pi) / 180
  lon2_rad <- (p_y_1 * pi) / 180
  
  # Calcular las diferencias de longitud y latitud
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  
  # Aplicar la fórmula del haversine
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan(sqrt(a / (1-a)))
  distancia_km <- 6371 * c  # radio de la Tierra en kilómetros
  return(distancia_km)
}


# Leer los datos de polígonos de comunas desde el archivo Shapefile
datos_comunas <- st_read("COMUNI.shp")

# Definir el CRS EPSG:3857
crs_20040 <- st_crs(20040)

# Transformar los datos de puntos al CRS EPSG:3857
datos_puntos_sf <- st_as_sf(datos_puntos2, coords = c("lon", "lat"), remove=FALSE, crs = crs_20040)

# Verificar el CRS de los datos de puntos
print(st_crs(datos_puntos_sf))


# Transformar los datos de polígonos de comunas al CRS EPSG:20040(Sirgas Chile)
datos_comunas <- st_transform(datos_comunas, crs = crs_20040)
# Verificar el CRS de los datos de polígonos de comunas
#print(st_crs(datos_comunas))
#str(datos_puntos_sf)
#str(datos_comunas)

# Visualizar los polígonos de las comunas y los puntos
#library(ggplot2)
#ggplot() +
#geom_sf(data = datos_comunas, fill = "lightblue") +
#geom_sf(data = datos_puntos_sf, color = "red", size = 1)

# Corregir la topología de los datos de comunas
datos_comunas <- st_make_valid(datos_comunas)
# Realizar la asociación espacial entre los puntos y las comunas
asociacion <- st_join(datos_puntos_sf, datos_comunas, join = st_intersects)


cercania=c()
for (i in 1:nrow(asociacion)) {
  print(i)
  cercania=c(cercania,distancia(as.numeric(asociacion$lat[i]),as.numeric(asociacion$lon[i]),asociacion$POINT_Y[i], asociacion$POINT_X[i]))
}
asociacion$Cercania=cercania



colnames(df.vacios2)=colnames(data.frame(asociacion))  


asociacion2=rbind(data.frame(asociacion),df.vacios2)
write.xlsx2(data.frame(asociacion2), file="Resultado200hc.xlsx", sheetName = "Sheet1", col.names = TRUE,
            row.names = FALSE, append = FALSE, showNA = TRUE, password = NULL)
