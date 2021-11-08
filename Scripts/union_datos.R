library(readxl)
library(dplyr)

factura <- read_excel("Datos/BD-unica/Venta X Factura.xlsx")%>%as.data.table()

factura_2019=read_excel("~/Danilo Mejia/Proyectos/20210708-Here We Go Again-Evok/Datos/Venta X Factura 2019.xlsx")

factura_2020=read_excel("~/Danilo Mejia/Proyectos/20210708-Here We Go Again-Evok/Datos/Venta X Factura.xlsx")%>%as.data.table()

factura_2020=factura_2020[year(Fecha)==2020]

all_factura=rbind(factura,factura_2020,factura_2019,use.names=FALSE)

detalle <- read_excel("Datos/BD-unica/Ventas X Detalle.xlsx")%>%as.data.table()
detalle_2019=read_excel("~/Danilo Mejia/Proyectos/20210708-Here We Go Again-Evok/Datos/Ventas Detalle 2019.xlsx")
detalle_2020=read_excel("~/Danilo Mejia/Proyectos/20210708-Here We Go Again-Evok/Datos/Ventas Detalle.xlsx")%>%as.data.table()
detalle_2020=detalle_2020[year(Fecha)==2020]

all_detalle=rbind(detalle,detalle_2019,detalle_2020,use.names=FALSE)

minimo=as.Date(min(all_factura$Fecha))
maximo=as.Date(max(all_factura$Fecha))

fwrite(all_factura,osPathJoin(datosPath,paste(paste("factura",minimo,maximo,sep = "_"),".csv",sep="")),sep = "|",dec = ".")

minimo=as.Date(min(all_factura$Fecha))
maximo=as.Date(max(all_factura$Fecha))

fwrite(all_detalle,osPathJoin(datosPath,paste(paste("detalle",minimo,maximo,sep = "_"),".csv",sep="")),sep = "|",dec = ".")



factura <-fread("Datos/factura_2019-01-01_2021-08-25.csv")
Venta_X_Factura$Fecha=as.Date(Venta_X_Factura$Fecha)

factura=factura[my_dia<"2021-08-01"]

library(readxl)
library(dplyr)
Venta_X_Factura <- read_excel("Datos/20210906-TableroEvok/Venta X Factura.xlsx")%>%as.data.table()
Venta_X_Factura$my_dia=as.Date(Venta_X_Factura$Fecha)

Venta_X_Factura=Venta_X_Factura[my_dia>="2021-08-01"]

all_factura=rbind(factura,Venta_X_Factura,use.names=FALSE)


Venta_X_Factura$Fecha=as.Date(Venta_X_Factura$Fecha)
factura$Fecha=as.Date(factura$Fecha)


