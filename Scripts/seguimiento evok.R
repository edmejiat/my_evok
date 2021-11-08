Venta_X_Factura <- fread("Datos/20210802-Tablero-Evok/Venta X Factura.csv")

Venta_X_Factura$Fecha=as.Date(Venta_X_Factura$Fecha)
Venta_X_Factura[,mes:=substring(Fecha,1,7)]

online=Venta_X_Factura[Origen=="ON LINE"]

pedidos_ventas=online[,.(cantidad_pedidos = .N,ventas_totales=sum(`Valor Bruto`)),by=mes][order(mes)]

write_sheet(pedidos_ventas,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "pedidos_ventas")

julio=online[mes=="2021-07"]

dia_dia_julio=julio[,.(cantidad_pedidos = .N,ventas_totales=sum(`Valor Bruto`)),by=Fecha][order(Fecha)]

write_sheet(dia_dia_julio,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "dia_dia_julio")

Venta_X_detalle <- fread("Datos/20210802-Tablero-Evok/Ventas Detalle.csv",encoding = "UTF-8")

Venta_X_detalle$Fecha=as.Date(Venta_X_detalle$Fecha)
Venta_X_detalle[,mes:=substring(Fecha,1,7)]

online_detalle=Venta_X_detalle[Origen=="ON LINE"]

resumen=online_detalle[Fecha>="2021-01-01",.(cantidad=sum(Cantidad),valor=sum(`Valor Bruto`)),by=c("mes","Des. Item")]

resumen_cantidades=dcast(resumen,`Des. Item`~mes,value.var = "cantidad")
resumen_cantidades[is.na(resumen_cantidades)] <- 0
resumen_cantidades[,total:=`2021-01`+`2021-02`+`2021-03`+`2021-04`+`2021-05`+`2021-06`+`2021-07`]
resumen_cantidades[][order(-total)]

write_sheet(resumen_cantidades,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "resumen_cantidades")

resumen_ventas=dcast(resumen,`Des. Item`~mes,value.var = "valor")
resumen_ventas[is.na(resumen_ventas)] <- 0
resumen_ventas[,total:=`2021-01`+`2021-02`+`2021-03`+`2021-04`+`2021-05`+`2021-06`+`2021-07`]
resumen_ventas[][order(-total)]

write_sheet(resumen_ventas,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "resumen_ventas")

library(rfm)

table_rfm=Venta_X_Factura[Origen=="ON LINE",.(`Id Cliente`,Fecha,`Valor Neto`)]
names(table_rfm)=c("id_ciente","fecha","valor")

fecha=as.Date("2021-08-01")

rfm_result =rfm_table_order(table_rfm,customer_id = id_ciente,order_date = fecha,revenue = valor,analysis_date = fecha
              ,  recency_bins = 3,frequency_bins = 3,monetary_bins = 3)

resultados=rfm_result$rfm
setDT(resultados)

resultados[,.N,by=rfm_score]

medalla_rfm=fread(osPathJoin(datosPath,"medalla_rfm.csv"))

resultados=merge(resultados, medalla_rfm,by.x = "rfm_score",by.y = "rfm", all.x=TRUE)

write_sheet(resultados,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "resultados_rfm")

cantidad=resultados[,.N,by=medalla]

medalla_cliente=resultados[,.(medalla,customer_id ,transaction_count  )]

online=merge(online, medalla_cliente,by.x = "Id Cliente",by.y = "customer_id", all.x=TRUE)

con_rfm=online[!is.na(medalla)]

ventas=con_rfm[,.(ventas=sum(`Valor Neto`)),by=medalla]

medalla_ventas=merge(cantidad, ventas,by.x = "medalla",by.y = "medalla", all.x=TRUE)

tdt <- function(inpdt){
  transposed <- t(inpdt[,-1,with=F]);
  colnames(transposed) <- inpdt[[1]];
  transposed <- data.table(transposed, keep.rownames=T);
  setnames(transposed, 1, names(inpdt)[1]);
  return(transposed);
}

medalla_ventas=tdt(medalla_ventas)

write_sheet(medalla_ventas,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "medalla_ventas")

medalla_ventas

medalla_cliente=resultados[,.(medalla,customer_id )]

frecuencia_ticket=online[,.(frecuencia=mean(transaction_count),ventas=mean(`Valor Neto`)),by=c("medalla","Id Cliente")]
frecuencia_ticket=frecuencia_ticket[,.(frecuencia=mean(frecuencia),ticket_pomedio=mean(ventas)),by=c("medalla")]

write_sheet(frecuencia_ticket,"1XVFhbdKOhdIbiW_ge9OB1R5ZVpSK5lDGqkmnGfxql1c",sheet = "frecuencia_ticket")


my_promedio=online[,.(promedio=mean(`Valor Neto`)),by="Id Cliente"]

quantile(my_promedio$promedio)
