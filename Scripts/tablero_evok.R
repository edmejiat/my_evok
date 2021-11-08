tablero_evok=function(all_factura){

library(googlesheets4)
  
range_clear("1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo", "indicadores_x_periodocanal_df")
range_clear("1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo", "info_catcliente_df")
range_clear("1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo", "results_RFM_df")
range_clear("1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo", "total_clientes_df")
  
  
all_factura=all_factura[,mes:=substring(Fecha,1,7)]

final=data.table()

canales=all_factura[,.N,by=Origen]

for (i in 1:dim(canales)[1]) {
nombre_canal=canales$Origen[i]

canal=all_factura[Origen==nombre_canal]


#Mes
#Ventas
ventas=canal[,.(ventas=sum(`Valor Neto`)),by=mes][order(mes)]

#Clientes
clientes=canal[,.N,by=c("Id Cliente","mes")]
clientes=clientes[,.N,by=mes]
names(clientes)[2]="clientes"

#Clientes Nuevos
primera_compra=canal[,.(primera=min(Fecha)),by="Id Cliente"]
primera_compra=primera_compra[,mes:=substring(primera,1,7)]
primera_compra=primera_compra[,.N,by=mes][order(mes)]
names(primera_compra)[2]="nuevos"

mes=merge(ventas, clientes, all.x=TRUE)
mes=merge(mes, primera_compra, all.x=TRUE)

mes[,recurrentes:=clientes-nuevos]
names(mes)[1]="periodo"
mes[,frecuencia:="mensual"]

mes[,canal:=nombre_canal]

mes=mes[,c("periodo","ventas","clientes","recurrentes","nuevos","frecuencia","canal")]

#Trimestre
trimestre=canal[,trimestre:=paste(year(Fecha),"Q",quarter(Fecha),sep = "")]

#Ventas
ventas=trimestre[,.(ventas=sum(`Valor Neto`)),by=trimestre][order(trimestre)]

#Clientes
clientes=canal[,.N,by=c("Id Cliente","trimestre")]
clientes=clientes[,.N,by=trimestre]
names(clientes)[2]="clientes"

#Clientes Nuevos
primera_compra=canal[,.(primera=min(Fecha)),by="Id Cliente"]
primera_compra=primera_compra[,trimestre:=paste(year(primera),"Q",quarter(primera),sep = "")]
primera_compra=primera_compra[,.N,by=trimestre][order(trimestre)]
names(primera_compra)[2]="nuevos"

trimeste=merge(ventas, clientes, all.x=TRUE)
trimeste=merge(trimeste, primera_compra, all.x=TRUE)

trimeste[,recurrentes:=clientes-nuevos]
names(trimeste)[1]="periodo"
trimeste[,frecuencia:="trimestral"]

trimeste[,canal:=nombre_canal]

trimeste=trimeste[,c("periodo","ventas","clientes","recurrentes","nuevos","frecuencia","canal")]

#Semestre
semestre=canal[,semestre:=paste(year(Fecha),"S",semester(Fecha),sep = "")]

#Ventas
ventas=semestre[,.(ventas=sum(`Valor Neto`)),by=semestre][order(semestre)]

#Clientes
clientes=canal[,.N,by=c("Id Cliente","semestre")]
clientes=clientes[,.N,by=semestre]
names(clientes)[2]="clientes"

#Clientes Nuevos
primera_compra=canal[,.(primera=min(Fecha)),by="Id Cliente"]
primera_compra=primera_compra[,semestre:=paste(year(primera),"S",semester(primera),sep = "")]
primera_compra=primera_compra[,.N,by=semestre][order(semestre)]
names(primera_compra)[2]="nuevos"

semestre=merge(ventas, clientes, all.x=TRUE)
semestre=merge(semestre, primera_compra, all.x=TRUE)

semestre[,recurrentes:=clientes-nuevos]
names(semestre)[1]="periodo"
semestre[,frecuencia:="semestral"]

semestre[,canal:=nombre_canal]

semestre=semestre[,c("periodo","ventas","clientes","recurrentes","nuevos","frecuencia","canal")]

#Anual
anual=canal[,year:=year(Fecha)]

#Ventas
ventas=anual[,.(ventas=sum(`Valor Neto`)),by=year][order(year)]

#Clientes
clientes=anual[,.N,by=c("Id Cliente","year")]
clientes=clientes[,.N,by=year]
names(clientes)[2]="clientes"

#Clientes Nuevos
primera_compra=canal[,.(primera=min(Fecha)),by="Id Cliente"]
primera_compra=primera_compra[,year:=year(primera)]
primera_compra=primera_compra[,.N,by=year][order(year)]
names(primera_compra)[2]="nuevos"

anual=merge(ventas, clientes, all.x=TRUE)
anual=merge(anual, primera_compra, all.x=TRUE)

anual[,recurrentes:=clientes-nuevos]
names(anual)[1]="periodo"
anual[,frecuencia:="anual"]

anual[,canal:=nombre_canal]

anual=anual[,c("periodo","ventas","clientes","recurrentes","nuevos","frecuencia","canal")]

final=rbind(final,mes,trimeste,semestre,anual)

}

indicadores_x_periodocanal_df=final[,canal:=ifelse(canal=="POS","Tiendas",ifelse(canal=="ERP","Institucional","On line"))]

write_sheet(indicadores_x_periodocanal_df,"1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo",sheet = "indicadores_x_periodocanal_df")

info_catcliente_df=data.table()


for (i in 1:dim(canales)[1]) {
  nombre_canal=canales$Origen[i]
  
  canal=all_factura[Origen==nombre_canal]
  
  canal[,periodo:=year(Fecha)]
  
  my_year=canal[,.N,by=periodo]
  
  for (j in 1:dim(my_year)[1]) {
    year_for=my_year$periodo[j]
    
    my_canal=canal[periodo==year_for]
    
    #compras
    compras=my_canal[,.N,by=c("Id Cliente","Origen","periodo")]
    compras[,cat_compras:=ifelse(N==1,"1 compra",ifelse(N==2,"2 compras",ifelse(N==3,"3 compras","4 ó más compras")))]
    compras=compras[,.(cat_compras,`Id Cliente`,periodo)]
    
    my_canal=merge(my_canal, compras, all.x=TRUE)
    
    #clientes
    clientes=my_canal[,.N,by=c("Id Cliente","periodo","cat_compras")]
    clientes=clientes[,.N,by=c("periodo","cat_compras")]
    names(clientes)[3]="clientes"
    
    #Pedidos y ventas
    pedidos_ventas=my_canal[,.(pedidos = .N, ventas = sum(`Valor Neto`)),by=c("periodo","cat_compras")]
    pedidos_ventas[,TP:=ventas/pedidos]
    pedidos_ventas[,canal:=nombre_canal]
    
    my_ultima_compra=my_canal[,.(ultima_compra=max(Fecha)),by=c("Id Cliente","periodo")]
    
    my_canal=my_canal[][order(`Id Cliente`,-Fecha)]
    my_ultima_compra=my_canal[,.SD[1],by=c("Id Cliente")]
    my_ultima_compra=my_ultima_compra[,.(`Id Cliente`,Fecha,Factura)]
    names(my_ultima_compra)[2]="fecha_ultima_compra"
    
    my_compra_previa=my_canal[!Factura %in% my_ultima_compra$Factura]
    my_compra_previa=my_compra_previa[][order(`Id Cliente`,-Fecha)]
    my_compra_previa=my_compra_previa[,.SD[1],by=c("Id Cliente")]
    my_compra_previa=my_compra_previa[,.(`Id Cliente`,Fecha,Factura)]
    names(my_compra_previa)[2]="fecha_compra_previa"
    
    prev_ultima=merge( my_ultima_compra,my_compra_previa,by.x=c("Id Cliente"),by.y=c("Id Cliente"), all.x=TRUE)
    prev_ultima[,days:=difftime(fecha_ultima_compra, fecha_compra_previa, units = "days")]
    prev_ultima$days=as.integer(prev_ultima$days)
    prev_ultima=prev_ultima[,.(`Id Cliente`,days)]
    prev_ultima[is.na(prev_ultima)] <- 0
    
    con_frecuencia=merge( prev_ultima,compras,by.x=c("Id Cliente"),by.y=c("Id Cliente"), all.x=TRUE)
    con_frecuencia=con_frecuencia[,.(frecuencia_dias=mean(days)),by=c("cat_compras","periodo")]
    
    #union
    all_t=merge( clientes,pedidos_ventas,by.x=c("cat_compras","periodo"),by.y=c("cat_compras","periodo"), all.x=TRUE)
    all_t=merge( all_t,con_frecuencia,by.x=c("cat_compras","periodo"),by.y=c("cat_compras","periodo"), all.x=TRUE)
    
    all_t=all_t[,.(cat_compras ,clientes ,ventas,TP,frecuencia_dias,canal,periodo )]
    
    info_catcliente_df=rbind(info_catcliente_df,all_t)

  }
}

info_catcliente_df=info_catcliente_df[,canal:=ifelse(canal=="POS","Tiendas",ifelse(canal=="ERP","Institucional","On line"))]

write_sheet(info_catcliente_df,"1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo",sheet = "info_catcliente_df")

library(rfm)

my_canal=all_factura[,.N,by=Origen]
results_RFM_df=data.table()

for (i in 1:dim(my_canal)[1]) {
  el_canal=my_canal$Origen[i]

for_rfm=all_factura[Fecha>today()-days(100) & Origen==el_canal]
for_rfm=for_rfm[,.(`Id Cliente`,Fecha,`Valor Neto`)]
names(for_rfm)=c("id_cliente","fecha","valor")
for_rfm=for_rfm[!is.na(id_cliente)]
for_rfm=for_rfm[id_cliente!="NULL"]
for_rfm$fecha=as.Date(for_rfm$fecha)

fecha=as.Date(today())

rfm_result =rfm_table_order(for_rfm,customer_id = id_cliente,order_date = fecha,revenue = valor,analysis_date = fecha
                            ,  recency_bins = 3,frequency_bins = 3,monetary_bins = 3)

rfm_final=rfm_result$rfm
setDT(rfm_final)

medalla_rfm=fread(osPathJoin(datosPath,"medalla_rfm.csv"))

resultados=merge(rfm_final, medalla_rfm,by.x = "rfm_score",by.y = "rfm", all.x=TRUE)

rfm_all_pre=resultados[, .(nro_clientes = .N, recencia_prom = mean(recency_days),
                           frecuencia_prom = mean(frequency_score),
                           monto_prom = mean( amount ),
                           ventas=sum(amount),
                           pedidos = sum(frequency_score)), by = medalla]

rfm_all_pre[,TP:=ventas/pedidos ]
rfm_all_pre[,canal:=el_canal ]

total_ventas=sum(rfm_all_pre$ventas)
total_clientes=sum(rfm_all_pre$nro_clientes)

rfm_all_pre[,pct_cientes:=round((nro_clientes/total_clientes)*100,0) ]
rfm_all_pre[,pct_ventas:=round((ventas /total_ventas)*100,0) ]
rfm_all_pre[,orden_segmetno:= ifelse(medalla=="oro",1,ifelse(medalla=="plata",2,3)) ]
names(rfm_all_pre)[1]="segmento"

rfm_all_pre=rfm_all_pre[,.(segmento,nro_clientes,recencia_prom,
                           frecuencia_prom,monto_prom,ventas,pct_cientes,pct_ventas,TP,orden_segmetno,canal)]

results_RFM_df=rbind(results_RFM_df,rfm_all_pre)

}

results_RFM_df[,canal:=ifelse(canal=="POS","Tiendas",ifelse(canal=="ERP","Institucional","On line"))]

write_sheet(results_RFM_df,"1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo",sheet = "results_RFM_df")

my_results_RFM_df=results_RFM_df[,.(nro_clientes,canal)]
my_results_RFM_df[,estado:="activos"]
my_results_RFM_df=my_results_RFM_df[,.(nro_clientes=sum(nro_clientes)),by=c("canal","estado")]

mis_clientes=all_factura[,.N,by=c("Id Cliente","Origen")]
mis_clientes=mis_clientes[,.N,by=c("Origen")]
mis_clientes[,estado:="clientes"]
mis_clientes=mis_clientes[,.(Origen,estado,N)]
mis_clientes[,Origen:=ifelse(Origen=="POS","Tiendas",ifelse(Origen=="ERP","Institucional","On line"))]

total_clientes_df =rbind(my_results_RFM_df,mis_clientes,use.names=FALSE)

total_clientes_df=dcast(total_clientes_df,canal~estado,value.var = "nro_clientes")
total_clientes_df[,inactivos:=clientes-activos]

total_clientes_df=total_clientes_df[,.(canal,clientes ,activos,inactivos )]

write_sheet(total_clientes_df,"1WVvX4xTeFNHJ2budKqM2reyx-MaM-o4EQsNDijKCxzo",sheet = "total_clientes_df")

}
