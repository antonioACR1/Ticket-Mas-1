#para formar un dataframe unico con dataframes viniendo de una lista
library("plyr")
#para obtener 4 productos por producto inicial
require(data.table)
#para el algoritmo reglas de asociacion
library("arules")
#para la conexion a SQL
library("RODBC")
connection <- odbcDriverConnect('driver={SQL Server};uid=****;pwd=****;server=****;database=VirtualMarketDMT;trusted_connection=true')
#para manipular dataframes como SQL
library("sqldf")

#semana a calcular
semana=201717
minFecha<-sqlQuery(connection,paste0("select min(identfecha) from lu_dia where idsemana =",semana))
maxFecha<-sqlQuery(connection,paste0("select max(identfecha) from lu_dia where idsemana =",semana))

#fechas maximo y minimo
minFecha=20170423
maxFecha=20170429

DF<-sqlQuery(connection,paste0("select id_fecha_venta,tiket,id_tienda,id_producto,piezas 
from fact_general_hist
where id_fecha_venta between ", minFecha, " and ", maxFecha)
)
DF_categorias<-sqlQuery(connection,"select a.id_producto,b.id_subcategoria,c.id_categoria from lu_productos a inner join lu_subcategorias b on a.id_subcategoria=b.id_subcategoria
inner join lu_categorias c on b.id_categoria=c.id_categoria")
codigo_barras<-sqlQuery(connection,"select id_producto,codigo_barras from lu_productos")


#parametros para el algoritmo "apriori" en R (reglas de asociacion)
sup=0.001
conf=0.001
#numero de vuelta (del bucle, cada vuelta contiene 5 tiendas)
j=5

write.csv(DF,paste0("semana",semana,".csv"),row.names=FALSE)
write.csv(DF_categorias,"DF_categorias.csv",row.names=FALSE)
write.csv(codigo_barras,"codigo_barras.csv",row.names=FALSE)

ticket_mas_uno<-function(semana,minFecha,maxFecha,sup,conf,j){
DF<-sqldf("select a.id_fecha_venta,a.tiket,a.id_tienda,a.id_producto,a.piezas,b.id_subcategoria,b.id_categoria from DF a inner join DF_categorias b on a.id_producto=b.id_producto")
DF1<- read.csv(paste0("semana",semana,".csv"),header=TRUE)
df<- DF1[c('tiket','id_producto','id_tienda')]

tiendas<-unique(df$id_tienda)
tiendas<-tiendas[(5*j-4):(5*j)]
df_tiendas<-ldply(tiendas,data.frame)
colnames(df_tiendas)<-c("id_tienda")
lista_tiendas=list()
for (i in tiendas){lista_tiendas[[match(i,tiendas)]]=df[df$id_tienda==i,]
}

regla<-function(df){i=split(df$id_producto,df$tiket)
txn<- as(i,"transactions")
basket_rules <- apriori(txn,parameter=list(sup=sup,conf=conf,target="rules"))
rules.sorted<-sort(basket_rules,by=c("lift","confidence"))
t<-as(rules.sorted,"data.frame")
t1<-t[t$lift>=1.1,]
return(t1)
}

reglas<-lapply(lista_tiendas,regla)
reglas_numerico <- lapply(reglas, '[', c("support", "confidence","lift"))

separar <- function(t){lapply( strsplit(as.character(t$rules), " => ", fixed = TRUE), function(x) {
  strsplit(  gsub("[{}]", "", x), ",", fixed = TRUE)
})}

#para convertir las reglas en dataframe

arreglos<-lapply(reglas,function(df){df<-as.data.frame(df)
asociaciones<-df$rules
return(asociaciones)})

separar<-function(asociacion){strsplit(as.character(gsub("[{}]","",asociacion)), " => ", fixed = TRUE)}
arreglos1<-lapply(arreglos,function(arreglo){separar(arreglo)})

f <- function(data) {
nCol <- max(vapply(data, length, 0))
data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
dataframe<-data.frame(data)
colnames(dataframe)<-c("producto_inicial","producto_sugerido")
return(dataframe)
}

separaciones_pulido<-lapply(arreglos1,f)



longitud<-length(separaciones_pulido)==length(reglas)
longitud

uniones1<-list()
for (i in 1:length(separaciones_pulido)){uniones1[[i]]=cbind(as.data.frame(separaciones_pulido[i]),as.data.frame(reglas_numerico[i]))}
uniones<-uniones1



comas<-function(df){
s <- strsplit(as.character(df$producto_inicial), split = ",")
data.frame(producto_inicial = unlist(s),producto_sugerido = rep(df$producto_sugerido, sapply(s, length)),
support = rep(df$support, sapply(s, length)) ,confidence = rep(df$confidence, sapply(s, length)),
lift = rep(df$lift, sapply(s, length)))
}
uniones<-lapply(uniones,comas)

cuatro<-function(new_df){tablita<-data.table(new_df,key="producto_inicial")
tabla<-tablita[,head(.SD,4),by="producto_inicial"]
tabla<-as.data.frame(tabla)
return(tabla)
}
respuesta<-lapply(uniones,cuatro)

uniones_completo<-list()
for (i in 1:length(uniones)){df=as.data.frame(respuesta[i])
df=df[c("producto_inicial","producto_sugerido")]
df$fecha_inicio=minFecha
df$fecha_fin=maxFecha
df$id_tienda=tiendas[[i]]
colnames(df)
uniones_completo[[i]]=df}

df <- ldply(uniones_completo, data.frame)
barras<-read.csv("codigo_barras.csv",header=TRUE)

inicial_barras<-sqldf("select a.producto_inicial,b.codigo_barras as codigo_barras_inicial from (select producto_inicial from df) a
left join (select * from barras) b on a.producto_inicial=b.id_producto")

sugerido_barras<-sqldf("select a.producto_sugerido,b.codigo_barras as codigo_barras_sugerido from (select producto_sugerido from df) a 
left join (select * from barras) b on a.producto_sugerido=b.id_producto")

archivo_prefinal<-cbind(df,inicial_barras,sugerido_barras)

archivo_final<-archivo_prefinal[c("id_tienda","fecha_inicio","fecha_fin","producto_inicial","codigo_barras_inicial","producto_sugerido","codigo_barras_sugerido")]

archivo_final<-na.omit(archivo_final)
archivo_final<-unique(archivo_final)

write.csv(archivo_final,"producto_sugerido.csv",row.names=FALSE)

#esto es para sugerir productos a los productos que no tienen sugerencia directa del algoritmo

h<-read.csv("producto_sugerido.csv",header=TRUE)
h1<-sqldf("select a.id_fecha_venta,a.tiket,a.id_tienda,a.id_producto,a.piezas,a.id_subcategoria,a.id_categoria from DF a inner join df_tiendas b on a.id_tienda=b.id_tienda")
h0<-sqldf("select * from h inner join (select id_producto,id_subcategoria,id_categoria from h1) b on h.producto_inicial=b.id_producto")
f0<-sqldf("select id_tienda as id_tienda_w,producto_inicial,producto_sugerido,id_subcategoria as id_subcategoria_w,id_categoria as id_categoria_w from h0")
f1<-sqldf("select id_tienda as id_tienda_o,id_producto as id_producto_o,id_subcategoria as id_subcategoria_o,id_categoria as id_categoria_o from h1")
f<-sqldf("select * from f1 left join f0 on f1.id_tienda_o=f0.id_tienda_w and f1.id_producto_o=f0.producto_inicial and f1.id_subcategoria_o=f0.id_subcategoria_w and f1.id_categoria_o=f0.id_categoria_w")
f<-unique(f)
f$producto_inicial[is.na(f$producto_inicial)]=f$id_producto_o[is.na(f$producto_inicial)]
f$id_subcategoria_w[is.na(f$id_subcategoria_w)]=f$id_subcategoria_o[is.na(f$id_subcategoria_w)]
f$id_categoria_w[is.na(f$id_categoria_w)]=f$id_categoria_o[is.na(f$id_categoria_w)]
f$id_tienda_w[is.na(f$id_tienda_w)]=f$id_tienda_o[is.na(f$id_tienda_w)]

sugeridos<-sqldf("select id_tienda_o as id_tienda, producto_inicial,producto_sugerido,id_subcategoria_o as id_subcategoria_inicial,id_categoria_o as id_categoria_inicial from f")
sin_sugerir<-sqldf("select * from sugeridos where producto_sugerido is null")
cat_sub_tienda<-sqldf("select distinct id_subcategoria_inicial,id_categoria_inicial,id_tienda from sugeridos")
sugeridos_filtro<-sqldf("select * from sugeridos where producto_sugerido is not null")

#truco del Fede para asociar un producto sugerido a los productos iniciales que no tenían producto sugerido, usando productos iniciales que sí tienen un producto sugerido
tiendas<-sqldf("select a.id_tienda,a.producto_inicial,a.id_categoria_inicial,a.id_subcategoria_inicial,c.producto_sugerido 
from sin_sugerir a 
left join cat_sub_tienda b 
on a.id_categoria_inicial=b.id_categoria_inicial 
and a.id_subcategoria_inicial=b.id_subcategoria_inicial
and a.id_tienda=b.id_tienda
left join 
sugeridos_filtro c
on b.id_categoria_inicial=c.id_categoria_inicial
and b.id_subcategoria_inicial=c.id_subcategoria_inicial
and b.id_tienda=c.id_tienda")

tiendas<-unique(tiendas)

tablita<-data.table(tiendas,key=c("id_tienda","producto_inicial"))
tabla<-tablita[,head(.SD,4),by=c("id_tienda","producto_inicial")]
tabla<-as.data.frame(tabla)

tabla<-na.omit(tabla)
producto_sugerido2<-tabla[c("id_tienda","producto_inicial","producto_sugerido")]

inicial_barras<-sqldf("select a.producto_inicial,b.codigo_barras as codigo_barras_inicial from (select producto_inicial from producto_sugerido2) a
left join (select * from barras) b on a.producto_inicial=b.id_producto")

sugerido_barras<-sqldf("select a.producto_sugerido,b.codigo_barras as codigo_barras_sugerido from (select producto_sugerido from producto_sugerido2) a 
left join (select * from barras) b on a.producto_sugerido=b.id_producto")

archivo_prefinal1<-cbind(producto_sugerido2,inicial_barras,sugerido_barras)

archivo_final1<-archivo_prefinal1[c("id_tienda","producto_inicial","codigo_barras_inicial","producto_sugerido","codigo_barras_sugerido")]

archivo_final1<-na.omit(archivo_final1)
archivo_final1$fecha_inicio=minFecha
archivo_final1$fecha_fin=maxFecha
archivo_final1<-archivo_final1[c("id_tienda","fecha_inicio","fecha_fin","producto_inicial","codigo_barras_inicial","producto_sugerido","codigo_barras_sugerido")]
write.csv(archivo_final1,"producto_sugerido_anexo.csv",row.names=FALSE)

z<-read.csv('producto_sugerido_anexo.csv',header=TRUE)



mag<-list(h,z)
m<-ldply(mag,data.frame)

write.csv(m,paste0("vuelta_",j,"_",semana,"_ticket_mas_1.csv"),row.names=FALSE)
}


for (j in 1:6){ticket_mas_uno(semana,minFecha,maxFecha,sup,conf,j)}