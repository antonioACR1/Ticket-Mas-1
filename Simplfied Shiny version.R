#SIMPLIFIED SHINY VERSION 

library('shiny')
library("plyr") #ldply method to create a single dataframe out of a list of dataframes
require(data.table) #to choose four suggested products per product
library("arules") #Apriori

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    fileInput("file1", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
        ),
    textOutput("instructions"),
    tableOutput("sample"),
    textOutput("furtherInstructions"),
    downloadButton("downloadSampleData","Download Sample Data")
    ),
    mainPanel(textOutput("instruccionesSuggestions"),
      downloadButton("downloadData","Download Suggestions"),
      tableOutput("final")
    )
  )
)

server <- function(input, output) {
  output$instructions<-renderText({paste('INSTRUCTIONS: UPLOAD DATA IN THE FORMAT SHOWN BELOW. PLEASE MAKE SURE THAT THE ORDER AND NAMES OF THE COLUMNS OF YOUR DATA MATCH THE ORDER AND NAMES OF THE FORMAT BELOW. WAIT UNTIL THE TABLE OF SUGGESTIONS DISPLAYS ON THE RIGHT. THEN CLICK THE DOWNLOAD BUTTON TO DOWNLOAD THE SUGGESTIONS.')})
 
  output$furtherInstructions<-renderText({paste("TO TEST HOW THIS PREDICTIVE MODEL WORKS, YOU CAN DOWNLOAD THE SAMPLE DATA BY CLICKING THE BUTTON BELOW, UPLOAD IT AND CHECK UNTIL THE TABLE OF SUGGESTIONS DISPLAYS ON THE RIGHT")})

sampleData<-reactive({
    sample<-read.csv("dummyTickets.csv",header=T)
    colnames(sample)<-c("id_ticket","id_store","id_product")
    return(sample)
    })

output$sample<-renderTable({
    sampleData<-head(sampleData())
    })

suggestions<-reactive({  
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    df<-read.csv(inFile$datapath, header = T)
    colnames(df)<-c("tiket","id_tienda","id_producto")
  

#PARAMETERS
sup=0.001
conf=0.001

#create "batches" of stores
tiendas<-unique(df$id_tienda)
df_tiendas<-ldply(tiendas,data.frame)
colnames(df_tiendas)<-c("id_tienda")

#splits dataframe by store
lista_tiendas=list()
for (i in tiendas){lista_tiendas[[match(i,tiendas)]]=df[df$id_tienda==i,]
}

#create method to fit a-priori
regla<-function(df){i=split(df$id_producto,df$tiket)
txn<- as(i,"transactions")
basket_rules <- apriori(txn,parameter=list(sup=sup,conf=conf,target="rules"))
rules.sorted<-sort(basket_rules,by=c("lift","confidence"))
t<-as(rules.sorted,"data.frame")
t1<-t[t$lift>=1.1,]
return(t1)
}

#apply previous method on each batch of stores
reglas<-lapply(lista_tiendas,regla)
reglas_numerico <- lapply(reglas, '[', c("support", "confidence","lift"))

#DATA WRANGLING ON THE OUTPUT!

#the output of A-Priori is of the following form:
# rules {a} => {b} support 0.005 confidence 0.999 lift 20.5 count 1
#the following lines aim to transform the previous output into the following form:
# product a suggestion b

separar <- function(t){lapply( strsplit(as.character(t$rules), " => ", fixed = TRUE), function(x) {
  strsplit(  gsub("[{}]", "", x), ",", fixed = TRUE)
})}

arreglos<-lapply(reglas,function(df){df<-as.data.frame(df)
asociaciones<-df$rules
return(asociaciones)})

separar <- function(asociacion){o<-strsplit(as.character(gsub("[{}]", "" ,asociacion)), " => ", fixed = TRUE)}
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
#df$fecha_inicio=minFecha
#df$fecha_fin=maxFecha
df$id_tienda=tiendas[[i]]
colnames(df)
uniones_completo[[i]]=df}

final <- ldply(uniones_completo, data.frame)

id_store<-final$id_tienda
id_initialProduct<-final$producto_inicial
id_suggestedProduct<-final$producto_sugerido

final<-data.frame(id_store=id_store,id_initialProduct=id_initialProduct,id_suggestedProduct=id_suggestedProduct)

final <- unique(final)
return(final)
})
output$final <- renderTable({
    suggestions()
  })

output$downloadData <- downloadHandler(
    filename = function() {"suggestions.csv"},
    content = function(file) {
    write.csv(suggestions(),file,row.names=F)
    }    
)

output$downloadSampleData <- downloadHandler(
    filename = function() {"sampleData.csv"},
    content = function(file) {
    write.csv(sampleData(),file,row.names=F)
    }    
)

output$instruccionesSuggestions<-renderText({
    "AFTER UPLOADING YOUR DATA, WAIT UNTIL THE TABLE OF SUGGESTIONS DISPLAY BELOW. THEN CLICK BUTTON BELOW."
    })

}

shinyApp(ui, server)
