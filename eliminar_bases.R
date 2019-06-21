library(Biostrings)

args<-commandArgs(TRUE)

nombre_entrada = args[1]
carpeta_salida = args[2]

data_trabajando<-read.csv(nombre_entrada)
sentido<-as.character(data_trabajando$sentido[1])

archivo_salida<-paste0(carpeta_salida, strsplit(nombre_entrada, "/")[[1]][2])

eliminar_4_bases<-function(secuencia, indice, extremo){
 
  indice_sel<-indice[secuencia[indice] != Inf]
  
  if(length(indice_sel) == 1){
    
    query_barcoding<-DNAString(as.character(secuencia[,indice_sel + 1]))
    
    barcoding<-DNAString(as.character(secuencia[,indice_sel + 3]))
    
    igualdad<-matchPattern(toString(query_barcoding), barcoding)
    
    if(extremo == "primero"){
      igualdad<-start(igualdad)
      eliminar<-5-igualdad
    }
    if(extremo == "ultimo"){
      igualdad<-end(igualdad)
      eliminar<-igualdad - length(barcoding) + 4
    }
    
    if(length(eliminar) > 0){
      if(eliminar > 0){
        
        sbjct_barcoding<-DNAString(as.character(secuencia[,indice_sel + 2]))
        
        if(extremo == "primero"){
          query_barcoding<-toString(query_barcoding[(1+eliminar):length(query_barcoding)])
          sbjct_barcoding<-toString(sbjct_barcoding[(1+eliminar):length(sbjct_barcoding)])
        }
        if(extremo == "ultimo"){
          query_barcoding<-toString(query_barcoding[1:(length(query_barcoding)-eliminar)])
          sbjct_barcoding<-toString(sbjct_barcoding[1:(length(sbjct_barcoding)-eliminar)])
        }
        secuencia[,indice_sel + 1]<-query_barcoding
        secuencia[,indice_sel + 2]<-sbjct_barcoding
        
      }
    }
  }
  return(secuencia)
}


to = ncol(data_trabajando)
indice_Index<-seq(from = 2, to = to, by = 4)[1:5]
indice_Reverse<-seq(from = 2, to = to, by = 4)
indice_Reverse<-indice_Reverse[length(indice_Reverse) - 1]


for(i in c(indice_Index + 1, indice_Index + 2, indice_Reverse + 1, indice_Reverse + 2)){
  
  data_trabajando[,i]<-as.character(data_trabajando[,i])
}

cantidad_datos<-nrow(data_trabajando)

if(sentido == "forward"){
  sentido_primero<-"primero"
  sentido_ultimo<-"ultimo"
}
if(sentido == "reverse complementario"){
  sentido_primero<-"ultimo"
  sentido_ultimo<-"primero"
}

for(i in 1:cantidad_datos){
  
  cat(i, " de ", cantidad_datos, "\n")
  data_trabajando[i,]<-eliminar_4_bases(data_trabajando[i,], indice_Index, sentido_primero)
  data_trabajando[i,]<-eliminar_4_bases(data_trabajando[i,], indice_Reverse, sentido_ultimo)

}


write.csv(data_trabajando, archivo_salida, row.names = F)

