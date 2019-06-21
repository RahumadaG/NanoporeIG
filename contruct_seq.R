nombre_archivo_1<-"sin_4_bases/2D_fail_7_1_466376_data_total_reverse_c.csv"
nombre_archivo_2<-"sin_4_bases/2D_pass_7_1_332726_data_total_reverse_c.csv"
nombre_archivo_3<-"sin_4_bases/2D_pass_8_1_100646_data_total_reverse_c.csv"
nombre_archivo_4<-"sin_4_bases/2D_pass_9_1_14321_data_total_reverse_c.csv"


data<-read.csv(nombre_archivo_1)
data<-rbind(data, read.csv(nombre_archivo_2))
data<-rbind(data, read.csv(nombre_archivo_3))
data<-rbind(data, read.csv(nombre_archivo_4))

carpeta_salida<-"constructo/"
archivo_salida<-paste0(carpeta_salida, "constructo_total_reverse_c.csv")

indice_barcoding<-seq(from = 2, to = (ncol(data) - 1), by = 4)

#query a string
for(i in (indice_barcoding+1)){
  
  data[,i]<-as.character(data[,i])
}

#sbjct a string
for(i in (indice_barcoding+2)){
  
  data[,i]<-as.character(data[,i])
}

# Constructo con data

constructo_query<-data[,(indice_barcoding+1)]
constructo_query$query<-""

for(i in 1:nrow(constructo_query)){
  print(i)
  for(j in 1:(ncol(constructo_query) - 1)){
    if(constructo_query$query[i] == ""){
      
      constructo_query$query[i]<-constructo_query[i,j]
    
    }else{
        
        if(constructo_query[i,j] != ""){
          constructo_query$query[i]<-paste0(constructo_query$query[i], "N", constructo_query[i,j])
        }
    }
    
  }
}


constructo_sbjct<-data[,(indice_barcoding+2)]
constructo_sbjct$sbjct<-""

for(i in 1:nrow(constructo_sbjct)){
  print(i)
  for(j in 1:(ncol(constructo_sbjct) - 1)){
    if(constructo_sbjct$sbjct[i] == ""){
      
      constructo_sbjct$sbjct[i]<-constructo_sbjct[i,j]
      
    }else{
      
      if(constructo_sbjct[i,j] != ""){
        constructo_sbjct$sbjct[i]<-paste0(constructo_sbjct$sbjct[i], "N", constructo_sbjct[i,j])
      }
    }
  }
}


#constructo para comparar

constructo_comparar<-cbind(as.character(data$Identificador), constructo_sbjct$sbjct, constructo_query$query, as.character(data$sentido))
constructo_comparar<-as.data.frame(constructo_comparar)

colnames(constructo_comparar)<-c("Id", "sbjct", "query", "sentido")

write.csv(constructo_comparar, archivo_salida, row.names = F)
