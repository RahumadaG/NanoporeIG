library(Biostrings)

nombre_datos = "resultados_distancia_secuencias_3/2D_pass_9_1_14321.csv"
carpeta_salida = "resultados_proc_datos/" 

barcoding<-read.table("configuracion_barcoding.txt", sep = "\t", header = T)
barcoding<-as.character(barcoding$barcoding)

constante_arch<-strsplit(nombre_datos, "/")
constante_arch<-constante_arch[[1]][length(constante_arch[[1]])]
constante_arch<-paste0(carpeta_salida, strsplit(constante_arch, ".csv")[[1]], "_")

print(constante_arch)



nombre_salida_data_total<-paste0(constante_arch, "data_total.csv")

datos<-read.csv(nombre_datos)

cat("Paso 1\n")
cat("La cantidad de reads son: ", nrow(datos), "\n")

obtener_minimos_2_lista<-function(lista1, lista2){
  
  lista1<-as.data.frame(lista1)
  lista2<-as.data.frame(lista2)
  
  colnames(lista1)<-c("1", "2", "3", "4")
  colnames(lista2)<-c("1", "2", "3", "4")
  
  for(i in 1:nrow(lista1)){
    print(i)
    if(as.numeric(lista1[i,1]) <= as.numeric(lista2[i,1])){
      
      menor<-lista1[i,]
      
    }else{
      
      menor<-lista2[i,]
    }
    if(i == 1){
      
      menores<-menor
      
    }else{
      
      menores<-rbind(menores, menor)
    }
  }
  return(menores)
}

sentido_unico<-function(sentido_1, sentido_2, distancia_1, distancia_2){
  
  for(i in 1:length(sentido_1)){
    
    print(i)
    if(distancia_1[i] <= distancia_2[i]){
      
      menor<-as.character(sentido_1[i])
      
    }else{
      
      menor<-as.character(sentido_2[i])
    }
    if(i == 1){
      
      menores<-menor
      
    }else{
      
      menores<-c(menores, menor)
    }
  }
  
  return(menores)
  
}

borrar_indel<-function(dnaseq){
  
  bandera<-T
  for(i in 1:length(dnaseq)){
    
    if(as.character(dnaseq[i]) != "-"){
      if(bandera == T){
        
        dnaseq2<-as.character(dnaseq[i])
        bandera<-F
        
      }else{
        dnaseq2<-c(dnaseq2, as.character(dnaseq[i]))
      }    
    }
  }
  
  return(DNAString(paste0(dnaseq2, collapse = "")))
} 

columnas<-colnames(datos)
indice_barcoding<-seq(from = 2, to = length(columnas), by = 4)

# Eliminamos datos de alineamiento con más de 6 mismatch
data<-datos

for(i in 1:nrow(data)){
  print(i)
  for(j in indice_barcoding){
    
    if(data[i,j] > 6 & data[i,j] != Inf){
      
      data[i,j] = Inf
      data[i,(j+1)] = ""
      data[i,(j+2)] = ""
      data[i,(j+3)] = ""
    }
  }
}
  
  
nombres_cort<-strsplit(colnames(data)[indice_barcoding], "barcoding")

for(i in 1:length(nombres_cort)){
  
  print(i)
  
  if(i == 1){
    
    nombres_sentidos<-paste0("sentido", nombres_cort[[i]][length(nombres_cort[[i]])])
    
  }else{
    
    nombres_sentidos<-c(nombres_sentidos, paste0("sentido", nombres_cort[[i]][length(nombres_cort[[i]])]))
    
  }
  
}

for(i in 1:length(barcoding)){
  
  print(i)
  
  cadena<-DNAString(as.character(read.csv(barcoding[i])[1,]))
  
  cadena_c<-complement(cadena)
  
  cadena_r<-reverse(cadena)
  
  cadena_rc<-reverseComplement(cadena)
  
  columna<-as.character(data[,(indice_barcoding[i]+3)])
  
  columna[is.na(columna)]<-""
  
  for(j in 1:length(columna)){
    
    sentido = ""
    if(columna[j] != ""){
      col_adn = borrar_indel(DNAString(columna[j]))
      if(cadena == col_adn){
        
        sentido<-"forward"
        
      }else{
        
        if(cadena_c == col_adn){
          
          sentido<-"forward complementario"
          
        }else{
          
          if(cadena_r == col_adn){
            
            sentido<-"reverse"
            
          }else{
            
            if(cadena_rc == col_adn){
              
              sentido<-"reverse complementario"
              
            }else{
              
              sentido<-"Error"
            } 
          } 
        } 
      }   
    }
    
    if(j == 1){
      
      sentidos<-sentido
      
    }else{
      
      sentidos<-c(sentidos, sentido)
    }
  }
  
  if(i == 1){
    
    sentidos_todos<-sentidos
    
  }else{
    
    sentidos_todos<-cbind(sentidos_todos, sentidos)
  }
}

sentidos_todos<-as.data.frame(sentidos_todos)
colnames(sentidos_todos)<-nombres_sentidos

sentidos_todos$sentido_IGL<-sentido_unico(sentidos_todos$sentido_IGL_C, sentidos_todos$sentido_IGL_T, data$barcoding_IGL_C, data$barcoding_IGL_T)
sentidos_todos$sentido_IGL_C<-sentidos_todos$sentido_IGL
sentidos_todos$sentido_IGL_T<-NULL
sentidos_todos$sentido_IGL<-NULL
colnames(sentidos_todos)[colnames(sentidos_todos) == "sentido_IGL_C"]<-"sentido_IGL"

data_IGL<-obtener_minimos_2_lista(data[,c(indice_barcoding[11]:(indice_barcoding[11]+3))], data[,c(indice_barcoding[12]:(indice_barcoding[12]+3))])

data_IGL$`2`[is.na(data_IGL$`2`)]<-""
data_IGL$`3`[is.na(data_IGL$`3`)]<-""
data_IGL$`4`[is.na(data_IGL$`4`)]<-""

data[,c(indice_barcoding[11]:(indice_barcoding[11]+3))]<-data_IGL

for(i in c(indice_barcoding[12]:(indice_barcoding[12]+3))){
  data[,indice_barcoding[12]]<-NULL  
}


colnames(data)[c(indice_barcoding[11]:(indice_barcoding[11]+3))]<-c("barcoding_IGL", "query_barcoding_IGL", "sbjct_barcoding_IGL", "barcoding_barcoding_IGL")
indice_barcoding<-seq(from = 2, to = ncol(data), by = 4)

#Se eliminan secuencias que tienen sentidos en dos direcciones y secuencias que no alinean

data$sentido<-""

data_aux<-data

bandera<-T

for(i in 1:nrow(sentidos_todos)){
  
  print(i)
  sentido<-unique(as.character(as.matrix(sentidos_todos[i,sentidos_todos[i,] != ""])))
  largo<-length(sentido)
  
  if(largo == 1){
    
    data_aux$sentido<-sentido
    
    if(bandera == T){
      
      data<-data_aux[i,]
      bandera<-F
      
    }else{
      
      data<-rbind(data, data_aux[i,])
    }
  }
}

data_aux<-NULL

if(nrow(data) != 0){
  
  cat("Paso 2\n")
  cat("Las secuencias que tienen solo motivos en un sentido son:", nrow(data), "\n")
  
  #query a string
  for(i in (indice_barcoding+1)){
    
    data[,i]<-as.character(data[,i])
  }
  
  #sbjct a string
  for(i in (indice_barcoding+2)){
    
    data[,i]<-as.character(data[,i])
  }
  
  #Eliminar reads que tengan más de un pacientes o tipo de Ig
  
  #Para pacientes
  contador<-0
  bandera<-T
  for(i in 1:nrow(data)){
    print(i)
    paciente<-paste0(data[i,indice_barcoding[1:5]+1], collapse = " ")
    paciente<-strsplit(paciente, " ")[[1]]
    paciente<-paciente[paciente != ""]
    if(length(paciente) > 1){
      contador<-contador+1
      if(bandera == T){
        indice_eliminar<-i
        bandera<-F
      }else{
        
        indice_eliminar<-c(indice_eliminar, i)
      }
    }
  }
  
  if(exists("indice_eliminar")){
    
    data<-data[c(1:nrow(data))[-indice_eliminar],] 
    rm(indice_eliminar)
  }
  
  
  
  cat("Paso 3\n")
  
  if(nrow(data) != 0){
    #Para Ig
    contador<-0
    bandera<-T
    for(i in 1:nrow(data)){
      print(i)
      Ig<-paste0(data[i,indice_barcoding[7:11]+1], collapse = " ")
      Ig<-strsplit(Ig, " ")[[1]]
      Ig<-paciente[Ig != ""]
      if(length(Ig) > 1){
        contador<-contador+1
        if(bandera == T){
          indice_eliminar<-i
          bandera<-F
        }else{
          
          indice_eliminar<-c(indice_eliminar, i)
        }
      }
    }
    
    
    if(exists("indice_eliminar")){
      
      data<-data[c(1:nrow(data))[-indice_eliminar],] 
      rm(indice_eliminar)
    }
    
    cat("reads con paciente o Ig unico", nrow(data), "\n")
    
    write.csv(data, nombre_salida_data_total, row.names = F)
  }
}
print("El script ha finalizado con exito")
