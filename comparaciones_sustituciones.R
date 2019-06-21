library(Biostrings)
library(tidyr)

ver_sustituciones<-function(datos){
  
datos_analizar<-datos[!(as.character(datos$sbjct) == as.character(datos$query)), ]
  
 cantidad_filas<-nrow(datos_analizar)
  for(i in 1:cantidad_filas){
    cat(i, " de ", cantidad_filas, "\n")
    query<-DNAString(as.character(datos_analizar$query[i]))
    sbjct<-DNAString(as.character(datos_analizar$sbjct[i]))
    
    if(as.character(query) != "" & as.character(sbjct) != ""){
    
      bandera<-TRUE
      for(j in 1:length(query)){
        
        if(query[j] != sbjct[j]){
          
          if(bandera == TRUE){
            
            pos<-j
            ref<-as.character(sbjct[j])
            alt<-as.character(query[j])
            
            if(j == 1){
              
              ant<-"-"
              
            }else{
              
              ant<-as.character(sbjct[(j - 1)])
            }
            
            if(j == length(sbjct)){
              
              sig<-"-"
              
            }else{
              
              sig<-as.character(sbjct[(j + 1)])
            }
            
            bandera<-FALSE
          
            }else{
              pos<-c(pos, j)
              ref<-c(ref, as.character(sbjct[j]))
              alt<-c(alt, as.character(query[j]))
              
              if(j == 1){
                
                ant<-c(ant, "-")
                
              }else{
                
                ant<-c(ant, as.character(sbjct[(j - 1)]))
              }
              
              if(j == length(sbjct)){
                
                sig<-c(sig, "-")
                
              }else{
                
                sig<-c(sig, as.character(sbjct[(j + 1)]))
              }
          }
        }
      }
      
      datos_secuencia<-cbind(rep(as.character(datos_analizar$Id[i]), length(pos)), pos, ref, alt, ant, sig)
      
      if(i == 1){
        
        datos_secuencia_total<-datos_secuencia
        
      }else{
        
        datos_secuencia_total<-rbind(datos_secuencia_total, datos_secuencia)
      }
    }
  }
  
  datos_secuencia_total<-as.data.frame(datos_secuencia_total)
  colnames(datos_secuencia_total)<-c("secuencia", "pos", "ref", "alt", "ant", "sig")
  
  data<-datos_secuencia_total
  
  data$substitution<-paste0(as.character(data$ref), ">", as.character(data$alt))
  data$context<-paste0(as.character(data$ant), as.character(data$ref), as.character(data$sig))
  
  return(data)
}

nombre_archivo<-"constructo/constructo_total_forward.csv"
carpeta_salida<-"frecuencia_sustituciones/"

archivo_salida<-paste0(carpeta_salida, strsplit(nombre_archivo, "/")[[1]][2])

datos<-read.csv(nombre_archivo)
data<-ver_sustituciones(datos)
table <- data %>%
  dplyr::count(substitution, context)
table<-as.data.frame(table)
View(table)

write.csv(table, archivo_salida)






