#/home/postgrado/rahumada/miniconda2/bin/
options(warn = -1)
library(Biostrings)

args<-commandArgs(TRUE)

carpeta = args[1]       
inicio = as.numeric(args[2])
fin = as.numeric(args[3])

#carpeta = "./2D_mayor_a_9/"       
#inicio = 1
#fin = 10

print(carpeta)
print(inicio)
print(fin)

carpeta_salida = "./resultados_distancia_secuencias_3/"
barcoding<-read.table("configuracion_barcoding.txt", sep = "\t", header = T)
barcoding<-as.character(barcoding$barcoding)

configuracion<-read.table("configuracion_alineamiento.txt", sep = "\t", header = T)
ruta_bl2seq = as.character(configuracion$ruta_bl2seq)
archivos<-paste0(carpeta,system(paste("ls", carpeta), intern = T))

obtener_distancia<-function(secuencia, barcoding){
  
  comando<-paste0(ruta_bl2seq,"bl2seq -p blastn -i ", secuencia, " -j ", barcoding, " | grep -m 1  -E 'Query:'")
  comando2<-paste0(ruta_bl2seq,"bl2seq -p blastn -i ", secuencia, " -j ", barcoding, " | grep -m 1  -E 'Sbjct:'")

  resultado_comando<-system(comando, intern = T)
  resultado_comando2<-system(comando2, intern = T)
  
  distancia<-Inf
  query<-""
  secuencia_B<-""
  sbjct<-""
  
  if(length(resultado_comando) != 0){
    
    query<-toupper(strsplit(resultado_comando, " ")[[1]][3])
    if(query == ""){
      query<-toupper(strsplit(resultado_comando, " ")[[1]][4])
      if(query == ""){
        query<-toupper(strsplit(resultado_comando, " ")[[1]][5])
        if(query == ""){
          query<-toupper(strsplit(resultado_comando, " ")[[1]][6])
        }
      }
    }
    
    sbjct<-toupper(strsplit(resultado_comando2, " ")[[1]][3])
    if(sbjct == ""){
      sbjct<-toupper(strsplit(resultado_comando2, " ")[[1]][4])
      if(sbjct == ""){
        sbjct<-toupper(strsplit(resultado_comando2, " ")[[1]][5])
        if(sbjct == ""){
          sbjct<-toupper(strsplit(resultado_comando2, " ")[[1]][6])
        }
      }
    }
    
    comando<-paste("awk 'NR == 2' ", barcoding)
    query2<-strsplit(system(comando, intern = T), "\r")[[1]][1]
    query2<-DNAString(query2)
    
    comando<-paste("./distancia_levenshtein", query, as.character(query2))
    distancia1<-as.numeric(system(comando, intern = T))
    
    comando<-paste("./distancia_levenshtein", query, as.character(complement(query2)))
    distancia2<-as.numeric(system(comando, intern = T))
    
    comando<-paste("./distancia_levenshtein", query, as.character(reverseComplement(query2)))
    distancia3<-as.numeric(system(comando, intern = T))
    
    comando<-paste("./distancia_levenshtein", query, as.character(reverse(query2)))
    distancia4<-as.numeric(system(comando, intern = T))
    
    
    distancia<-distancia1
    secuencia_B<-as.character(query2)
    
    if(distancia > distancia2){
      
      distancia<-distancia2
      secuencia_B<-as.character(complement(query2))
    }
    if(distancia > distancia3){
      
      distancia<-distancia3
      secuencia_B<-as.character(reverseComplement(query2))
    }
    if(distancia > distancia4){
      
      distancia<-distancia4
      secuencia_B<-as.character(reverse(query2))
    }
  }  
  
  lista<-list(distancia, query, sbjct, secuencia_B)
  
  return(lista)
}

         
         
for(i in inicio:fin){
  if(inicio != fin){
    print(paste(round(((i - inicio)/(fin - inicio))*100, digits = 0), "%"))
  }
  for(j in 1:length(barcoding)){
    distancia<-obtener_distancia(archivos[i], barcoding[j])
    if(j == 1){
      resultado<-cbind(strsplit(archivos[i], "/")[[1]][3], distancia[[1]], distancia[[2]], distancia[[3]], distancia[[4]])
    }else{
      resultado<-cbind(resultado, distancia[[1]], distancia[[2]], distancia[[3]], distancia[[4]])
    }
  }
  if(i == inicio){
    
    resultados_total<-resultado
  }else{
    
    resultados_total<-rbind(resultados_total, resultado)
    
  }
}

resultados_total<-as.data.frame(resultados_total)

cabecera<-"Identificador"

for(i in 1:length(barcoding)){

  nombre_barcoding<-strsplit(barcoding[i], ".fasta")[[1]][1]
  cabecera<-c(cabecera, nombre_barcoding, paste0("query_",nombre_barcoding), paste0("sbjct_", nombre_barcoding), paste0("barcoding_",nombre_barcoding))

}

colnames(resultados_total)<-cabecera

nombre_salida=paste0(carpeta_salida, strsplit(carpeta, "/")[[1]][2],"_", inicio,"_",fin,".csv")

write.csv(resultados_total,nombre_salida, row.names = F, quote = F)

print("100 %")

# contador<-0
# for(i in 1:9091){
#   
#   
#   if(as.character(resultados_total$secuencia[i]) != as.character(resultados_total$sbjct[i])){
#     print(i)
#     print("Hay")
#     contador<-contador+1
#   }
#   
# }
