library(Biostrings)

archivo_fastas<-"todos_2Dpass.fasta"
carpeta_salida<-"fastas_individuales_2D_pass/"

inmuno_globulinas<-readDNAStringSet(archivo_fastas)

for(i in 1:length(inmuno_globulinas)){
  
  identificador<-strsplit(names(inmuno_globulinas[i]), " ")[[1]][1]
  print(paste0(i, ": ", identificador))
  writeXStringSet(inmuno_globulinas[i], paste0(carpeta_salida,identificador,".fasta"), format="fasta")
  
}
