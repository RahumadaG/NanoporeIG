load("datos_total.RData")

datos<-as.data.frame(cbind(as.character(datos_total$read_id), as.numeric(as.character(datos_total$mean_qscore_template))))
colnames(datos)<-c("read_id","mean_qscore_template")

read_id_2<-as.character(datos[as.numeric(as.character(datos$mean_qscore_template)) >= 1 & as.numeric(as.character(datos$mean_qscore_template))< 2,]$read_id)

for(i in 1:length(read_id_2)){
  
  print(paste0(i, ": ", read_id_2[i]))
  
  file.copy(from = paste0("fastas_individuales_2D_fail/",read_id_2[i],".fasta"), to=paste0("2D_fail_2/", read_id_2[i], ".fasta"))
}


