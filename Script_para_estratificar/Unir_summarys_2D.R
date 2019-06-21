for(i in 0:362){

  nombre_archivo<-paste0("./summarys_2D/summary_",i,".txt")
  print(nombre_archivo)
  datos<-read.csv(nombre_archivo, sep = "\t")

  if(i == 0){
    datos_total<-datos

  }else{

    datos_total<-rbind(datos_total, datos)
  }
}


save(datos_total, file = "datos_total.RData")
