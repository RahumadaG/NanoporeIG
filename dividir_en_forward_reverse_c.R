nombre_archivo<-"resultados_proc_datos/2D_pass_9_1_14321_data_total.csv"
carpeta_salida<-"reads_totales_forward_reverse_c/"
nombre_salida_forward<-paste0(carpeta_salida, strsplit(strsplit(nombre_archivo, "/")[[1]][2], ".csv")[[1]], "_forward.csv")
nombre_salida_reverse_c<-paste0(carpeta_salida, strsplit(strsplit(nombre_archivo, "/")[[1]][2], ".csv")[[1]], "_reverse_c.csv")

data<-read.csv(nombre_archivo)
data_forward<-data[data$sentido == "forward",]
data_reverse_c<-data[data$sentido == "reverse complementario",]



write.csv(data_forward, nombre_salida_forward, row.names = F)
write.csv(data_reverse_c, nombre_salida_reverse_c, row.names = F)
