obtener_contadores<-function(datos){
  
  datos$X<-NULL
  #indices interes
  
  indices<-seq(from = 2, to = 50, by = 4)
  indices<-c(indices, 51)
  
  # datos de index analizar
  datos<-datos[,indices]
  rm(indices)
  datos$sentido<-NULL
  print(all(datos$tiene_Ig))
  datos$tiene_Ig<-NULL
  datos$index<-datos$barcoding_DCR != Inf | datos$barcoding_JGP != Inf | datos$barcoding_LAH != Inf | datos$barcoding_MAN != Inf | datos$barcoding_MVC != Inf
  
  datos$barcoding_DCR<-NULL
  datos$barcoding_JGP<-NULL
  datos$barcoding_LAH<-NULL
  datos$barcoding_MAN<-NULL
  datos$barcoding_MVC<-NULL
  
  datos$Anchor<-datos$Switching_Anchor != Inf
  datos$Switching_Anchor<-NULL
  
  datos$constante_chain<-datos$barcoding_IGA != Inf | datos$barcoding_IGG != Inf | datos$barcoding_IGM != Inf | datos$barcoding_IGK != Inf | datos$barcoding_IGL != Inf
  
  datos$barcoding_IGA<-NULL
  datos$barcoding_IGG<-NULL
  datos$barcoding_IGM<-NULL
  datos$barcoding_IGK<-NULL
  datos$barcoding_IGL<-NULL
  
  
  datos$end<-datos$barcoding_reverse != Inf
  datos$barcoding_reverse<-NULL
  
  matriz=matrix(nrow=15,ncol=4, c(T,F,F,F,
                                  T,T,F,F,
                                  T,F,T,F,
                                  T,F,F,T,
                                  T,T,T,F,
                                  T,T,F,T,
                                  T,F,T,T,
                                  T,T,T,T,
                                  F,T,F,F,
                                  F,T,T,F,
                                  F,T,F,T,
                                  F,T,T,T,
                                  F,F,T,F,
                                  F,F,T,T,
                                  F,F,F,T),byrow=TRUE)
  
  for(i in 1:nrow(matriz)){
    
    if(i == 1){
      
      contadores<-nrow(datos[datos$index == matriz[i,1] & datos$Anchor == matriz[i,2]& datos$constante_chain == matriz[i,3] & datos$end == matriz[i,4],])
      
    }else{
      
      contadores<-c(contadores,nrow(datos[datos$index == matriz[i,1] & datos$Anchor == matriz[i,2]& datos$constante_chain == matriz[i,3] & datos$end == matriz[i,4],]))
    }  
    
  }  
  
  return(contadores)
}

plot_resultados<-function(contadores, main, n = 1){
  
  matriz=matrix(nrow=15,ncol=4, c(T,F,F,F,
                                  T,T,F,F,
                                  T,F,T,F,
                                  T,F,F,T,
                                  T,T,T,F,
                                  T,T,F,T,
                                  T,F,T,T,
                                  T,T,T,T,
                                  F,T,F,F,
                                  F,T,T,F,
                                  F,T,F,T,
                                  F,T,T,T,
                                  F,F,T,F,
                                  F,F,T,T,
                                  F,F,F,T),byrow=TRUE)
  
  if(n == 1){
    
    plot(NaN, NaN, type = "l", frame = FALSE, xlim = c(0,5), ylim = c(0,150000), xaxt='n',
         cex.axis=1.8, xlab = "Estructura de los amplicones",
         ylab = "Reads", cex.lab = 1.8, cex.main = 2,
         main = main)
    
  }else{
    
    plot(NaN, NaN, type = "l", frame = FALSE, xlim = c(0,5), ylim = c(0,100000), xaxt='n',
         yaxt='n',
       xlab = "Amplicons Structure",
         ylab = "",
         main = main,
         bty="n")
    
  }
  
  
  for(i in 1:nrow(matriz)){
    if(matriz[i, 1] == T){
      lines(c(0.0,1.0), c(contadores[i],contadores[i]), col = "#4CAB8B", lwd = 0.5)
    }
    
    if(matriz[i, 2] == T){
      lines(c(1.0,2.0), c(contadores[i],contadores[i]), col = "#527DC2", lwd = 0.5)
    }
    
    lines(c(2.0,3.0), c(contadores[i],contadores[i]), col = "#FA8258", lwd = 0.5)
    
    if(matriz[i, 3] == T){
      lines(c(3.0,4.0), c(contadores[i],contadores[i]), col = "#4CAB8B", lwd = 0.5)
    }
    
    if(matriz[i, 4] == T){
      lines(c(4.0,5.0), c(contadores[i],contadores[i]), col = "#527DC2", lwd = 0.5)
    }
    
  }
  
  text(0.5,0, "5' Index", cex = 1.5)
  text(1.5,0, "Anchor", cex = 1.5)
  text(2.5,0, "V(D)J Rearrangement", cex = 1.5)
  text(3.5,0, "Constant Chain", cex = 1.5)
  text(4.5,0, "End Adaptor 3'", cex = 1.5)
}



datos<-read.csv("datos_analizar_covertura_forward.csv")
contadores_forward<-obtener_contadores(datos)
datos<-read.csv("datos_analizar_covertura_reverse_c.csv")
contadores_reverse<-obtener_contadores(datos)

#par(mfrow=c(1,2))

#par (ps = 12, cex = 1, cex.main = 1)

bitmap("../../Escritorio/Escrito tesis final/cobertura_total_2.tiff", height = 8, width = 17.5, units = 'cm', type="tiff24nc", res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot_resultados(contadores_forward + contadores_reverse, "Cobertura", 1)
dev.off()
#plot_resultados(contadores_reverse, "Read Reverse Complement\nCoverage", 2)





