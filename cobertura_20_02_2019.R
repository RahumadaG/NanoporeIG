#Datos forward
datos<-read.csv("datos_analizar_covertura_forward.csv")
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

contadores_forward<-contadores
# porcentajes<-round(contadores/sum(contadores), 3)
# 
# 
# plot(NaN, NaN, type = "l", frame = FALSE, xlim = c(0,5), ylim = c(0,max(porcentajes)+0.05), 
#      xlab = "", ylab = "", xaxt='n',
#      cex.axis=0.75)
# 
# 
# for(i in 1:nrow(matriz)){
#   if(matriz[i, 1] == T){
#     lines(c(0.0,1.0), c(porcentajes[i],porcentajes[i]), col = "#33A6FF", lwd = 1)
#   }
#   
#   if(matriz[i, 2] == T){
#     lines(c(1.0,2.0), c(porcentajes[i],porcentajes[i]), col = "#F8BE03", lwd = 1)
#   }
#   
#   lines(c(2.0,3.0), c(porcentajes[i],porcentajes[i]), col = "#F2634B", lwd = 1)
#   
#   if(matriz[i, 3] == T){
#     lines(c(3.0,4.0), c(porcentajes[i],porcentajes[i]), col = "#33A6FF", lwd = 1)
#   }
#   
#   if(matriz[i, 4] == T){
#     lines(c(4.0,5.0), c(porcentajes[i],porcentajes[i]), col = "#F8BE03", lwd = 1)
#   }
# 
# }
# 
# text(0.5,0, "5' Index", cex = 0.7)
# text(1.5,0, "Anchor", cex = 0.7)
# text(2.5,0, "V(D)J Rearragement", cex = 0.7)
# text(3.5,0, "Constant Chain", cex = 0.7)
# text(4.5,0, "End Adaptor 3'", cex = 0.7)



#Datos reverse
datos<-read.csv("datos_analizar_covertura_reverse_c.csv")
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

contadores_reverse<-contadores




plot(NaN, NaN, type = "l", frame = FALSE, xlim = c(0,5), ylim = c(0,100000), xaxt='n',
     cex.axis=0.75, xlab = "Amplicons' Structure",
     ylab = "Reads", cex.lab = 0.75)


for(i in 1:nrow(matriz)){
  if(matriz[i, 1] == T){
    lines(c(0.0,1.0), c(contadores_forward[i],contadores_forward[i]), col = "blue", lwd = 1)
  }
  
  if(matriz[i, 2] == T){
    lines(c(1.0,2.0), c(contadores_forward[i],contadores_forward[i]), col = "blue", lwd = 1)
  }
  
  lines(c(2.0,3.0), c(contadores_forward[i],contadores_forward[i]), col = "blue", lwd = 1)
  
  if(matriz[i, 3] == T){
    lines(c(3.0,4.0), c(contadores_forward[i],contadores_forward[i]), col = "blue", lwd = 1)
  }
  
  if(matriz[i, 4] == T){
    lines(c(4.0,5.0), c(contadores_forward[i],contadores_forward[i]), col = "blue", lwd = 1)
  }
}

for(i in 1:nrow(matriz)){
  if(matriz[i, 1] == T){
    lines(c(0.0,1.0), c(contadores_reverse[i],contadores_reverse[i]), col = "red", lwd = 1)
  }
  
  if(matriz[i, 2] == T){
    lines(c(1.0,2.0), c(contadores_reverse[i],contadores_reverse[i]), col = "red", lwd = 1)
  }
  
  lines(c(2.0,3.0), c(contadores_reverse[i],contadores_reverse[i]), col = "red", lwd = 1)
  
  if(matriz[i, 3] == T){
    lines(c(3.0,4.0), c(contadores_reverse[i],contadores_reverse[i]), col = "red", lwd = 1)
  }
  
  if(matriz[i, 4] == T){
    lines(c(4.0,5.0), c(contadores_reverse[i],contadores_reverse[i]), col = "red", lwd = 1)
  }
}

text(0.5,0, "5' Index", cex = 0.7)
text(1.5,0, "Anchor", cex = 0.7)
text(2.5,0, "V(D)J Rearragement", cex = 0.7)
text(3.5,0, "Constant Chain", cex = 0.7)
text(4.5,0, "End Adaptor 3'", cex = 0.7)
