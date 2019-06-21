library(ggplot2)
# 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_patterns <- function(wl, colors, y_max){
  colorby = "substitution"
  
  cambiar_label<-function(string) {
    string<-""
  }
  
  p<-ggplot(wl) +
    geom_col(aes_string(x = "context", y = "count", fill = "substitution"))+
    facet_grid(sample ~ substitution, labeller = labeller(sample = cambiar_label))+
    scale_fill_manual(values = colors)+
    theme_minimal()+
    theme(
      panel.grid.major.x = element_blank(),
      strip.text.y = element_text(size = 12),
      strip.text.x = element_text(size = 11),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      legend.position = "none"
    )+
    xlab("Motif") + ylab("Percentage")+
    ylim(0, y_max + 0.5)
  
    return(p)
}


# grafico_simple<-function(data){
#   
#   # table <- data %>%
#   #   dplyr::count(substitution)
#   
#   colors<-rep(c("blue", "black", "red", "gray", "green", "pink"),2)
#   
#   wl<-tidyr::gather(table, sample, count, -substitution, -context)
#   suma<-sum(wl$count)
#   wl$count<-round((wl$count/suma)*100, digits = 1)
#   
#   p<-ggplot(wl) +
#     geom_col(aes_string(x = "substitution", y = "count", fill = "substitution"))+
#     scale_fill_manual(values = colors)+
#     theme_minimal()+
#     theme(
#       panel.grid.major.x = element_blank(),
#       strip.text.y = element_text(size = 12),
#       legend.position = "none"
#     )+
#     xlab("Substitution") + ylab("Percent")
#   
#     return(p)
# }


data<-read.csv("frecuencia_sustituciones/constructo_total_forward.csv")
indice_insercion<-substr(as.character(data$substitution), 1, 1) == "-"
indice_delecion<-substr(as.character(data$substitution), 3, 3) == "-"

data_insercion<-data[indice_insercion,]
data_delecion<-data[indice_delecion,]

data_sustitucion<-data[!indice_insercion & !indice_delecion,]
data_sustitucion_context_estandar<-data_sustitucion[substr(as.character(data_sustitucion$context), 1, 1) != "-" & substr(as.character(data_sustitucion$context), 2, 2) != "-" & substr(as.character(data_sustitucion$context), 3, 3) != "-",]

table<-data_sustitucion_context_estandar

wl<-tidyr::gather(table, sample, count, -substitution, -context)
wl<-as.data.frame(wl)

colors_1<-c("blue", "black", "red")
colors_2<-c("gray", "green", "pink")
colors_3<-c("red", "black", "blue")
colors_4<-c("pink", "green", "gray")

wl<-wl[wl$sample != "X", ]
wl$count<-round(wl$count/sum(wl$count) * 100, digits = 2)

wl_C<-wl[substr(as.character(wl$substitution), 1, 1) == "C",]
wl_T<-wl[substr(as.character(wl$substitution), 1, 1) == "T",]
wl_G<-wl[substr(as.character(wl$substitution), 1, 1) == "G",]
wl_A<-wl[substr(as.character(wl$substitution), 1, 1) == "A",]

y_max<-max(wl$count)

p1<-plot_patterns(wl = wl_C, colors = colors_1, y_max = y_max)
p2<-plot_patterns(wl = wl_T, colors = colors_2, y_max = y_max)
p3<-plot_patterns(wl = wl_G, colors = colors_3, y_max = y_max)
p4<-plot_patterns(wl = wl_A, colors = colors_4, y_max = y_max)

multiplot(p1, p3, p2, p4, cols=2)

data_sustitucion_context_no_estandar<-data_sustitucion[substr(as.character(data_sustitucion$context), 1, 1) == "-" | substr(as.character(data_sustitucion$context), 2, 2) == "-" | substr(as.character(data_sustitucion$context), 3, 3) == "-",]

#deleciones
table<-data_delecion

wl<-tidyr::gather(table, sample, count, -substitution, -context)
wl<-as.data.frame(wl)
wl<-wl[wl$sample != "X", ]
wl$count<-round(wl$count/sum(wl$count) * 100, digits = 2)
y_max<-max(wl$count)

wl_C<-wl[substr(as.character(wl$substitution), 1, 1) == "C",]
wl_T<-wl[substr(as.character(wl$substitution), 1, 1) == "T",]
wl_G<-wl[substr(as.character(wl$substitution), 1, 1) == "G",]
wl_A<-wl[substr(as.character(wl$substitution), 1, 1) == "A",]

p1<-plot_patterns(wl = wl_C, colors = "blue", y_max = y_max)
p2<-plot_patterns(wl = wl_T, colors = "black", y_max = y_max)
p3<-plot_patterns(wl = wl_G, colors = "red", y_max = y_max)
p4<-plot_patterns(wl = wl_A, colors = "green", y_max = y_max)

multiplot(p1, p3, p2, p4, cols=2)

#inserciones
data_insercion_bases_conocidas<-data_insercion[substr(as.character(data_insercion$context), 1, 1) != "-" & substr(as.character(data_insercion$context), 3, 3) != "-",] 
table<-data_insercion_bases_conocidas

wl<-tidyr::gather(table, sample, count, -substitution, -context)
wl<-as.data.frame(wl)
wl<-wl[wl$sample != "X", ]
wl$count<-round(wl$count/sum(wl$count) * 100, digits = 2)
y_max<-max(wl$count)

wl_C<-wl[substr(as.character(wl$substitution), 3, 3) == "C",]
wl_T<-wl[substr(as.character(wl$substitution), 3, 3) == "T",]
wl_G<-wl[substr(as.character(wl$substitution), 3, 3) == "G",]
wl_A<-wl[substr(as.character(wl$substitution), 3, 3) == "A",]

p1<-plot_patterns(wl = wl_C, colors = "blue", y_max = y_max)
p2<-plot_patterns(wl = wl_T, colors = "black", y_max = y_max)
p3<-plot_patterns(wl = wl_G, colors = "red", y_max = y_max)
p4<-plot_patterns(wl = wl_A, colors = "green", y_max = y_max)

multiplot(p1, p3, p2, p4, cols=2)
