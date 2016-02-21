#############################Isotop########################################
library(ggplot2)
setwd("/home/zsuzsa/Documents/data_vis")

data <- read.csv(file = "IsotopeData.csv")
data <- data[-1]
colnames(data)[2:3] <- c("carbon", "nitrogen") 

data$Species <- factor(data$Species, levels=c(1,2,3,4,5), labels=c('T.inermis','T.raschii','T.longi',"M.norvegica","N.megalops"))

scatter = ggplot( data, aes(x =carbon , y= nitrogen , color = factor(Species), shape = factor(Species))) + #color = factor(popularity), continuous vs discrete, 
       geom_point(size = 3 ) + 
       scale_colour_discrete(name="Species") +
       scale_shape_discrete(name="Species") +
       labs(title="Stable isotopes of carbon and nitrogen by species") +
       theme(legend.justification=c(0,1), legend.position=c(0,1))

library(reshape2)
datamelt<-melt(data, measure.vars = 2:3)   
box = ggplot(datamelt, aes(x=factor(Species), y=value, fill=factor(Species)))+
  geom_boxplot()+
  facet_wrap( ~variable, scales = "free")+
  labs(x="Species") +
  #theme(axis.text.x=element_text(angle=0, vjust=0.4,hjust=1)) +
  theme(legend.position="none") +
  labs(title="Distribution of stable isotopes of carbon and nitrogen by species") +

library(grid)
#Funtion to create multiple plot setup
vp.setup <- function(x,y) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(x,y)))
}
#Function to access layout
vp.layout <- function(x,y){
  viewport(layout.pos.row=x,layout.pos.col=y)
}

pdf(file = "isotope.pdf")
#Set layout
vp.setup(2,1)

#Place plots

print(scatter,vp=vp.layout(1,1))
print(box,vp=vp.layout(2,1))
dev.off()

##########################Temperature#################################

cl_data<-read.csv("eklima2.csv")
cl_data<-cl_data[,2:4]
cl_data$time<-paste(month.abb[as.numeric(cl_data$m)], cl_data$y, sep="-" ) 


cl_data$scaled = ave(cl_data$ta, cl_data$m, FUN = function(.x) (.x-mean(.x))/sd(.x) ) 
#tapply(cl_data$scaled , cl_data$m , function(x) sd(x) )


ggplot(cl_data, aes( factor(y),factor(m)))+
  geom_tile(data=cl_data, aes(fill=scaled), color="white")+
  scale_fill_gradient2(low="blue", high="red", mid="lightgrey" )

ggplot(cl_data, aes(x=y, y=ta) ) +
  geom_line() +
  facet_grid( m~. , scales = "free_x" )
