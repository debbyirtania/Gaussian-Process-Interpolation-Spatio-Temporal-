###PACKAGES###
library(maptools)
library(spdep)
library(sp)
library(RColorBrewer)
library(lattice)
library(gstat)
library(raster)
require(splancs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(extrafont)
library(ggmap)
library(maps)
library(mapdata)
library(broom)

data
attach(data)
#### MAP INDO ###
dataIndo<-getData('GADM',country='IDN',level=1)
Indo
Indo$NAME_1
jabar<-Indo[Indo$NAME_1=="Jawa Barat",]
plot(jabar)
row.names(jabar)<-c("Jawa Barat")
COOR<-coordinates(jabar)
COOR
plot(jabar,axes=T,col="gray90")
text(data[4,1],data[5,1],"stasiun 1",col="black",cex=0.8,pos=1.5)
points(data[4,1],data[5,1],pch=19,cex=0.7,col="red")
text(COOR[,1],COOR[,2],row.names(jabar),col="black",cex=0.8,pos=1.5)
points(COOR[,1],COOR[,2],pch=19,cex=0.7,col="red")

grd<-makegrid(jabar,n=10000)
colnames(grd)<-c('x','y')
grd
str(jabar)
outline<-jabar@polygons[[1]]@Polygons[[3]]@coords
outline
new_grd<-grd[inout(grd,outline),]
new_grd
coordinates(new_grd)<-~x+y
coordinates(new_grd)

## INDO - KOTA/KAB
Indo1<-getData('GADM',country='IDN',level=2)
Indo1
Indo1$NAME_2
jabar1<-Indo1[Indo1$NAME_1=="Jawa Barat",]
jabar1
jabar1@data
plot(jabar1)
row.names(jabar)<-c("Jawa Barat")
jabar@data
loc_t=broom::tidy(jabar1,region="NAME_2") 
Loc=fortify(loc_t)
head(Loc)

### GAUSSIAN PROCESS INTERPOLATION ###

attach(data)
library(spTimer)
library(spT.Gibbs)
require(spT.Gibbs)
library(timeSeries)
library(forecast)
library(tseries)


rp<-coordinates(new_grd)
Rain.Prediction<-as.data.frame(rbind(rp,rp,rp,rp,rp,rp,rp,rp,rp,rp,rp,rp))


set.seed(11)
post.gp.rain<-spT.Gibbs(formula=CH ~ 1 , data=data, model ="AR", coords =~x+y,scale.transform="SQRT",spatial.decay=spT.decay(distribution=Gamm(2,2),tuning=0.1))
summary(post.gp.rain)
post.gp.rain$priors
print(post.gp.rain)
names(post.gp.rain)



##histogram plot 
hist(post.gp.rain$rhop,prob=TRUE,main="Histogram of rho",xlab="rho",30)
lines(density(post.gp.rain$rhop))
hist(post.gp.rain$betap,prob=TRUE,main="Histogram of intercept",xlab="intercept",30)
lines(density(post.gp.rain$betap))
hist(post.gp.rain$sig2ep,prob=TRUE,main="Histogram of sig2ep", xlab="sig2ep",breaks=30)
lines(density(post.gp.rain$sig2ep))
hist(post.gp.rain$sig2etap,prob=TRUE,main="Histogram of sig2eta",xlab="sig2eta",breaks=20)
lines(density(post.gp.rain$sig2etap))
hist(post.gp.rain$phip,prob=TRUE,main="Histogram of phi", xlab="phi",breaks=20)
lines(density(post.gp.rain$phip))
par(mfrow=c(1,2))


##normalitas


resid(post.gp.rain)
plot(resid(post.gp.rain))
post.gp.rain$fitted
plot(post.gp.rain$fitted[,1])
plot(post.gp.rain)
plot(post.gp.rain,residual=TRUE)
summary(post.gp.rain,pack="coda") # mcmc summary statistics using coda package
confint(post.gp.rain)
formula(post.gp.rain)


##prediction
set.seed(11)
pred.gp.rain<-predict(post.gp.rain,newdata=Rain.Prediction, newcoords=~x+y)
pred.gp.rain$predN
names(pred.gp.rain)
summary(pred.gp.rain)
Mean<-t(pred.gp.rain$Mean)
save.image(file = "my_work_space.RData")

pred.gp.rain$pred.coords
MAP<-as.data.frame(cbind(Mean,pred.gp.rain$pred.coords))
MAP
colnames(MAP)<-c("A","B","C","D","E","F","G","H","I","J","K","L","x","y")
MAP
write.csv(MAP,file="MAPAR.csv")
summary(MAP)
library(sp)

#true value
tv<-read.csv(file.choose(),sep=";")
o<-tv[,c(1:2,3)]
plot(jabar)
ras<-rasterFromXYZ(o)

##plot

plot(jabar)
MAP[,c(13:14,1)]
attach(MAP)
rasjan<-rasterFromXYZ(MAP[,c(13:14,1)])
rasfeb<-rasterFromXYZ(MAP[,c(13:14,2)])
rasmar<-rasterFromXYZ(MAP[,c(13:14,3)])
rasapr<-rasterFromXYZ(MAP[,c(13:14,4)])
rasmei<-rasterFromXYZ(MAP[,c(13:14,5)])
rasjun<-rasterFromXYZ(MAP[,c(13:14,6)])
rasjul<-rasterFromXYZ(MAP[,c(13:14,7)])
rasags<-rasterFromXYZ(MAP[,c(13:14,8)])
rassep<-rasterFromXYZ(MAP[,c(13:14,9)])
rasokt<-rasterFromXYZ(MAP[,c(13:14,10)])
rasnov<-rasterFromXYZ(MAP[,c(13:14,11)])
rasdes<-rasterFromXYZ(MAP[,c(13:14,12)])
plot(rasjan,xlab="longitude",ylab="latitude",main="Rain in January",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasfeb,xlab="longitude",ylab="latitude",main="Rain in February",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasmar,xlab="longitude",ylab="latitude",main="Rain in March",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasapr,xlab="longitude",ylab="latitude",main="Rain in April",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasmei,xlab="longitude",ylab="latitude",main="Rain in May",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasjun,xlab="longitude",ylab="latitude",main="Rain in June",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasjul,xlab="longitude",ylab="latitude",main="Rain in July",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasags,xlab="longitude",ylab="latitude",main="Rain in August",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rassep,xlab="longitude",ylab="latitude",main="Rain in September",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasokt,xlab="longitude",ylab="latitude",main="Rain in October",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasnov,xlab="longitude",ylab="latitude",main="Rain in November",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))
plot(rasdes,xlab="longitude",ylab="latitude",main="Rain in December",legend.args=list(text='rainfall mm',side=4,font=2,line=2.5,cex=0.8))

attach(MAP)
data
MAP
#ggplot
ggplot()+geom_tile(data = MAP, aes(x = x, y =y, fill = L), alpha = 1) +
  geom_sf()+
  #stat_contour(data = MAP, aes(x = x, y = y, z = B), color="black") +
  ggtitle("December") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_distiller(palette="Spectral", na.value="white") +
  scale_fill_viridis_c(name="precipitation",direction = -1) +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  coord_map()

MAP<-read.csv(file.choose())
MAP
MAP[,-1]
##PLOT COBA
MAPS=MAP%>%as.data.frame%>%
  gather(Month,Precipitation,A:L)
MAPS
Month_names <- c( `A` = "January", `B` = "February", `C` = "March", `D` = "April", `E` = "May", `F` = "June",`G` = "July",`H` = "August", `I` = "September", `J` = "October", `K` = "November", `L` = "December")
ggplot(data=MAPS,aes(x=x, y=y)) + geom_tile(aes(fill=Precipitation)) + facet_wrap(.~Month,labeller = as_labeller(Month_names), ncol=4)+
  coord_fixed(ratio = 1) + scale_fill_gradientn(colours = rev(rainbow(7))) +
  theme_bw()+ylab("Latitude")+xlab("Longitude")+
  labs(fill = "Prepicipitation")+ theme(legend.position="bottom")+ theme(text
                                                                         = element_text(size=12)) +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(fill = guide_colorbar(title.position = "left", title.vjust = 1,
                               frame.colour = "black", barwidth = 15, barheight = 1.7))+
  geom_map(data = Loc,map =Loc,aes(x=long,y=lat,map_id=id),fill="white",alpha=0.0,color="white",size=0.1)

