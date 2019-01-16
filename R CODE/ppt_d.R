#????????????????????????
library(statnet)
library(UserNetR)
data(Moreno)
gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

gender
temp <- Moreno %s% which(gender<2)#??????

Moreno
temp

plot(temp, vertex.cex = 1.2)
plot(temp, vertex.cex = 1.2)
plot(temp, vertex.cex = 1.2)


#???????????????????????????
alldeg <- degree(temp)
temp %v% "alldeg" <- degree(temp)#??????

test <- temp %s% which(alldeg>5)

test

plot(test, vertex.cex = 1.2)


#?????????????????????
library(statnet)
library(UserNetR)
data(Moreno)

plot(Moreno,mode="circle",vertex.cex=1.5)

plot(Moreno,mode="fruchtermanreingold",vertex.cex=1.5)



library(statnet)
library(UserNetR)
data(Moreno)
op <- par(mar = rep(0, 4),mfrow=c(1,2))#??????????????????
plot(Moreno,mode="circle",vertex.cex=1.5)
plot(Moreno,mode="fruchtermanreingold",vertex.cex=1.5)
par(op)



library(statnet)
library(UserNetR)
data(Moreno)
op <- par(mar = c(0,0,4,0),mfrow=c(1,2))
gplot(Moreno,gmode="graph",mode="random",vertex.cex=1.5,main="Random layout")
gplot(Moreno,gmode="graph",mode="fruchtermanreingold",vertex.cex=1.5,main="Fruchterman-Reingold")
par(op)


#??????layout.par????????????
library(statnet)
library(UserNetR)
data(Moreno)
gender <- Moreno %v% "gender"
temp <- Moreno %s% which(gender<2)
alldeg <- degree(temp)
temp %v% "alldeg" <- degree(temp)
test <- temp %s% which(alldeg>2)
test

op <- par(mar = c(0,0,4,0),mfrow=c(1,2))
set.seed(123)
plot(test, vertex.cex=1.5, layout.par=list(niter=0-1))
set.seed(123)
gplot(test, vertex.cex=1.5, layout.par=list(niter=0-1))
par(op)

op <- par(mar = c(0,0,4,0),mfrow=c(1,2))
plot(test, vertex.cex=1.5, layout.par=list(niter=0-1))
gplot(test, vertex.cex=1.5, layout.par=list(niter=0-1))
par(op)

#???????????????layout??????
op <- par(mar=c(0,0,0,0),mfrow=c(2,3))
for(k in seq(0,50,by=10)){
  set.seed(123)
  plot(test, vertex.cex=1.5, layout.par=list(niter=k-1))
}
par(op)

op <- par(mar = c(0,0,0,0),mfrow=c(1,1))
set.seed(123)
plot(test, vertex.cex=1.5)
par(op)


#???????????????????????????
library(statnet)
library(UserNetR)
data(Bali)
op <- par(mar=c(0,0,4,0),mfrow=c(2,3))
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='circle',main="circle")
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='eigen',main="eigen")
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='random',main="random")
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='spring',main="spring")
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='fruchtermanreingold',main='fruchtermanreingold')
gplot(Bali,gmode="graph",edge.col="grey75",vertex.cex=1.5,mode='kamadakawai',main='kamadakawai')
par(op)


#
library(statnet)
library(UserNetR)
data(Bali)
op <- par(mar = rep(0, 4),mfrow=c(1,1))
mycoords1 <- gplot(Bali,gmode="graph",vertex.cex=1.5)
mycoords2 <- mycoords1
mycoords2[,2] <- mycoords1[,2]*1.5
mycoords1
mycoords2
par(op)

op <- par(mar=c(4,3,4,3),mfrow=c(1,2))
gplot(Bali,gmode="graph",coord=mycoords1,vertex.cex=1.5,suppress.axes = FALSE,ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),main="Original coordinates")
gplot(Bali,gmode="graph",coord=mycoords2,vertex.cex=1.5,suppress.axes = FALSE,ylim=c(min(mycoords2[,2])-1,max(mycoords2[,2])+1),main="Modified coordinates")
par(op)


#??????plot?????????
detach(package:statnet)
library(igraph)
library(intergraph)
iBali <- asIgraph(Bali)
op <- par(mar=c(0,0,3,0),mfrow=c(1,3))
plot(iBali,layout=layout_in_circle,main="Circle")
plot(iBali,layout=layout_randomly,main="Random")
plot(iBali,layout=layout_with_kk,main="Kamada-Kawai")
par(op)


#???????????????????????????
detach("package:igraph", unload=TRUE)
library(statnet)
library(UserNetR)
data(Bali)
col2rgb('slateblue2')#????????????,?????????colors(),rgb(),vertex.col option can be used to change colors
op <- par(mar=c(0,0,0,0),mfrow=c(1,3))
set.seed(123)
gplot(Bali,vertex.col="slateblue2",gmode="graph")
set.seed(123)
gplot(Bali,vertex.col=rgb(122,103,238,maxColorValue=255),gmode="graph")
set.seed(123)
gplot(Bali,vertex.col="#7A67EE",gmode="graph")
par(op)



ndum <- rgraph(300,tprob=0.025,mode="graph")
op <- par(mar = c(0,0,2,0),mfrow=c(1,2))
gplot(ndum,gmode="graph",vertex.cex=2,vertex.col=rgb(0,0,139,maxColorValue=255),edge.col="grey80",edge.lwd=0.5,main="Fully opaque")
gplot(ndum,gmode="graph",vertex.cex=2,vertex.col=rgb(0,0,139,alpha=80,maxColorValue=255),edge.col="grey80",edge.lwd=0.5,main="Partly transparent")
par(op)


#different colora can be assigned to different nodes in a graph
#try palette(),library(RColorBrewer),display.brewer.pal(5, "Dark2")
rolelab <- Bali %v% "role"
as.integer(as.factor(rolelab))
op <- par(mar=c(0,0,0,0))
set.seed(123)
plot(Bali,usearrows=FALSE,vertex.cex=1.5,label=rolelab,displaylabels=T,vertex.col=as.integer(as.factor(rolelab)))
par(op)


palette()
library(RColorBrewer)
display.brewer.pal(5, "Dark2")
my_pal <- brewer.pal(5,"Dark2")
my_pal
Bali
rolelab <- Bali %v% "role"###??????
op <- par(mar=c(0,0,0,0))
set.seed(123)
plot(Bali,vertex.cex=1.5,label=rolelab,displaylabels=T,vertex.col=my_pal[as.integer(as.factor(rolelab))])
par(op)

#change the nodes shape
op <- par(mar=c(0,0,0,0))
set.seed(123)
plot(Bali,vertex.cex=3,label=rolelab,displaylabels=T,vertex.col=my_pal[as.integer(as.factor(rolelab))],vertex.sides=c(3:7)[as.integer(as.factor(rolelab))])
par(op)


#???igraph ??????
detach("package:igraph", unload=TRUE)
library(statnet)
library(UserNetR)
data(Bali)
op <- par(mar = c(0,0,2,0),mfrow=c(1,3))
set.seed(123)
plot(Bali,vertex.cex=0.5,main="Too small")
set.seed(123)
plot(Bali,vertex.cex=2,main="Just right")
set.seed(123)
plot(Bali,vertex.cex=6,main="Too large")
par(op)


#??? degree closeness betweenness??????????????? 
deg <- degree(Bali,gmode="graph")
deg
cls <- closeness(Bali,gmode="graph")
cls
bet <- betweenness(Bali,gmode="graph")
bet

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
set.seed(123)
plot(Bali,usearrows=T,vertex.cex=deg,main="Raw")
set.seed(123)
plot(Bali,usearrows=FALSE,vertex.cex=log(deg),main="Adjusted")
par(op)

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
set.seed(123)
plot(Bali,usearrows=T,vertex.cex=cls,main="Raw")
set.seed(123)
plot(Bali,usearrows=FALSE,vertex.cex=4*cls,main="Adjusted")
par(op)

op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
set.seed(123)
plot(Bali,usearrows=T,vertex.cex=bet,main="Raw")
set.seed(123)
plot(Bali,usearrows=FALSE,vertex.cex=sqrt(bet+1),main="Adjusted")
par(op)

#???????????????degree
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}
op <- par(mar=c(0,0,1,0))
set.seed(123)
plot(Bali,vertex.cex=rescale(deg,1,6),main="Adjusted node sizes with rescale function.")
par(op)


#The default node labels are in the specific vertex attribute vertex.names

get.vertex.attribute(Bali,"vertex.names")
op <- par(mar = c(0,0,0,0))
set.seed(123)
plot(Bali,displaylabels=TRUE,label.cex=0.8,pad=0.4,label.col="darkblue")
par(op)
rolelab <- get.vertex.attribute(Bali,"role")
op <- par(mar = c(0,0,0,0))
set.seed(123)
plot(Bali,usearrows=FALSE,label=rolelab,displaylabels=T,label.col="darkblue")
par(op)


#Edge width can be particularly useful if the network data include valued ties
Bali
IClevel <- Bali %e% "IC"
op <- par(mar = c(0,0,0,0))
set.seed(123)
plot(Bali,vertex.cex=1.5,edge.lwd=1.5*IClevel)
par(op)


#Edge colors can be set if the network data include this information
n_edge <- network.edgecount(Bali)
edge_cat <- sample(1:3,n_edge,replace=T)
op <- par(mar = c(0,0,0,0))
set.seed(123)
plot(Bali,vertex.cex=1.5,vertex.col="grey25",edge.col=c("blue","red","green")[edge_cat],edge.lwd=2)
par(op)

#Edge type (solid, dashed, dotted, dotdashed) can be set if the network data include this information
n_edge <- network.edgecount(Bali)
edge_cat <- sample(1:3,n_edge,replace=T)
op <- par(mar = c(0,0,0,0))
set.seed(123)
gplot(Bali,vertex.cex=0.8,gmode="graph",vertex.col="gray50",edge.lwd=1.5,edge.lty=c(2,3,4)[edge_cat])
par(op)



my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(Bali,"role"))
op <- par(mar = c(0,0,0,0))
set.seed(123)
plot(Bali,vertex.cex=rescale(deg,1,5),vertex.col=my_pal[rolecat])
legend("bottomleft",legend=c("BM","CT","OA","SB","TL"),col=my_pal,pch=19,pt.cex=1.5,bty="n",title="Terrorist Role")
par(op)