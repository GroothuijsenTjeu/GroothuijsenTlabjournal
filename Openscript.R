#Makin Matrices

dicho<-sample(x=c(0:1),size=10000,replace=TRUE)
net1<-matrix(data=dicho,100,100)
diag(net1)=0
sumnet1<-rowSums(data[net1,1:100])
net1b<-t(net1)
.list<-net1,net1b

net2<-matrix(runif(n=10000,min=0,max=10),nrow=100,ncol=100)
diag(net2)=0

#small for playing with triads

dichos<-sample(x=c(0:1),size=16,replace=TRUE)
net3<-matrix(data=dichos,4,4)
net3t<-t(net3)
diag(net3)=0
net3s<-net3+net3t
net3s<-ifelse(net3s==2,1,0)
netGs<-graph_from_adjacency_matrix(net3s)
plot(netGs)

set.seed(123643)
net4<-matrix(sample(0:1,100,replace=TRUE),nrow=10,ncol=10)
diag(net4)=0
net4s<-net4+t(net4)
net4s<-ifelse(net4s==2,1,0)
net4s
netG<-graph_from_adjacency_matrix(net4s)
netGa<-graph_from_adjacency_matrix(net4)
plot(netG)
dyad_census(netGa)
triad_census(netGa)

install.packages("sna")
require(sna)
?sna
triad.census(net4s)


