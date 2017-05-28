dt2=subset(dt, dt$indication!="")
dt2=dt2[1:10000,]
mymatrix=matrix(data=NA, nrow=(length(unique(dt2$contrast))), ncol=(length(unique(dt2$indication))))
rownames(mymatrix)=unique(dt2$contrast)
colnames(mymatrix)=unique(dt2$indication)

ind_freq= as.data.frame(table(dt2$indication))

ind<-unique(dt2$indication)
con<-unique(dt2$contrast)



for( r in 1:length(unique(dt2$contrast))){
  #print(paste(r, length(unique(dt2$contrast)), sep="/"))
  for(c in 1:length(unique(dt2$indication))){
    tmp=subset(dt2, dt2$contrast==unique(dt2$contrast)[r] & dt2$indication==unique(dt2$indication)[c])
    mymatrix[r,c]=nrow(tmp)
  }
}

heatmap(mymatrix)