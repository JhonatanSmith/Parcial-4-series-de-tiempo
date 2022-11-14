
```{r}
qQ=list()
for(i in 1:14) qQ[[i]]=c(i-1,0)
qQ[[15]]=c(0,1)
qQ[[16]]=c(1,1)
pP=qQ

dt_params=c()
for(i in 1:16){
  for(j in 1:16){
    temp=c(pP[[i]][1],1,qQ[[j]][1],pP[[i]][2],1,
           qQ[[j]][2],12)
    dt_params=rbind(temp,dt_params)
  }
}
colnames(dt_params)=c("p","d","q","P","D","Q","T")
rownames(dt_params)=1:256
```

#AGREGAR COMENTARIO DE MODELOS QUE SE CREAN 256:
  
  
  # se demora en correr
  models = vector('list', 256)
for(i in 1:256){
  try(models[[i]]<-forecast::Arima(x,order = dt_params[i,1:3],
                                   seasonal = list(order=dt_params[i,4:6],period=12),
                                   lambda = NULL))
}


models = vector('list', 256)
for(i in 1:256){
  try(models[[i]]<-forecast::Arima(x,order = dt_params[i,1:3],
                                   seasonal = list(order=dt_params[i,4:6],period=12),
                                   lambda = NULL))
}


library(caschrono)
aa=rep(NA,256)
for(i in 1:256){
  if(length(models[[i]]$residuals)>1){
    a=Box.test.2(x = models[[i]]$residuals,nlag = 10,type = "Box-Pierce")
    z=prod(1-(a[,2]<.05))
    if(z==1) aa[i]="y"
    else aa[i]="n"
  }
  
}
dt_params2=data.frame(dt_params)
dt_params2$residuals=aa


#Seleccion modelo AIC

aic=rep(NA,256)
model_names=rep(NA,256)
for(i in 1:256){
  if(length(models[[i]]$aic)>0){
    aic[i]=models[[i]]$aic
    model_names[i]=as.character(models[[i]])
  }
}
dt_params2$aic=aic
dt_params2$model=model_names


library(DT)
dt_params2$aic=round(dt_params2$aic,4)
dt_params2=na.omit(dt_params2)
datatable(dt_params2,rownames = F)

i=as.numeric(rownames(dt_params2)[which(dt_params2$aic<260)])
res=sapply(i, function(x)as.character(models[[x]]))
res



#res=sapply(i, function(x)as.character(models[[x]]))
x_test=sapply(i, function(x)(min(AIC((models[[x]])))))


models[205]


bb=rep(NA,16)
for(j in 1:16){
  temp=t(x_test[[j]])[,2]
  z=prod((temp<.05))
  bb[j]=z
}
bb
