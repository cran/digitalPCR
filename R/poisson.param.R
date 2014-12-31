poisson.param <-
function(thr)
{
lamda=c(seq(0.01, 0.09, 0.01), seq(0.1,20,0.1))
th=seq(1,10,1)
if (thr){
th=1
}
result=NULL
for (la in lamda){
for (tt in th){
result=rbind(result, c(la, tt))
}
}
colnames(result)=c("Copy", "Thres")
return (result)
}
