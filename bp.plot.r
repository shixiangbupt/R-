bp.plot <- function(gen,rv.sim,...,reps =1,logplot=TRUE){
  # 模拟分支过程的人口变化曲线
  # rv.sim() 模拟从后代分布中选 n 随机值
  Z <- matrix(0,nrow=reps,ncol=gen+1)
  for(i in 1:reps){
    Z[i,] <- bp(gen,rv.sim,...)
  }
  if (logplot){
    Z <- log(Z)
  }
  plot(c(0,gen),c(0,max(Z)),type="n",xlab="generation",ylab="if(logplot)  log populatio")
       for(i in 1:reps){
         lines(0:gen,Z[i,])
       }
       return(invisible(Z))
}