# branching process simulation
# 从第 0 代到第 gen 代的人口变化情况
# bp.r
bp <- function(gen,rv.sim,...){
Z <- rep(0,gen+1)
Z[1] <- 1
for (i in 1:gen){
if (Z[i]>0){
#Z[i] 是第 i-1 代人口数量
Z[i+1] <- sum(rv.sim(Z[i],...))
}
}
return(Z)
}


