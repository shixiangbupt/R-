library(TSP)
library(igraph)

##----------求最短路径----------------------------------##
path <- function(to, from = 15){
    num <- 1:n
    names(num) <- cityNames
    path <- get.shortest.paths(g, from = from)
    p <- path$vpath[num[to]][[1]]
    cityNames[p]
}

##----------用TSP程序包中的函数求解TSP--------------------##
TSP_solve <- function(dist, cityNames){
    tsp <- TSP(dist, labels = cityNames)
    initial_tour <- solve_TSP(tsp, method = "nn")
    tour <- solve_TSP(tsp, method = "2-opt", 
            control = list(tour = initial_tour))
    list(length = tour_length(tsp, tour), 
         tour = labels(tour))
}

##----------构造距离矩阵--------------------------------##
## 镇和乡的名称
cityNames <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
               'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
                as.character(1:35), 
                'z1', 'z2', 'z3', 'z4', 'z5', 'z6')
## 读数据
rt <-  scan(file = "cmcm1998B.data", what = list(from="", to="", dist=0))
## 构造距离矩阵
n <- length(cityNames) 
D <- matrix(Inf, nr = n, nc = n, dimnames = list(cityNames, cityNames) )
for ( i in 1:n) D[i,i]<-0
m <- length(rt$from)
for (k in 1:m){
    D[rt$from[k], rt$to[k]] <- rt$dist[k]
    D[rt$to[k], rt$from[k]] <- rt$dist[k]
}

##----------计算最短路------------------------------------##
g <- graph.adjacency(adjmatrix = D, 
     mode = "undirected", weighted = TRUE)
D <- shortest.paths(g)

##----------求全部点的TSP---------------------------------##
group <- 1:(n-6)
TSP_solve(dist = D[group, group], cityNames = cityNames[group])

##----------第1次划分------------------------------------##
## 第1组
group <- c("O", "A", "B", "C", "P", "Q", "R", 
           as.character(1:3), as.character(28:35))
TSP_solve(dist = D[group, group], cityNames = group)
path(from="28", to="30")

## 第2组
group <- c("O", "D", "E", "F", "G", "H", "J", 
           as.character(4:14))
path(from="O", to="5")

## 第3组
group <- c("O", "M", "N", "K", "I", "L", 
           as.character(15:27))
path(from="O", to="26")

##----------第2次划分------------------------------------##
## 第1组
group <- c("O", "A", "B", "C", "D", "P", "Q", "R", 
           as.character(1:4), as.character(28:35))
TSP_solve(dist = D[group, group], cityNames = group)

## 第2组
group <- c("O", "E", "F", "G", "H", "J", 
           as.character(5:14))


##----------第3次划分------------------------------------##
## 第1组
group <- c("O", "A", "B", "C", "P", "Q", "R", 
           as.character(1), as.character(26:35))
TSP_solve(dist = D[group, group], cityNames = group)
path(from="30", to="28")

## 第2组
group <- c("O", "D", "E", "F", "G", "H", 
           as.character(2:14))

## 第3组
group <- c("O", "M", "N", "K", "I", "L", "J", 
           as.character(15:25))
path(from="15", to="16")


##----------第4次划分------------------------------------##
## 第1组
group <- c("O", "A", "B", "C", "R", "Q", 
           as.character(1), as.character(29:35))
TSP_solve(dist = D[group, group], cityNames = group)

## 第2组
group <- c("O", "D", "E", "F", "G", 
           as.character(3:4), as.character(7:12))

## 第3组
group <- c("O", "L", "J", "I", "H", 
           as.character(c(2, 5, 6, 13:15, 18:20)))

## 第4组
group <- c("O", "P", "N", "M", "K", 
           as.character(c(16:17, 21:28)))

