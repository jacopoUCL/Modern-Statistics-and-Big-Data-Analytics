library(dplyr)

#### EXERCISE 1 #### 
## Load Datasets

olive_all <- as.data.frame(read.table("oliveoil.dat", header = TRUE))
olive <- select_if(olive_all, is.numeric)
olive_scl <- scale(olive)

## Kmeans
cl_3 = kmeans(olive, centers = 3)
cl_scl_3 = kmeans(olive_scl, centers = 3)
cl_9 = kmeans(olive, centers = 9)
cl_scl_9 = kmeans(olive_scl, centers = 9)

## Table 
t_3 = table(olive_all$macro.area, cl_3$cluster)
t_scl_3 = table(olive_all$macro.area, cl_scl_3$cluster)
t_9 = table(olive_all$macro.area, cl_9$cluster)
t_scl_9 = table(olive_all$macro.area, cl_scl_9$cluster)

t_3
t_scl_3
t_9
t_scl_9

# One way of comparing the clusterings, given the real labels: 
# Assign the label to each cluster of the most frequent label within that cluster 
correct_3 <- sum(apply(t_3, 2, max))
correct_scl_3 <- sum(apply(t_scl_3, 2, max))
correct_9 <- sum(apply(t_9, 2, max))
correct_scl_9 <- sum(apply(t_scl_9, 2, max))

correct_3 < correct_scl_3
correct_9 < correct_scl_9
# In both cases, the scaled versions outperform the unscaled versions (more observations are assigned correctly)

#### EXERCISE 3 ####
boston <- read.table("Boston.dat", header = TRUE)
mboston <- as.matrix(boston)
boston_scl <- scale(mboston)

pairs(boston)

wss <- c()
for (n in 1:10) {
    cl_n = kmeans(boston_scl, centers = n)
    wss <- c(wss, cl_n$tot.withinss)
}

plot(wss)
lines(wss)
# -> Reasonable choices seem to be 2 or 7, according to the elbow principle 
cl_7 <- kmeans(boston_scl, centers = 7)
pairs(boston, col = cl_7$cluster)


#### EXERCISE 4 ####
library(pracma)

kmpp <- function(X, k) {
    n <- nrow(X)
    C <- numeric(k)
    C[1] <- sample(1:n, 1)

    for (i in 2:k) {
        dm <- distmat(X, X[C, ])
        pr <- apply(dm, 1, min) 
        pr[C] <- 0
        C[i] <- sample(1:n, 1, prob = pr)
    }

    kmeans(X, X[C, ])
}

# The intuition behind this approach is that the initial starting
# points should be well distributed. Thus first we choose a random 
# cluster starting point of our data (C[1]) and then we choose the next 
# the starting point for the next cluster again from our data randomly,
# but the probability depends on the squared distance to the already chosen centers (distmat(X, X[C,])).
# The further away from existing centroids the better, thus this algorithm tends to give a 
# fairly well-spread set of observations from the data that will be used as the starting centroids. 

# -> X[C,] are the observations that are used as centroids 
# -> distmat(X, X[C, ]) calculates the squared distance between each observations and each centroids
# -> pr <- apply(dm, 1, min) Choose the distance to the closest centroid (min)
# -> pr[C] <- 0 We dont want to choose a centroid we already use
# -> C[i] <- sample(1:n, 1, prob = pr) Choose a new centroid from our data randomly, with the probability 
#                                      depending on the distance to the centroids 

# Then, we apply kmeans with the chosen centroid starting points (C contains the indices of the starting points)

## Apply kmeans++ to olive and boston 
set.seed(1233)
cl_pp_o <- kmpp(olive_scl, 3)
cl_pp_b <- kmpp(boston_scl, 7) 

cl_o_1 <- kmeans(olive_scl, 3, nstart = 1)
cl_b_1 <- kmeans(boston_scl, 7, nstart = 1) 
cl_o_100 <- kmeans(olive_scl, 3, nstart = 100)
cl_b_100 <- kmeans(boston_scl, 7, nstart = 100) 


c(cl_pp_o$tot.withinss, cl_o_1$tot.withinss, cl_o_100$tot.withinss) 
c(cl_pp_b$tot.withinss, cl_b_1$tot.withinss, cl_b_100$tot.withinss )

# In the example the best result is always achieved with the nstart = 100 algorithm
# For the olive oil dataset, the kmeans++ performs as well as kmeans with nstart = 100
# while, kmeans with nstart = 1 is significantly worse
# For the boston dataset, the kmeans++ is a bit worse than kmeans with nstart = 100
# kmeans with nstart = 1 is much worse than the two other algorithms  