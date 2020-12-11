# Homework 2
# reading the data
setwd("~/Desktop/Homework2")
x.train <- read.table("~/Desktop/Homework2/uWaveGestureLibrary_X_TRAIN", quote="\"", comment.char="")
y.train <- read.table("~/Desktop/Homework2/uWaveGestureLibrary_Y_TRAIN", quote="\"", comment.char="")
z.train <- read.table("~/Desktop/Homework2/uWaveGestureLibrary_Z_TRAIN", quote="\"", comment.char="")
colnames(x.train) <- c("class", paste0("t",1:315))
colnames(y.train) <- c("class", paste0("t",1:315))
colnames(z.train) <- c("class", paste0("t",1:315))

# libraries
library(data.table, warn.conflicts=FALSE)
library(ggplot2, warn.conflicts=FALSE)
library(dplyr, warn.conflicts = FALSE)
library(scatterplot3d, warn.conflicts=FALSE)

# Part A
# creating velocity vectors
class <- as.data.frame(x.train$class)
x.velocity <- c()
for(i in 1:896){
    x.velocity <- as.data.frame(rbind(x.velocity, cumsum(as.matrix(x.train[i,2:316], ncol=1, nrow=315))))
}
x.velocity <- cbind(class, x.velocity)
colnames(x.velocity) <- c("class", paste0("t",1:315))

y.velocity <- c()
for(i in 1:896){
    y.velocity <- as.data.frame(rbind(y.velocity, cumsum(as.matrix(y.train[i,2:316], ncol=1, nrow=315))))
}
y.velocity <- cbind(class, y.velocity)
colnames(y.velocity) <- c("class", paste0("t",1:315))

z.velocity <- c()
for(i in 1:896){
    z.velocity <- as.data.frame(rbind(z.velocity, cumsum(as.matrix(z.train[i,2:316], ncol=1, nrow=315))))
}
z.velocity <- cbind(class, z.velocity)
colnames(z.velocity) <- c("class", paste0("t",1:315))

# creating position vectors
x.position <- c()
for(i in 1:896){
    x.position <- as.data.frame(rbind(x.position, cumsum(as.matrix(x.velocity[i,2:316], ncol=1, nrow=315))))
}
x.position <- cbind(class, x.position)
colnames(x.position) <- c("class", paste0("t",1:315))

y.position <- c()
for(i in 1:896){
    y.position <- as.data.frame(rbind(y.position, cumsum(as.matrix(y.velocity[i,2:316], ncol=1, nrow=315))))
}
y.position <- cbind(class, y.position)
colnames(y.position) <- c("class", paste0("t",1:315))

z.position <- c()
for(i in 1:896){
    z.position <- as.data.frame(rbind(z.position, cumsum(as.matrix(z.velocity[i,2:316], ncol=1, nrow=315))))
}
z.position <- cbind(class, z.position)
colnames(z.position) <- c("class", paste0("t",1:315))

# selecting one instance from each class
ins.no <- c()
for(i in 1:8){
    ins.no <- c(ins.no, which(x.velocity$class == i)[3])
}

# scatterplots
x.position <- as.matrix(x.position, ncol=316, nrow=896)
y.position <- as.matrix(y.position, ncol=316, nrow=896)
z.position <- as.matrix(z.position, ncol=316, nrow=896)

for(i in 1:8){
    scatterplot3d(x.position[ins.no[i],2:316], y.position[ins.no[i],2:316], 
                  z.position[ins.no[i],2:316], main=paste("Gesture ",i), 
                  xlab="X-Position", ylab="Y-Position", zlab="Z-Position", color="lightskyblue")
}

# Part B
# long format of data
x.position <- as.data.frame(x.position)
y.position <- as.data.frame(y.position)
z.position <- as.data.frame(z.position)

time.series.id <- as.matrix(rep(1:896,each=315), ncol=1)
time.index <- as.matrix(rep(1:315,896), ncol=1)
class <- as.matrix(rep(x.train$class, each=315), ncol=1)

x.col <- c()
for(i in 1:nrow(x.position)){
    x.col <- rbind(x.col,t(x.position[i,2:316]))
}
y.col <- c()
for(i in 1:nrow(y.position)){
    y.col <- rbind(y.col,t(y.position[i,2:316]))
}
z.col <- c()
for(i in 1:nrow(z.position)){
    z.col <- rbind(z.col,t(z.position[i,2:316]))
}
train.long <- as.data.frame(cbind(time.series.id, time.index, x.col, y.col, z.col, class), ncol=6)
colnames(train.long) <- c("time.series.id", "time.index", "x.col", "y.col", "z.col", "class")

# applying pca
pca <- princomp(train.long[,3:5], cor=T)
summary(pca, loadings=TRUE)
pca.data <- train.long
pca.data$comp1 <- 0.209*pca.data$x.col+0.723*pca.data$y.col+0.658*pca.data$z.col

# selecting random time series for each class
index1 <- c()
index2 <- c()
for(i in 1:8){
    index1 <- c(index1, unique(pca.data$time.series.id[which(pca.data$class==i)])[24])
    index2 <- c(index2, unique(pca.data$time.series.id[which(pca.data$class==i)])[75])
}

# visualization of reduced dimensions as time series in a single plot
for(i in 1:8){
    plot <-
        ggplot() + 
        geom_line(aes(x=time.index[which(time.series.id==index1[i])], 
                      y=pca.data$comp1[which(time.series.id==index1[i])]),color='darkturquoise') + 
        geom_line(aes(x=time.index[which(time.series.id==index2[i])],
                      y=pca.data$comp1[which(time.series.id==index2[i])]),color='darkorange') + 
        ylab('Component1')+xlab('Time Index') +
        ggtitle(paste('Class ', i))
    print(plot)
}

# Part c
# pca for each class
for(i in 1:8){
    pca.class <- princomp(subset(pca.data, class==i)[,3:5], cor=T)
    cat( "\n", "The PCA results for Class ", i, "\n")
    print(summary(pca.class, loadings=TRUE))
}

# Part D
# creating distance matrix
x.distance <- dist(x.train[,2:316], method="euclidean", diag=TRUE, upper=TRUE)
y.distance <- dist(y.train[,2:316], method="euclidean", diag=TRUE, upper=TRUE)
z.distance <- dist(z.train[,2:316], method="euclidean", diag=TRUE, upper=TRUE)
dist.matrix <- as.matrix(x.distance + y.distance + z.distance)
dist.matrix[1:5,1:5]

# multidimensional scaling
mds <- cmdscale(dist.matrix, eig = TRUE, k = 2)
x <- mds$points[, 1]
y <- mds$points[, 2]
ggplot() + geom_point(aes(x=x,y=y,colour=as.factor(x.train$class))) +
    ylab('Y') + xlab('X') + ggtitle('Multidimensional Scaling')
