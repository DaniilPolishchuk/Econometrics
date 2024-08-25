data = read.csv("D://Econometrics/data.csv")

#test_data_wacor = read.csv("test_data_without_autocor")
#cor(test_data_wacor[,3:6])
x <- data[1:25, c(3+1, 3+10, 3+11, 3+14)]
X.st <- (t(x) - sapply(x,mean) )/ sapply(x,sd)


r <- cor(data[1:25, c(3+1, 3+10, 3+11, 3+14)])


Z <- solve(r)
Z

chisq.st <- -(25 - 1 - 1/6 * (2 * 25 + 5)) * log(det(r))
chisq.kr <- qchisq(1 - 0.05, 6)

cbind(chisq.st, chisq.kr, "T/F" = chisq.st > chisq.kr)

F.st <- (diag(Z) - 1) * (25 - 5)/( 5-1)
F.st

F.kr <- qf(1 - 0.05, 5 - 1, 25 - 5)
F.kr

#part 2 

r_ki <- rep(0, 6)
pos <- 1
for (i in 1:ncol(Z)){
  for (j in i+1: ncol(Z)){
    if (j > ncol(Z)) { break}
    r_ki[pos] <- -Z[i,j] / sqrt(Z[i,i] * Z[j,j])
    pos <- pos + 1 
    #print(c(i,j, pos, -Z[i,j] / sqrt(Z[i,i] * Z[j,j])))
  }
}
r_ki

t.st <- abs(r_ki) * sqrt((25 - 6)/(1 - r_ki^2)) 
t.st

t.kr <- qt(1 - 0.05, 21)
t.kr
# part 3 
eigen_vector <- eigen(r)$vectors
eigen_values <- eigen(r)$values

eigen(r)
eigen(Z)

eigen_values
eigen_vector

sum(diag(Z))
sum(eigen_values)

eigen_values
sum(eigen_values[1] + eigen_values[2])/4
# so the fist 2 componets discribe the 

as.matrix(eigen_vector[,1:3] ) %*% as.matrix(eigen_values[1:3])


L <- matrix(rep(0, 4 * 4), ncol = 4)
diag(L) <- eigen_values

eigen_vector[,4] = eigen_vector[,4] * (-1)

as.matrix(eigen_vector) %*% L^(1/2)

L^(1/2) %*% as.matrix(eigen_vector)

t(as.matrix(eigen_vector)) %*% L^(1/2)



eig_vec <- data.frame(eigen_vector)
colnames(eig_vec) <- eigen_values 

 sort(as.double(colnames(eig_vec)))

 
t(X.st) %*%  as.matrix(eigen_vector)


t(as.matrix(eigen_vector)) %*% X.st

p <- princomp(t(X.st))
s <- summary(p)
p$loadings[, 1:3]

library(factoextra)
fviz_pca_var(p, col.var = "black")
plot(X.st)
