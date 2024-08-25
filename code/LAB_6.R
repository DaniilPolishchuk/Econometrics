data = read.csv("D://Econometrics/data.csv")
data
nrow(data)

x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

T = nrow(x)
N = 2
lm(y ~ x[,1])
z <- summary(lm(y ~ x[,1]), data = data)
model_A <- lm(y ~ x[,1] + x[,2] + x[,3] + x[,4])
model_test <- lm(y ~ x[,1])
anova(lm(y ~ x[,1]))
1 - (1 - z$r.squared) * (T-1)/(T-N)

sum(z$residuals**2)/(25 - 2)

library(olsrr)
ols_mallows_cp(model_A, model_A)

#lets somehow automotisate the models 
#firstly lets find the 2 factor models 
# make funtion finally 
names <- c("2","3","4","5","6")
rowname <- c()

n_models = 4
N = 2
T = nrow(x)
# Group A -- working 
group_A <- matrix(rep(N,n_models* 5), ncol = 5)
for (i in 1:n_models){
  model_temp <- lm(y ~ x[,i])
  print(summary(model_temp)$r.squared) 
  group_A[i,2] <- 1 - (1 - summary(model_temp)$r.squared) * (T-1)/(T-N)
  group_A[i,3] <- sum(summary(model_temp)$residuals^2)/(T - N)
  group_A[i,4] <- ols_mallows_cp(lm(y ~ x[,i]), model_A)
  y_hat_temp   <- cbind(matrix( rep(1, nrow(x))),x[,i]) %*% as.matrix(model_temp$coefficients)
  group_A[i,5] <- sum((y - y_hat_temp)^2) * (T + N)/(T-N)
  rowname <- c(rowname, paste(1,names[i]))
}
group_A
#group_A[which(group_A[,3] - N == min(group_A[,3] - N)),]

n_models = 6
N = 3
group_B <- matrix(rep(N,n_models * 4), ncol = 4)
pos <- 1
for (i in 1:ncol(x)){
  for (j in (i+1):ncol(x)){
    if(j == ncol(x) + 1){break}
    model_temp <- lm(y ~ x[,i] + x[,j], data = data)
    group_B[pos,2] <- 1 - (1 - summary(model_temp )$r.squared) * (T-1)/(T-N)
    group_B[pos,3] <- sum(summary(model_temp)$residuals**2)/(T - N) 
    group_B[pos,4] <- ols_mallows_cp(model_temp, model_A)
    pos <- pos + 1
    rowname <- c(rowname, paste(1,names[i], names[j]))
  }
}
group_B



n_models = 4
N = 4
group_C <- matrix(rep(N,n_models * 4 ), ncol = 4)
pos <- 1
for (i in 1:ncol(x)){
  for (j in (i+1):ncol(x)){
    if(j == ncol(x) + 1){break}
    for (q in (j+1):ncol(x)){
      if( q == ncol(x)+1){break}
      model_temp <- lm(y ~ x[,i] + x[,j] + x[,q], data = data)
      group_C[pos,2] <- 1 - (1 - summary(model_temp)$r.squared) * (T-1)/(T-N)
      group_C[pos,3] <- sum(summary(model_temp)$residuals**2)/(T - N) 
      group_C[pos,4] <- ols_mallows_cp(model_temp, model_A)
      pos <- pos + 1
      rowname <- c(rowname, paste(1,names[i],names[j], names[q]))
    }
  }
}
group_C
  

n_models = 1
N = 5
group_D <- matrix(rep(N,n_models*4), ncol = 4)
pos <- 1
for (i in 1:ncol(x)){
  for (j in (i+1):ncol(x)){
    if(j == ncol(x) + 1){break}
    for (q in (j+1):ncol(x)){
      if( q == ncol(x)+1){break}
      for (w in (q+1):ncol(x)){
        if( w == ncol(x)+1){break}
        rowname <- c(rowname, paste(1,names[i], names[j], names[q], names[w]))
        model_temp <- lm(y ~ x[,i] + x[,j] + x[,q] + x[,w], data = data)
        group_D[pos,2] <- 1 - (1 - summary(model_temp)$r.squared) * (T-1)/(T-N)
        group_D[pos,3] <- sum(summary(model_temp)$residuals**2)/(T - N) 
        group_D[pos,4] <- ols_mallows_cp(model_temp, model_A)
        pos <- pos + 1
      }
      #print(c(i,j))
      
    }
  }
}
group_D

rbind(group_A,group_B,group_C,group_D)

df5 <- data.frame(rbind(group_A,group_B,group_C,group_D))
row.names(df5) <- rowname  
colnames(df5) <- c("p", "R^2_t","sigma^2", "C_p")
df5


library(ggplot2)

ggplot(data=df5, aes(x=p, y=C_p, geroup = p)) +
  geom_point()+
  geom_line(y = 5)



plot(x=df5$p, 
     y = df5$C_p, 
     ylim = c(0,10), 
     xlim = c(0,10),
     col = df5$p,
     type = "p",
     pch = 19)
lines(x = c(0,5,10), y = c(0,5,10), col= "red")
text(x = df5$p, 
     y = df5$C_p,
     labels = rownames(df5), 
     pos = 3,
     cex = 0.5) 
grid(nx = 10 , ny = 10)

coef <- as.matrix(model_A$coefficients)
X <- as.matrix(cbind(matrix( rep(1, nrow(x))),x))
y_hat  <- X %*% coef
y_hat_2 <- y_hat ^ 2 
y_hat_3 <- y_hat ^ 3
y_hat_4 <- y_hat ^ 4

RSS_X <- sum((y - y_hat)^2)
RSS_X
x_1 <- as.matrix(cbind(x, y_hat_2, y_hat_3, y_hat_4))
#x_2 <- as.matrix(cbind(x, y_hat_2, y_hat_3, y_hat_4))

coef_1 <- lm(y ~ x_1,na.action = na.omit)$coefficients
p <- na.exclude(coef_1)

X_1 <- as.matrix(cbind(matrix( rep(1, nrow(x_1))),x_1))
y_hat_1 <- X_1 %*% coef_1
RSS_X1 <- sum((y - y_hat_1)^2)
RSS_X1
summary(lm(y ~ x_2,na.action = na.omit))

f.stat = (RSS_X -RSS_X1)*(T-3)/(3 * RSS_X1)
f.test <- qf(1- 0.05,3,T - 3)
ifelse(f.stat < f.test, "Good", "Bad")


read.csv("models_characteristics.csv")

