data = read.csv("D://Econometrics/data.csv")


x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

moed




# TAsk 1 for the test data
# Test autocorrelation 
test_model <- lm(y ~ data[1:25,4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)

# H0 : rho = 0
# H1 : rho != 0

alpha = 0.1 

x <- data[c("X2", "X3", "X7", "X12", "X13")]
Y <- y <- data$Y3
model <- lm(y ~ x[,1] + x[,2] + x[,3] + x[,4] + x[,5])



u <- summary(test_model)$residuals
u <- summary(model)$residuals

temp_sum <- 0

for( i in 2:length(u)){
  temp_sum <- temp_sum + (u[i] - u[i-1])^2
}
d.st <- temp_sum/sum(u^2)

library(car)
durbinWatsonTest(test_model)
durbinWatsonTest(model)

d_0 = 1.04
d_u = 1.77

d_0 = (1.34+1.37)/2
d_u = 1.77

cbind(d_0,d.st,d_u, 4 - d_u)

# the value is inn grey area so we can not say exactly 
#so lets assume we have autocorelation 

temp_sum1 <- 0
for (i in 2:length(u)){
  temp_sum1 <- temp_sum1 + (u[i-1] * u[i])
}
rho_hat <- temp_sum1/sum(u^2)
rho_hat

#T= 25
T = 53
T_A <- matrix(rep(0,T * T), ncol = T)


diag(T_A) <- 1
T_A[1,1] <- sqrt(1 - rho_hat^2)
T_A[2,1] <- (-rho_hat)
T_A[T,T] <- 1
T_A[T,T-1] <- - rho_hat
for (i in 1:T){
  for (j in 1:T){
    if (j == i - 1){
      T_A[i,j] = - rho_hat
    }
  }
}

Y_fix <- T_A %*% Y
#X_fix <- T_A %*% as.matrix(x)

X_fix <- T_A %*% as.matrix(cbind(matrix( rep(1, nrow(x))),x))




#round(lm(Y_fix ~ X_fix, data = data)$coefficients,5)

round(lm(Y_fix ~ X_fix[,2] + 
           X_fix[,3] +
           X_fix[,4] + 
           X_fix[,5] +
           X_fix[,6])$coefficients,5)

aov(Y_fix ~ X_fix[,2] + 
     X_fix[,3] +
     X_fix[,4] + 
     X_fix[,5] )

lm.fit(x,y)

model_res <- lm(Y_fix ~ X_fix[,2] + X_fix[,3] + X_fix[,4] + X_fix[,5] + X_fix[,6])

head(X_fix)

beta_hat = solve(t(X_fix) %*% X_fix) %*% t(X_fix) %*% Y_fix

round(beta_hat,2)


round(model_res$coefficients,2)

dw <- durbinWatsonTest(lm(Y_fix ~ X_fix[,2] + X_fix[,3] + X_fix[,4] + X_fix[,5] + X_fix[,6]))

head(data)

X_fix
Y_fix
y
# test for rho_hat 

test_x_fix <- matrix(rep(0,T * 4), ncol = 4)
for (i in 2:T){
  for (j in 1:4){
    test_x_fix[i,j] <- x[i,j] - rho_hat* x[i-1,j]
  }
}
test_y_fix <- matrix(rep(0,T), ncol = 1)
for (i in 2:T){
    test_y_fix[i,1] <- y[i] - rho_hat* y[i-1]
}

round(lm(test_y_fix ~ test_x_fix)$coefficients,5)
durbinWatsonTest(lm(test_y_fix ~ test_x_fix))


library(orcutt)
cochrane.orcutt(test_model, 
                convergence = 0, 
                max.iter = 2)


result <- cochrane.orcutt(model,
                          convergence = 0, 
                          max.iter = 2)


head(X_fix)
head(result$model)



