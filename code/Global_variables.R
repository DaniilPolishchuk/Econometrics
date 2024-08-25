# GLOBAL VARIABLES #
library(car) 

#Getting data ####
data = read.csv("D://Econometrics/data.csv")
data

# Y_3 <- data$Y3
# X_2 <- data$X2
# X_3 <- data$X3
# X_7 <- data$X7
# X_12 <- data$X12
# X_13 <- data$X13

#CORRELATION ####

x <- data[c("X2", "X3", "X7", "X12", "X14")]
y <- data$Y3
#vova data 
# Add Y for the 1st task 
x <- data[, c(2+3,3+3,3+4,3+ 6, 3+14)]
y <- data$Y1
# ADD Y for the first task 
names <- c("X2","X3","X4","X6","X14")

R <- cor(x)
Z <- solve(R)



names <- c("Y3","X2", "X3", "X7", "X12", "X14")
result <- matrix(rep(0, 5 * 7), ncol = 7)
for(j in 1:5){
  q <- -Z[1,j]/sqrt(Z[1,1] * Z[j,j])
  result[j,1] <- cor(y = y, x = x[j])
  result[j,2] <- q
  #t <- paste( names[j], " ",cor(y = Y_3, x = x[j]), " ", q, "\n")
  #result <- paste(result,t)
}
result


k = 4
alpha = 0.05
t_test <- qt(0.05, nrow(data) - k - 2 , lower.tail = F)
#results <- c(1,1.449657,3.432729,4.893193,0.5512443,2.430535)
results <- c(1,
             1.024416,
             0.9010732,
             1.68503,
             0.5016514,
             1.142438)
for(j in 2:6){
  r_hat <- -R[1,j]/sqrt(Z[1,1] * Z[j,j])
  t_obs <- abs(r_hat) * sqrt((nrow(data) - k - 2)/(1 -r_hat^2))
  #print(t_obs)
  #d <- X[j]
  
  d <- as.numeric(as.character(unlist(x[j])))
  result[j-1,3] <- cor.test(x = d, y = y)$p.value
  result[j-1,4] <- cor.test(x = d, y = y)$p.value < 0.2
  result[j-1,5] <- results[j]
  #result[j-1,5] <- cor.test(x = d, y = y)$p.value
  result[j-1,6] <- t_test
  result[j-1,7] <- results[j] > t_test
  #result[j-1,7] <- results[j] > t_test
   # cat("sign.lev:",
  #     cor.test(x = d, y = Y_3)$p.value,
  #     cor.test(x = d, y = Y_3)$p.value < 0.2,
  #     names[j],
  #     results[j],
  #     t_test,
  #     results[j] > t_test,
  #     "\t",
  #     
  #     '\n')
}

result
df <- data.frame(result)
colnames(df) <- c("pair.corr", "part.corr", "sign.level","T/F", "t.test", "t.statistic", "T/F")
rownames(df) <- names[2:6]

#list_test <- list("M1" = c(1,3,4,5,6), "M2" = c(1,2,3,4,5,6))

write.csv(df, "D:\\Econometrics/correlation.csv")

#### MODELS ####


model_coef <- lm(Y ~ data$X2+ 
                      data$X3+ 
                      data$X7+
                      data$X12+ 
                       data$X13, data = data)$coefficients

names <- c("X2", "X3", "X7", "X12", "X13")
Y <- data$Y1

result <- matrix(rep(0, 4*6), ncol = 4)

for(i in 0:5){
   
   if (i == 0){
     x <- data[names]
     X = as.matrix(cbind(matrix( rep(1, nrow(x))),x))
     
     beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
     
     # extra data 
     Y_hat = X %*% beta_hat
     U_t_hat <- as.matrix(Y - Y_hat)
     T = nrow(X)
     N = ncol(X)
     
     #sigma^2
     sigma = sum(U_t_hat^2)/(T-N)
     result[i+1,1] = sigma
     print(N)
     #MCE 
     mce = sum(U_t_hat^2)/T
     result[i+1,2] = mce
     print(N)
     #MAPE
     mape = sum(abs(U_t_hat) / Y) / T * 100
     result[i+1,3] = mape
     #M(u)
     M_u = sum(U_t_hat)/T
     result[i+1,4] = M_u
   
   }
    else{
      x <- data[names[i]]
      X = as.matrix(cbind(matrix( rep(1, nrow(x))),x))
      
      beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
      
      # extra data 
      Y_hat = X %*% beta_hat
      U_t_hat <- as.matrix(Y - Y_hat)
      T = nrow(X)
      N = ncol(X)
      #sigma^2
      sigma = sum(U_t_hat^2)/(T-N)
      result[i+1,1] = sigma
      print(N)
      #MCE 
      mce = sum(U_t_hat^2)/T
      result[i+1,2] = mce
      print(N)
      #MAPE
      mape = sum(abs(U_t_hat) / Y) / T * 100
      result[i+1,3] = mape
      #M(u)
      M_u = sum(U_t_hat)/T
      result[i+1,4] = M_u
    }
  
}


result 




df2 <- data.frame(result)
colnames(df2) <- c("sigma^2", "MSE", "MAPE", "M(u)")
rownames(df2) <- c("A","B","C","D","E","F")

#list_test <- list("M1" = c(1,3,4,5,6), "M2" = c(1,2,3,4,5,6))

write.csv(df, "D:\\Econometrics/models_characteristics.csv")

#### Standardised beta hat####
# model_A <- lm(data$Y1 ~ 
#                 data$X2 + 
#                 data$X3 + 
#                 data$X4 + 
#                 data$X7 +
#                 data$X14)
# 
# beta_hat = as.matrix(model_A$coefficients)
# 
# X_list  <- cbind(matrix( rep(1, nrow(x))),x)
# sd_x <- sapply(X_list, sd)
# sd_y <- sd(data$Y1)
# 
# beta_hat_standardized <- beta_hat * sd_x/sd_y
# cbind.data.frame("beta_hat"  = beta_hat)
# cbind.data.frame("standardized beta hat" = beta_hat_standardized)




#### AUTOCOR for the test data ####

x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

test_model <- lm(y ~ data[1:25,4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)

u <- summary(test_model)$residuals


temp_sum1 <- 0
for (i in 2:length(u)){
  temp_sum1 <- temp_sum1 + (u[i-1] * u[i])
}
rho_hat <- temp_sum1/sum(u^2)

T = nrow(x)
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

Y_fix <- T_A %*% y
X_fix <- T_A %*% as.matrix(cbind(matrix( rep(1, nrow(x))),x))

df7 <- data.frame(Y_fix)
df7 <- cbind(df7,X_fix)
colnames(df7) <- c("Y_fix", "X1", "x2", 'x3', "x4", 'x5')

write.csv(df7, file = "D:\\Econometrics/test_data_without_autocor", row.names = F)
####

#### AUTOCOR for my data ####

x <- data[c("X2", "X3", "X7", "X12", "X13")]
X <- as.matrix(cbind(matrix( rep(1, nrow(x))),x))
Y <- y <- data$Y3

model <- lm(data$Y3 ~
                data$X2 +
                data$X3 +
                data$X4 +
                data$X7 +
                data$X13)

u <- summary(model)$residuals

temp_sum1 <- 0
for (i in 2:length(u)){
  temp_sum1 <- temp_sum1 + (u[i-1] * u[i])
}
rho_hat <- temp_sum1/sum(u^2)

T = nrow(x)
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
Y_fix <- T_A %*% y
X_fix <- T_A %*% as.matrix(cbind(matrix( rep(1, nrow(x))),x))

df8 <- data.frame(Y_fix)
df8 <- cbind(df8,X_fix)
colnames(df8) <- c("Y_fix", "x0", "x2", 'x3', "x4", 'x7', 'x13')
write.csv(df8, file = "D:\\Econometrics/data_without_autocor", row.names = F)




##### AUTO CoR for the right my data ####
x <- data[c("X2", "X3", "X7", "X12", "X14")]
X <- as.matrix(cbind(matrix( rep(1, nrow(x))),x))
Y <- y <- data$Y3

model <- lm(data$Y3 ~
              data$X2 +
              data$X3 +
              data$X4 +
              data$X7 +
              data$X14)

u <- summary(model)$residuals

temp_sum1 <- 0
for (i in 2:length(u)){
  temp_sum1 <- temp_sum1 + (u[i-1] * u[i])
}
rho_hat <- temp_sum1/sum(u^2)

T = nrow(x)
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
Y_fix <- T_A %*% y
X_fix <- T_A %*% as.matrix(cbind(matrix( rep(1, nrow(x))),x))

df9 <- data.frame(Y_fix)
df9 <- cbind(df9,X_fix)
colnames(df9) <- c("Y_fix", "x0", "x2", 'x3', "x4", 'x7', 'x14')
write.csv(df9, file = "D:\\Econometrics/data_without_autocor_1", row.names = F)






