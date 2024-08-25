# getting data
data = read.csv("D://Econometrics/data.csv")
data
nrow(data)

Y_3 <- data$Y3
X_2 <- data$X2
X_3 <- data$X3
X_7 <- data$X7
X_12 <- data$X12
X_13 <- data$X13

n <- length(Y_3)


# BUILD MODEL
model <- lm(data$Y1 ~ data$X1 + data$X10 + data$X11 + data$X14, data = data)

summary(model)


#Test model

test_model <- lm(y ~ data[1:25,4] + 
                   data[1:25,13] + 
                   data[1:25, 14]+
                   data[1:25,17], data = data)
summary_test_model <- summary(test_model)

#task 1 
summary_test_model$coefficients[,3]
qt(1 - 0.1/2, nrow(x) - ncol(x)-1)

#task 2
#YaroBoy data
x <-data[1:25, c( 3+1, 3+10, 3+11, 3+14)]
y <- data[1:25, 1]

X  <- as.matrix(cbind(matrix( rep(1, nrow(x))),x))
Y<- y

#calculate hear sigma 
sigma <- read.csv("models_characteristics.csv")[1,2]

sigma <- 1.280

Sigma_hat <- sigma * solve(t(X) %*% X)

LL <- test_model$coefficients - qt(1 - 0.1/2, df = 23 - 3 ) * sqrt(diag(Sigma_hat))
UL <- test_model$coefficients + qt(1 - 0.1/2, df = 23 - 3 ) * sqrt(diag(Sigma_hat))
  
cbind(LL, UL)

# make the result dataframe 

result <- matrix(rep(0, 5 * 5), ncol = 5 )
result[,1] <- summary_test_model$coefficients[,3]
result[,2] <- qt(1 - 0.1/2, nrow(x) - ncol(x)-1)
result[,3] <- round(summary_test_model$coefficients[,4],4)
result[,4] <- LL
result[,5] <- UL

df3 <- data.frame(result)
colnames(df3) <- c("t-test","t-stat", "p-value", "LL", "UL")
rownames(df3) <- c("X2", "X3", "X7", "X12", "X13")
#Task 3####

xbar <- sapply(x, mean)
xbar <- c(1,xbar)

#calculate predicted value
y_predicted <- sum(test_model$coefficients * xbar)

sigma_e <- sqrt(t(xbar) %*% Sigma_hat %*% xbar ) 

LL_sigma_e <- y_predicted - qt(1 - 0.1/2, 25- 3) * sigma_e
UL_sigma_e <- y_predicted + qt(1 - 0.1/2, 25- 3) * sigma_e

temp <- cbind(y_predicted,LL_sigma_e,UL_sigma_e)
colnames(temp) <- c("y_pred","LL","UL")
temp


sigma_e_i <- sqrt(sigma + sigma_e^2)

LL_sigma_e_i <- y_predicted - qt(1 - 0.1/2, 25- 3) * sigma_e_i
UL_sigma_e_i <- y_predicted + qt(1 - 0.1/2, 25- 3) * sigma_e_i



