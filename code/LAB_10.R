data = read.csv("data_1.csv")
head(data)

lm(Y1 ~ ., data = data[i:j,])

length(data[i:j,])

# task 1 
head(data)
data$Z1 <- ifelse(data$X2 == "С" | data$X2 == "К", 1,0 )
data$Z2 <- ifelse(data$X8 == "Н", 1,0 )

lm(Y1 ~ X1 + X3 + X4 +X5 + X6 + X7  +Z1 + Z2, data = data[i:j,])
# CK_building <- rbind(subset(data[i:j,], X2 == "C"), 
#                      subset(data, X2 == "K"))
# 
# 
# PM_building <- rbind(subset(data, X2 == "Π"), 
#                       subset(data, X2 == "M"))
# 
# 
# 
# 
# lm(Y1 ~ X1  + X3 + X4 + X5 + X6 + X7 + X8, data = CK_building)
# lm(Y1 ~ X1 + X3 + X4 + X5 + X6 + X7 + X8, data = PM_building)

# task2 
new_building <- subset(data[i:j,], X8 == "Н")
old_building <- subset(data[i:j,], X8 == "В")

lm(Y1 ~ X1 + X3 + X4 + X5 + X6 + X7 + Z1, data = new_building)
lm(Y1 ~ X1 + X3 + X4 + X5 + X6 + X7 + Z1, data = old_building)

# my data 
i = 11
i = 17
j = i + 39

