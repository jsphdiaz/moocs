install.packages('kernlab')
library('kernlab')


data <- as.matrix(read.table('credit_card_data.txt', sep = '\t'))
data <- read.table('credit_card_data.txt', sep = '\t')


# call ksvm. Vanilladot is a simple linear kernel.
model <- ksvm(data[,1:10],data[,11],type="C-svc",kernel="vanilladot",C=10000000,scaled=TRUE)
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)


# calculate a1…am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- -model@b
a0
# see what the model predicts
pred <- predict(model,data[,1:10])
pred
# see what fraction of the model’s predictions match the
#actual classification
sum(pred == data[,11]) / nrow(data)


nrow(data)

