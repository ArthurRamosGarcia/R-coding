

library(readr)
library(GGally)
library(class)
library(gmodels)


data = read.csv('C:/Users/Arthur/Downloads/wisc_bc_data.csv', stringsAsFactors=F)


str(data)

data=data[-1]

table(data$diagnosis)

data$diagnosis = factor(data$diagnosis, levels = c('B', 'M'),
                        labels = c('Benign', 'Malignant'))

summary(data)

data%>%
  GGally::ggpairs(columns = c(1:10))


round(prop.table(table(data$diagnosis)) * 100, digits = 1)


normalize = function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize(c(1,2,3,4,5))

data_normalized = as.data.frame(lapply(data[2:ncol(data)], normalize))

head(data_normalized$radius_mean)

data_train = data_normalized[1:469,]
data_test = data_normalized[470:569,]

data_train_labels=data[1:469,1]
data_test_labels=data[470:569,1]


previsões_teste=class::knn(train = data_train, test = data_test, cl=data_train_labels, k=21)

acuracia_k_21 = sum(previsões_teste==data_test_labels) / length(previsões_teste)

CrossTable(x = data_test_labels, y = previsões_teste,
           prop.chisq = FALSE)

k = 1:50
acuracia = data.frame()

for (i in seq_along(k)) {
  previsoes_teste = class::knn(train = data_train, test = data_test, cl = data_train_labels, k = k[i])
  acuracia_k = sum(previsoes_teste == data_test_labels) / length(previsoes_teste)
  acuracia = rbind(acuracia, data.frame(K = k[i], Acuracia = acuracia_k))
}

acuracia%>%
  ggplot(aes(x=K))+
  geom_line(mapping = aes(y=acuracia$Acuracia))

plot(acuracia$K, acuracia$Acuracia)




