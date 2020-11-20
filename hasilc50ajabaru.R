#by damalialutfiani
library(C50)

#input file.csv
dataset=read.csv("c5aja.csv", sep = ";")
summary(dataset)
str(dataset)
dataa <- dataset
dataa$Risk <- factor(dataa$Risk)
str(dataa)
View(dataa)

#C5.0Control
ctrl = C5.0Control(fuzzyThreshold = TRUE,
                   sample = 0, earlyStopping = FALSE
)

#c5.0Default untuk membentuk model pohon keputusan
#kalotrial disini,boosting/trial di model summary-nya
C50model <- C5.0(dataa[ ,-3],
                 dataa[ ,3],
                 control=ctrl, trials = 10
)

#plot tree,kalo trial di plot = modelganti
plot(C50model,
     type="s",
     main="Decision Tree C5.0", trial = 9
     )
summary(C50model)

#uji data testing
train=dataa[1:floor(nrow(dataa)*0.95), ]
test=dataa[(floor(nrow(dataa)*0.95)+1):nrow(dataa), ]
C50predict <- predict(C50model, test[ ,-3], trials = 10)
summary(C50predict)

#keterangan nilai True/False
tmp=C50predict==test$Risk
#View(cbind(dataset, C50predict, tmp))
#jumlah data bernilai T/F
length(which(tmp))
#jumlah data keseluruhan
length(tmp)

accuracy=length(which(tmp))/length(tmp)*100.0
sprintf("The Accuracy = %.2f", accuracy)
submitC50 <- data.frame(VLC=test$VLC, Jarak=test$Jarak, Risk=test$Risk, Risk=C50predict)
write.csv(submitC50, file = "hasil955t10.csv", row.names = FALSE)

