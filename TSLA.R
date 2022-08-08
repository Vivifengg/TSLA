tsla <- read.csv("TSLA.csv")

adjr = function(indep){
  dep = "X90.VaR"
  formula = paste(dep,"~",indep)
  fit <- lm(formula, data=tsla)
  adjr = summary(fit)[[9]] #Adjusted R-squared
  return(adjr)
}

indepNames<-names(tsla)[3:10]

lapply(indepNames,adjr)

plot(tsla$ForwardP.E, tsla$X90.VaR, )
plot(tsla$PEGRatio, tsla$X90.VaR)
plot(tsla$Price.Sales, tsla$X90.VaR)

plot(tsla$Price.Book, tsla$X90.VaR) #adjr=0.8768692
abline(fit$coefficients, col = "red")

fit<-lm(X90.VaR~Price.Book, tsla)
summary(fit)
#what is the predicted 90% VaR for Price/Book = 10?
0.0798824-10*0.0011155
#Change in 90% VaR for Price/Book increase from 20 to 30?
(0.0798824-30*0.0011155)-(0.0798824-20*0.0011155)


library(ggplot2)
ggplot(data = tsla,
       mapping = aes(x = Price.Book, y = X90.VaR, col = Date)
) + geom_point(size = 2) +
  xlim(20, 40) + ylim(0.03, 0.06) 

aswhole<-lm(X90.VaR~Price.Book+ForwardP.E+PEGRatio+Price.Sales, tsla)
summary(aswhole)
#VaR = 0.0664342 -0.0029070*Price.Book+0.0025519*ForwardP.E-
#      0.0632896*PEGRatio+0.0002158*Price.Sales
#adjr=0.8772

#FLAWS in our analysis
#1. The data is too limited. The analysis can be more reliable only if more data can be involved.
#Maybe we can extend the timeline to 3 or 5 years or we need to find a way to get the daily data.
#2. There may be multicollinearity between the independent variables.
#The market risk is complex and it can have a plenty of factors to influence the result.
#More independent variables and more practical models need to be considered.
