##code
library(ggplot2)
library(mgcv)
library(MASS)
#Q1
#fulldata = readcsv("HousePricestxt", sep = " ", header = TRUE, quote = "")
code = 0753031
fulldata = readcsv("HousePricestxt", sep = " ", header = TRUE)
digitsum = function(x) sum(floor(x/10^(0:(nchar(x)-1)))%%10)
setseed(code)
mysum = digitsum(code)
if((mysum %% 2) == 0) { # number is even
  rownumbers = sample(1:327,150,replace=F)
} else { # number is odd
  rownumbers = sample(309:459,150,replace=F)
}
mydata = fulldata[rownumbers,]

#以pricehouse 和 shops 二者为例演示平滑参数k的选择,
testmodel <- gam(log(PriceHouse)~s(Shops,k=3), data = mydata)##change k value
gamcheck(testmodel)##compare k' and edf, if the two value is too close, means k value too low, than lager k
testmodel <- gam(log(PriceHouse)~s(Shops,k=6), data = mydata)
gamcheck(testmodel)
testmodel <- gam(log(PriceHouse)~s(Shops,k=8), data = mydata)
gamcheck(testmodel)
testmodel <- gam(log(PriceHouse)~s(Shops,k=10), data = mydata)
gamcheck(testmodel)
testmodel <- gam(log(PriceHouse)~s(Shops,k=20), data = mydata)#As the value of k increases, the curve bends gradually
gamcheck(testmodel)
## we can choose k= 7 or 8

##now we use all variable to build the model in different k value
testmodel1<-gam(log(PriceHouse)~s(Shops)+s(Bankruptcies)+s(MeanIncome)+s(TaxForms)+s(HotelRestaurant)+s(Industries)+s(HealthSocial),data = mydata)
testmodel2<-gam(log(PriceHouse)~s(Shops)+Bankruptcies+MeanIncome+TaxForms+HotelRestaurant+Industries+HealthSocial, data = mydata) 
testmodel3<-gam(log(PriceHouse)~Shops+s(Bankruptcies)+MeanIncome+TaxForms+HotelRestaurant+Industries+HealthSocial, data = mydata) 
testmodel4<-gam(log(PriceHouse)~Shops+Bankruptcies+s(MeanIncome)+TaxForms+HotelRestaurant+Industries+HealthSocial, data = mydata) 
testmodel5<-gam(log(PriceHouse)~Shops+Bankruptcies+MeanIncome+s(TaxForms)+HotelRestaurant+Industries+HealthSocial, data = mydata) 
testmodel6<-gam(log(PriceHouse)~Shops+Bankruptcies+MeanIncome+TaxForms+s(HotelRestaurant)+Industries+HealthSocial, data = mydata) 
testmodel7<-gam(log(PriceHouse)~Shops+Bankruptcies+MeanIncome+TaxForms+HotelRestaurant+s(Industries)+HealthSocial, data = mydata) 
testmodel8<-gam(log(PriceHouse)~Shops+Bankruptcies+MeanIncome+TaxForms+HotelRestaurant+Industries+s(HealthSocial), data = mydata) 
testmodel9<-gam(log(PriceHouse)~Shops+s(Bankruptcies)+MeanIncome+s(TaxForms)+HotelRestaurant+Industries+HealthSocial, data = mydata)
testmodel10<-gam(log(PriceHouse)~Shops+s(Bankruptcies)+MeanIncome+s(TaxForms)+HotelRestaurant+s(Industries)+HealthSocial, data = mydata) 
testmodel11<-gam(log(PriceHouse)~Shops+s(Bankruptcies)+MeanIncome+s(TaxForms)+s(HotelRestaurant)+s(Industries)+HealthSocial, data = mydata) 

AIC(testmodel1,testmodel2,testmodel3,testmodel4,testmodel5,testmodel6,testmodel7,testmodel8,testmodel9,testmodel10,testmodel11)

#Q2
Qumodel<-gam(log(PriceHouse)~s(TaxForms)+s(HealthSocial)+ti(TaxForms,HealthSocial), data = mydata) 

#Q3
##use glmmPQL VS glm
hglm_model <- glmmPQL(log(PriceHouse) ~ TaxForms, random = ~1|Province, family = gaussian, data = mydata)
glm_model <- glm(log(PriceHouse) ~ TaxForms, family = gaussian, data = mydata)

## compare two model  -> use residuals and t.test
plot(hglm_model$residuals[,1])
plot(glm_model$residuals)
mean(hglm_model$residuals[,1])
mean(glm_model$residuals)
t.test(hglm_model$residuals[,1],glm_model$residuals )
#ggplot(aes(x = Municipality, y= PriceHouse,colour = Province),data = mydata)+geom_point()
#ggplot(aes(x = Municipality, y= TaxForms,colour = Province),data = mydata)+geom_point()
all_hglm_model <- glmmPQL(log(PriceHouse) ~ Shops+Bankruptcies+MeanIncome+TaxForms+HotelRestaurant+Industries+HealthSocial, random = ~1|Province, family = gaussian, data = mydata)
all_glm_model <- glm(log(PriceHouse) ~ Shops+Bankruptcies+MeanIncome+TaxForms+HotelRestaurant+Industries+HealthSocial, family = gaussian, data = mydata)
t.test(all_hglm_model$residuals[,1],all_glm_model$residuals )
#Q4
## 题意中的第一个模型的建立（low Industries  ）
library(ggplot2)
ggplot(mydata,aes(x= Industries)) + geom_histogram(binwidth = 20)+scale_x_continuous(limits = c(0, 500))+ggtitle('industries < 500')
##the plot shows some very big value exists
## choose Industries < 500
small_Ind_data <- mydata[which(mydata$Industries < 200),]
small_Ind_data <- small_Ind_data[which(small_Ind_data$Industries > 40),]
small_Ind_model <- gam(log(PriceHouse)~s(Shops)+s(Bankruptcies)+s(MeanIncome)+s(TaxForms)+s(HotelRestaurant)+s(Industries)+s(HealthSocial),data = small_Ind_data)

small_Ind_vec <- lapply(small_Ind_data[,5:11],median)#get median vector
small_Ind_predict <- exp(predict(small_Ind_model,small_Ind_vec))#predict


## 第二个模型(high HotelRestaurant )
ggplot(mydata,aes(x= HotelRestaurant)) + geom_histogram(binwidth = 20)+scale_x_continuous(limits = c(0, 500))

small_hotel_data <- mydata[which(mydata$Industries < 300),]
small_hotel_data <- small_hotel_data[which(small_hotel_data$Industries > 40),]
small_hotel_model <- gam(log(PriceHouse)~s(Shops)+s(Bankruptcies)+s(MeanIncome)+s(TaxForms)+s(HotelRestaurant)+s(Industries)+s(HealthSocial),data = small_hotel_data)

small_hotel_vec <- lapply(small_hotel_data[,5:11],median)
small_hotel_predict <- exp(predict(small_hotel_model,small_hotel_vec))
##observe Industries and HotelRestaurant
plot(x = mydata$Industries,y=mydata$HotelRestaurant)

##Q5 i -> four method
library(glmnet)
#lasso
ind_lasso <- cv.glmnet(as.matrix(small_Ind_data[,5:11]), as.matrix(log(small_Ind_data$PriceHouse)), family="gaussian",alpha=1)
#ridge regression
ind_ridge <- cv.glmnet(as.matrix(small_Ind_data[,5:11]), as.matrix(log(small_Ind_data$PriceHouse)), family="gaussian",alpha=0)
#elastic net
ind_elastic <- cv.glmnet(as.matrix(small_Ind_data[,5:11]), as.matrix(log(small_Ind_data$PriceHouse)), family="gaussian",alpha=0.4)
##maxlik
X <- model.matrix(~Shops + Bankruptcies + MeanIncome + TaxForms + HotelRestaurant + Industries + HealthSocial, data = small_Ind_data)
loss.f <- function(par, X, y){
  muij <- X %*% par
  Qij <- (y - muij) ^2
  Q <- sum(Qij)
  return(Q)
}

starts <- as.matrix(coefficients(ind_ridge))
ind_maxlik <- optim(par = starts, fn = lossf, y = log(small_Ind_data$PriceHouse), X=X, method = "CG", hessian = T)
ind_maxlik$par
ind_coef<-as.matrix(cbind(coefficients(ind_lasso),coefficients(ind_ridge),coefficients(ind_elastic),ind_maxlik$par))
colnames(ind_coef)<-c('lasso','ridge','elastic','maxlik')

#write.csv(as.data.frame(ind_coef), 'ind_coef.csv')

##Q5 ii
#lasso
hotel_lasso <- cv.glmnet(as.matrix(small_hotel_data[,5:11]), as.matrix(log(small_hotel_data$PriceHouse)), family="gaussian",alpha=1)
#ridge regression
hotel_ridge <- cv.glmnet(as.matrix(small_hotel_data[,5:11]), as.matrix(log(small_hotel_data$PriceHouse)), family="gaussian",alpha=0)
#elastic net
hotel_elastic <- cv.glmnet(as.matrix(small_hotel_data[,5:11]), as.matrix(log(small_hotel_data$PriceHouse)), family="gaussian",alpha=0.4)
##maxlik
X <- model.matrix(~Shops + Bankruptcies + MeanIncome + TaxForms + HotelRestaurant + Industries + HealthSocial, data = small_hotel_data)
loss.f <- function(par, X, y){
  muij <- X %*% par
  Qij <- (y - muij) ^2
  Q <- sum(Qij)
  return(Q)
}
starts <- as.matrix(coefficients(hotel_ridge))
hotel_maxlik <- optim(par = starts, fn = lossf, y = log(small_hotel_data$PriceHouse), X=X, method = "CG", hessian = T)
hotel_maxlik$par
hotel_coef<-as.matrix(cbind(coefficients(hotel_lasso),coefficients(hotel_ridge),coefficients(hotel_elastic),hotel_maxlik$par))
colnames(hotel_coef)<-c('lasso','ridge','elastic','maxlik')
#write.csv(as.data.frame(hotel_coef), 'hotel_coef.csv')

##set two vector
small_Ind_vec<-t(c(1, (as.matrix(lapply(small_Ind_data[,5:11],median)))))
small_hotel_vec<-t(c(1, (as.matrix(lapply(small_hotel_data[,5:11],median)))))
##build a predict function
calpre <- function(vec,model_coef){
  pre <- 0                 
 for ( i in 1:8){
   pre <- pre + as.numeric(vec[i]) * model_coef[i]
   }
   return(exp(pre))
}
# result matrix
res_mat <-matrix(NA,4,2)
res_mat<-as.data.frame(res_mat)
colnames(res_mat)<-c('hotel','ind')
rownames(res_mat)<-c('lasso','ridge','elastic','maxlik')
##output matrix :4 method, 2 data
res_mat[1,1]<-calpre(small_hotel_vec,coefficients(hotel_lasso))
res_mat[2,1]<-calpre(small_hotel_vec,coefficients(hotel_ridge))
res_mat[3,1]<-calpre(small_hotel_vec,coefficients(hotel_elastic))
res_mat[4,1]<-calpre(small_hotel_vec,hotel_maxlik$par)
res_mat[1,2]<-calpre(small_Ind_vec,coefficients(ind_lasso))
res_mat[2,2]<-calpre(small_Ind_vec,coefficients(ind_ridge))
res_mat[3,2]<-calpre(small_Ind_vec,coefficients(ind_elastic))
res_mat[4,2]<-calpre(small_Ind_vec,ind_maxlik$par)
