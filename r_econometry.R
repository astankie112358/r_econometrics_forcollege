a=read.csv("D:/Downloads/gretlxd.csv", sep=",",header = TRUE)

a_temp<-a
mod <- lm(a[,2] ~ a[,3] + a[,4] + a[,5] +a[,6] + a[,7] + a[,8] + a[,9] + a[,10] + a[,11], data = a)
d_gw<-4/((nrow(a)-ncol(a)-1))
sum<-(summary(mod)[4])
t_val<-abs((sum$coefficients[21:(19+ncol(a))]))
min_t_val<-min(t_val)
col_to_del<-match(min_t_val,t_val)
a_temp<-a[-c(2+col_to_del)]
mod <- lm(a[,2] ~ a[,3] + a[,4] + a[,5] +a[,6] + a[,7] + a[,8] + a[,9] + a[,10], data = a_temp)
cook<-cooks.distance(mod)
i<-1
repeat{
    
    max<-(max(cook))
    id<-match(max,cook)
    if(max<d_gw)
    {
      break()
    }else
    {
      cat("T_Value kolumny: ",min_t_val,'\n')
      cat("Usuwam kolumne:",2+col_to_del,'\n')
      cat("Iteracja numer: ",i,'\n')
    
      cat("Usuwam zmienn¹: ",id,'\n')
      
      cat("Odleglosc Cooka: ",max,'\n')
      
      cat("Parametr d* :",d_gw,'\n\n\n')
      
      
      a<-a[-c(id),]
      
      mod <- lm(a[,2] ~ a[,3] + a[,4] + a[,5] +a[,6] + a[,7] + a[,8] + a[,9] + a[,10] + a[,11], data = a)
      sum<-(summary(mod)[4])
      t_val<-abs((sum$coefficients[21:(19+ncol(a))]))
      min_t_val<-min(t_val)
      col_to_del<-match(min_t_val,t_val)
      a_temp<-a[-c(2+col_to_del)]
      mod <- lm(a[,2] ~ a[,3] + a[,4] + a[,5] +a[,6] + a[,7] + a[,8] + a[,9] + a[,10], data = a_temp)
      cook<-cooks.distance(mod)
      d_gw<-4/((nrow(a)-ncol(a)-1))
      i<-i+1
    }
    
}


