



#install.packages("broom")
install.packages("broom", repos = "http://cran.us.r-project.org")
library(broom)
Azad_mutiple_Regression=function(data_Y,data_X){
  BB1=c() ###create an empty
  for(i in 1:ncol(data_Y)){
    mod11=lm(scale(log(data_Y[,i]))~.,data_X)
    PValues=data.frame(coef(summary(mod11))[,"Pr(>|t|)"])
    b=cbind(coef(mod11),confint(mod11))
    RC1=cbind(b,PValues)
    BB1=rbind(BB1,RC1)
  }
  return(BB1)
}



Azad_reg_summary=function(data_Y,data_X){
    RC1=Azad_mutiple_Regression(data_Y,data_X);
    dim(RC1)
    RC11=cbind(rownames(RC1),round(RC1[,1],10),round(RC1[,2],10),round(RC1[,3],10),round(RC1[,4],10))
    RC11=data.frame(RC11)
    colnames(RC11)=c("Variables","Estimates", "Lower Confidence Interval","Upper Confidence Interval","P Values")

    #######
    RC222 <- data.frame(matrix())
    sec_Y_intcept=which(RC11$Variables=="(Intercept)1")

    for(i in 1:ncol(data_Y)){
                   RC22=data.frame(RC11[(1+(i-1)*(sec_Y_intcept-1)):((sec_Y_intcept-1)*i),],check.names=F)
                   colnames(RC22)=c(paste0(names(data_Y)[i]),paste0(names(data_Y)[i]," Estimates"),paste0(names(data_Y)[i]," Lower Confidence Interval"),
                   paste0(names(data_Y)[i]," Upper Confidence Interval"),paste0(names(data_Y)[i]," Pvalues"))

                   RC222=cbind(RC222,RC22) }


    colnames(RC222)[1]="Dependent Variables"
    RC222$`Dependent Variables`=RC222[,2]

    for(i in 1:ncol(data_Y)){
                  RC222[,(2+(i-1)*5)]= " " }

    ##View(RC222)

return(RC222)

     }

#
# View(Azad_reg_summary)
# View(Azad_reg_summary(data_Y,data_X))
#
# RC1=Azad_reg_summary(data_Y,data_X);
# dim(RC1)
# View(RC1)
##############################################################################################################
##############################################################################################################
##############################################################################################################
Azad_Rsquare=function(data_Y,data_X){
  BB1=c() ###create an empty
  for(i in 1:ncol(data_Y)){
    mod11=lm(scale(log(data_Y[,i]))~.,data_X)
    R.sq=as.vector(glance(mod11)[,c(1,2)]) ##broom package
    rownames(R.sq)=c("R-squares")
    BB1=rbind(BB1,R.sq)
  }
  colnames(BB1)=c("R-square","R-square Adjusted")
  row.names(BB1)=colnames(data_Y)
  return(BB1)
}

# RC1=Azad_Rsquare(data_Y,data_X);
# dim(RC1)
# View(RC1)
##############################################################################################################
##############################################################################################################
##############################################################################################################
