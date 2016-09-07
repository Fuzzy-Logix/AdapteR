devtools::load_all(".")
flt <- FLTable("tblLoanDataTrain","Loanid",whereconditions = "LoanID<200000")
fltest <- FLTable("tblLoanDataTest","Loanid")
excludeCols=c("sub_grade","emp_name",
              "emp_length","addr_city",
              "addr_state","bc_util",
              "earliest_cr_line")
system.time(vresFL <- glm(default_ind~.,data=flt,
                          classSpec=list(term="36 months",
                                         grade="A",
                                         home_ownership="RENT",
                                         is_inc_v="TRUE",
                                         purpose="debt_consolidation"),
                          makeDataSparse=TRUE,
                          minStdDev=0.1,
                          maxCorrel=0.7,
                          excludeCols=excludeCols))
vfit <- predict(vresFL,fltest)
#vdf <- read.csv("C:/Users/phani/Downloads/tblTwitterBuzz.dat",sep="|",header = FALSE)
#colnames(vdf)<-colnames(flt)
#vdf1 <- vdf
vdf <- sqlQuery(connection,"SELECT * FROM tblLoanDataTrain ORDER BY LoanID")
vdf$OBSID<-NULL
system.time(vresR <- lm(Buzz_Magnitude~.,data=vdf))
head(vresR$coefficients)
head(vresFL$coefficients)
head(vresR$residuals)
head(vresFL$residuals)
head(vresR$fitted.values)
head(vresFL$fitted.values)
plot(vresR)
plot(vresFL)
summary(vresR)
summary(vresFL)
Rfit <- predict(vresR,vdf)
FLfit <- predict(vresFL,flt)
head(Rfit)
head(FLfit)
options(debugSQL=TRUE)
