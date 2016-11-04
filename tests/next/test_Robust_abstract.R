t <- rlm(stackloss~., data = dt, psi = "huber")

dtR <- as.R(dt)
t <- rlm(Diameter~ ShuckedWeight +  VisceraWeight, data = dtR, psi = "huber")



                                        #PLS, OPLS

for pls what to compute still and what is done

done:
Xmeans, Ymeans, method(not needed),

to-do:
fitted.values, residuals, predict, Yscores, loading weights, Yloadings, 


deeptbl  <- FLTable("tblPLSDeep2y", "ObsID", "VarID", "Num_Val")
rtbl <- as.R(deeptbl)
names(rtbl) <- letters[1:16]
flmod<- pls(A~., data =deeptbl, nfactor = 15 )
rmod <- mvr(a~., data = rtbl)

                                        #Robust Regression

                                        #Deep Table
library(MASS)
#options(debugSQL =FALSE)
deeptbl  <- FLTable("tblRobustRegr", "ObsID","VarID", "Num_Val")
q <- rlm(a~., data = deeptbl)
predict(q)
summary(q)
q$fitted.values


                                        #Wide Table:
widetbl <- FLTable("tblautompg", "ObsID")
t <- rlm(Weight~ Acceleration , data = widetbl)
summary(t)
coefficients(t)
residuals(t)

                                        #for R:
rtbl <- as.R(deeptbl)
names(rtbl) <- letters[1:3]
rmod <- rlm(a~., data =rtbl)





## PLS $ operator part.



## move to file lm.R
#' @export
`$.FLLinRegr`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property=="coefficients"){
        coefficientsvector <- coefficients(object)
        assign(parentObject,object,envir=parent.frame())
        return(coefficientsvector)
    }


    if(property == "Yloadings")
    {
        
        str <- paste0("SELECT *
FROM ",object@vfcalls["statstablename"]," a
WHERE a.AnalysisID = '",object@AnalysisID,"'
AND a.VectorName = 'XBetaT'
ORDER BY FactorNumber, VectorName")
        loadings <- sqlQuery(connection, str)
        return(loadings)
        
    }
#    if(property == "Ymeans")
 #   {
  #   Y_mean <- mean(#tablenameall.vars(object@formula)[1])
   # }
    if(property == "Xmeans")
    {}
##    if(property == "fitted.values")
  ##  {}
 ##   if(property == "residuals")
 ##   {}
 ##   if(property == "loadings")
##    {}
 ##   if(property == "call")
##    {}
    if(property == "Yscores")

    {
                str <- paste0("SELECT *
FROM ",object@vfcalls["statstablename"]," a
WHERE a.AnalysisID = '",object@AnalysisID,"'
AND a.VectorName = 'ScoreY'
ORDER BY FactorNumber, VectorName")
        y_scores<- sqlQuery(connection, str)
        return(y_scores)


    }
    
    if(property == "method")
    {}
    if(property == "Yloadings")
    {}


    

    }
