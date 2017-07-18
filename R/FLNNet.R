#' @export
setClass(
    "FLNnet",
    slots = list(formula = "formula",
                 scoreTable = "character",
                 results = "list",
                 table = "FLTable"
                 ) )






neuralnet <- function (formula,data=list(),...) {
    UseMethod("neuralnet", data)
}
#' @export
neuralnet.default <- function (formula,data=list(),...) {
    if (!requireNamespace("neuralnet", quietly = TRUE)){
        stop("neuralnet package needed for neuralnet. Please install it.",
             call. = FALSE)
    }
    else return(neuralnet::neuralnet(formula=formula,data=data,...))
}



#' \code{neuralnet} performs Neural Network on FLTable objects.
#' The DB Lytix function called is FLNNetUDT. Artificial neural networks are a family
#' of statistical learning algorithms inspired by biological neural networks
#' and are used to estimate or approximate transformations that depend on a
#' large number of inputs. Thesetransformation are then used to model the output.
#' In DB Lytix, neural network is implemented as a user-defined table function which takes
#' the input in deep format, the topography of the network along with some
#' hyper-parameters to calculate the neuron connection weights using the
#' back-propagation algorithm.
#' @seealso \code{\link[neuralnet]{neuralnet}} for R reference implementation.
#' @param formula A symbolic description of model to be fitted
#' @param data An object of class FLTable.
#' @param hidden  Number of neurons in the hidden layer
#' @param layers Number of layers of Neural Net model as of now at Max 2 are alloed.
#' @param Learningrate  A symbolic description of model to be fitted
#' @param isSigmoid Used to select execution mode: Regression or Classification
#' @param epoch Maximum number of iterations

#' @slot results cache list of results computed
#' @slot table Input data object
#' @method plot FLNnet
#' @method predict FLNnet
#' @return \code{neuralnet} returns an object of class \code{FLNnet}
#' @examples
#' tbl <- FLTable("tblwinetrain", obs_id_colname = "OBSID")
#' rtbl <- as.R(tbl)
#' rtbl <- rtbl[, -c(1)]
#' n <- names(rtbl)
#' f <- as.formula(paste("Wine_Type ~", paste(n[!n %in% "Wine_Type"], collapse = " + ")))
#' For 1 layer.
#' flmod <- neuralnet(f, data = tbl, hidden = c(5), layers = 1)
#' rmod <- neuralnet(f, data = rtbl, hidden = c( 5))
#' For 2 layer 
#' flmod <-neuralnet(f, data = tbl, hidden = c(10,5))
#' rmod <- neuralnet(f, data = rtbl, hidden = c(10, 5))

#' library(neuralnet)
#' flmod <- neuralnet(Wine_Type~.,data=tbl, hidden = c(10, 5))
#' flmod <- neuralnet(Wine_Type~ Alcohol + Ash,data=tbl)
#' rmod <- neuralnet(Wine_Type~ Alcohol + Ash,data=rtbl, hidden = c(5,5))
#'
#' R Example:
#' library(MASS)
#' rdata <- Boston
#' n <- names(rdata)
#' f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
#' maxs <- apply(rdata, 2, max)
#' mins <- apply(rdata, 2, min)
#' rdata <- as.data.frame(scale(rdata, center = mins, scale = maxs - mins))
#' fltbl <- as.FL(rdata)
#' rmod <- neuralnet(f,data=rdata,hidden=c(5,3),linear.output=T)
#' flmod <- neuralnet(f, data = fltbl, hidden = c(5,3))
#' R example 2:
#' set.seed(100)
#' traininginput <-  as.data.frame(runif(50, min=0, max=100))
#' trainingoutput <- sqrt(traininginput)
#' rtbl <- cbind(traininginput,trainingoutput)
#' colnames(rtbl) <- c("InputCol","OutputCol")
#' fltbl <- as.FL(rtbl)
#' rmod <- neuralnet(OutputCol~InputCol,data=rtbl,hidden=c(10),linear.output=T)
#' flmod <- neuralnet(OutputCol~InputCol,data=fltbl,hidden=c(10), IsSigmoid = 0, layers = 1)
#' @export
neuralnet.FLTable <- function(formula, data, fetchID = TRUE,hidden = 5,layers = 2,learningrate = .2,epoch = 500,IsSigmoid = 1, ...)
{##browser()
    vcallObject <- match.call()
    deeptblname <- gen_unique_table_name("nnetdeep")
    vdeeptbl <- data
    if(!isDeep(data))
    {
        FLdeep <- prepareData(formula         = formula ,
                              data            = data,
                              outDeepTable    = deeptblname,
                              makeDataSparse  = 1,
                              performVarReduc = 0,
                              minStdDev       = .01,
                              perfromNorm = 1,     
                              maxCorrel       = .8,
                              fetchIDs        = FALSE
                              )
        vdeeptbl <- FLdeep$deepx
    }
    vmap <- FLdeep$vmapping[FLdeep$vmapping != 0]
    outtblname <- gen_unique_table_name("nnetout")
    
    data <- setAlias(data,"")
    functionName <- "FLNNetUdt"
    
    cnames <- c(GroupID = 1,
                ObsID = FLdeep$deepx@select@variables$obs_id_colname,
                VarID = FLdeep$deepx@select@variables$var_id_colname,
                Num_Val= FLdeep$deepx@select@variables$cell_val_colname )
    n1 <- n2 <- 5
    if(length(hidden) == 2){
        n1 <- hidden[1]
        n2 <- hidden[2]
    }
    else if(length(hidden) == 1){
        n1 <- n2 <- hidden
    }

    varg <- c(NeuronCountOne = n1)

    
    if(layers ==1){
        n2 <- NULL
        varg <- c(varg,NeuronCountTwo = list(NULL))                  
    }
    else
        varg <- c(varg,NeuronCountTwo = n2)    
        
    varg <- c(varg,
              LearningRate= learningrate,
              MaxEpochs = epoch,
              IsSigmoid = IsSigmoid)



    t <- createTable(outtblname, 
                     pSelect =  constructUDTSQL(pViewColnames = cnames,
                                                pFuncName = functionName,
                                                pOutColnames = c("a.*"),
                                                pSelect = FLdeep$deepx@select@table_name,
                                                pLocalOrderBy=c("GroupID", "ObsID", "VarID"), 
                                                pNest = TRUE, 
                                                pFromTableFlag = TRUE,
                                                pArg = varg))
    return(new("FLNnet",
               formula=formula,
               scoreTable="",
               table=data,
               results=list(call=vcallObject,
                            deeptbl = vdeeptbl,
                            vspec = outtblname,
                            vneurons = list(layers = layers, l1 =n1,l2 = n2 ),
                            vvars = names(vmap)
                            )))   
}





#' @export
`$.FLNnet`<-function(object,property){
                                        #parentObject <- deparse(substitute(object))
    parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

    if(property=="weights"){
        vstr <- paste0("select LayerID,Weight from ",object@results$vspec," order by LayerID, TargetNeuronID,isBias DESC ,NeuronID")
        rdf <- sqlQuery(connection, vstr)

        n1 <- object@results$vneuron$l1
        n2 <- object@results$vneuron$l2
        if(!is.null(n2))
        {vin = ((nrow(rdf) - 1 - 2*n2 - n1*n2)/n1) - 1
            suppressWarnings(
                weights <- list(list(layer1 =  matrix(rdf$Weight[rdf$LayerID ==1],
                                                      nrow = vin+1,
                                                      ncol = n1),
                                     layer2 = matrix(rdf$Weight[rdf$LayerID ==2],
                                                     nrow = n1+1,
                                                     ncol =n2 ),
                                     layer3 =matrix(rdf$Weight[rdf$LayerID ==3],
                                                    nrow = n2 +1,
                                                    ncol = 1) )))}
        else
        {
            vin <- (nrow(rdf) - 1 - 2*n1)/n1
            suppressWarnings(
                weights <- list(list(layer1 =  matrix(rdf$Weight[rdf$LayerID ==1],
                                                      nrow = vin+1,
                                                      ncol = n1),
                                     layer2 = matrix(rdf$Weight[rdf$LayerID ==2],
                                                     nrow = n1+1,
                                                     ncol =1 ) ))) }                

            
        return(weights) }

    if(property == "call"){
        return(object@results$call) }

    if(property == "cost"){
        vstr <- paste0("select distinct(Cost) AS Cost from ",object@results$vspec,"")
        rdf <- sqlQuery(connection, vstr)
        return(rdf$Cost) }

    if(property == "model.list"){
        return(list(response = object@results$vvars[1],
                    variables = object@results$vvars[2:length(object@results$vvars)]))
    }

    if(property == "linear.output"){
        return(TRUE)
    }

    if(property == "result.matrix"){
        vstr <- paste0("select Weight from ",object@results$vspec," order by LayerID, TargetNeuronID,NeuronID")
        rdf <- sqlQuery(connection, vstr)
        vrow <- nrow(rdf) + 3
        vreq <- c(1, .05, 500,rdf$Weight)
        vreq <- matrix(vreq,nrow = vrow, ncol = 1)
        rownames(vreq) <- c("error", "reached.threshold", "steps", 1:nrow(rdf))
        return(vreq)

    }

      
}

predict.FLNnet <- function(object,newdata = object@table, ...){
    var <- getVariables(object@results$deeptbl)
    tblname <- gen_score_table_name("neuralscore")
    ret <- sqlStoredProc(connection,"FLNNetUdtScore",
                         ModelTable = object@results$vspec,
                         InTable = getTableNameSlot(object@results$deeptbl),
                         GroupIDCol = NULL,
                         ObsIDCol = var[[1]],
                         VarIDCol = var[[2]],
                         NumValCol = var[[3]],
                         ScoreTable = tblname,
                         outputParameter = c(OutTable = 'a')                            
                         )
    vstr <- paste0("select OBS_ID as ObsID, Actual as res from ",tblname,"")
    tblfunqueryobj <- new("FLTableFunctionQuery",
                          connectionName = getFLConnectionName(),
                          variables = list(
                              obs_id_colname = "ObsID",
                              cell_val_colname = "res"),
                          whereconditions="",
                          order = "ObsID",
                          SQLquery=vstr)
    flv <- new("FLSimpleVector",
               select = tblfunqueryobj,
               dimColumns = c("ObsID","res"),
               Dimnames = list(rownames(newdata)),
               dims = as.integer(newdata@dims[1]),
               type = "integer")

    return(flv) }

## set at NULL:- covariate, call, response, err.fact,
## act.fact, startweights, generalized.weights, net.result, data.
## model.list for labelling of data, result.matrix
plot.FLNnet <- function(object, ...){
    reqList <- structure(
        list(call = object$call,
             weights = object$weights,
             model.list = object$model.list,
             result.matrix = object$result.matrix
             ),
        class="nn")
    return(plot(reqList,...))
}
