#' @include FLTable.R
#' @include FLMatrix.R
#' @include FLVector.R
#' @include utilities.R
NULL
#' An S4 class to represent FLGAM

## Example Run:-
## widetable <- FLTable("FL_DEMO","tblGAMSimData","ObsID")
## myformula <- yVal~x0Val+bs(x1Val,degree=3,knots=10)+lo(x1Val,x2Val)+bs(x2Val)
## gamobject <- gam(myformula,data=widetable)
## predictedValues <- predict(gamobject,widetable)
## gamobject$coefficients (NOT SIMILAR TO R's Coeff Vector)

setClass(
	"FLGAM",
	slots=list(
		formula="formula",
		AnalysisID="character",
		table="FLTable",
		results ="list",
		knots="numeric",
		splines="character",
		specid = "character"
	)
)

setGeneric("gam",
	function(formula,
			family=stats::gaussian,
			data,...)
	standardGeneric("gam"))

setMethod("gam",
	signature(formula="ANY"),
	function(formula,...)
	gam::gam(formula,...))

setMethod("gam",
	signature(formula="formula",
		data="FLTable"),
	function(formula,family=stats::poisson,
		data,offset=NULL,
		model=TRUE,maxiter=500,...)
	gam.FLTable(formula=formula,
		family=stats::poisson,
		data=data,
		offset=offset,
		model=model,
		maxiter=maxiter,...
		))

fquote <- function(pname) return(paste0("'",pname,"'"))

gam.FLTable <- function(formula,family=stats::poisson,
						data,offset=NULL,
						model=TRUE,maxiter=500,FLGAMParams=list(),...)
{
	# browser()
	if(is.character(family) && base::toupper(family)!="POISSON")
	stop("only poisson family is supported currently")
	if(is.function(family) && !base::identical(family,stats::poisson))
	stop("only poisson family is supported currently")

	vallVars <- base::all.vars(formula)
	vcolnames <- colnames(data)

	sapply(vallVars,function(x)
		if(!(x %in% vcolnames))
		stop(x," not in colnames of data"))

	if(is.null(offset)) offset <- "NULL"
	if(!is.numeric(maxiter) || any(maxiter<1))
	stop("maxiter should be numeric and >= 1")
	else maxiter <- as.integer(base::max(maxiter))

	argList  <- as.list(environment())

	typeList <- list(offset="character",
					model="logical",
					maxiter="integer"
					)
	validate_args(argList, typeList)

	sapply(vallVars,function(x) base::assign(x,rnorm(10),pos=.GlobalEnv))
	tryCatch(vmodelFrame <- stats::model.frame(formula),
		error=function(e) stop("formula may be incorrect. Error is:",e))

	# vattributes <- apply(vmodelFrame,2,function(x){
	# 					return(attributes(x))
	# 					})
	vspecid <- genRandVarName()
	vgamParams <- data.frame(specid=vspecid,termid=1,
							varname=1,splinetype=1,
							splinedegree=1,numofknots=1,
							byvarname=1,smoothparam=1,
							smoothdegree=1,numofpredpts=1)

	vtermid <- 1
	for(i in 2:ncol(vmodelFrame))
	{
		vattributes <- attributes(vmodelFrame[[i]])
		##no-fit
		if(is.null(vattributes))
		{
			d <- data.frame(fquote(vspecid),vtermid,
						fquote(as.character(attr(terms(formula),"variables")[i+1])),fquote("na"),
						"NULL","NULL",
						"NULL","NULL",
						"NULL",20)
			vtermid <- vtermid + 1
			colnames(d) <- colnames(vgamParams)
			vgamParams <- base::rbind(vgamParams,d)
		}
		## lo fit
		else if(base::identical(vattributes$class,c("smooth","matrix")))
		{
			for(j in 1:vattributes$ncols)
			{
				d <- data.frame(fquote(vspecid),vtermid,
								fquote(vattributes$dimnames[[2]][j]),fquote("bs"),
								3,4,
								"NULL",0,
								0,20)
				colnames(d) <- colnames(vgamParams)
				vgamParams <- base::rbind(vgamParams,d)
			}
			vtermid <- vtermid + 1
		}
		## bs fit
		else if(base::identical(vattributes$class,c("bs","basis","matrix")))
		{
			vcall <- as.character(attr(terms(formula),"variables")[i+1])
			## May use Regexpressions
			vname <- base::strsplit(vcall,"(",fixed = T)[[1]][2]
			vexp1index <- base::regexpr(")",vname,fixed=TRUE)[1]
			vexp2index <- base::regexpr(",",vname,fixed=TRUE)[1]

			if(vexp1index==-1) vexp <- ","
			else if(vexp2index==-1) vexp <- ")"
			else
			{
				if(vexp1index<vexp2index) vexp <- ")"
				else vexp <- ","
			}
			vname <- base::strsplit(vname,vexp,fixed = T)[[1]][1]

			d <- data.frame(fquote(vspecid),vtermid,
							fquote(vname),fquote("bs"),
							vattributes$degree,ifelse(length(vattributes$knots)==0,vattributes$degree+1,vattributes$knots),
							"NULL",0,
							0,20)
			vtermid <- vtermid + 1
			colnames(d) <- colnames(vgamParams)
			vgamParams <- base::rbind(vgamParams,d)
		}
		## s fit
		else if(base::identical(vattributes$class,c("smooth")))
		{
			vcall <- as.character(attr(terms(formula),"variables")[i+1])
			## May use Reg expressions
			vname <- base::strsplit(vcall,"(",fixed = T)[[1]][2]
			vexp1index <- base::regexpr(")",vname,fixed=TRUE)[1]
			vexp2index <- base::regexpr(",",vname,fixed=TRUE)[1]

			if(vexp1index==-1) vexp <- ","
			else if(vexp2index==-1) vexp <- ")"
			else
			{
				if(vexp1index<vexp2index) vexp <- ")"
				else vexp <- ","
			}
			vname <- base::strsplit(vname,vexp,fixed = T)[[1]][1]
			# d <- data.frame(fquote(vspecid),vtermid,
			# 			fquote(vname),fquote("bs"),
			# 			vattributes$df,vattributes$df,
			# 			"NULL",0,
			# 			0,20)
			d <- data.frame(fquote(vspecid),vtermid,
						fquote(vname),fquote("na"),
						"NULL","NULL",
						"NULL","NULL",
						"NULL",20)
			vtermid <- vtermid + 1
			colnames(d) <- colnames(vgamParams)
			vgamParams <- base::rbind(vgamParams,d)
		}
		else stop("currently only b,bs and lo are supported")
	}
	vgamParams <- vgamParams[-1,]
	## Storing FLGAMParams
	vsqlstr <- base::apply(vgamParams,1,function(x){
		paste0(" INSERT INTO ",getOption("ResultDatabaseFL"),".fzzlGAMParams VALUES(",
			paste0(x,collapse=","),");")})
	vsqlstr <- paste0(vsqlstr,collapse="\n")

	sqlSendUpdate(getOption("connectionFL"),vsqlstr)
	
	vsqlstr <- paste0(" CALL FLGAM(",fquote(data@select@table_name),",",
								fquote(getVariables(data)[["obs_id_colname"]]),",",
								fquote(vallVars[1]),",",
								offset,",",
								fquote(vspecid),",'POISSON',",
								maxiter,",",
								fquote(vspecid),",AnalysisID);")

	vresult <- sqlQuery(getOption("connectionFL"),vsqlstr)
	if(is.character(vresult) && length(vresult)==2)
	stop(vresult)
	else vanalysisId <- as.character(vresult[1,1])
	return(new("FLGAM",
				formula=formula,
				AnalysisID=vanalysisId,
				table=data,
				results=list(),
				knots=c(1),
				splines="",
				specid=vspecid
				))
}

#' @export
`$.FLGAM`<-function(object,property)
{
	#parentObject <- deparse(substitute(object))
	parentObject <- unlist(strsplit(unlist(strsplit(as.character(sys.call()),"(",fixed=T))[2],",",fixed=T))[1]

	if(property=="coefficients")
	{
		coefficientsvector <- coefficients.FLGAM(object)
		assign(parentObject,object,envir=parent.frame())
		return(coefficientsvector)
	}
	# else if(property=="fitted.values")
	# {
	# 	fitvector <- fitted.values.FLGAM(object)
	# 	assign(parentObject,object,envir=parent.frame())
	# 	return(fitvector)
	# }
	# else if(property=="residuals")
	# {
	# 	residualsvector <- residuals.FLGAM(object)
	# 	assign(parentObject,object,envir=parent.frame())
	# 	return(residualsvector)
	# }
	else stop(property," is not a valid property")
}

coefficients.FLGAM <- function(object)
{
	# sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
	# 					" CoeffID+1 AS vectorIndexColumn,",
	# 					" CoeffValue AS vectorValueColumn",
	# 				" FROM fzzlGAMCoeffs",
	# 				" WHERE AnalysisID=",fquote(object@AnalysisID))

	# tblfunqueryobj <- new("FLTableFunctionQuery",
 #                        connection = getOption("connectionFL"),
 #                        variables = list(
	# 		                obs_id_colname = "vectorIndexColumn",
	# 		                cell_val_colname = "vectorValueColumn"),
 #                        whereconditions="",
 #                        order = "",
 #                        SQLquery=sqlstr)

	# flv <- new("FLVector",
	# 			select = tblfunqueryobj,
	# 			dimnames = list(1:length(object@coeffnames),
	# 							"vectorValueColumn"),
	# 			isDeep = FALSE)
	return(sqlQuery(getOption("connectionFL"),
		paste0("SELECT * FROM fzzlGAMCoeffs where AnalysisID=",fquote(object@AnalysisID))))
}

predict.FLGAM <- function(object,
						newdata=object@table,
						scoreTable="")
{
	if(newdata@isDeep) stop("input wide table for scoring")
	if(scoreTable=="")
	scoreTable <- paste0(getOption("ResultDatabaseFL"),".",gen_score_table_name(object@table@select@table_name))
	else if(!grep(".",scoreTable)) stop("scoreTable should have the form database.tablename")

	sqlstr <- paste0("CALL FLGAMScore(",fquote(object@specid),",",
										fquote(object@AnalysisID),",",
										fquote(newdata@select@table_name),",",
										fquote(getVariables(newdata)[["obs_id_colname"]]),",",
										"NULL,",
										"NULL,",
										"NULL,",
										fquote(scoreTable),",",
										fquote(paste0("scoring ",newdata@select@table_name,
											" with ",object@specid," from AdapteR")),",",
										"AnalysisID);")

	AnalysisId <- sqlQuery(getOption("connectionFL"),
								sqlstr)

	sqlstr <- paste0(" SELECT '%insertIDhere%' AS vectorIdColumn,",
						"ObsID AS vectorIndexColumn,",
						"fpred AS vectorValueColumn",
					" FROM ",scoreTable)

	tblfunqueryobj <- new("FLTableFunctionQuery",
                        connection = getOption("connectionFL"),
                        variables = list(
			                obs_id_colname = "vectorIndexColumn",
			                cell_val_colname = "vectorValueColumn"),
                        whereconditions="",
                        order = "",
                        SQLquery=sqlstr)

	flv <- new("FLVector",
				select = tblfunqueryobj,
				dimnames = list(1:nrow(newdata),
								"vectorValueColumn"),
				isDeep = FALSE)

	return(flv)

}