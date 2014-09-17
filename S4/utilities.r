# /**
#  * Converts List to where clause
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx='x' AND Vary='y'"
#  */
list.to.whereClause <- function (x) {
	     whereClause <- paste(names(x),x,sep="=\'",collapse="\' AND ");
	     whereClause <- paste(whereClause,"\'",sep="");
	     whereClause <- ifelse(nchar(whereClause) > 1, whereClause, "1=1");
	     whereClause
     }

# /**
#  * Converts List to class Spec. Used for Data Prep
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx(x), Vary(y)"
#  */
list.to.classSpec <- function (x) {
	classSpec <- paste(names(x),x,sep="(",collapse="), ")
	classSpec <- paste(classSpec,")",sep="")
	classSpec <- ifelse(nchar(classSpec) > 1, classSpec, "");
	classSpec
}

# /**
#  * Converts List to class Spec. Used for Data Prep
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx(x), Vary(y)"
#  */
list.to.excludeClause <- function (x) {
	excludeClause <- paste(x, collapse=", ")
	excludeClause
}