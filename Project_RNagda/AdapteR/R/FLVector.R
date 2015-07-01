#' @include utilities.R
#' @include FLTable.R
NULL
#' An S4 class to represent FLVector
#'
#' @slot table object of class FLTable
#' @slot col_name A character column name
setClass(
	"FLVector",
	slots = list(
		table = "FLTable",
		col_name = "character"
	)
)
