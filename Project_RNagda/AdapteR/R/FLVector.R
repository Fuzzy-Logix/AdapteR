setOldClass("FLTable")

setClass(
	"FLVector", 
	slots = list( 
		table = "FLTable",
		col_name = "character"
	)
)