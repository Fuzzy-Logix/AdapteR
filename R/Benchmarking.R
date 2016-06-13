FL_benchmarking_generic<-function(specs=list(list(n=5,isSquare = TRUE,...),list(n =5,isRowVec = FALSE,...)),
                          classes = c("FLMatrix","FLVector"),operator = "+"){
    
  FLenv<-new.env()
  #browser()
  lapply(1:length(classes),function(i){
    obj<-initFgeneric(specs[[i]],classes[i])
    x=i
    assign(paste0("a",x),obj,envir = FLenv)
  })
  Renv<-as.Renvironment(FLenv)
  x = Sys.time()
  obj1<-do.call(operator,lapply(ls(FLenv),function(x)do.call("$",list(FLenv,paste0(x)))))
  y= Sys.time()
  obj2<-do.call(operator,lapply(ls(Renv),function(x)do.call("$",list(Renv,paste0(x)))))
  z = Sys.time()
  print(paste0("Result of FL operation is ",obj1))
  print(paste0("Result of R operation is",obj2))
  print(paste0("Time taken for FL operation is ",y-x))
  print(paste0("Time taken for FL operation is ",z-y))
  print(paste0("Size of FL object is ",object.size(obj1)))
  print(paste0("Size of R object is ",object.size(obj2)))
}