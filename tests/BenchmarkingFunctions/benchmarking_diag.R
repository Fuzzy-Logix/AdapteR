#Data Initialisation.Large matrix takes a lot of time to form in FLenv.

Renv = new.env(parent = globalenv())

#Takes lots of time to run.Can change nrow and ncol .
Renv$mat1 = matrix(data = sample(1:200000,1000000,replace = TRUE),nrow = 1000,ncol = 1000,byrow = TRUE)

FLenv = as.FL(Renv)

#Initialisation of variable containing global environment.
env = globalenv()
final = data.frame(description = "",r.Runtime = 0,fl.Runtime =0)

#To run function for n times.
#n = readline(prompt = "Enter number of times you want to run function")
#Default value is set to be 1 i.e. will run for 1 time.
n = 1
for(x in 1:n){
test_that("Benchmarking for diag ",
  {
    result1=eval_expect_equal({test1<-diag(mat1)},Renv,FLenv,check.attributes = FALSE)
    env$final = rbind(final,result1) 
  })
}

#Writes result in csv format in the mentioned file 
write.table(final,"C:/Users/admin/Downloads/Benchmarking_Results/benchmarking_diag.csv",append = TRUE,row.names = FALSE)

#Returns recent benchmarking results.
recent_result = final
print(recent_result)

#Shows all the benchmarking results done .

#ans=readline(prompt = "Do you want to see overall benchmarking results.If yes, type "y" else type "n"")

overall_results = read.table("C:/Users/admin/Downloads/Benchmarking_Results/benchmarking_diag.csv")
print(overall_results)
