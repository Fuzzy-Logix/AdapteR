function (input, output, session) {
          counter <- reactiveValues()
          counter$m <- 0
          counter$n <- 0
          output$woman <- renderUI({
            if(counter$m %% 2 == 0){
                table <- NULL
                model <- NULL
                output$Dependent <- renderUI({})
                output$independentVariables <- renderUI({})
                output$man <- renderUI({})
                output$d <- renderPlot({})
                output$a <- renderText({})
                output$FetchCoefficients <- renderUI({})
                output$FetchFittedValues<- renderUI({})
                output$FetchSummary<- renderUI({})
                output$FetchStats<- renderUI({})
                output$c <- renderText({})
                output$b <- renderText({})
                output$k <- renderText({})
                output$plot1 <- renderPlot({})
                output$Plot <- renderUI({})
                output$well2 <- renderUI({})
                actionButton("tblGenerator1", "Create Table")

           } else {  
             actionButton("tblGenerator2", "Start again with new table")
            }
          })
          output$well4 <- renderUI({
            if(counter$m %% 2 != 0){
              
             wellPanel(              
                actionButton("head", "Table - Head")
              )
            }
          })
          observeEvent(input$tblGenerator2,{
            counter$n <- 0
            counter$m <- counter$m + 1
            })          
          table <- eventReactive(input$tblGenerator1,{
            counter$m <- counter$m + 1
            vSampleDataTables <- suppressWarnings(SampleData(pTableName=input$tbl,
                                                 pObsIDColumn=input$id,
                                                 pTrainTableName="ARtblTrain",
                                                 pTestTableName="ARtblTest",
                                                 pTrainDataRatio=input$SplitRatio,
                                                 pTemporary=FALSE,
                                                 pDrop=TRUE))
            if(input$SplitRatio == 1){
              FLTable( getTestTableName(input$tbl), input$id)
            }else {
              FLTable( "ARtblTrain", input$id)
            }
            
            })
          observeEvent(input$head, {
            output$b <- renderDataTable({
              as.R(head(table()))
              })
            })
          observeEvent(input$tblGenerator1,{
          output$well2 <- renderUI({
            v <- names(table())
            if(counter$n %% 2 == 0){
              model <- NULL
              output$d <- renderPlot({})
              output$a <- renderText({})
              output$c <- renderText({})
              
              output$plot1 <- renderPlot({})
              output$Plot <- renderUI({})
              wellPanel(
                selectInput("Dep", "Dependent Variable: ", v, selected= v[[3]])     
                )
              }
            })
          
          output$well3 <- renderUI({ 
              v <- names(table())
              if(counter$n %% 2 == 0){
                model <- NULL
                output$d <- renderPlot({})
                output$a <- renderText({})
                output$c <- renderText({})
                output$b <- renderText({})
                output$plot1 <- renderPlot({})
                output$Plot <- renderUI({})
                wellPanel( 
                  checkboxGroupInput("variable", "Independent Variables:", v[!v %in% input$Dep]),
                  actionButton("do1", "Create Model")
                  )
                
              } else {  
                wellPanel(
                  actionButton("do2", "Start again with same table")
                  )
                
              }
            })
          })
            
                   
          observeEvent(input$do2, {
            counter$n <- counter$n + 1
            })
          formula <- eventReactive(input$do1,{
            if(length(input$variable) == 0){
                  formula <- NULL
                }
                else if(length(input$variable) == 1){
                  formula <- paste(input$Dep, "~", input$variable[1], sep = " ")
                }
                else{
                  formula <- paste(input$Dep, "~", input$variable[1], sep = " ")
                  if(length(input$variable) > 2){
                    for(i in 2:(length(input$variable) - 1)){
                      formula <- paste(formula, input$variable[i], sep = "+")
                    }
                  }
                  paste(formula, input$variable[length(input$variable)], sep = "+")
                }
            })
          model <- eventReactive(input$do1, {  
            counter$n <- counter$n + 1  
              withProgress(message = 'Calculation in progress',
                             value = 0, {
                lm(as.formula(formula()), table())
            })
              
          })
          observeEvent(input$do1,{
                output$a <- renderText({
                  "The model has been created."
                })
                output$FetchCoefficients <- renderUI({
                  actionButton("coeffs", "Fetch Coefficients")
                })
                output$FetchFittedValues<- renderUI({
                  actionButton("fittedvalues", "Fetch Fitted Values")
                })
                output$FetchSummary<- renderUI({
                  actionButton("summar", "Fetch Summary")
                })
                output$FetchStats<- renderUI({
                  actionButton("stats", " Fetch Stats ")
                  })
                v <- c("Residuals vs fitted values",
                       "Normal Q-Q plot",
                       "Scale location of residuals vs fitted values",
                       "Cook's distances vs row lables",
                       "Residuals against leverages",
                       "Cook's distance against leverages")
                
                output$Plot<- renderUI({
                      tagList(
                      selectInput('plot_lab', 'Type of Plot', v,
                                  selected= "")
                      )
                })
                print(model()$s)
            })    
          observeEvent(input$coeffs, {
                withProgress(message = 'Calculation in progress',
                             value = 0, {
                             coefficients <- as.data.frame(model()$coefficients)
                             coefficients[,2] <- coefficients[,1]
                             coefficients[,1] <- rownames(coefficients)
                             colnames(coefficients) <- c("Variable", "Coefficient")
                })
                output$d <- renderPlot({})
                output$b <- renderDataTable({
                  coefficients
                })
                output$c <- renderText({})
            
          })
          
          observeEvent(input$fittedvalues, {
                withProgress(message = 'Calculation in progress',
                             value = 0, {
                             fitvalues <- as.R(model()$fitted.values)
                })
                output$d <- renderPlot({
                  withProgress(message = 'Calculation in progress',
                               value = 0, {
                    tt <- plot(x = as.R(table()[,input$Dep]), y = fitvalues, 
                               ylab = "Fitted Values",
                               xlab = "Dependent Variable")  
                    
                  })
                }, height= 400, width= 600)
                output$c <- renderText({})
                output$b <- renderDataTable({})
            
          })
          
          observeEvent(input$summar, {
                withProgress(message = 'Calculation in progress',
                             value = 0, {
                             summ <- summary(model())
                             shit <- as.data.frame(summ$coefficients)
                             shit <- cbind(shit, shit[,1])
                             shit[,1] <- rownames(shit)
                             colnames(shit) <- c("Variable", "Coefficient")
                })       
                output$b <- renderDataTable({
                  shit
                })
                output$c <- renderPrint({
                  cat(noquote(paste("Formula:", formula(), sep= " ")), "\n")
                })
                output$d <- renderPlot({})
          })
          observeEvent(input$stats, {
                withProgress(message = 'Calculation in progress',
                             value = 0, {
                             stt <- model()$FLLinRegrStats
                             stt <- stt[,4:21]
                             stt <- t(stt)
                             stt <- cbind(stt, stt[,1])
                             stt[,1] <- rownames(stt)
                             colnames(stt) <- c("Stats", "Value")
                })
                output$d <- renderPlot({})
                output$b <- renderDataTable({
                  stt
                })
                output$c <- renderText({})
            
          })
          
          
          observeEvent(input$plot_lab, {
                output$b <- renderDataTable({})
                output$c <- renderText({})
                output$d <- renderPlot({})
                v <- c("Residuals vs fitted values",
                       "Normal Q-Q plot",
                       "Scale location of residuals vs fitted values",
                       "Cook's distances vs row lables",
                       "Residuals against leverages",
                       "Cook's distance against leverages")
                output$b <- renderPrint({})
                output$plot1 <- renderPlot({
                  withProgress(message = 'Calculation in progress',
                               value = 0, {
                    plot(model(), which= c(match(input$plot_lab,v)))
                  })
                }, height= 400, width= 600)
          })
                         
}