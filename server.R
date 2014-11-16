library(shiny)
##data preparation
#hunger <<- read.csv("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv")
#hunger <<-hunger[hunger$SEX!="BTSX",]
#countrylist <<- sort(unique(hunger$COUNTRY))
#countrylist <<- as.character(countrylist)
#countrylist <<-c("ALL",countrylist)
#countrylist <<- as.factor(countrylist)
lmB <- lm(Numeric~YEAR, data=hunger)
lmM <- lm(Numeric[SEX=="MLE"]~YEAR[SEX=="MLE"], data=hunger)
lmF <- lm(Numeric[SEX=="FMLE"]~YEAR[SEX=="FMLE"], data=hunger)
lmC <- lm(Numeric[COUNTRY=="USA"]~YEAR[COUNTRY=="USA"], data=hunger)


shinyServer(
  function(input,output)
  {
    #groupflag <- reactive(as.character(input$checkGroup))
    #countryflag <- reactive(as.character(input$country))
    
    output$debug <- renderText(
      {
      #paste(countryflag)
       paste("Selected Analysis Group 1: ",input$checkGroup[1], "Selected Analysis Group 2: ",input$checkGroup[2],"Selected Analysis Group 3: ",input$checkGroup[3]," / Selected Country for Analysis: ", input$country,hunger$Numeric[1])
      })
    
    #output$table<- renderTable(hunger)
    output$table <-renderDataTable(hunger,  options = list(pageLength = 5,initComplete = I("function(settings, json) {alert('Done.');}")))
    

    output$plot <- renderPlot (
      {
        plot(hunger$YEAR[hunger$SEX=="MLE"], hunger$Numeric[hunger$SEX=="MLE"], pch=19, col= "blue", xlab="Calendar Year", ylab="% Hungry Children", col.lab="brown")
        points(hunger$YEAR[hunger$SEX=="FMLE"], hunger$Numeric[hunger$SEX=="FMLE"], pch=19, col= "magenta")
        title(main="World Child Hunger Statistics from WHO", col.main ="red")
       legend("topright", c("Female", "Male","ALL", "By Country"), fill=c("magenta","blue","darkgrey", "brown"))
       if ("Male" %in% input$checkGroup)#((input$checkGroup[1] =="Male" ) | (input$checkGroup[2]=="Male") | (input$checkGroup[3] =="Male"))
           {lines(hunger$YEAR[hunger$SEX=="MLE"], lmM$fitted, lwd =1, col = "blue")}
                
          
        if ("Female" %in% input$checkGroup) #(input$checkGroup[1]=="Female" | input$checkGroup[2]=="Female" | input$checkGroup[3]=="Female")
            {lines(hunger$YEAR[hunger$SEX=="FMLE"], lmF$fitted, lwd =1, col = "magenta")}
       
        if ("ALL" %in% input$checkGroup)#(input$checkGroup[1]=="ALL" | input$checkGroup[2]=="ALL" | input$checkGroup[3]=="ALL")
            {lines(hunger$YEAR, lmB$fitted, lwd= 1, col="darkgrey")}
        
        if (!input$country == "ALL")
          {
            lmC <- lm(Numeric[COUNTRY==input$country]~YEAR[COUNTRY==input$country], data=hunger)
            lines(hunger$YEAR[hunger$COUNTRY==input$country],lmC$fitted, lwd = 4, col = "brown")}
        else
        {
          lines(hunger$YEAR, lmB$fitted, lwd= 1, col="darkgrey")
        }
      
      },height = 400, width = 800
       )
    
  }
       )  
  