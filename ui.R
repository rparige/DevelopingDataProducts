library(shiny)
##data preparation
hunger <<- read.csv("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.csv")
hunger <<-hunger[hunger$SEX!="BTSX",]
countrylist <<- sort(unique(hunger$COUNTRY))
countrylist <<- as.character(countrylist)
countrylist <<-c("ALL",countrylist)

#countrylist <<- as.factor(countrylist)
shinyUI(fluidPage(
  titlePanel("World Child Hunger [Under the age of 5] - Regression Analysis"),
 sidebarLayout(
  sidebarPanel
    (
      # we are going to select what to plot and what country to add)
      checkboxGroupInput("checkGroup", 
                         label = h4("Linear Regression Model - World Wide"), 
                         choices = list("Female Children Only" = "Female", 
                                        "Male Children Only" = "Male", "All Children" = "ALL"), selected = "ALL"),
      selectInput("country", label=h4("Linear Regression Model - Country Specific"), countrylist, selected = countrylist[1])
    ),
  
  mainPanel
    (
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot"),width = "100%"),
      tabPanel("Table", dataTableOutput("table")),
      tabPanel("About",includeMarkdown("about.md"))
    )
  )
 )
))