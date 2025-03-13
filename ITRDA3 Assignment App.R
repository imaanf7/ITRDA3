#-------------------------------------------------------------------------------------------------------------------------------
# Question 3
#-------------------------------------------------------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)

ui <- dashboardPage( skin = "green",
  dashboardHeader(title = "Eduvos IT Graduate Survey Analysis", titleWidth = 370),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Tools", tabName = "top_tools", icon = icon("book")),
      menuItem("Industry", tabName = "industry", icon = icon("building")),
      menuItem("Job Roles", tabName = "roles", icon = icon("person")),
      menuItem("Employment", tabName = "employment", icon = icon("dollar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("top_tools",
              fluidRow(
                box(plotOutput("programming_languages", height = "750px"), width = 10),
                box(plotOutput("databases", height = "650px"), width = 10),
                box(plotOutput("platform"), width = 10),
                box(plotOutput("web_frameworks", height = "650px"), width = 12),
                box(plotOutput("aisearch"), width = 12),
                box(plotOutput("aitools"), width = 12)
              )
      ),
      tabItem("industry",
              fluidPage(
                box(plotOutput("industry", height = "750px"), width = 30)
              )
      ),
      tabItem("roles",
              fluidPage(
                box(plotOutput("roles", height = "750px"), width = 12)
              )
      ),
      tabItem("employment",
              fluidPage(
                box(plotOutput("employment"), width = 12)
              )
      )
    ),
    
   
  )
)

server <- function(input, output){
  output$programming_languages <- renderPlot({
    plot1
  })
  
  output$databases <- renderPlot({
    plot2  
  })
  
  output$platform <- renderPlot({
    plot3  
  })
  
  output$web_frameworks <- renderPlot({
    plot4
  })
  
  output$aisearch <- renderPlot({
    plot5  
  })
  
  output$aitools <- renderPlot({
    plot6  
  })
  
  output$industry <- renderPlot({
    plot7
  })
  
  output$roles <- renderPlot({
    plot8  
  })
  
  output$employment <- renderPlot({
    plot9
  })
}

shinyApp(ui, server)
