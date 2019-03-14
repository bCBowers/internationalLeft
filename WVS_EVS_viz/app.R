# Load packages

library(shiny)
library(ggplot2)

# Define UI

ui <- fluidPage(
  titlePanel("Economic Ideology and Racial Resentment"),
  sidebarLayout(position = "right", 
    sidebarPanel(
      selectInput("country", label = "Country", 
                  choices = levels(W_EVS$country), selected = "United States")),
    mainPanel(
      plotOutput("scatterplot"))))

# Define server

server <- function(input, output) {
  
  filtered <- reactive({
    W_EVS %>% filter(country == input$country)
  })
  
  output$scatterplot <- renderPlot({
    filtered() %>% 
      ggplot(aes(x = ec_ideo, y = rac_ideo)) + geom_point(color = "darkblue") + 
      geom_smooth(method = "lm", mapping = aes(weight = S017), color = "red") + 
      xlab("Economic Ideology (0 = left, 1 = right)") + ylab("Racial Resentment (0 = low, 1 = high)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

runGitHub("internationalLeft", "Jklein29", subdir = "WVS_EVS_viz")
