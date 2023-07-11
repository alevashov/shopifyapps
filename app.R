# Install necessary packages if not already installed
# install.packages(c("shiny", "DT", "dplyr"))

# Load necessary libraries
library(shiny)
library(DT)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Shopify Apps Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categoryInput", "Category:", choices = NULL)
    ),
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read data from CSV file
  data <- reactive({
    df <- read.csv("shopify-apps.csv")
    colnames(df) <- gsub("\\.", " ", colnames(df))  # Replace dots with spaces in column names
    df$`App Name` <- sprintf('<a href="%s">%s</a>', df$`App URL`, df$`App Name`)  # Make URL linked from App name
    df <- df %>%
      select(-c("Short Description", "App URL"))  # Remove Description and URL columns
    df
  })
  
  # Update the choices in the category filter
  observe({
    updateSelectInput(session, "categoryInput", choices = c("All", unique(data()$Category)))
  })
  
  # Render datatable
  output$table <- DT::renderDataTable({
    if (input$categoryInput != "All") {
      df <- data() %>%
        filter(Category == input$categoryInput)
    } else {
      df <- data()
    }
    datatable(df, escape = FALSE, filter = "top", options = list(pageLength = 10))
  }, server = FALSE)  # Set server = FALSE to enable HTML content
}

# Run the application 
shinyApp(ui = ui, server = server)
