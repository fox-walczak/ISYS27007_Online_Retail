# From https://youtu.be/jxsKUxkiaLI?si=bumTItsDcequcWRV
load_pkg <- function(package_name) {
  if (!require(package_name, character.only=TRUE)) {
    install.packages(package_name,
                     dependencies=TRUE)
  }
  else {
    update.packages(ask=FALSE,
                    checkBuilt=TRUE)
  }
  library(package_name, character.only=TRUE)
}
load_packages <- function(package_names) {
  for(package_name in package_names) {
    load_pkg(package_name)
  }
}
# LIBRARIES

load_packages(c(
	"shiny", "bslib",            # Shiny
	"rsconnect",                 # Shinyapps.io
	"modeldata", "DataExplorer", # Modeling
	"plotly",                    # Widgets
	"tidyverse"                  # Core
))

# LOAD DATASETS

#online_retail_file_path <- "C:\\Users\\artyp\\OneDrive - Nottingham Trent University\\Uni Vault\\ISYS27007_Business_Intelligence_&_Solutions\\_assets\\OnlineRetail.csv"
#online_retail <- read_csv(online_retail_file_path)
#online_retail$date <- as.Date(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
#online_retail$date_time <- as_datetime(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")

# Filter by price less than or greater than 0

utils::data("stackoverflow", "car_prices", "Sacramento", package="modeldata")

data_list = list(
  "StackOverflow"      = stackoverflow,
  "Car Prices"         = car_prices,
  "Sacramento Housing" = Sacramento
)

# UI

ui <- navbarPage(
  title = "Bundle and Promote",
  theme = bslib::bs_theme(version=4,bootswatch="minty"),
  tabPanel(
    title = "Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h1("Explore a Dataset"),
        shiny::selectInput(
          inputId = "dataset_choice",
          label   = "Data Connection",
          choices = c("StackOverflow", "Car Prices", "Sacramento Housing")
        ),
        hr(),
        h3("Apps by Business Science"),
        p("Go from beginner to building full-stack shiny apps."),
        p("Learn Shiny Today!") %>%
          a(
            href   = 'https://www.business-science.io/',
            target = "_blank",
            class  = "btn btn-lg btn-primary"
          ) %>%
          div(class="text-center")
      ),
      mainPanel(
        h1("Correlation"),
        plotlyOutput("corrplot",height=700)
      )
    )
  )
)

# SERVER

server <- function(input, output) {
  rv <- reactiveValues()
  observe({
    rv$data_set <- data_list %>% pluck(input$dataset_choice)
  })
  output$corrplot <- renderPlotly({
    g <- DataExplorer::plot_correlation(rv$data_set)
    plotly::ggplotly(g)
  })
}

# RUN

#rsconnect::deployApp(appName="Online_Retail_Bulk_and_Promote_Dashboard", appTitle="Online Retail Bulk and Promote Dashboard")
shinyApp(ui=ui, server=server)
#shiny::runApp()