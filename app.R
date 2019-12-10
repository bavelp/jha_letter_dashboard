library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# Load data ----

# Load in dataset from .csv.
# Eventually want to integrate with database in local server.
df <-
  read_csv("letter_data.csv") %>%
  # "IDOCdoc_yes_no" does not contain any information
  select(-c("IDOCdoc_yes_no"))

# Create variables ----
# Create function to create vectors from df columns in alphabetical order
# with unique values for dashboard filter dropdowns.
alpha_order_unique <- function(vector) {
  output <- sort(unique(vector))
  return(output)
}

# Create vectors for filter dropdowns.
v.mailed_from <- alpha_order_unique(df$Facility_Mailed_From)
v.about <- alpha_order_unique(df$Facility_About)
v.code <- alpha_order_unique(df$Code)
v.subcode <- alpha_order_unique(df$`Sub-code`)
v.offense <- alpha_order_unique(df$Off_Type)
v.race <- alpha_order_unique(df$Race)
v.gender <- alpha_order_unique(df$Gender)

# Create named vector of all time variables
v.time <- c(
  'Date of Birth' = 'DOB',
  'Admittance Date' = 'adminDate',
  'Projected Release Date' = 'ProjectedReleaseDate',
  'Letter Received' = 'Letter Received',
  'Letter Processed' = 'Letter Processed'
)

# Create named vector of columns that describe letters/writers.
v.char <- c(
  'Facility Mailed From' = 'Facility_Mailed_From',
  'Facility About' = 'Facility_About',
  'Code' = 'Code',
  'Sub-code' = 'Sub-code',
  'Race' = 'Race',
  'Gender' = 'Gender',
  'Offense Type' = 'Off_Type'
)

# Create named vector of demographic columns.
v.demo <- c(
  'Race' = 'Race',
  'Gender' = 'Gender',
  'Offense Type' = 'Off_Type',
  'Facility Mailed From' = 'Facility_Mailed_From',
  'Facility About' = 'Facility_About'
)

`%notin%` <- Negate(`%in%`)


# application UI setup ----
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    titlePanel(
      "Filters"
    ),
    dateRangeInput(
      inputId = "dob",
      label = "Date of Birth Range",
      start = "1930-01-01",
      format = "mm-dd-yyyy"
    ),
    dateRangeInput(
      inputId = "admin_date",
      label = "Admittance Date",
      start = "1930-01-01",
      format = "mm-dd-yyyy"
    ),
    dateRangeInput(
      inputId = "release",
      label = "Projected Release Date",
      start = "2000-01-01",
      format = "mm-dd-yyyy"
    ),
    dateRangeInput(
      inputId = "received",
      label = "Letter Received",
      start = "2010-01-01",
      format = "mm-dd-yyyy"
    ),
    dateRangeInput(
      inputId = "processed",
      label = "Letter Processed",
      start = "2010-01-01",
      format = "mm-dd-yyyy"
    ),
    selectInput(
      inputId = "mailed_from",
      label = "Facility Mailed From",
      choices = v.mailed_from,
      multiple = TRUE
    ),
    selectInput(
      inputId = "about",
      label = "Facility About",
      choices = v.about,
      multiple = TRUE
    ),
    selectInput(
      inputId = "code",
      label = "Code",
      choices = v.code,
      multiple = TRUE
    ),
    selectInput(
      inputId = "subcode",
      label = "Sub-code",
      choices = v.subcode,
      multiple = TRUE
    ),
    selectInput(
      inputId = "offense",
      label = "Offense Type",
      choices = v.offense,
      multiple = TRUE
    ),
    selectInput(
      inputId = "race",
      label = "Race",
      choices = v.race,
      multiple = TRUE
    ),
    selectInput(
      inputId = "gender",
      label = "Gender",
      choices = v.gender,
      multiple = TRUE
    )
  ),
  dashboardBody(
    titlePanel("Prisoner Response Letter Dashboard"),
    fluidRow(
      column(
        width = 6,
        # plotlyOutput("letterDensityPlot")
        DTOutput("letterTable")
      ),
      column(
        width = 6,
        verticalLayout(
          selectInput(
            inputId = "timeDimension",
            label = "Time Dimension",
            choices = v.time,
            multiple = FALSE
          ),
          plotlyOutput("timeDensityPlot")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = "demoDimension",
          label = "Demographic Dimension",
          choices = v.char,
          multiple = FALSE
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        verticalLayout(
          # INCLUDE TABLE OF SUMMARY STATISTICS, SOME OF WHICH TO INCLUDE:
          #   - NUM. LETTERS VS. CORRESPONDING INMATES
          #   - AVG. NUM. LETTERS PER CORRESPONDING INMATES
          #   - 
          # 
          # DENSITY PLOT BY NUM. LETTERS
          plotlyOutput("demoPieChart")
        )
      ),
      column(
        width = 6,
        plotlyOutput("demoBarChart")
      )
    )
  )
)

# application server setup ----
server <- function(input, output) {
  
  filtered_df <- reactive({
    origin = "1970-01-01"
    
    df %>%
      filter(
        # Change POSIX fields to Dates.
        DOB >= as.Date(input$dob[1], origin = origin),
        DOB <= as.Date(input$dob[2], origin = origin),
        adminDate >= as.Date(input$admin_date[1], origin = origin),
        adminDate <= as.Date(input$admin_date[2], origin = origin),
        ProjectedReleaseDate >= as.Date(input$release[1], origin = origin),
        ProjectedReleaseDate <= as.Date(input$release[2], origin = origin),
        `Letter Received` >= as.Date(input$received[1], origin = origin),
        `Letter Received` <= as.Date(input$received[2], origin = origin),
        `Letter Processed` >= as.Date(input$processed[1], origin = origin),
        `Letter Processed` <= as.Date(input$processed[2], origin = origin),
        {if (is.null(input$mailed_from)) TRUE else Facility_Mailed_From %in% input$mailed_from},
        {if (is.null(input$about)) TRUE else Facility_About %in% input$about},
        {if (is.null(input$code)) TRUE else Code %in% input$code},
        {if (is.null(input$subcode)) TRUE else `Sub-code` %in% input$subcode},
        {if (is.null(input$offense)) TRUE else Off_Type %in% input$offense},
        {if (is.null(input$race)) TRUE else Race %in% input$race},
        {if (is.null(input$gender)) TRUE else Gender %in% input$gender}
      )
  })
  
  output$letterTable <- renderDT({
    filtered_df() %>%
      count(DOC_Number, name = "Number of Letters per Correspondent") %>%
      count(`Number of Letters per Correspondent`, name = "Number of Correspondents") %>%
      add_row(
        `Number of Letters per Correspondent` = "Total Correspondents",
        `Number of Correspondents` = sum(.$`Number of Correspondents`),
        .before = 1
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          "searching" = FALSE,
          "scroller" = TRUE
        )
      )
  })
  
  output$timeDensityPlot <- renderPlotly({
    df <- filtered_df()

    # Selecting <<distinct>> inmates when determining demographic data
    if (input$timeDimension %in% c('DOB', 'adminDate', 'ProjectedReleaseDate')) {
      df <-
        df %>%
        distinct(DOC_Number, .keep_all = TRUE)

      title_unit = "Corresponding Inmates"
    } else {
      title_unit = "Letters"
    }

    plot_ly(
      x = ~df[[input$timeDimension]],
      type = 'histogram' #,
      # xbins = list('size' = '12month')
    ) %>%
      layout(
        title = paste("Count of ", title_unit, " by ", input$timeDimension, " over Time")
      )
  })
  
  output$demoPieChart <- renderPlotly({
    # Selecting <<distinct>> inmates when determining demographic data
    df <- filtered_df()
    
    if (input$demoDimension %in% c('v.demo')){
      df <-
        df%>%
        distinct(DOC_Number, .keep_all = TRUE)
      
      title_unit = "Corresponding Inmates"
    } else {
      title_unit = "Letters"
    }
    
    val_table <- table(df[[input$demoDimension]])
    
    plot_ly(
      labels = ~names(val_table),
      values = ~unname(val_table),
      type = 'pie'
    ) %>%
      layout(
        title = paste("Proportion of ", title_unit, " by ", input$demoDimension)
      )
  })

  output$demoBarChart <- renderPlotly({
    # Selecting <<distinct>> inmates when determining demographic data
    df <- filtered_df()

    if (input$demoDimension %in% c('v.demo')){
      df <-
        df%>%
        distinct(DOC_Number, .keep_all = TRUE)

      title_unit = "Corresponding Inmates"
    } else {
      title_unit = "Letters"
    }

    val_table <- table(df[[input$demoDimension]])

    plot_ly(
      x = ~names(val_table),
      y = ~unname(val_table),
      type = 'bar'
    ) %>%
      layout(
        title = paste("Proportion of ", title_unit, " by ", input$demoDimension)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)