library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

# Load data ----

# load in dataset
# (currently via csv, want to integrate with database in local server)
df <-
  read_csv("letter_data.csv") %>%
  # remove unnecessary field "IDOCdoc_yes_no"
  select(-c("IDOCdoc_yes_no"))

# vectors of categorical variables ----

# create function to organize df vectors into alphabetical order
# with unique values
alpha_order_unique <- function(vector) {
  output <- sort(unique(vector))
  return(output)
}

# create vectors of alphabetical, unique values for filters
v.mailed_from <- alpha_order_unique(df$Facility_Mailed_From)
v.about <- alpha_order_unique(df$Facility_About)
v.code <- alpha_order_unique(df$Code)
v.subcode <- alpha_order_unique(df$`Sub-code`)
v.offense <- alpha_order_unique(df$Off_Type)
v.race <- alpha_order_unique(df$Race)
v.gender <- alpha_order_unique(df$Gender)

# create named vector of time variables
v.time <- c(
  'Date of Birth' = 'DOB',
  'Admittance Date' = 'adminDate',
  'Projected Release Date' = 'ProjectedReleaseDate',
  'Letter Received' = 'Letter Received',
  'Letter Processed' = 'Letter Processed'
)

# create named vector of characteristics associated with letters
v.char <- c(
  'Facility Mailed From' = 'Facility_Mailed_From',
  'Facility About' = 'Facility_About',
  'Code' = 'Code',
  'Sub-code' = 'Sub-code',
  'Race' = 'Race',
  'Gender' = 'Gender',
  'Offense Type' = 'Off_Type'
)

# create named vector of demographic info options
v.demo <- c(
  'Race' = 'Race',
  'Gender' = 'Gender',
  'Offense Type' = 'Off_Type',
  'Facility Mailed From' = 'Facility_Mailed_From',
  'Facility About' = 'Facility_About'
)

# create a negated version of `%in%`
`%notin%` <- Negate(`%in%`)

# application UI setup ----
ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  
  titlePanel("Prisoner Response Letter Dashboard"),
  
  fluidRow(
    column(
      width = 3,
      verticalLayout(
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
      )
    ),
    column(
      width = 9,
      verticalLayout(
        # INCLUDE TABLE OF SUMMARY STATISTICS, SOME OF WHICH TO INCLUDE:
        #   - NUM. LETTERS VS. CORRESPONDING INMATES
        #   - AVG. NUM. LETTERS PER CORRESPONDING INMATES
        #   - 
        # 
        # DENSITY PLOT BY NUM. LETTERS
        plotlyOutput("letterDensityPlot"),
        selectInput(
          inputId = "timeDimension",
          label = "Time Dimension",
          choices = v.time,
          multiple = FALSE
        ),
        plotlyOutput("timeDensityPlot"),
        selectInput(
          inputId = "demoDimension",
          label = "Demographic Dimension",
          choices = v.char,
          multiple = FALSE
        ),
        plotlyOutput("demoPieChart"),
        plotlyOutput("demoBarChart")
      )
    )
  ),
  fluidRow()
)

server <- function(input, output) {
  
  filtered_df <- reactive({
    df %>%
      filter(
        DOB >= as.Date(input$dob[1], origin = "1970-01-01"),
        DOB <= as.Date(input$dob[2], origin = "1970-01-01"),
        adminDate >= as.Date(input$admin_date[1], origin = "1970-01-01"),
        adminDate <= as.Date(input$admin_date[2], origin = "1970-01-01"),
        ProjectedReleaseDate >= as.Date(input$release[1], origin = "1970-01-01"),
        ProjectedReleaseDate <= as.Date(input$release[2], origin = "1970-01-01"),
        `Letter Received` >= as.Date(input$received[1], origin = "1970-01-01"),
        `Letter Received` <= as.Date(input$received[2], origin = "1970-01-01"),
        `Letter Processed` >= as.Date(input$processed[1], origin = "1970-01-01"),
        `Letter Processed` <= as.Date(input$processed[2], origin = "1970-01-01"),
        {if (is.null(input$mailed_from)) TRUE else Facility_Mailed_From %in% input$mailed_from},
        {if (is.null(input$about)) TRUE else Facility_About %in% input$about},
        {if (is.null(input$code)) TRUE else Code %in% input$code},
        {if (is.null(input$subcode)) TRUE else `Sub-code` %in% input$subcode},
        {if (is.null(input$offense)) TRUE else Off_Type %in% input$offense},
        {if (is.null(input$race)) TRUE else Race %in% input$race},
        {if (is.null(input$gender)) TRUE else Gender %in% input$gender}
      )
  })
  
  output$letterDensityPlot <- renderPlotly({
    df <-
      filtered_df() %>%
      count(DOC_Number)
    
    plot_ly(
      x = ~df$n,
      type = 'histogram',
      xbins = list('size' = 1)
    ) %>%
      layout(
        title = "Number of Corresponding Inmates by Letters Sent in Given Dataset",
        xaxis = list(title = "Number of Letters Sent per Inmate"),
        yaxis = list(title = "Number of Corresponding Inmates")
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