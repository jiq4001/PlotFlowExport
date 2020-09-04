library(DT)
library(shinydashboard)


sidebar <- dashboardSidebar(
  sidebarMenu(
    #1
    menuItem("UploadData", tabName = "uploaddata"),
    #2
    menuItem("Plot Preview", tabName = "plotpreview")
  )
)

#############################################
body <- dashboardBody(
  tabItems(
    #1
    tabItem(
      tabName = "uploaddata",
      
      fileInput("file1", "Choose CSV File",
                multiple = F,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      actionButton("file.in", "Read in Data"),
      
      # output input data
      tags$hr(), 
      DT::dataTableOutput("flowdata"), 
      style = "height:500px; overflow-y: scroll; overflow-x: scroll",
      
      # select plotting parameters 
      tags$hr(),
      selectInput("column_2_plot","Select Columns to Run Batch Plot\nOptional, default to plot all % columns",
                  choices = NULL, multiple = TRUE),
      selectInput("meta_col","Select Meta Data Columns Such as Timepoint, or Sample Type",
                  choices = NULL, multiple = TRUE),
      
      selectInput("filt_row","Select Column of Count for Filtering (optional)",
                 choices = NULL, multiple = F),
      plotOutput(outputId = "filt_row_hist"),
      numericInput("filt_row_cutoff", "Low Cell Count Cutoff\nOptional Filterer, 0 means no filtering", 0),
  
      actionButton("filt_data", "Confirm Filtering Data For Plot"),
      
      # output filtered data for plot 
      tags$hr(),
      DT::dataTableOutput("data_2_plot"), 
      style = "height:500px; overflow-y: scroll; overflow-x: scroll",
      
      # summary data for plot
      tags$hr(),
      verbatimTextOutput("summary")
      ),
    
    #2
    tabItem(
      tabName = "plotpreview",
      
      selectInput("group","Metacolumn to Color with (eg. Patient_ID):",
                  choices = NULL, multiple = F),
      selectInput("facet","Metacolumn to Seprate Panel With (optional, eg.Treatment Group)",
                  choices = NULL, multiple = F, selectize = F, selected = F, size = 4),
      selectInput("timepoint","Select Timepoint Column",
                  choices = NULL, multiple = F),

      uiOutput('tplevel'), # selectinput depends on observed selectinput

      actionButton("update_tp", "Update Timepoint Sequence"),
      
      tags$hr(),
      #actionButton("norm_bl", "(Optional)Normalize by Baseline Value\nPlot log(timepoint/baseline)")
      radioButtons('plot_tpye', 'Plot Tpye', 
                   c('Dot', 'Box', 'Violine'),
                   inline = TRUE),
      
      tags$hr(),
      #DT::dataTableOutput("data_2_plot_u")
      plotOutput('plot')
    )
  )
)
##################################################
ui <- dashboardPage(
  dashboardHeader(
    title = "Plot Flow Data"
  ),
  
  sidebar,
  
  body
)