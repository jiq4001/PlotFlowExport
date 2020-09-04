
library(DT)
library(magrittr)
library(tidyverse)

server <- function(input, output, session) {
  
  # readin upload csv file
  read.in <- eventReactive(input$file.in, {
    validate(
      need(!is.null(input$file1), "File path is required")
    )
    df <- read.csv(input$file1$datapath) %>%
      janitor::clean_names() %>%
      mutate_if(is.character, as.factor)
    iso_col <- which(unlist(lapply(df, function(x){
      sum(grepl("iso", ignore.case = T, x)) != 0
    })))
    df[grepl("iso", ignore.case = T, df[, iso_col]), ]
  })
  
  # render readin datatable
  output$flowdata <- DT::renderDT({
    read.in()
  })
    
  # update selectcolumn to plot
  # update selectinput filter by col cell count
  observe({
    updateSelectInput(session, "column_2_plot",
                      "Select Columns to Run Batch Plot\nOptional, default to plot all % columns",
                      choices = colnames(read.in())[grepl("percent", 
                                                          ignore.case = T, 
                                                          colnames(read.in()))])
    updateSelectInput(session, "meta_col",
                      "Select Meta Data Columns",
                      choices = colnames(read.in())[!grepl("percent|count", 
                                                           ignore.case = T, 
                                                           colnames(read.in()))])
    updateSelectInput(session, "filt_row",
                      "Select Column of Count for Filtering (optional)", 
                      choices = colnames(read.in())[grepl("count", 
                                                          ignore.case = T, 
                                                          colnames(read.in()))])
  })
  
  # selected column count histogram
  output$filt_row_hist <- renderPlot({
    hist(read.in()[[input$filt_row]], main = input$filt_row)
  })
  
  # filter and gather data for plot
  data_2_plot <- eventReactive(input$filt_data, {
    df <- cbind(
      read.in()[, input$meta_col],
      read.in()[, input$column_2_plot]
    ) 
    df[read.in()[ ,input$filt_row]> input$filt_row_cutoff, ]
  })
  
  # render datatable for plot
  output$data_2_plot <- DT::renderDT({
    data_2_plot()
  })
 
  # output summary of data for plot 
  observe({
    output$summary <- renderPrint({
      summary(data_2_plot())
    })
  })

  # tabe 2
  # re level timepoint level
  observe({
    updateSelectInput(session, "group","Metacolumn to Color with (eg. Patient_ID):",
                      choices = colnames(data_2_plot()))
    
    updateSelectInput(session, "facet","Metacolumn to Seprate Panel With (optional, eg.Treatment Group)",
                choices = colnames(data_2_plot()))
    
    updateSelectInput(session, "timepoint","Select Timepoint Column",
                choices = colnames(data_2_plot()))
  })
  
  output$tplevel <- renderUI({
    req(input$timepoint)
    tp <- unique(data_2_plot()[, input$timepoint])
    selectInput("re_level_tp","Rearrange Timepoints Order",
                          choices = tp, multiple = T)
  })
  
  
  # relevel timepoint update data for plot
  data_2_plot_1 <- eventReactive(input$update_tp, {
    req(input$re_level_tp, input$timepoint, data_2_plot())
    temp <- data_2_plot()
    temp[ ,input$timepoint] <- factor(temp[ ,input$timepoint], 
                                               levels = input$re_level_tp)
    temp
  })
  
  #output$data_2_plot_u <- DT::renderDT({
  #  data_2_plot_1()
  #})
  
  #https://gist.github.com/wch/5436415 https://stackoverflow.com/questions/36799901/r-markdown-shiny-renderplot-list-of-plots-from-lapply
  # update if normalized by baseline
  # generate plot
  
  renderUI({
    req(input$column_2_plot)
    plot_list <- lapply(1 : length(input$column_2_plot), function(x){
      plot_name <- paste("plot", x, sep = "")
      plotOutput(plot_name, width = "70%", height = "400px")
    })
    do.call(tagList, plot_list)
  })
  
# plot_1 <- reactive({
#   req(data_2_plot_1())
#   lapply(input$column_2_plot, function(x){
#     data_2_plot_1()[, c(input$meta_col, x)] 
#     
      #    if(input$plot_tpye == "Dot"){
      #      plot_1 <- plot_1+
      #        geom_point()
      #    }
      #    else if(input$plot_tpye == "Box"){
      #      plot_1 <- plot_1+
      #        geom_boxplot()
      #    }
      #    else {
      #      plot_1 <- plot_1+
      #        geom_violin()
      #    }
      #    
      #    if(!is.null(input$facet)){
      #      plot_1 <- plot_1+
      #        facet_wrap(. ~ !!as.symbol(input$facet))
      #    }
#    })
#  })
  
  
  observe({
    req(data_2_plot_1())
    for(i in 1:length(input$column_2_plot)){
      local({
        mi <- i
        plot_name <- paste("plot", mi, sep = "")
        output[[plot_name]] <- renderPlot({
          p <- data_2_plot_1()%>%
            ggplot(aes_string(input$timepoint, input$column_2_plot[mi], 
                       color = input$group))+
            geom_point()
          print(p)
        })
      })
    }
  })
  
}