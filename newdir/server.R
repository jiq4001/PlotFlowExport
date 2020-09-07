
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
    req(input$filt_row)
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
                      choices = input$meta_col)
    
    updateSelectInput(session, "facet","Metacolumn to Seprate Panel With (optional, eg.Treatment Group)",
                choices = c("", input$meta_col))
    
    updateSelectInput(session, "timepoint","Select Timepoint Column",
                choices = input$meta_col)
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
  
  #https://gist.github.com/wch/5436415 https://stackoverflow.com/questions/36799901/r-markdown-shiny-renderplot-list-of-plots-from-lapply
  # update if normalized by baseline
  # generate plot
  
  output$plot <- renderUI({
    req(input$column_2_plot, data_2_plot_1())
    
    plot_list <- lapply(1 : length(input$column_2_plot), function(x){
      plot_name <- paste("plot", x, sep = "")
      plotOutput(plot_name, width = 700, height = 500)
    })
    do.call(tagList, plot_list)
  })
  
  plot_opt <- reactive({
    list(input$plot_tpye,
         input$facet,
         input$group)
  })
  
  observeEvent(plot_opt(), {
    req(data_2_plot_1())
    
    df <- data_2_plot_1()
    
    if(!is.null(input$facet)){
      df_mean <- aggregate(df[, input$column_2_plot],
                list(df[, input$timepoint],
                     df[, input$group],
                     df[, input$facet]),
                mean)
      colnames(df_mean)[1:3] <- c(input$timepoint, input$group, input$facet)
    }
    else {
      df_mean <- aggregate(df[, input$column_2_plot],
                           list(df[, input$timepoint],
                                df[, input$group]),
                           mean)
      colnames(df_mean)[1:2] <- c(input$timepoint, input$group)
    }
   
    for(i in 1:length(input$column_2_plot)){
      local({
        plot_name <- paste("plot", i, sep = "")
        yvar <- input$column_2_plot[i]
        
        output[[plot_name]] <- renderPlot({
          p <- df %>%
            ggplot(aes_string(x = input$timepoint, y = yvar, color = input$group))+
            labs(title = yvar)
          
          if(input$plot_tpye == "dot"){
              p <- p+
                geom_point()
            }
          if(input$plot_tpye == "box"){
            p <- p+
              geom_boxplot()
          }
          if(input$plot_tpye == "line") {
            p <- df_mean %>%
              ggplot(aes_string(x = input$timepoint, y = yvar, color = input$group))+
              labs(title = yvar)+
              geom_point()+
              geom_line(aes_string(group = df_mean[, input$group]))
          }
          if(!is.null(input$facet)){
            p <- p+
              facet_wrap(get(input$facet) ~ ., scales = "free_y")+
              theme_classic()+
              theme(plot.title = element_text(hjust = 0.5, size = 12))
          }
          else{
            p <- p+
              theme_classic()+
              theme(plot.title = element_text(hjust = 0.5, size = 12))
          }
          print(p)
        })
      })
    }
  })
  
}