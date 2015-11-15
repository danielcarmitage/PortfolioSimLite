library(curl)
library(plyr)
library(data.table)
library(reshape)
library(ggplot2)
library(scales)
library(devtools)
library(downloader)
library(readxl)
library(Quandl) #https://www.quandl.com/tools/r
library(grid)

source("helpers.R")

###API key for Quandl
quandl_key <<- "GFHcsSjXc_LFxoW_xobL"

###Quandl API authorization
Quandl.auth(quandl_key)

##################### SHINY #######################
shinyServer(
  function(input, output, session) {    

    ############# PORTFOLIO SIM TAB ###################
    
    ################## BUILDER ########################
    ###Selectize input for tickers
    output$builder_symbol_select <- renderUI({
      withProgress(value = 0, message = "Loading...", {
        builder_list <- quandl_equity_list()
        selectInput("builder_symbol_select", "Select Stock:", choices = setNames(paste(builder_list$quandl.code, sep = ""), paste(builder_list$name, " (", builder_list$ticker, ")", sep = "")), selected = "WIKI/TSLA", width = "100%")
      })
    })  

    ###Get Data button
    observe({
      if(input$builder_get_symbols > 0){
        withProgress(value = 1, message = "Pulling Data...", {
            
          builder_list <- quandl_equity_list()
          
          if(exists("builder_data") & exists("builder_symbols")){
            qname <- builder_list[quandl.code == isolate(input$builder_symbol_select), name]
            validate(need(!(qname %in% builder_data$name), "This data has already been pulled"))
          }
          
          quandl_data <- quandl_equity_download(isolate(input$builder_symbol_select),builder_list[quandl.code == isolate(as.character(input$builder_symbol_select)), name])
          builder_simulate(quandl_data, isolate(input$builder_date_range[1]), isolate(input$builder_date_range[2]),isolate(input$builder_position_size))
          
        })
      }  
    })

    ###Clear All button
    observe({
      if(input$builder_clear_all > 0){
        if(exists("builder_data")){
          rm(builder_data, envir = .GlobalEnv)
          rm(builder_symbols, envir = .GlobalEnv)
        }
      }
    })

    ###Clear Selected button
    observe({
      if(input$builder_clear_selected > 0){
        if(exists("builder_data")){
          if(length(unique(builder_data$name)) == 1){
            rm(builder_data, envir = .GlobalEnv)
            rm(builder_symbols, envir = .GlobalEnv)
          } else{
            builder_data <<- builder_data[!(name %in% isolate(input$builder_asset_list))]
            builder_symbols <<- unique(builder_data$name)  
          }
        }
      }
    })
    
    ###List of assets
    output$builder_asset_list <- renderUI({
      input$builder_get_symbols
      input$builder_clear_selected
      input$builder_clear_all
      
      if(exists("builder_symbols")){
        checkboxGroupInput("builder_asset_list", "Select Assets to Display", choices = builder_symbols, selected = builder_symbols)
      }
    })

    ###Renders the Simulator Plot
    output$builder_plot <- renderPlot({
      input$builder_update
      input$builder_get_symbols
      input$builder_clear_selected
      input$builder_clear_all
      
      shiny::validate(
        need(exists("builder_data"), message = "\n The Portfolio Simulator tracks the performance of equities for a user-defined date range and position size. The data is pulled from the Quandl Stock Wiki. \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Input position size \n 4. Press 'Get Data' \n 5. Enjoy!"))      

      graph_data <- builder_data
      
      asset_options <- input$builder_asset_list
      
      graph_data <- graph_data[name %in% asset_options]
      
      if(input$builder_daily_or_acc == F){
        setnames(graph_data, "acc_notional", "notional")
      } else{
        setnames(graph_data, "daily_notional", "notional")
      }
      
      if(input$builder_cluster == T){
        graph_data <- graph_data[, list(notional = sum(notional)), by =.(date)]
        graph_data$name <- rep("Clustered Data", nrow(graph_data))
      }
      
      if(input$builder_daily_or_acc == F){
        ggplot(graph_data, aes(x=date,y=notional, group=name,colour=name)) + geom_line() +  
          theme_minimal() + 
          scale_y_continuous(labels = comma) +
          theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 
      } else if (input$builder_daily_or_acc == T) {
        ggplot(graph_data, aes(x=date,y=notional, group=name,colour=name)) + 
          geom_bar(stat = "identity", position = position_dodge()) +  
          scale_y_continuous(labels = comma) +
          theme_minimal() + 
          theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 
      }
    })
    
    ###Renders the hover data for the Simulator Plot
    output$builder_hover_info <- renderText({
      if(!is.null(input$builder_hover)){
        shiny::validate(need(exists("builder_data"), ""))
        paste("$ ",round(input$builder_hover$y, 2), "on ", as.Date(input$builder_hover$x))
      }
    })

    ###Renders the Portfolio Composition graph
    output$builder_pie <- renderChart2({
      input$builder_get_symbols
      input$builder_clear_selected
      input$builder_clear_all
      
      shiny::validate(need(exists("builder_data"), ""))
      
      if(input$builder_portfolio_composition == T){
        graph_data <- builder_data[builder_data[, .I[date == max(date)], by=name]$V1]
        graph_data$position_value <- graph_data$close*graph_data$position_size
        
        n3 = nPlot(x = "name", y = "position_value", data = graph_data, type = "pieChart")
        n3
      } else {
        graph_data <- builder_data[0,]
        n3 = nPlot(x = "name", y = "position_value", data = graph_data, type = "pieChart")
        n3
      }
    })
    
    ###Renders the Price/Volume graphs
    output$builder_pv_chart <- renderPlot ({
      input$builder_get_symbols
      input$builder_clear_selected
      input$builder_clear_all
      
      shiny::validate(
        need(exists("builder_data"), message = "\n The Portfolio Simulator tracks the performance of equities for a user-defined date range and position size. The data is pulled from the Quandl Stock Wiki. \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Input position size \n 4. Press 'Get Data' \n 5. Enjoy!"))
      
      graph_data <- builder_data
      
      asset_options <- input$builder_asset_list
      
      graph_data <- graph_data[name %in% asset_options]
      
      g.top <- ggplot(graph_data, aes(x=date,y=close, group=name,colour=name)) +   
        geom_line()+
        theme_minimal() + 
        ylab("Price ($)") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
      
      g.bottom <- ggplot(graph_data, aes(x=date,y=volume, group=name,colour=name)) +   
        geom_bar(stat = "identity", position = position_dodge(), fill = "#000000")+
        theme_minimal() + 
        ylab("Volume") +
        scale_y_continuous(labels = comma)+ 
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1.9)) 
    
      grid.draw(rbind(ggplotGrob(g.top), ggplotGrob(g.bottom), size="first"))
    })

    ###Renders the Price/Volume table
    output$builder_pv_table <- renderDataTable ({
      input$builder_get_symbols
      input$builder_clear_selected
      input$builder_clear_all      
      
      shiny::validate(need(exists("builder_data"), message = "\n The Portfolio Simulator tracks the performance of equities for a user-defined date range and position size. The data is pulled from the Quandl Stock Wiki. \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Input position size \n 4. Press 'Get Data' \n 5. Enjoy!"))
      
      table_data <- builder_data
      
      asset_options <- input$builder_asset_list
      
      table_data <- table_data[name %in% asset_options][,c("name", "date", "close", "volume"), with = FALSE]
      
      setnames(table_data, c("name", "date", "close", "volume"), c("Name", "Date", "Close", "Volume"))

      table_data
    })

    ###Builder CSV Export Button
    output$builder_export_csv <- downloadHandler(
      filename = function() {
        paste(format(Sys.Date(), format = "%Y_%m_%d"),"price_volume.csv", sep = "_")
      },
      
      content = function(file_name) {
        if(file.exists(file_name)) {file.remoce(file_name)}
        asset_options <- input$builder_asset_list          
        data <- builder_data[name %in% asset_options][,c("name", "date", "close", "volume"), with = FALSE]
        write.csv(data, file = file_name, row.names=FALSE)
      }  
    )
    ################ END BUILDER ######################
    ################## CURRENT ########################
    observe({
      if(input$current_password == "admin"){
        updateButton(session, "current_password_button", " Load Current Portfolio", disabled = F, icon = icon("unlock-alt"), style = "primary")
      } else{
        updateButton(session, "current_password_button", " Load Current Portfolio", disabled = T, icon = icon("lock"), style = "danger")
      }
    })
    ################# END CURRENT #####################
    
    ########### END PORTFOLIO SIM TAB #################

    
    ################# ALL DATA ########################
    
    #################### RAW ##########################
    ###Raw search function call
    quandl_raw_search <- reactive({
      input$raw_search_button
      result <- quandl_raw_search_results(isolate(input$raw_search))
      return(result)      
    })

    ###Renders the table displaying the raw search results
    output$raw_search_results <- renderDataTable({
      input$raw_search_button      
      
      shiny::validate(need(isolate(input$raw_search) != "", message = "\n Search all databases for a keyword. For example, search 'crude oil' and a list of all datasets containing information on crude oil will be returned.  \n \n Instructions: \n --------------- \n 1. Enter key words to search \n 2. Press 'Search' \n 3. Copy ID number \n 4. Paste ID on the 'Data' tab \n 5. Press 'Get Data' \n 6. Export data to CSV \n 7. Enjoy!"))
      
      table_data <- quandl_raw_search()
    
      table_data <- table_data[, c("code", "name", "description", "from_date", "to_date"), with = FALSE]
      setnames(table_data,c("code", "name", "description", "from_date", "to_date"), c("ID", "Name", "Description", "From Date", "To Date"))
      
      table_data
    })

    ###Raw data function call
    quandl_raw_download <- reactive({
      input$raw_download_button
      result <- quandl_raw_download_results(isolate(input$raw_download))
      return(result)
    })

    ###Renders the table displaying the raw data
    output$raw_download_results <- renderDataTable({
      input$raw_download_button      
      
      shiny::validate(need(isolate(input$raw_download) != "", "\n Search all databases for a keyword. For example, search 'crude oil' and a list of all datasets containing information on crude oil will be returned.  \n \n Instructions: \n --------------- \n 1. Enter key words to search \n 2. Press 'Search' \n 3. Copy ID number \n 4. Paste ID on the 'Data' tab \n 5. Press 'Get Data' \n 6. Export data to CSV \n 7. Enjoy!"))
      
      table_data <- quandl_raw_download()
      
      table_data
    })

    ###Raw CSV Export Button
    output$raw_export_csv <- downloadHandler(
      filename = function() {
        paste(format(Sys.Date(), format = "%Y_%m_%d"),"data.csv", sep = "_")
      },
      
      content = function(file_name) {
        if(file.exists(file_name)) {file.remoce(file_name)}         
        data <- quandl_raw_download()
        write.csv(data, file = file_name, row.names=FALSE)
      }  
    )
    
    ###Renders the selectize input for the raw futures data
    output$raw_futures_symbol_select <- renderUI({
      withProgress(value = 0, message = "Loading...", {
        raw_futures_list <- quandl_raw_futures_list()
        selectInput("raw_futures_symbol_select", "Select Futures Contract:", choices = list(
          CBT = setNames(paste(raw_futures_list[exchange == "CBT", quandl.code], ""), paste(raw_futures_list[exchange == "CBT", name], " (", raw_futures_list[exchange == "CBT", ticker], ")", sep = "")),
          CEC = setNames(paste(raw_futures_list[exchange == "CEC", quandl.code], ""), paste(raw_futures_list[exchange == "CEC", name], " (", raw_futures_list[exchange == "CEC", ticker], ")", sep = "")),
          CME = setNames(paste(raw_futures_list[exchange == "CME", quandl.code], ""), paste(raw_futures_list[exchange == "CME", name], " (", raw_futures_list[exchange == "CME", ticker], ")", sep = "")),
          EUREX = setNames(paste(raw_futures_list[exchange == "EUREX", quandl.code], ""),paste(raw_futures_list[exchange == "EUREX", name], " (", raw_futures_list[exchange == "EUREX", ticker], ")", sep = "")),
          ICE = setNames(paste(raw_futures_list[exchange == "ICE", quandl.code], ""),paste(raw_futures_list[exchange == "ICE", name], " (", raw_futures_list[exchange == "ICE", ticker], ")", sep = "")),
          NYM = setNames(paste(raw_futures_list[exchange == "NYM", quandl.code], ""), paste(raw_futures_list[exchange == "NYM", name], " (", raw_futures_list[exchange == "NYM", ticker], ")", sep = "")),
          NYMEX = setNames(paste(raw_futures_list[exchange == "NYMEX", quandl.code], ""),paste(raw_futures_list[exchange == "NYMEX", name], " (", raw_futures_list[exchange == "NYMEX", ticker], ")", sep = ""))),selected = "CME/CL ", width = "100%")
      })
    })
    
    ###Renders the selectize input for the raw futures data month 
    output$raw_futures_month_select <- renderUI({
      raw_futures_list <- quandl_raw_futures_list()
      selected_futures_name <- input$raw_futures_symbol_select
      validate(need(!is.null(selected_futures_name), ""))
      selected_futures_name <- strsplit(selected_futures_name, " ")
      selected_futures_name <- head(selected_futures_name[[1]],1)
      futures_months <- data.table(month.name = c("January","February","March","April","May","June","July","August","September","October","November","December"), months = c("F","G","H","J","K","M","N","Q","U","V","X","Z"), key = "months")
      selected_futures_months <- data.table(months = strsplit(raw_futures_list[quandl.code == selected_futures_name, months], "")[[1]], key = "months")
      selected_futures_months <- futures_months[selected_futures_months]
      selectInput("raw_futures_month_select", "Select Month:", choices = setNames(selected_futures_months$months, paste(selected_futures_months$month.name, " (", selected_futures_months$months, ")", sep = "")), width = "100%")
    })
    
    ###Raw futures download button
    quandl_raw_futures_download <- reactive({
      input$raw_futures_download_button
      selected_futures_name <- isolate(input$raw_futures_symbol_select)
      selected_futures_name <- strsplit(selected_futures_name, " ")
      selected_futures_name <- head(selected_futures_name[[1]],1)
      
      search <- paste(selected_futures_name, isolate(input$raw_futures_month_select), isolate(input$raw_futures_year_select), sep = "")
      result <- quandl_raw_download_results(search)
      return(result)
    })
    
    ###Renders the table to display the raw futures data
    output$raw_futures_download_results <- renderDataTable({
      input$raw_download_button      
      
      shiny::validate(need(input$raw_futures_download_button == T, "\n Find any futures contract traded in the CBT, CEC, CME, EUREX, ICE, NYM, and NYMEX. \n \n Instructions: \n --------------- \n 1. Select the futures contract \n 2. Select the month  \n 3. Enter the year \n 4. Toggle the 'Get Data' button to display data \n 5. Export data to CSV \n 6. Enjoy!"))
      
      table_data <- quandl_raw_futures_download()
      
      table_data
    })
    
    ###Raw futures CSV Export Button
    output$raw_futures_export_csv <- downloadHandler(
      filename = function() {
        paste(format(Sys.Date(), format = "%Y_%m_%d"),"data.csv", sep = "_")
      },
      
      content = function(file_name) {
        if(file.exists(file_name)) {file.remoce(file_name)}         
        data <- quandl_raw_futures_download()
        write.csv(data, file = file_name, row.names=FALSE)
      }  
    )
    
    ################## END RAW ########################

    ################### EQUITY ########################
    ###Renders the selectize input for the equities list
    output$equity_symbol_select <- renderUI({
      withProgress(value = 0, message = "Loading...", {
        equity_list <- quandl_equity_list()
        selectInput("equity_symbol_select", "Select Stock:", choices = setNames(paste(equity_list$quandl.code, sep = ""), paste(equity_list$name, " (", equity_list$ticker, ")", sep = "")), selected = "WIKI/TSLA", width = "100%")
      })
    })

    ###Get Data button
    observe({
      if(input$equity_get_symbols > 0){
        withProgress(value = 1, message = "Pulling Data...", {
          
          equity_list <- quandl_equity_list()
          
          if(exists("equity_data") & exists("equity_symbols")){
            qname <- equity_list[quandl.code == isolate(input$equity_symbol_select), name]
            validate(need(!(qname %in% equity_data$name), "This data has already been pulled"))
          }
          
          quandl_data <- quandl_equity_download(isolate(input$equity_symbol_select),equity_list[quandl.code == isolate(as.character(input$equity_symbol_select)), name])
          equity_data_globalize(quandl_data)
        })
      }  
    })

    ###Renders the checkboxes displaying the pulled data
    output$equity_asset_list <- renderUI({
      input$equity_get_symbols
      input$equity_update
      input$equity_clear_all
      input$equity_clear_selected
      
      if(exists("equity_symbols")){
        checkboxGroupInput("equity_asset_list", "Select Assets to Display", choices = equity_symbols, selected = equity_symbols)
      }
    })

    ###Clear All button
    observe({
      if(input$equity_clear_all > 0){
        if(exists("equity_data")){
          rm(equity_data, envir = .GlobalEnv)
          rm(equity_symbols, envir = .GlobalEnv)
        }
      }
    })
    
    ###Clear Selected button
    observe({
      if(input$equity_clear_selected > 0){
        if(exists("equity_data")){
          if(length(unique(equity_data$name)) == 1){
            rm(equity_data, envir = .GlobalEnv)
            rm(equity_symbols, envir = .GlobalEnv)
          } else{
            equity_data <<- equity_data[!(name %in% isolate(input$equity_asset_list))]
            equity_symbols <<- unique(equity_data$name)  
          }
        }
      }
    })
    
    ###Renders the Price/Volume graphs
    output$equity_pv_chart <- renderPlot ({
      input$equity_get_symbols
      input$equity_update
      input$equity_clear_all
      input$equity_clear_selected
      
      shiny::validate(
        need(exists("equity_data"), message = "\n Pull price and volume data for stocks.  \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Press 'Get Data' \n 4. Export data to CSV \n 5. Enjoy!"))
      
      graph_data <- equity_data
      
      asset_options <- input$equity_asset_list
      graph_data <- graph_data[name %in% asset_options]
      
      graph_data <- graph_data[date >= input$equity_date_range[1] & date <= input$equity_date_range[2]]
      
      g.top <- ggplot(graph_data, aes(x=date,y=close, group=name,colour=name)) +   
        geom_line()+
        theme_minimal() + 
        ylab("Price ($)") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
      
      g.bottom <- ggplot(graph_data, aes(x=date,y=volume, group=name,colour=name)) +   
        geom_bar(stat = "identity", position = position_dodge(), fill = "#000000")+
        theme_minimal() + 
        ylab("Volume") +
        scale_y_continuous(labels = comma)+ 
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1.9)) 
      
      grid.draw(rbind(ggplotGrob(g.top), ggplotGrob(g.bottom), size="first"))
    })
    
    ###Renders the price volume table 
    output$equity_pv_table <- renderDataTable({
      input$equity_get_symbols
      input$equity_update
      input$equity_clear_all
      input$equity_clear_selected
      
      shiny::validate(
        need(exists("equity_data"), message = "\n Pull price and volume data for stocks.  \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Press 'Get Data' \n 4. Export data to CSV \n 5. Enjoy!"))
      
      table_data <- equity_data
      
      table_data <- table_data[date >= input$equity_date_range[1] & date <= input$equity_date_range[2]]
      
      asset_options <- input$equity_asset_list
      
      table_data <- table_data[name %in% asset_options][,c("name", "date", "close", "volume"), with = FALSE]
      
      setnames(table_data, c("name", "date", "close", "volume"), c("Name", "Date", "Close", "Volume"))
      
      table_data
    })
    
    ###Equity CSV Export Button
    output$equity_export_csv <- downloadHandler(
      filename = function() {
        paste(format(Sys.Date(), format = "%Y_%m_%d"),"data.csv", sep = "_")
      },
      
      content = function(file_name) {
        if(file.exists(file_name)) {file.remoce(file_name)}         
        asset_options <- input$equity_asset_list          
        data <- equity_data[name %in% asset_options][,c("name", "date", "close", "volume"), with = FALSE]
        write.csv(data, file = file_name, row.names=FALSE)
      }  
    )
    ################# END EQUITY ######################

    ################## FUTURES ########################
    ###Renders the selectize input for the futures list
    output$futures_symbol_select <- renderUI({
      withProgress(value = 0, message = "Loading...", {
        futures_list <- quandl_futures_list()
        selectInput("futures_symbol_select", "Select Futures Contract:", choices = setNames(paste(futures_list$Quandl.Code, "1", sep = ""), paste(futures_list$Name, " (", futures_list$Ticker, ")", sep = "")), selected = "CHRIS/CME_CL1", width = "100%")
      })
    })
    
    ###Get Data button
    observe({
      if(input$futures_get_symbols > 0){
        withProgress(value = 1, message = "Pulling Data...", {
          
          futures_list <- quandl_futures_list()
          
          qcode <- isolate(as.character(input$futures_symbol_select))
          qcode <- substring(qcode, 1, nchar(qcode) - 1)
          
          if(exists("futures_data") & exists("futures_symbols")){
            qname <- futures_list[Quandl.Code == qcode, Name]
            validate(need(!(qname %in% futures_data$name), "This data has already been pulled"))
          }
          
          quandl_data <- quandl_futures_download(isolate(input$futures_symbol_select),futures_list[Quandl.Code == qcode, Name])
          futures_data_globalize(quandl_data)
        })
      }  
    })
    
    ###Renders the checkboxes displaying the pulled data
    output$futures_asset_list <- renderUI({
      input$futures_get_symbols
      input$futures_update
      input$futures_clear_all
      input$futures_clear_selected
      
      if(exists("futures_symbols")){
        checkboxGroupInput("futures_asset_list", "Select Assets to Display", choices = futures_symbols, selected = futures_symbols)
      }
    })
    
    ###Clear All button
    observe({
      if(input$futures_clear_all > 0){
        if(exists("futures_data")){
          rm(futures_data, envir = .GlobalEnv)
          rm(futures_symbols, envir = .GlobalEnv)
        }
      }
    })
    
    ###Clear Selected button
    observe({
      if(input$futures_clear_selected > 0){
        if(exists("futures_data")){
          if(length(unique(futures_data$name)) == 1){
            rm(futures_data, envir = .GlobalEnv)
            rm(futures_symbols, envir = .GlobalEnv)
          } else{
            futures_data <<- futures_data[!(name %in% isolate(input$futures_asset_list))]
            futures_symbols <<- unique(futures_data$name)  
          }
        }
      }
    })
    
    ###Renders the Price/Volume graphs
    output$futures_pv_chart <- renderPlot ({
      input$futures_get_symbols
      input$futures_update
      input$futures_clear_all
      input$futures_clear_selected
      
      shiny::validate(
        need(exists("futures_data"), message = "\n Pull price and volume data for stocks.  \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Press 'Get Data' \n 4. Export data to CSV \n 5. Enjoy!"))
      
      graph_data <- futures_data
      
      asset_options <- input$futures_asset_list
      graph_data <- graph_data[name %in% asset_options]
      
      graph_data <- graph_data[date >= input$futures_date_range[1] & date <= input$futures_date_range[2]]
      
      g.top <- ggplot(graph_data, aes(x=date,y=settle, group=name,colour=name)) +   
        geom_line()+
        theme_minimal() + 
        ylab("Price ($)") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "none", legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
      
      g.bottom <- ggplot(graph_data, aes(x=date,y=volume, group=name,colour=name)) +   
        geom_bar(stat = "identity", position = position_dodge(), fill = "#000000")+
        theme_minimal() + 
        ylab("Volume") +
        scale_y_continuous(labels = comma)+ 
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1.9)) 
      
      grid.draw(rbind(ggplotGrob(g.top), ggplotGrob(g.bottom), size="first"))
    })
    
    #Renders the price volume table
    output$futures_pv_table <- renderDataTable({
      input$futures_get_symbols
      input$futures_update
      input$futures_clear_all
      input$futures_clear_selected
      
      shiny::validate(
        need(exists("futures_data"), message = "\n Pull price and volume data for front month futures contracts.  \n \n Instructions: \n --------------- \n 1. Select ticker \n 2. Select date range \n 3. Press 'Get Data' \n 4. Export data to CSV \n 5. Enjoy!"))
      
      table_data <- futures_data
      
      table_data <- table_data[date >= input$futures_date_range[1] & date <= input$futures_date_range[2]]
      
      asset_options <- input$futures_asset_list
      
      table_data <- table_data[name %in% asset_options]
      
      setnames(table_data, c("name", "date", "open", "high", "low", "settle", "volume"), c("Name", "Date", "Open", "High", "Low", "Settle", "Volume"))
      
      table_data
    })
    
    ###Futures CSV Export Button
    output$futures_export_csv <- downloadHandler(
      filename = function() {
        paste(format(Sys.Date(), format = "%Y_%m_%d"),"data.csv", sep = "_")
      },
      
      content = function(file_name) {
        if(file.exists(file_name)) {file.remoce(file_name)}         
        asset_options <- input$futures_asset_list          
        data <- futures_data[name %in% asset_options][,c("name", "date", "open", "high", "low", "settle", "volume"), with = FALSE]
        write.csv(data, file = file_name, row.names=FALSE)
      }  
    )
    ################ END FUTURES ######################

    ##################### FX ##########################
    ###Get Data button
    observe({
      if(input$fx_get_symbols > 0){
        withProgress(value = 1, message = "Pulling Data...", {
          quandl_data <- quandl_fx_download(isolate(input$fx_symbol_select))
          validate(need(!is.null(quandl_data), message = "Please enter a valid currency pair. e.g. 'USDCAD'"))
          fx_data_globalize(quandl_data)
        })
      }  
    })

    ###Renders the checkboxes displaying the pulled data
    output$fx_asset_list <- renderUI({
      input$fx_get_symbols
      input$fx_update
      input$fx_clear_all
      input$fx_clear_selected
      
      if(exists("fx_symbols")){
        checkboxGroupInput("fx_asset_list", "Select Assets to Display", choices = fx_symbols, selected = fx_symbols)
      }
    })

    ###Clear All button
    observe({
      if(input$fx_clear_all > 0){
        rm(fx_data, envir = .GlobalEnv)
        rm(fx_symbols, envir = .GlobalEnv)
      }
    })
    
    ###Clear Selected button
    observe({
      if(input$fx_clear_selected > 0){
        if(exists("fx_data")){
          if(length(unique(fx_data$name)) == 1){
            rm(fx_data, envir = .GlobalEnv)
            rm(fx_symbols, envir = .GlobalEnv)
          } else{
            fx_data <<- fx_data[!(name %in% isolate(input$fx_asset_list))]
            fx_symbols <<- unique(fx_data$name)  
          }
        }
      }
    })
    
    ###Renders the Price/Volume graphs
    output$fx_rate_chart <- renderPlot ({
      input$fx_get_symbols
      input$fx_update
      input$fx_clear_all
      input$fx_clear_selected
      
      shiny::validate(
        need(exists("fx_data"), message = "\n Pull currency exchange rates.  \n \n Instructions: \n --------------- \n 1. Enter currency pair (e.g. USDCAD) \n 2. Select date range \n 3. Press 'Get Data' \n 4. Export data to CSV \n 5. Enjoy!"))
      
      graph_data <- fx_data
      
      asset_options <- input$fx_asset_list
      graph_data <- graph_data[name %in% asset_options]
      
      graph_data <- graph_data[date >= input$fx_date_range[1] & date <= input$fx_date_range[2]]
      
      ggplot(graph_data, aes(x=date,y=rate, group=name,colour=name)) +   
        geom_line()+
        theme_minimal() + 
        ylab("Rate ($)") +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(size = 16, color = "#2C3E50"),legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) 
    })
    
    #Renders the FX rate table
    output$fx_rate_table <- renderDataTable({
      input$fx_get_symbols
      input$fx_update
      input$fx_clear_all
      input$fx_clear_selected
      
      shiny::validate(
        need(exists("fx_data"), message = "\n Pull currency exchange rates.  \n \n Instructions: \n --------------- \n 1. Select currency pair \n 2. Select date range \n 3. Press 'Get Data' \n 4. Export data to CSV \n 5. Enjoy!"))
      
      table_data <- fx_data
      
      table_data <- table_data[date >= input$fx_date_range[1] & date <= input$fx_date_range[2]]
      
      asset_options <- input$fx_asset_list
      
      table_data <- table_data[name %in% asset_options][,c("name", "date", "rate", "high", "low"), with = FALSE]
      
      setnames(table_data, c("name", "date", "rate", "high", "low"), c("Name", "Date", "Rate", "High", "Low"))
      
      table_data
    })
    
    ###FX CSV Export Button
    output$fx_export_csv <- downloadHandler(
      filename = function() {
        paste(format(Sys.Date(), format = "%Y_%m_%d"),"data.csv", sep = "_")
      },
      
      content = function(file_name) {
        if(file.exists(file_name)) {file.remoce(file_name)}         
        asset_options <- input$fx_asset_list          
        data <- fx_data[name %in% asset_options][,c("name", "date", "rate", "high", "low"), with = FALSE]
        write.csv(data, file = file_name, row.names=FALSE)
      }  
    )
    ################### END FX ########################

    ############### END ALL DATA ######################
    
  })    
################# END SHINY #######################