shinyServer(function(input, output) {
  
  ## ------------- Page 2 ------------------
  
  
  # --- PLOT 1
  output$plot1_2 <- renderHighchart({
    
    dt_sector <- 
      dow_companies %>% 
      
      group_by(Sector) %>% 
      summarise(Total = sum(Marketcap)) %>% 
      ungroup()
    
    hc <- dt_sector %>%
      
      hchart(
        "treemap",
        hcaes(x = Sector, value = round(Total/sum(Total)*100,2), color = Total)) %>% 
      hc_title(text = "Market Capitalization of Each Sector", align = "left",
               style = list(fontWeight = "bold", fontSize = "30px")) %>% 
      hc_subtitle(text = "Based on Stocks in the Dow Jones Index In percent %",
                  style = list(fontWeight = "bold"), align = "left") %>% 
      hc_add_theme(hc_theme_ffx()) %>% 
      hc_yAxis(opposite = TRUE,
               labels = list(format = "{value}%"))
    
    hc
    
                                })
  
  # --- PLOT 2
  output$plot2_2 <-  renderPlotly({
    # Vis
    col <- 
      ggplot(data = dow_companies, mapping = aes(x = Marketcap, y = reorder(Longname, Marketcap),
                                                 text = glue("Company : {Longname}
                                                        Marketcap : {comma(round(Marketcap/1000000, 2))} M") )) +
      geom_col(aes(fill = Marketcap), show.legend = FALSE) +
      
      labs(title = "Market Capitalizationon of Each Company",
           x = "Value",
           y = NULL) +
      scale_x_continuous(labels = unit_format(scale = 10e-7, suffix = "Million")) +
      scale_fill_distiller(palette = "Spectral") +
      theme_classic()
    
    ggplotly(col, tooltip = "text")
                                  })
  
  # --- PLOT 3
  output$plot3_2 <- renderPlotly({
    # Data
    dow <- dow_companies[dow_companies$Symbol != c("JPM", "GS", "AXP"),]
    
    # Vis
    col1 <- 
      ggplot(data = dow, mapping = aes(x = Ebitda, y = reorder(Longname, Ebitda),
                                       text = glue("Company : {Longname}
                                             EBITDA : {comma(round(Ebitda/1000000, 2))} M"))) +
      geom_col(aes(fill = Ebitda), show.legend = FALSE) +
      scale_fill_continuous(low = "red", high = "black") +
      labs(title = "EBITDA of Each Company",
           x = "Value",
           y = NULL) +
      scale_x_continuous(labels = unit_format(scale = 10e-7, suffix = "M")) +
      theme_light()
    
    
    ggplotly(col1, tooltip = "text")
    
                                })
  
  ## ------------- Page 3 ------------------

# --- Sub Basic
  
  # --- PLOT 1
  
  output$plot1_3 <- renderPlotly({
    
    df1 <-
      cl_dow_stock18 %>%
      filter(Symbol == input$input_company) %>% 
      filter(Date >= input$input_date[1] & Date <= input$input_date[2])
    
    plt <-
      ggplot(data = df1, aes(x = Date, y = round(Adj.Close, 2), group = 1,
                             text = glue("Date : {Date},
                                         Price : {round(Adj.Close,2)}"))) +
      geom_line(col = "blue") +
      labs(title = glue("{input$input_company} 
                        Stock Price Movement (Adjustment Close)"),
           x = "Date",
           y = "Price $") +
      theme_minimal()
    
    if (input$input_radio == "Marker") {plt <- plt + geom_point()}
    
    ggplotly(plt, tooltip = "text")
    
  })
  
  # # --- PLOT 2
  #
  output$plot2_3 <-  renderPlotly({
    # Data
    df1 <-
      cl_dow_stock18 %>%
      filter(Symbol == input$input_company) %>%
      filter(Date >= input$input_date[1] & Date <= input$input_date[2])

    dt_pvt <-
      pivot_longer(data = df1,
                   cols = c("Close", "Open"))

    # vis
    plt1 <-
      ggplot(data = dt_pvt, aes(x = Date, y = value, group = name, col = name,
                                text = glue("Date : {Date},
                                      Price : {round(value, 2)}"))) +
      geom_line() +
      scale_color_manual(values = c("black", "red")) +
      labs(title = glue("{input$input_company} 
                        Stock Price Movement (Open-Close)"),
           col = "Type :",
           y = "Price $") +
      theme_minimal()

    if (input$input_radio == "Marker") {plt1 <- plt1 + geom_point()}


    ggplotly(plt1, tooltip = "text")


  })

  # --- PLOT 3
  output$plot3_3 <- renderPlotly({
    # Data
    df1 <-
      cl_dow_stock18 %>%
      filter(Symbol == input$input_company) %>%
      filter(Date >= input$input_date[1] & Date <= input$input_date[2])

    dt_pvt1 <-
      pivot_longer(data = df1,
                   cols = c("High", "Low"))
    # Vis
    plt2 <-
      ggplot(dt_pvt1, aes(x = Date, y = value, group = name, col = name,
                          text = glue("Date : {Date},
                                Price : {round(value, 2)}"))) +
      geom_line() +
      labs(title = glue("{input$input_company} 
                        Stock Price Movement (High-Low)"),
           y = "Price $") +
      theme_minimal()


    if (input$input_radio == "Marker") {plt2 <- plt2 + geom_point()}

    ggplotly(plt2, tooltip = "text")
  #
  })

# ---- Sub expert
  output$plot3e_1 <- renderPlotly({

    # Data Preparation

    dt_exp <- dt_exp %>% filter(Symbol == input$input_companyxp)

    # create Bollinger Bands
    bbands <- BBands(dt_exp[,c("High","Low","Close")])

    # join and subset data
    dt_exp <- subset(cbind(dt_exp, data.frame(bbands[,1:3])),
                     Date >= input$input_datexp[1] & Date <= input$input_datexp[2])


    # colors column for increasing and decreasing
    for (i in 1:length(dt_exp[,1])) {
      if (dt_exp$Close[i] >= dt_exp$Open[i]) {
        dt_exp$direction[i] = 'Increasing'
      } else {
        dt_exp$direction[i] = 'Decreasing'
      }
    }

    i <- list(line = list(color = '#008000'))
    d <- list(line = list(color = '#FF0000'))

    # plot candlestick chart

    fig <- dt_exp %>% plot_ly(x = ~Date, type="candlestick",
                              open = ~Open, close = ~Close,
                              high = ~High, low = ~Low, name = "Candlestick",
                              increasing = i, decreasing = d)
    fig <- fig %>% add_lines(x = ~Date, y = ~up , name = " Bollinger Band",
                             line = list(color = '#ccc', width = 0.7),
                             legendgroup = "Bollinger Bands",
                             hoverinfo = "none", inherit = F)
    fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = " Bollinger Band",
                             line = list(color = '#ccc', width = 0.7),
                             legendgroup = "Bollinger Bands", inherit = F,
                             showlegend = FALSE, hoverinfo = "none")
    fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Moving Avg",
                             line = list(color = '#E377C2', width = 0.7),
                             hoverinfo = "none", inherit = F)
    fig <- fig %>% layout(yaxis = list(title = "Price"))

    # plot volume bar chart
    fig2 <- dt_exp
    fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = glue("{input$input_companyxp} Volume"),
                             color = ~direction, colors = c('#17BECF','#7F7F7F'))
    fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))

    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.70, y = -0.455,
               xanchor = 'tail', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                 list(count=1,
                      label='1 MO',
                      step='month',
                      stepmode='backward'),
                 list(count=3,
                      label='3 MO',
                      step='month',
                      stepmode='backward'),
                 list(count=6,
                      label='6 MO',
                      step='month',
                      stepmode='backward'),
                 list(count=1,
                      label='1 YR',
                      step='year',
                      stepmode='backward'),
                 list(count = 1,
                      label = "YTD",
                      step = "year",
                      stepmode = "todate"),
                 list(step = "all")
               ))


    # # subplot with shared x axis
    fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                   shareX = TRUE, titleY = TRUE)
    fig <- fig %>% layout(title = paste(input$input_companyxp, ":",
                                        input$input_datexp[1], "to", input$input_datexp[2]),
                          xaxis = list(rangeselector = rs),
                          legend = list(orientation = 'h', x = 0.5, y = 1,
                                        xanchor = 'center', yref = 'paper',
                                        font = list(size = 10),
                                        bgcolor = 'transparent'))

    fig
    })

  
  ## ---- DATASET stock
  output$dataset_stock <- DT::renderDataTable(cl_dow_stock18,
                                              options = list(scrollX=T,
                                                             scrollY=T))



})
  
  