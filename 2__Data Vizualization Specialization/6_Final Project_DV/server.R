shinyServer(function(input, output) {
  
  # ---------------------- PAGE 1  ----------------
  
  # ---------- PLOT 1
  output$page1_1 <- renderHighchart({
    
    # Data
    df_3_met <- 
      df %>% 
      group_by(yearMonth) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(yearMonth = as.character(yearMonth)) %>% 
      ungroup()
    
    
    # Visualisasi 
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "<b>Superstore Performance Trends</b>",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "Jan 2011 - Des 2014",
                  style = list(color = "black", useHTML = TRUE)) %>% 
      hc_xAxis(categories = df_3_met$yearMonth) %>%
      hc_add_series(data = df_3_met$Total_Sales, name = "Total Sales") %>%
      hc_add_series(data = df_3_met$Total_Profit, name = "Total Profit") %>%
      hc_add_series(data = df_3_met$Total_Quantity, name = "Total Quantity") %>% 
      hc_yAxis(labels = list(format = "{value:,.0f}")) %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red')) %>%
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}")
    
  })
  
  # ---------- PLOT 2
  
  output$page1_2 <- renderHighchart({
    
    # ---- Segment
    # data
    seq_df <- 
      df %>% 
      group_by(Segment, year) %>% 
      summarise(Total = sum(Profit)) %>% 
      pivot_wider(names_from = Segment, values_from = Total)

    seq_1 <- 
      seq_df %>% select("Consumer")
    names(seq_1) <- NULL
    seq_2 <- 
      seq_df %>% select("Corporate")
    names(seq_2) <- NULL
    seq_3 <- 
      seq_df %>% select("Home Office")
    names(seq_3) <- NULL
    
    # Vis
    highchart() %>%
      hc_title(text = "<b>Profit Value by Segment </b>",
               align = "left",
               style = list(color = "black", useHTML = TRUE)) %>% 
      hc_subtitle(text = "2011 - 2014 in Overall",
                  align = "left",
                  style = list(color = "black", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      
      
      hc_add_series(name = 'Segment', innerSize =  '50%', center = c('50%', '50%'),
                    data = list(list(name = 'Consumer', sequence = c(seq_1[[1]])), 
                                list(name = 'Corporate', sequence = c(seq_2[[1]])),
                                list(name = 'Home Office', sequence = c(seq_3[[1]])))) %>%
      
      hc_plotOptions(pie = list(size = 340)) %>%
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>%
      hc_motion(enabled = TRUE, loop = TRUE, axisLabel = 'year',
                labels = as.character(c(2011, 2012, 2013, 2014)), series = c(1, 0)) %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
      
    
  })
  
  
  
  # ---------- PLOT 3
  output$page1_3 <- renderHighchart({
    
    
    # ---- Data Category
    ctg_df <- 
      df %>% 
      group_by(Category, year) %>% 
      summarise(Total = sum(Profit)) %>% 
      pivot_wider(names_from = Category, values_from = Total)
    
    ctg_1 <- 
      ctg_df %>% select("Furniture")
    names(ctg_1) <- NULL
    
    ctg_2 <- 
      ctg_df %>% select("Office Supplies")
    names(ctg_1) <- NULL
    
    ctg_3 <- 
      ctg_df %>% select("Technology")
    names(ctg_1) <- NULL
    
    # Vis
    highchart() %>%
      hc_title(text = "<b>Profit Value by Category </b>",
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>% 
      hc_subtitle(text = "2011 - 2014 in Overall",
                  align = "center",
                  style = list(color = "black", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      hc_add_series(name = 'Ship Mode', innerSize =  '50%', center = c('50%', '50%'),
                    data = list(list(name = 'Technology', sequence = c(ctg_3[[1]])),
                                list(name = 'Office Supplies', sequence = c(ctg_2[[1]])),
                                list(name = 'Furniture', sequence = c(ctg_1[[1]])))) %>%
      hc_plotOptions(pie = list(size = 265)) %>%
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>%
      hc_motion(enabled = TRUE, loop = TRUE, axisLabel = 'year', labels = as.character(c(2011, 2012, 2013, 2014)), series = c(1, 0)) %>%
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
  })

  
  
  # ---------- PLOT 4
  output$page1_4 <- renderHighchart({
    
    
    # ---- Data Ship.Mode
    ship_df <- 
      df %>% 
      group_by(Ship.Mode, year) %>% 
      summarise(Total = sum(Profit)) %>% 
      pivot_wider(names_from = Ship.Mode, values_from = Total)
    
    shp_1 <- 
      ship_df %>% select("Standard Class")
    names(shp_1) <- NULL
    
    shp_2 <- 
      ship_df %>% select("Second Class")
    names(shp_2) <- NULL
    
    shp_3 <- 
      ship_df %>% select("First Class")
    names(shp_3) <- NULL
    
    shp_4 <- 
      ship_df %>% select("Same Day")
    names(shp_4) <- NULL
    
    # Vis
    highchart() %>%
      hc_title(text = "<b>Profit Value by Ship Mode </b>",
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>% 
      hc_subtitle(text = "2011 - 2014 in Overall",
                  align = "center",
                  style = list(color = "black", useHTML = TRUE)) %>%
      hc_chart(type = "pie") %>%
      hc_add_series(name = 'Ship Mode', innerSize =  '50%', center = c('50%', '50%'),
                    data = list(list(name = 'Standard Class', sequence = c(shp_1[[1]])),
                                list(name = 'Second Class', sequence = c(shp_2[[1]])),
                                list(name = 'First Class', sequence = c(shp_3[[1]])),
                                list(name = 'Same Day', sequence = c(shp_4[[1]])))) %>%
      hc_plotOptions(pie = list(size = 265)) %>%
      hc_yAxis(labels = list(format = "{value:,.0f}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>%
      hc_motion(enabled = TRUE, loop = TRUE, axisLabel = 'year', labels = as.character(c(2011, 2012, 2013, 2014)), series = c(1, 0)) %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  })
  
  
  
  # ---------------------- PAGE 2a  --------------
  
  # ---------- PLOT 1
  
  output$page2a_1 <- renderHighchart({
    
    # ------------ grouped None -----------------    
    
if(input$input_2a_an == "None") { 

# ------------ Based Region
  
  if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Sales") {  # Region + Total Sales
df_bar <- 
    df %>% 
    group_by(Region) %>% 
    summarise(Total = sum(Sales)) %>% 
    arrange(-Total) %>% 
    ungroup()  
  
df_bar %>% 
    hchart('bar', hcaes(x = Region, y = Total)) %>% 
    hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
    hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
    hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))

  }
  
  else if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Profit") { # Region + Total Profit
    df_bar <- 
      df %>% 
      group_by(Region) %>% 
      summarise(Total = sum(Profit)) %>% 
      arrange(-Total) %>% 
      ungroup()  

    df_bar %>% 
      hchart('bar', hcaes(x = Region, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  }
  
  else if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Quantity") { # Region + Total Quantity
    df_bar <- 
      df %>% 
      group_by(Region) %>% 
      summarise(Total = sum(Quantity)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Region, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  }
  
  
  # ------------ Based Market
  
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Sales") {  # Market + Total Sales
    df_bar <- 
      df %>% 
      group_by(Market) %>% 
      summarise(Total = sum(Sales)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Market, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
  }
  
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Profit") { # Market + Total Profit
    df_bar <- 
      df %>% 
      group_by(Market) %>% 
      summarise(Total = sum(Profit)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Market, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  }
  
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Quantity") { # Market + Total Quantity
    df_bar <- 
      df %>% 
      group_by(Market) %>% 
      summarise(Total = sum(Quantity)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Market, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  }
  
  
  # ------------ Based Segment
  
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Sales") {  # Segment + Total Sales
    df_bar <- 
      df %>% 
      group_by(Segment) %>% 
      summarise(Total = sum(Sales)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Segment, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
  }
  
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Profit") { # Segment + Total Profit
    df_bar <- 
      df %>% 
      group_by(Segment) %>% 
      summarise(Total = sum(Profit)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Segment, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  }
  
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Quantity") { # Segment + Total Quantity
    df_bar <- 
      df %>% 
      group_by(Segment) %>% 
      summarise(Total = sum(Quantity)) %>% 
      arrange(-Total) %>% 
      ungroup()  
    
    df_bar %>% 
      hchart('bar', hcaes(x = Segment, y = Total)) %>% 
      hc_title(text = glue("{input$input_2a_tg} in Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  }
  
} 

    ## ------------  grouped Category -----------------      
    
else if(input$input_2a_an == "Category"){ 
 
  # ------------ Based Region
  
  if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Sales") {  # Region + Total Sales
  df_wider <- 
    df %>% 
    group_by(Region, Category) %>% 
    summarise(total_gp = sum(Sales)) %>% # total_gp = total per masiing2 jenis
    ungroup() %>% 
    pivot_wider(names_from = Category, values_from = total_gp)
  
  urut <-   
    df_wider %>% 
    group_by(Region) %>% 
    summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
  
  # Data buat plot
  df_wider <- df_wider %>% 
    mutate(
      total_k = urut$total_k
    ) %>% arrange(-total_k)

highchart() %>% 
    hc_chart(type = "bar") %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_xAxis(list(categories = df_wider$Region)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
    hc_series( 
      list(name = "Office Supplies",
           data = df_wider$`Office Supplies`,
           legendIndex = 2), 
      list(name = "Furniture",
           data = df_wider$Furniture,
           legendIndex = 3),
      list(name = "Technology",
           data = df_wider$Technology, 
           legendIndex = 1)) %>%
    hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
    hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
    hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
    hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
  
  }

  
  else if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Profit") { # Region + Total Profit  
    
    df_wider <- 
      df %>% 
      group_by(Region, Category) %>% 
      summarise(total_gp = sum(Profit)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
    
    
  else if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Quantity") { # Region + Total Quantity   
    
    df_wider <- 
      df %>% 
      group_by(Region, Category) %>% 
      summarise(total_gp = sum(Quantity)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series( 
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    }
      
      
      
  # ------------ Based Market
      
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Sales") {  # Market + Total Sales 
    
    
    df_wider <- 
      df %>% 
      group_by(Region, Category) %>% 
      summarise(total_gp = sum(Sales)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series( 
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
        
        
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Profit") { # Market + Total Profit 
    
    
    df_wider <- 
      df %>% 
      group_by(Region, Category) %>% 
      summarise(total_gp = sum(Profit)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
          
          
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Quantity") { # Market + Total Quantity  
    
    
    df_wider <- 
      df %>% 
      group_by(Region, Category) %>% 
      summarise(total_gp = sum(Quantity)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
            
            
            
  # ------------ Based Segment
            
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Sales") {  # Segment + Total Sales 
    
    
    df_wider <- 
      df %>% 
      group_by(Segment, Category) %>% 
      summarise(total_gp = sum(Sales)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Segment) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Segment)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
              
              
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Profit") { # Segment + Total Profit 
    
    
    df_wider <- 
      df %>% 
      group_by(Segment, Category) %>% 
      summarise(total_gp = sum(Profit)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Segment) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Segment)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
                
                
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Quantity") { # Segment + Total Quantity 
    
    
    df_wider <- 
      df %>% 
      group_by(Segment, Category) %>% 
      summarise(total_gp = sum(Quantity)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = Category, values_from = total_gp)
    
    urut <-   
      df_wider %>% 
      group_by(Segment) %>% 
      summarise(total_k = Technology + `Office Supplies` + Furniture) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Segment)) %>%    # ---------
    hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "Office Supplies",
             data = df_wider$`Office Supplies`,
             legendIndex = 2), 
        list(name = "Furniture",
             data = df_wider$Furniture,
             legendIndex = 3),
        list(name = "Technology",
             data = df_wider$Technology, 
             legendIndex = 1)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("#0073C2FF", "#EFC000FF", 'red'))
    
    
    }
                  
  
} 
    
    
    #  ------------ grouped Ship.Mode -----------------  
    
else { 

  # ------------ Based Region
  
  if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Sales") {  # Region + Total Sales
    
  df_wider <- 
    df_rn %>% 
    group_by(Region, `Ship Mode`) %>% 
    summarise(total_gp = sum(Sales)) %>% # total_gp = total per masiing2 jenis
    ungroup() %>% 
    pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
  
  urut <-   
    df_wider %>% 
    group_by(Region) %>% 
    summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
  
  # Data buat plot
  df_wider <- df_wider %>% 
    mutate(
      total_k = urut$total_k
    ) %>% arrange(-total_k)
  
highchart() %>% 
    hc_chart(type = "bar") %>% 
    hc_plotOptions(column = list(stacking = "normal")) %>% 
    hc_xAxis(list(categories = df_wider$Region)) %>% 
    hc_plotOptions(series = list(stacking= 'normal'))%>%
    hc_series(  
      list(name = "First Class",
           data = df_wider$`First Class`,
           legendIndex = 1), 
      list(name = "Same Day",
           data = df_wider$`Same Day`,
           legendIndex = 2),
      list(name = "Second Class",
           data = df_wider$`Second Class`, 
           legendIndex = 3),
      list(name = "Standard Class",
           data = df_wider$`Standard Class`, 
           legendIndex = 4)) %>%
    hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
    hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
    hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
    hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))

  }

  else if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Profit") { # Region + Total Profit 
  
    
    df_wider <- 
      df_rn %>% 
      group_by(Region, `Ship Mode`) %>% 
      summarise(total_gp = sum(Profit)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>% 
      hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "First Class",
             data = df_wider$`First Class`,
             legendIndex = 1), 
        list(name = "Same Day",
             data = df_wider$`Same Day`,
             legendIndex = 2),
        list(name = "Second Class",
             data = df_wider$`Second Class`, 
             legendIndex = 3),
        list(name = "Standard Class",
             data = df_wider$`Standard Class`, 
             legendIndex = 4)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  }
  
  else if(input$input_2a_ri == "Region" & input$input_2a_tg == "Total Quantity") { # Region + Total Quantity 
    
    
    df_wider <- 
      df_rn %>% 
      group_by(Region, `Ship Mode`) %>% 
      summarise(total_gp = sum(Quantity)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
    
    urut <-   
      df_wider %>% 
      group_by(Region) %>% 
      summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Region)) %>% 
      hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "First Class",
             data = df_wider$`First Class`,
             legendIndex = 1), 
        list(name = "Same Day",
             data = df_wider$`Same Day`,
             legendIndex = 2),
        list(name = "Second Class",
             data = df_wider$`Second Class`, 
             legendIndex = 3),
        list(name = "Standard Class",
             data = df_wider$`Standard Class`, 
             legendIndex = 4)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    }
  
  
  
  # ------------ Based Market
  
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Sales") {  # Market + Total Sales 
      
      df_wider <- 
        df_rn %>% 
        group_by(Market, `Ship Mode`) %>% 
        summarise(total_gp = sum(Sales)) %>% # total_gp = total per masiing2 jenis
        ungroup() %>% 
        pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
      
      urut <-   
        df_wider %>% 
        group_by(Market) %>% 
        summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
      
      # Data buat plot
      df_wider <- df_wider %>% 
        mutate(
          total_k = urut$total_k
        ) %>% arrange(-total_k)
      
      highchart() %>% 
        hc_chart(type = "bar") %>% 
        hc_plotOptions(column = list(stacking = "normal")) %>% 
        hc_xAxis(list(categories = df_wider$Market)) %>% 
        hc_plotOptions(series = list(stacking= 'normal'))%>%
        hc_series(  
          list(name = "First Class",
               data = df_wider$`First Class`,
               legendIndex = 1), 
          list(name = "Same Day",
               data = df_wider$`Same Day`,
               legendIndex = 2),
          list(name = "Second Class",
               data = df_wider$`Second Class`, 
               legendIndex = 3),
          list(name = "Standard Class",
               data = df_wider$`Standard Class`, 
               legendIndex = 4)) %>%
        hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
        hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
        hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
        hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  }
  
  
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Profit") { # Market + Total Profit
  
    
    df_wider <- 
      df_rn %>% 
      group_by(Market, `Ship Mode`) %>% 
      summarise(total_gp = sum(Profit)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
    
    urut <-   
      df_wider %>% 
      group_by(Market) %>% 
      summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Market)) %>% 
      hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "First Class",
             data = df_wider$`First Class`,
             legendIndex = 1), 
        list(name = "Same Day",
             data = df_wider$`Same Day`,
             legendIndex = 2),
        list(name = "Second Class",
             data = df_wider$`Second Class`, 
             legendIndex = 3),
        list(name = "Standard Class",
             data = df_wider$`Standard Class`, 
             legendIndex = 4)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  }
  
  
  else if(input$input_2a_ri == "Market" & input$input_2a_tg == "Total Quantity") { # Market + Total Quantity 
    
    
    df_wider <- 
      df_rn %>% 
      group_by(Market, `Ship Mode`) %>% 
      summarise(total_gp = sum(Quantity)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
    
    urut <-   
      df_wider %>% 
      group_by(Market) %>% 
      summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Market)) %>% 
      hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "First Class",
             data = df_wider$`First Class`,
             legendIndex = 1), 
        list(name = "Same Day",
             data = df_wider$`Same Day`,
             legendIndex = 2),
        list(name = "Second Class",
             data = df_wider$`Second Class`, 
             legendIndex = 3),
        list(name = "Standard Class",
             data = df_wider$`Standard Class`, 
             legendIndex = 4)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>%  
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  }
  
  
  
  # ------------ Based Segment
  
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Sales") {  # Segment + Total Sales 
      
      df_wider <- 
        df_rn %>% 
        group_by(Segment, `Ship Mode`) %>% 
        summarise(total_gp = sum(Sales)) %>% # total_gp = total per masiing2 jenis
        ungroup() %>% 
        pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
      
      urut <-   
        df_wider %>% 
        group_by(Segment) %>% 
        summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
      
      # Data buat plot
      df_wider <- df_wider %>% 
        mutate(
          total_k = urut$total_k
        ) %>% arrange(-total_k)
      
      highchart() %>% 
        hc_chart(type = "bar") %>% 
        hc_plotOptions(column = list(stacking = "normal")) %>% 
        hc_xAxis(list(categories = df_wider$Segment)) %>% 
        hc_plotOptions(series = list(stacking= 'normal'))%>%
        hc_series(  
          list(name = "First Class",
               data = df_wider$`First Class`,
               legendIndex = 1), 
          list(name = "Same Day",
               data = df_wider$`Same Day`,
               legendIndex = 2),
          list(name = "Second Class",
               data = df_wider$`Second Class`, 
               legendIndex = 3),
          list(name = "Standard Class",
               data = df_wider$`Standard Class`, 
               legendIndex = 4)) %>%
        hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
        hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
        hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
        hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  }
  
  
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Profit") { # Segment + Total Profit
    
    
    df_wider <- 
      df_rn %>% 
      group_by(Segment, `Ship Mode`) %>% 
      summarise(total_gp = sum(Profit)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
    
    urut <-   
      df_wider %>% 
      group_by(Segment) %>% 
      summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Segment)) %>% 
      hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "First Class",
             data = df_wider$`First Class`,
             legendIndex = 1), 
        list(name = "Same Day",
             data = df_wider$`Same Day`,
             legendIndex = 2),
        list(name = "Second Class",
             data = df_wider$`Second Class`, 
             legendIndex = 3),
        list(name = "Standard Class",
             data = df_wider$`Standard Class`, 
             legendIndex = 4)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>% 
      hc_tooltip(enabled = TRUE, pointFormat = "$ {point.y:,.2f}") %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
    
  }
  
  
  else if(input$input_2a_ri == "Segment" & input$input_2a_tg == "Total Quantity") { # Segment + Total Quantity 
    
    
    df_wider <- 
      df_rn %>% 
      group_by(Segment, `Ship Mode`) %>% 
      summarise(total_gp = sum(Quantity)) %>% # total_gp = total per masiing2 jenis
      ungroup() %>% 
      pivot_wider(names_from = `Ship Mode`, values_from = total_gp)  
    
    urut <-   
      df_wider %>% 
      group_by(Segment) %>% 
      summarise(total_k = `First Class` + `Same Day` +  `Second Class` + `Standard Class`) # total_k = total keseluruhan
    
    # Data buat plot
    df_wider <- df_wider %>% 
      mutate(
        total_k = urut$total_k
      ) %>% arrange(-total_k)
    
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      hc_xAxis(list(categories = df_wider$Segment)) %>% 
      hc_plotOptions(series = list(stacking= 'normal'))%>%
      hc_series(  
        list(name = "First Class",
             data = df_wider$`First Class`,
             legendIndex = 1), 
        list(name = "Same Day",
             data = df_wider$`Same Day`,
             legendIndex = 2),
        list(name = "Second Class",
             data = df_wider$`Second Class`, 
             legendIndex = 3),
        list(name = "Standard Class",
             data = df_wider$`Standard Class`, 
             legendIndex = 4)) %>%
      hc_title(text = glue("{input$input_2a_tg} of Each {input$input_2a_an}")) %>% 
      hc_subtitle(text = glue("In Each {input$input_2a_ri}")) %>%  
      hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}") %>% 
      hc_colors(c("green","#0073C2FF", "#EFC000FF", 'red'))
    
  } 
  
}
    
    
    
})

  
output$page2a_2 <- renderPlotly({
  
  # Columns names for input
  grp_cols <- names(df)
  
  # Convert character vector to list of symbols
  dots <- lapply(grp_cols, as.symbol)
  
  # Proses after input user
  if(input$input_2a_best == "Customer Name") {
    gp <- 6
  } else if(input$input_2a_best == "Country") {
    gp <- 10
  } else if(input$input_2a_best == "City") {
    gp <- 8
  } else if(input$input_2a_best == "State") {
    gp <- 9
  } else if((input$input_2a_best== "Sub Category")) {
    gp <- 15
  } 
  
  # Update data
  df_rank <- 
    df %>% 
    group_by_(dots[[gp]], dots[[21]]) %>%   #dengan urutan nama koloom
    summarise(Total_Sales = sum(Sales),
              Total_Profit = sum(Profit)) %>% 
    ungroup()
  
  # Visualisasi
  
  # Proses based input
  # ------------------ Customer Names
  if(input$input_2a_best == "Customer Name" & input$input_2a_met == "Total Sales"){
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Sales) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(Customer.Name, Total_Sales, decreasing = T), y = Total_Sales,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Sales, 2))}"))) + 
      geom_col(aes(fill = Total_Sales)) 
  } 
  
  else if(input$input_2a_best == "Customer Name" & input$input_2a_met == "Total Profit") {
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Profit) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(Customer.Name, Total_Profit, decreasing = T), y = Total_Profit,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Profit, 2))}"))) + 
      geom_col(aes(fill = Total_Profit)) 
  }
  
  # ------------------------ Country
  
  else if(input$input_2a_best == "Country" & input$input_2a_met == "Total Sales"){
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Sales) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(Country, Total_Sales, decreasing = T), y = Total_Sales,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Sales, 2))}"))) + 
      geom_col(aes(fill = Total_Sales)) 
  } 
  
  else if(input$input_2a_best == "Country" & input$input_2a_met == "Total Profit") {
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Profit) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(Country, Total_Profit, decreasing = T), y = Total_Profit,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Profit, 2))}"))) + 
      geom_col(aes(fill = Total_Profit))
  } 
  
  # ------------------------ City
  
  else if(input$input_2a_best == "City" & input$input_2a_met == "Total Sales"){
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Sales) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(City, Total_Sales, decreasing = T), y = Total_Sales,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Sales, 2))}"))) + 
      geom_col(aes(fill = Total_Sales)) 
  } 
  
  else if(input$input_2a_best == "City" & input$input_2a_met == "Total Profit") {
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Profit) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(City, Total_Profit, decreasing = T), y = Total_Profit,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Profit, 2))}"))) + 
      geom_col(aes(fill = Total_Profit)) 
  } 
  
  
  # ------------------------ State
  else if(input$input_2a_best == "State" & input$input_2a_met == "Total Sales"){ 
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Sales) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(State, Total_Sales, decreasing = T), y = Total_Sales,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Sales, 2))}"))) + 
      geom_col(aes(fill = Total_Sales)) 
  } 
  
  else if(input$input_2a_best == "State" & input$input_2a_met == "Total Profit") {
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Profit) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(State, Total_Profit, decreasing = T), y = Total_Profit,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Profit, 2))}"))) + 
      geom_col(aes(fill = Total_Profit)) 
  } 
  
  # ------------------------ Sub Category
  else if(input$input_2a_best == "Sub Category" & input$input_2a_met == "Total Sales"){
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Sales) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(Sub.Category, Total_Sales, decreasing = T), y = Total_Sales,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Sales, 2))}"))) + 
      geom_col(aes(fill = Total_Sales)) 
  } 
  
  else if(input$input_2a_best == "Sub Category" & input$input_2a_met == "Total Profit") {
    plt_top5 <- df_rank %>% filter(year == input$input_2a_year) %>% 
      arrange(-Total_Profit) %>% head(input$input_2a_no) %>% 
      ggplot(aes(x = reorder(Sub.Category, Total_Profit, decreasing = T), y = Total_Profit,
                 text = glue("{input$input_2a_met} : $ {comma(round(Total_Profit, 2))}"))) + 
      geom_col(aes(fill = Total_Profit)) 
  }
  
plt_top5 <-   
  plt_top5 + scale_fill_gradient(low="#EFC000FF", high="red") +
  labs(title = glue("Top {input$input_2a_no} {input$input_2a_best} Based on {input$input_2a_met} in {input$input_2a_year}"),
       y = glue("{input$input_2a_met}"),
       x = NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 25)) 

  
ggplotly(plt_top5, tooltip = "text")



})

  
  # ---------------------- PAGE 2b

# Plot 1

output$page2b_1 <- renderEcharts4r({
  
  
  if(input$input_2b_fil == "All") {
    df_map <- df %>% filter(year == input$input_2b_year)
  } else {
    df_map <- df %>% 
      filter(Category == input$input_2b_fil,
             year == input$input_2b_year)
  }
  
  if(input$input_2b_met == "Total Sales") {
    
    plot_map <- 
      df_map %>%
      group_by(Country) %>% 
      summarise(Total = sum(Sales)) %>% 
      e_charts(Country) %>% 
      e_map(Total) %>% 
      e_visual_map(Total) %>% 
      e_tooltip(trigger = "item",
                formatter = JS("
                                 function(params){return(
                                 '<b>' + params.name 
                                 + ':' + '</b>' 
                                 + ' $' + 
                                 (params.value).toLocaleString('en-US', 
                                 {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                                 )}")) %>% 
      e_title(text = glue("{input$input_2b_met} Distribution from Each Country in {input$input_2b_year} ({input$input_2b_fil})"))
    
  } else if(input$input_2b_met == "Total Profit") {
    plot_map <- 
      df_map %>%
      group_by(Country) %>% 
      summarise(Total = sum(Profit)) %>% 
      e_charts(Country) %>% 
      e_map(Total) %>% 
      e_visual_map(Total) %>% 
      e_tooltip(trigger = "item",
                formatter = JS("
                                 function(params){return(
                                 '<b>' + params.name 
                                 + ':' + '</b>' 
                                 + ' $' + 
                                 (params.value).toLocaleString('en-US', 
                                 {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                                 )}")) %>% 
      e_title(text = glue("{input$input_2b_met} Distribution from Each Country in {input$input_2b_year} ({input$input_2b_fil})"))
    
  } else {
    plot_map <- 
      df_map %>%
      group_by(Country) %>% 
      summarise(Total = sum(Quantity)) %>% 
      e_charts(Country) %>% 
      e_map(Total) %>% 
      e_visual_map(Total) %>% 
      e_tooltip(trigger = "item",
                formatter = JS("
                                 function(params){return(
                                 '<b>' + params.name 
                                 + ':' + '</b>' 
                                 + ' ' + 
                                 (params.value).toLocaleString('en-US', 
                                 {maximumFractionDigits: 2, minimumFractionDigits: 2 })
                                 )}")) %>% 
      e_title(text = glue("{input$input_2b_met} Distribution from Each Country ({input$input_2b_fil})"))
  }
  
#)

plot_map
  
  
  
  
  
})


# PLot 2

output$page2b_2 <- renderHighchart({
  
df_new <- 
    df %>% mutate(
      yearweek = yearweek(Order.Date),
      quarter = quarter(Order.Date, with_year = T),
      semester = semester(Order.Date, with_year = T),
    )
  
  # Data
  
  # -------------- Yearly
  if(input$input_2b_per == "Yearly"){ 
    
    df_3_met <- 
      df_new %>% 
      filter(Country == input$input_2b_country) %>% 
      group_by(year) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(year = as.character(year)) %>% 
      ungroup() 
    
    hc <- 
      highchart() %>%
      hc_xAxis(categories = df_3_met$year)
    
    
  } 
  
  # -------------- Semesterly
  else if(input$input_2b_per == "Semesterly"){
    
    df_3_met <- 
      df_new %>% 
      filter(Country == input$input_2b_country) %>% 
      group_by(semester) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(semester = as.character(semester)) %>% 
      ungroup()
    
    hc <- 
      highchart() %>%
      hc_xAxis(categories = df_3_met$semester)
    
  } 
  
  # -------------- Quarterly
  
  else if(input$input_2b_per == "Quarterly"){
    
    df_3_met <- 
      df_new %>% 
      filter(Country == input$input_2b_country) %>% 
      group_by(quarter) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(quarter = as.character(quarter)) %>% 
      ungroup()
    
    hc <- 
      highchart() %>%
      hc_xAxis(categories = df_3_met$quarter)
    
  } 
  
  # -------------- Monthly
  
  else if(input$input_2b_per == "Monthly"){
    
    df_3_met <- 
      df_new %>% 
      filter(Country == input$input_2b_country) %>% 
      group_by(yearMonth) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(yearMonth = as.character(yearMonth)) %>% 
      ungroup()
    
    hc <- highchart() %>%
      hc_xAxis(categories = df_3_met$yearMonth)
    
  }
  
  # -------------- Weekly
  
  else if(input$input_2b_per == "Weekly"){
    
    df_3_met <- 
      df_new %>% 
      filter(Country == input$input_2b_country) %>% 
      group_by(yearweek) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(yearweek = as.character(yearweek)) %>% 
      ungroup()
    
    hc <- highchart() %>%
      hc_xAxis(categories = df_3_met$yearweek)
    
  } 
  
  # -------------- Daily
  
  else {
  df_3_met <- 
      df_new %>% 
      filter(Country == input$input_2b_country) %>% 
      group_by(Order.Date) %>% 
      summarise(Total_Sales = sum(Sales),
                Total_Profit = sum(Profit),
                Total_Quantity = sum(Quantity)) %>%
      mutate(Order.Date = as.character(Order.Date)) %>% 
      ungroup()
    
    hc <- highchart() %>%
      hc_xAxis(categories = df_3_met$Order.Date)
    
  }
  
hc <- hc %>%       
    hc_chart(type = "line") %>%
    hc_title(text = glue("{input$input_2b_country} Superstore Performance Trends"),
             style = list(color = "black", useHTML = TRUE)) %>%
    hc_subtitle(text = "Jan 2011 - Des 2014",
                style = list(color = "black", useHTML = TRUE)) %>%
    hc_add_series(data = df_3_met$Total_Sales, name = "Total Sales") %>%
    hc_add_series(data = df_3_met$Total_Profit, name = "Total Profit") %>%
    hc_add_series(data = df_3_met$Total_Quantity, name = "Total Quantity") %>% 
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>% 
    hc_colors(c("#0073C2FF", "#EFC000FF", 'red')) %>% 
    hc_tooltip(enabled = TRUE, pointFormat = "{point.y:,.2f}")
  
hc

})


# ---------------------- PAGE 3a


  
            
  # ---------------------- PAGE 3b

## ---- DATASET stock
output$dataset_superstore <- DT::renderDataTable(df, options = list(scrollX=T,
                                                scrollY=T))
  
  
})