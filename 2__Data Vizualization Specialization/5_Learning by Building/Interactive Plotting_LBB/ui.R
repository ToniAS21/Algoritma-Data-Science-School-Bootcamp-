dashboardPage(skin = "purple",
  
  # --- Header
  dashboardHeader(title = "Stock Dashboard",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Created by :",
                                 message = "Toni Andreas Susanto"
                               ),
                               messageItem(
                                 from = "Toni" ,
                                 message = "connected to me?",
                                 icon =  icon("linkedin", "fa-2x"),
                                 href="https://www.linkedin.com/in/toni-andreas-s"
                               ),
                               messageItem(
                                 from = "Toni Andreas Susanto",
                                 message = "Thank you for viewing this dashboard",
                                 icon = icon("smile"),
                                 time = today()
                               ))
            
                  ),
  
  
  # --- SideBar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("home")),
      menuItem("Overview", tabName = "page2", icon = icon("bullhorn")),
      menuItem("Charts", tabName = "page3", icon = icon("chart-line"),
               menuSubItem("Basic",
                           tabName = "basic"),
               menuSubItem("Expert",
                           tabName = "expert")),
      menuItem("Dataset", tabName = "page4", icon = icon("server"))
               
      
               )
                  ),

  
  # --- Body
  dashboardBody(tabItems(
    
    # ------- Page 1 ---------
    tabItem(tabName = "page1",
                   h5("Announcement : This dashboard only contains the stocks in the Dow Jones Index."),
                   
                   p(strong(h1("Dow Jones Industrial Average (DJIA)"))),
                   p(strong(h3("What Is the Dow Jones Industrial Average (DJIA)?"))),
                   h4("The Dow Jones Industrial Average (DJIA), also known as the Dow 30, 
               is a stock market index that tracks 30 large, publicly-owned blue-chip 
               companies trading on the New York Stock Exchange (NYSE) and Nasdaq. 
               The Dow Jones is named after Charles Dow, who created the index in 1896 along 
               with his business partner Edward Jones."),
               p(strong(h3("Understanding the Dow Jones Industrial Average (DJIA)"))),
               h4('The DJIA is the second-oldest U.S. market index; the first was the 
               Dow Jones Transportation Average (DJTA). The DJIA was designed to 
               serve as a proxy for the health of the broader U.S. economy. Often 
               referred to simply as "the Dow," the DJIA is one of the most-watched 
               stock market indexes in the world. While the Dow includes a range of 
               companies, all can be described as blue-chip companies with consistently 
               stable earnings.'),
               h4("When the index initially launched in 1896, it included only 12 companies. 
               Those companies were primarily in the industrial sector, including railroads, 
               cotton, gas, sugar, tobacco, and oil."),
               h4("In the early 20th century, the performance of industrial companies was typically 
               tied to the overall growth rate in the economy. That cemented the relationship 
               between the Dow's performance and the overall economy. Even today, for many 
               investors, a strong-performing Dow equals a strong economy (while a weak-performing 
               Dow indicates a slowing economy)."),
               h4("As the economy changes over time, so does the composition of the index. A component 
               of the Dow may be dropped when a company becomes less relevant to current trends of 
               the economy, to be replaced by a new name that better reflects the shift."),
               h4("A company that loses a large percentage of its market capitalization due to financial 
               distress might be removed from the Dow. Market capitalization is a method of measuring 
               the value of a company by multiplying the number of shares outstanding by its stock price."),
               h4("Stocks with higher share prices are given greater weight in the index. So a higher percentage 
               move in a higher-priced component will have a greater impact on the final calculated value. 
               At the Dow's inception, Charles Dow calculated the average by adding the prices of the twelve 
               Dow component stocks and dividing by twelve. The result was a simple average. Over time, there 
               have been additions and subtractions to the index, such as mergers and stock splits that had to 
               be accounted for. At that point, a simple mean calculation no longer made sense"),
               p(strong(h3("Dow Index Components"))),
               h4("The Dow is often re-evaluated to replace companies that no longer meet the 
               listing criteria with those that do. By 1928, the index had grown to 30 components. 
               Its composition has changed many times since then."),
               p(strong(h3("Limitations of the DJIA"))),
               h4("Many critics of the Dow argue that it does not significantly represent the state of 
               the U.S. economy as it consists of only 30 large-cap U.S. companies. They believe the 
               number of companies is too small and it neglects companies of different sizes. Many 
               critics believe the S&P 500 is a better representation of the economy as it includes 
               significantly more companies, 500 versus 30."),
               h4("Furthermore, critics believe that factoring only the price of a stock in the calculation 
               does not accurately reflect a company, as much as considering a company's market cap would. 
               In this manner, a company with a higher stock price but a smaller market cap would have more 
               weight than a company with a smaller stock price but a larger market cap, which would poorly 
               reflect the true size of a company."),
               br(),
               h4("Reference :", a(href="https://www.investopedia.com/terms/d/djia.asp", "Dow Jones Industrial Average (DJIA)"))
              
              
              
            
            
            
            
            
            
            
            
            ),
    
    
    
    # ------- Page 2 ---------
    tabItem(tabName = "page2",
            # Row 1
            
            fluidRow(
              valueBox(value = "US Stock", subtitle = tags$p("Dow Jones Index", style = "font-size: 150%;"),
                       color = "aqua", width = 4,
                       icon = icon("chart-bar")),
              valueBox(value = length(unique(dow_companies$Shortname)), subtitle = tags$p("Companies", style = "font-size: 150%;"),
                       color = "yellow", width = 3,
                       icon = icon("building")),
              valueBox(value = "Period :", subtitle = tags$p("01 Jan 2018 to 27 Jul 2022", style = "font-size: 150%;"),
                       color = "red", width = 5,
                       icon = icon("calendar-alt"))
            ),

            
            # --- Row 2
            fluidRow(
              box(width = 12,
                  highchartOutput(outputId = "plot1_2"))),
            
            
            
            h5('Note : Market Capitalization  or "market cap", is the aggregate 
               market value of a company represented in a dollar amount. Since 
               it represents the “market” value of a company, it is computed based 
               on the current market price (CMP) of its shares and the total number 
               of outstanding shares.'),
            
            # RoW 3
            fluidRow(
              box(width = 12,
                  plotlyOutput(outputId = "plot2_2"))),
            
            h5('Note : EBITDA, or "earnings before interest, taxes, depreciation, and amortization", 
               is a measure of a company’s overall financial performance and is used as an alternative 
               to net income in some circumstances. '),
            
            fluidRow(
              box(width = 12,
                  plotlyOutput(outputId = "plot3_2")))
            
            ),
    
    
    
    # ------- Page 3 ---------
    
    # --- SUB BASIC
    tabItem(tabName = "basic",
            
          
            fluidRow(
              #  Row 1 Input company
              box(width = 4,
                  height = 95,
                  selectInput(inputId = "input_company",
                              label = "Choose Company",
                              selected = "AAPL",
                              choices = unique(cl_dow_stock18$Symbol))),
              
              # Row 1 Input Date
              box(width = 6,
                  height = 95,
                  dateRangeInput(inputId = "input_date", 
                                 label = "Choose Date",
                                 start = "2022-06-01",
                                 end = "2022-07-01",
                                 min = "2018-01-01",
                                 max = "2022-07-27")),
              # Row 1 Radio Button
              box(width = 2,
                  height = 95,
                  radioButtons(inputId = "input_radio",
                               label = "Option",
                               choices = c("Marker", "None"),
                               selected = "Marker"))
                    ),
            
            
            
            # Row 2
            fluidRow(
              box(width = 12,
                  plotlyOutput(outputId = "plot1_3"))
                    ),
            # Row 3
            fluidRow(
              box(width = 12,
                  plotlyOutput(outputId = "plot2_3"))
                    ),
            
            fluidRow(
              box(width = 12,
                  plotlyOutput(outputId = "plot3_3"))
                    )
            
            ),
    # --- SUB Expert
    
    tabItem(tabName = "expert",
  
            fluidRow(
              #  Row 1 Input company
              box(width = 4,
                  height = 95,
                  selectInput(inputId = "input_companyxp",
                              label = "Choose Company",
                              selected = "AAPL",
                              choices = unique(cl_dow_stock18$Symbol))),
              
              # Row 1 Input Date
              box(width = 8,
                  height = 95,
                  dateRangeInput(inputId = "input_datexp", 
                                 label = "Choose Date",
                                 start = "2022-06-01",
                                 end = "2022-07-01",
                                 min = "2018-01-01",
                                 max = "2022-07-27"))
            ),
            
           fluidRow(box(width = 12,
                         plotlyOutput(outputId = "plot3e_1"))),
           
           # Descripstion
           br(),
           h2("Keyword:"),
           
           p(strong(h3("1. What Is A Candlestick?"))),
           h4('A candlestick is a type of price chart used in technical 
              analysis that displays the high, low, open, and closing 
              prices of a security for a specific period. It originated 
              from Japanese rice merchants and traders to track market 
              prices and daily momentum hundreds of years before becoming 
              popularized in the United States. The wide part of the candlestick 
              is called the "real body" and tells investors whether the 
              closing price was higher or lower than the opening price 
              (black/red if the stock closed lower, white/green if the 
              stock closed higher),', a(href = "https://www.investopedia.com/terms/c/candlestick.asp", "more information.")),
           p(strong(h3("2. What Is a Bollinger Band?"))),
           h4("A Bollinger Band is a technical analysis tool defined 
              by a set of trendlines plotted two standard deviations 
              (positively and negatively) away from a simple moving 
              average (SMA) of a security's price, but which can be 
              adjusted to user preferences. Bollinger Bands® were 
              developed and copyrighted by famous technical trader 
              John Bollinger, designed to discover opportunities that 
              give investors a higher probability of properly 
              identifying when an asset is oversold or overbought,", 
              a(href = "https://www.investopedia.com/terms/b/bollingerbands.asp", "more information.")),
            p(strong(h3("3. What Is a Moving Average (MA)?"))),
            h4("In statistics, a moving average is a calculation used to analyze 
               data points by creating a series of averages of different subsets 
               of the full data set. In finance, a moving average (MA) is a stock 
               indicator that is commonly used in technical analysis. The reason 
               for calculating the moving average of a stock is to help smooth 
               out the price data by creating a constantly updated average price,", 
               a(href = "https://www.investopedia.com/terms/m/movingaverage.asp", "more information."))
           
           
      
      
    ),
    
    tabItem(tabName = "page4",
            h4("Source from Kaggle :", a(href="https://www.kaggle.com/datasets/andrewmvd/sp-500-stocks", "S&P 500 Stocks (daily updated)")),
            fluidRow(
              box(width = 12,
                  title = "Data About Stock",
                  DT::dataTableOutput(outputId = "dataset_stock"))),
            
            
            
            )
    
    
                    )
    
    
    
  )
)
