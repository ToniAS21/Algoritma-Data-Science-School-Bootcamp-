dashboardPage(skin = "purple",
  
  # ------------------------- Header --------------------------
  dashboardHeader(
                  title = "Sales Dashboard",
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
  
  
  # ------------------------- Sidebar --------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "page1", icon = icon("home")),
      menuItem("Charts", tabName = "page2", icon = icon("chart-line"),
               menuSubItem("Comprehensive",
                           tabName = "page2a",
                           icon = icon("cog", lib = "glyphicon")),
               menuSubItem("Global",
                           tabName = "page2b",
                           icon = icon("globe"))),
      menuItem("Dataset", tabName = "page3", icon = icon("server")),
      menuItem("Closing", tabName = "page4", icon = icon("bullseye"))

    )
    
    
    
    
  ),
  
  
  # ------------------------- Body --------------------------
  dashboardBody(tabItems(
    
    # ---------------------- PAGE 1 ----------------------------
    tabItem(tabName = "page1",
           
            # --------------- ROW 1
            fluidRow(
              tags$head(tags$style(HTML(".small-box {height: 130px}"))),
              valueBox(value = comma(sum(df$Sales)/length(unique(df$yearMonth))), 
                       subtitle = tags$p("Average Sales / Month", style = "font-size: 150%;"),
                       color = "blue", width = 4,
                       icon = icon("chart-bar")),
               valueBox(value = comma(sum(df$Profit)/length(unique(df$yearMonth))), 
                        subtitle = tags$p("Average Profit / Month", style = "font-size: 150%;"),
                        color = "yellow", width = 4,
                       icon = icon("building")),
               valueBox(value = comma(sum(df$Quantity)/length(unique(df$yearMonth))), 
                       subtitle = tags$p("Average Quantity / Month", style = "font-size: 150%;"),
                       color = "red", width = 4,
                       icon = icon("chart-pie"))),
            
            
            # --------------- ROW 2
            h3(strong("1. What is The Best Segment?")),
            fluidRow(box(width = 12, highchartOutput(outputId = "page1_2"))),
            
            # --------------- ROW 3
            h3(strong("2. Which Month has The Best Performance?")),
            fluidRow(
              box(width = 12, highchartOutput(outputId = "page1_1"))),
            
            
            # --------------- ROW 4
            h3(strong("3. What is The Best Category and Ship Mode?")),
           fluidRow(
              box(width = 6, highchartOutput(outputId = "page1_3")),
              box(width = 6, highchartOutput(outputId = "page1_4"))
            )
            
            
            ),
    
    # ---------------------- PAGE 2a ----------------------------
    tabItem(tabName = "page2a",
            
            # -------------- ROW 1
            
            h3(strong("Comprehensive Performance Analysis")),
            fluidRow(
                # --------- Input bagian kiri --------------
              box(width = 4,
                  height = 115,
                  radioButtons(inputId = "input_2a_ri",
                               label = "Based",
                               choices = c("Region",
                                           "Market", 
                                           "Segment"),
                               selected = "Region")),
              
              # ------ Input bagian tengah --------------
              box(width = 4,
                  height = 115,
                  radioButtons(inputId = "input_2a_tg",
                               label = "Metrics",
                               choices = c("Total Sales", 
                                           "Total Profit", 
                                           "Total Quantity"),
                               selected = "Total Sales")),

              # ------ Input bagian kanan --------------
              box(width = 4,
                  height = 115,
                  radioButtons(inputId = "input_2a_an",
                               label = "Grouped",
                               choices = c("Category", "Ship Mode", "None"),
                               selected = "Category"))
            ),
            
            
            # -------------- ROW 2
            fluidRow(
              box(width = 12, highchartOutput(outputId = "page2a_1"))),
            
            # -------------- ROW 3
            
            # ------ Input bagian best
            h3(strong("Top Analysis")),
            fluidRow(
              box(width = 3,
                  height = 100,
                  selectInput(inputId = "input_2a_best",
                               label = "Best",
                               choices = c("Customer Name",
                                           "Country",
                                           "City", 
                                           "State", 
                                           "Sub Category"),
                               selected = "Customer Name")),
              
              # ------ Input bagian metrict
              box(width = 3,
                  height = 100,
                  selectInput(inputId = "input_2a_met",
                               label = "Metrics",
                               choices = c("Total Sales", 
                                           "Total Profit"),
                               selected = "Total Sales")),
              
              # ------ Input bagian top
              box(width = 3,
                  height = 100,
                  sliderInput(inputId = "input_2a_no",
                               label = "Top",
                               value = 5,
                               min = 1,
                               max = 10)),

              # ------ Input bagian year
              box(width = 3,
                  height = 100,
                  selectInput(inputId = "input_2a_year",
                               label = "Year",
                               choices = unique(df$year),
                               selected = 2014))),
            #  PLOT 2
            fluidRow(
              box(width = 12, plotlyOutput(outputId = "page2a_2")))
            
            
            ),
    
    
    
    
    # ---------------------- PAGE 2b ----------------------------
    tabItem(tabName = "page2b",
            
            # -------------- ROW 1
            h3(strong("Performance of Each Country in General")),
            fluidRow(
              box(width = 4,
                  height = 100,
                  selectInput(inputId = "input_2b_fil",
                              label = "Filter",
                              choices = c("All" = "All",
                                          "Furniture Category" = "Furniture", 
                                          "Office Supplies Category" = "Office Supplies", 
                                          "Technology Category" = "Technology"),
                              selected = "All")),
              
              box(width = 4,
                  height = 100,
                  selectInput(inputId = "input_2b_met",
                              label = "Metric",
                              choices = c("Total Sales",
                                          "Total Profit",
                                          "Total Quantity"),
                              selected = "Total Sales")),
              
              box(width = 4,
                  height = 100,
                  selectInput(inputId = "input_2b_year",
                              label = "Year",
                              choices = unique(df$year),
                              selected = 2014))),

            
            # -------------- ROW 2
            
            # PLOT 1
            fluidRow(
              box(width = 12, echarts4rOutput(outputId = "page2b_1"))),
            
            # --------------- Row 3
            h3(strong("Performance of Each Country in Comperhensif")),
            fluidRow(
              box(width = 6,
                  height = 100,
                  selectInput(inputId = "input_2b_country",
                              label = "Country",
                              choices = unique(df$Country),
                              selected = "Indonesia")),
              
              box(width = 6,
                  height = 100,
                  selectInput(inputId = "input_2b_per",
                              label = "Period",
                              choices = c("Yearly",
                                          "Semesterly",
                                          "Quarterly",
                                          "Monthly",
                                          "Weekly",
                                          "Daily"),
                              selected = "Monthly"))),
            
            fluidRow(
              box(width = 12, highchartOutput(outputId = "page2b_2")))
            
            
            ),
    
    
    # ---------------------- PAGE 3 ----------------------------
    tabItem(tabName = "page3",
            h4("Source from Kaggle :", a(href="https://www.kaggle.com/datasets/jr2ngb/superstore-data", "Superstore Data")),
            fluidRow(
              box(width = 12,
                  title = "Data About Superstore",
                  DT::dataTableOutput(outputId = "dataset_superstore")))
            ),
    # ---------------------- PAGE 4 ----------------------------
    tabItem(tabName = "page4",
            p(strong(h2("1. Guide"))),
            h4("When I created this project I realized that the 
               essence of visualization is helping other people 
               to get information from raw data so it is necessary 
               to create simple visualizations. But on the one hand 
               I also realize that sometimes visualization is also 
               needed by experts in finding various kinds of information. 
               Therefore, in this project I tried a visualization that 
               combines the previous two things, namely simple and comprehensive."),
            h4("For readers who feel that the visualization is so complex, 
               you can press the icon like the one below to disable some 
               charts to make it simpler."),
            tags$img(src = "guide.png"),
            h4("For readers who feel that the visualization is very simple, 
               you can also press the icon like the one above to activate 
               several charts to make it more comprehensive."),
            
            # Conclusion text
            p(strong(h2("2. Conclusion"))),
            h4("In this project, I created a sales dashboard that 
               can display the performance of the superstore. This 
               dashboard has a lot of features that can be adjusted 
               according to the needs of its users."),
            h4("This project consists of an Overview Page that 
               describes the general performance of our superstore. 
               Then a Comprehensive Page that contains many features 
               for ranking and comprehensive analysis. Next on the 
               Global Page that contains our superstore performance 
               from each branch country. In addition, there is also 
               a Dataset Page that contains the source and 
               description of the data used."),
            h4("I hope this project can add insight for me and can 
               also provide insight for various people. I'm also 
               happy if readers want to discuss or provide 
               suggestions for improving this project, this can be 
               done via the message icon on the top right of my 
               linkedin contact. I also thank you very much for 
               viewing my project.")
            
            
    )
    
    
    
  )
    
    
  )
  
  
  
)