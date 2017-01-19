
# SMS Analysis ------------------------------------------------------------


# ---- The Setup ----

# Install these packages if you haven't already
list.of.packages <- c("dplyr", "tidyr", "readr", "lubridate", "stringr", "tibble", "forcats",
                      "ggplot2", "viridis", "ggthemes", 
                      "plotly","pacman",
                      "shiny", "shinydashboard", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# --- Packages ---
pacman::p_load(dplyr, tidyr, readr, lubridate, stringr, tibble, forcats,
               ggplot2, viridis, ggthemes,
               plotly,
               shiny, shinydashboard, DT)



# ---- Loading Data ----
SMS_Analysis <-

  # - Original Data -
  # read_csv(list.files(pattern = "Full SMS.csv")) %>% tbl_df()

  # - Anonymous Data -
  # read_csv(list.files(pattern = "Anon SMS.csv")) %>% tbl_df()

  # - Load in RDS file -
  readRDS("Anonymous SMS.RDS")

# Date Range
Data_Date_Range <-
  SMS_Analysis %>%
  distinct(Date) %>%
  summarize(Max = max(as.Date(Date)),
            Min = min(as.Date(Date)))




# ---- Dashboard Header ----
Header <-
  dashboardHeader(title = "Message Exploration")




# ---- Dashboard Sidebar ----
Sidebar <- dashboardSidebar(

  # * Sidebar Links ----
  sidebarMenu(

    menuItem(
      "Descriptives",
      tabName = "Descriptives"
    ),

    menuItem(
      "Top Contacts",
      tabName = "Top_Contacts"
    ),

    menuItem(
      "Selected Contact",
      tabName = "Selected_Contact"
    ),

    menuItem(
      "Messages Sent",
      tabName = "Messages_Sent"
    )

  ),




  # * Sidebar Inputs ----


  # Selected Contact
  selectInput("Contact_Select",
              "Select Contact",
              choices = "None Selected"),


  # Number of Top Contacts
  sliderInput("Top_Contacts_N", label = "Number of Top Contacts",
              min = 10, max = 40, value = 20),


  # Date Slider
  dateRangeInput(
    "Slider_Date_Range",
    label = "Date Range",
    start = Data_Date_Range$Min,
    end = Data_Date_Range$Max,
    startview = "year"),


  # Date Buttons
    column(10,
           align = "center",
           offset = 1,
           actionButton("Default_Dates",
                        label = "All Dates",
                        width = "100%")),

    column(10,
           align = "center",
           offset = 1,
           actionButton("Last_Month",
                        width = "100%",
                        label = "Last Month")),

    column(10,
           align = "center",
           offset = 1,
           actionButton("Three_Months",
                        width = "100%",
                        label = "Last Three Months")),

    column(10,
           align = "left",
           offset = 1,
           actionButton("Six_Months",
                        width = "100%",
                        label = "Last Six Months"))
)




# Dashboard Body ----
Body <- dashboardBody(

  tabItems(

    # * Descriptives ----
    tabItem(tabName = "Descriptives",


            # First Row
            fluidRow(
              infoBoxOutput("Date_Updated"),
              infoBoxOutput("Days_Recorded"),
              infoBoxOutput("Contacts_Recorded")
            ),


            # Second Row
            fluidRow(
              valueBoxOutput("Texts_Number"),
              valueBoxOutput("Sent_Number"),
              valueBoxOutput("Received_Number")
            ),


            # Third Row
            dataTableOutput("Data_Descriptives")
    ),



    # * Top Contacts ----
    tabItem(tabName = "Top_Contacts",


            # First Row
            fluidRow(
              box(
                plotlyOutput("Line_Chart"),
                width = 12,
                title = "Message Count by Day",
                collapsible = T
              )
            ),


            # Second Row
            fluidRow(

              # First Column
              tabBox(width = 9,


                     # Panel 1
                     tabPanel(
                       "Sent vs Received",
                       plotlyOutput("Bar_Charts", height = 700)),


                     # Panel 2
                     tabPanel(
                       "Date Ranges",
                       plotOutput("Range_Chart", height = 650)),


                     # Panel 3
                     tabPanel(
                       "Differences",

                       # Hover Overlay
                       div(
                         style = "position:relative",
                         plotOutput("Difference_Chart",
                                    height = 650,
                                    hover = hoverOpts("plot_hover",
                                                      delay = 100,
                                                      delayType = "debounce")),
                         uiOutput("hover_info")))

                     ),

              # Second Column
              box(
                width = 3,
                solidHeader = T,
                title = "Top Contacts",
                div(style = "font-size:90%",
                    dataTableOutput("Data_Top_Contacts")))

            )
    ),



    # * Selected Contact ----
    tabItem(tabName = "Selected_Contact",


            # First Row
            fluidRow(


              # First Column
              valueBoxOutput("Info_Selected_Contact",
                            width = 3),


              # Second Column
              box(
                width = 9,
                collapsible = TRUE,
                title = "Contact Details",
                div(style = "font-size:100%",
                    dataTableOutput("Data_Contact_Details")))

            ),


            # Second Row
            fluidRow(


              # First Column
              box(
                width = 4,
                collapsible = T,
                title = "Sent and Received",
                plotOutput("Pie_Chart",
                           height = 200,
                           width = "100%")),


              # Second Column
              box(
                width = 8,
                collapsible = T,
                title = "Messages per Day",
                plotOutput("Echo_Chart",
                           height = 150,
                           width = "100%"))
            ),


            # Third Row
            fluidRow(
              tabBox(
                width = 12,
                title = "Messages by Week",
                selected = "Message Length",


                # First Panel
                tabPanel("Message Length",
                         plotOutput("Line_Chart_Single_Count")),


                # Second Panel
                tabPanel("Mean Message Length",
                         plotOutput("Line_Chart_Single_Length")))
            ),

            # Fourth Row
            fluidRow(
              tabBox(
                width = 12,
                title = "Weekday by Hour",


                # First Panel
                tabPanel("Heatmap",
                         plotOutput("Heatmap",
                                    height = 500)),


                # Second Panel
                tabPanel("Scatter Plot",
                         plotOutput("Hour_Scatter",
                                    height = 350)),


                # Third Panel
                tabPanel("Line Plot",
                         plotOutput("Hour_Line",
                                    width = 500,
                                    height = 500)))
            )
    ),



    # * Me ----
    tabItem(tabName = "Messages_Sent",

            # First Row
            fluidRow(

              valueBoxOutput("Info_Me_Count",
                             width = 3),

              valueBoxOutput("Info_Me_Mean_Count_Daily",
                             width = 3),

              valueBoxOutput("Info_Me_Mean_Length_Daily",
                             width = 3),

              valueBoxOutput("Info_Me_Mean_Message_Length",
                             width = 3)
            ),


            # Second Row
            fluidRow(

              box(plotOutput("Me_Line"),
                  width = 12,
                  title = "Sent Messages",
                  collapsible = T,
                  collapsed = T)

            ),


            # Third Row
            fluidRow(

              box(plotOutput("Me_Overtime",
                             height = 500),
                  width = 12,
                  title = "Measures Over Time",
                  collapsible = T,
                  collapsed = T)

            ),


            # Fourth Row
            fluidRow(

              box(plotOutput("Me_Heatmap"),
                  width = 12,
                  title = "Message Count",
                  collapsible = T,
                  collapsed = F))
    )
  )
)



# --- Create UI ---
UI <- dashboardPage(Header, Sidebar, Body)




# Server ------------------------------------------------------------------

Server <- function(input, output, session) {

  # Action: Update Dates ----
  observeEvent(
    input$Last_Month, {
      updateDateRangeInput(
        session,
        "Slider_Date_Range",
        label = "Date Range",
        start = floor_date(floor_date(Data_Date_Range$Max, "month") - 1, "month"),
        end = floor_date(Data_Date_Range$Max, "month") - 1
    )
  })

  # Default Dates ---
  observeEvent(
    input$Default_Dates, {
      updateDateRangeInput(
        session,
        "Slider_Date_Range",
        label = "Date Range",
        start = Data_Date_Range$Min,
        end = Data_Date_Range$Max
      )
    })



  # Three Months ---
  observeEvent(
    input$Three_Months, {
      updateDateRangeInput(
        session,
        "Slider_Date_Range",
        label = "Date Range",
        start = floor_date(Data_Date_Range$Max %m-% months(3), "month"),
        end = floor_date(Data_Date_Range$Max, "month") %m-% days(1)
      )
    })

  # Six Months ---
  observeEvent(
    input$Six_Months, {
      updateDateRangeInput(
        session,
        "Slider_Date_Range",
        label = "Date Range",
        start = floor_date(Data_Date_Range$Max %m-% months(6), "month"),
        end = floor_date(Data_Date_Range$Max, "month") %m-% days(1)
      )
    })



  # Data: Raw Date Filter ----
  Data_Raw_Date_Filter <-
    reactive({
      SMS_Analysis %>%
        filter(Date >= input$Slider_Date_Range[1],
               Date <= input$Slider_Date_Range[2])
    })



  # Data: Full Dates ----
  Data_Full_Dates <- reactive({
    data_frame(
      Day = seq.Date(input$Slider_Date_Range[1],
                     input$Slider_Date_Range[2],
                     "days"),
      Week = floor_date(Day, "week"),
      Month = floor_date(Day, "month")
    )
  })



  # Data: Descriptives ----
  Data_Descriptives <-
    reactive({

      Data_Raw_Date_Filter() %>%
        group_by(Contact, Type) %>%
        summarise(
          Count = n(),
          Length = str_count(Text) %>% sum(),
          Mean_Length = str_count(Text) %>% mean() %>% round(digits = 2)
        ) %>%

        full_join(
          Data_Raw_Date_Filter() %>%
            mutate(Date = floor_date(Date, unit = "days")) %>%
            distinct(Date, Contact) %>%
            count(Contact) %>%
            rename(Days_Active = n)
        ) %>%

        mutate(Messages_per_Day = round(Count / Days_Active, 2)) %>%

        full_join(
          Data_Raw_Date_Filter() %>%
            mutate(Day = floor_date(Date, "day")) %>%
            group_by(Contact, Type) %>%
            summarise(First_Contact = format(min(Day), "%Y-%m-%d"),
                      Last_Contact = format(max(Day), "%Y-%m-%d"))
        )
    })

  # Data: Top Contacts ----
  Data_Top_Contacts <- reactive({
    Data_Raw_Date_Filter() %>%
      count(Contact) %>%
      arrange(desc(n)) %>%
      top_n(input$Top_Contacts_N) %>%
      left_join(Data_Descriptives()) %>%
      select(Contact,
             Days_Active,
             Total = n,
             Type,
             Count,
             Length,
             Mean_Length)
  })

  # Data: Daily ----
  Data_Daily <- reactive({
    Data_Raw_Date_Filter() %>%
      mutate(Date = as.Date(floor_date(Date, unit = "day")),
             Text_Length = str_count(Text)) %>%
      group_by(Date, Contact) %>%
      summarise(Count = n(),
                Length = sum(Text_Length),
                Mean_Length = mean(Text_Length) %>% round(digits = 2)) %>%
      right_join(Data_Top_Contacts() %>%
                 distinct(Contact))
  })


  # Data: Difference ----
  Data_Difference <- reactive({
    Data_Top_Contacts() %>%
      gather(Attribute, Value, Count:Mean_Length) %>%
      arrange(Contact) %>%
      mutate(Attribute_2 = str_c(Attribute, Type, sep = "_")) %>%
      select(Contact, Attribute_2, Value) %>%
      spread(Attribute_2, Value) %>%
      mutate(Count_Difference = Count_Sent - Count_Received,
             Length_Difference = Length_Sent - Length_Received,
             Mean_Length_Difference = Mean_Length_Sent - Mean_Length_Received)
  })


  # Data: Order ----
  Data_Order_Date <- reactive({
    Data_Raw_Date_Filter() %>%
      distinct(Contact) %>%
      rownames_to_column("Date_Order_A") %>%
      mutate(Date_Order_D = rev(Date_Order_A))
  })


  Data_Order_Rank <- reactive({
    Data_Top_Contacts() %>%
      distinct(Contact) %>%
      rownames_to_column("Rank_Order_A") %>%
      mutate(Rank_Order_D = rev(Rank_Order_A))
  })


  Data_Order <- reactive({
    full_join(Data_Order_Date(), Data_Order_Rank()) %>%
      group_by(Contact) %>%
      mutate_all(as.numeric) %>%
      select(Contact, contains("Date"), contains("Rank"))
  })

  # Data: Daily Top ----
  Data_Daily_Top <- reactive({
    Data_Daily() %>%
    group_by(Date, Contact) %>%
    summarise(Count = sum(Count),
              Length = sum(Length)) %>%
    right_join(Data_Order()) %>%
    filter(!is.na(Rank_Order_A))
  })



  # Data: Contact Daily ----
  Data_Contact_Daily <- reactive({
    Data_Daily() %>%
      ungroup() %>%
      filter(Contact == input$Contact_Select) %>%
      right_join(Data_Full_Dates() %>% select(Day), by = c("Date" = "Day")) %>%
      complete(Date, Type, fill = list(Count = 0,
                                       Length = 0,
                                       Mean_Length = 0)) %>%
      arrange(Date)
  })

  # Data: Echo Chart ----
  Data_Echo_Chart <- reactive({
    Data_Raw_Date_Filter() %>%
      filter(Contact == input$Contact_Select) %>%
      mutate(Date = as.Date(floor_date(Date, unit = "day"))) %>%
      group_by(Date, Contact) %>%
      summarise(Count = n()) %>%
      right_join(Data_Full_Dates() %>% select(Day), by = c("Date" = "Day")) %>%
      arrange(Date) %>%
      replace_na(replace = list(Count = 0))
  })

  # Data: Single Line Chart ----
  Data_Single_Line_Chart <- reactive({

    # -- Weekly --
    # Data_Raw_Date_Filter() %>%
    #   filter(Contact == input$Contact_Select) %>%
    #   mutate(Date = as.Date(floor_date(Date, unit = "week")),
    #          Text_Length = str_count(Text)) %>%
    #   group_by(Date, Contact, Type) %>%
    #   summarise(Count = n(),
    #             Length = sum(Text_Length),
    #             Mean_Length = mean(Text_Length) %>% round(digits = 2)) %>%
    #   ungroup() %>%
    #   right_join(Data_Full_Dates() %>% select(Week), by = c("Date" = "Week")) %>%
    #   complete(Date, Type, fill = list(Count = 0,
    #                                    Length = 0,
    #                                    Mean_Length = 0)) %>%
    #   arrange(Date) %>%
    #   distinct()

    # -- Daily --
    Data_Raw_Date_Filter() %>%
      filter(Contact == input$Contact_Select) %>%
      mutate(Date = as.Date(floor_date(Date, unit = "day")),
             Text_Length = str_count(Text)) %>%
      group_by(Date, Contact, Type) %>%
      summarise(Count = n(),
                Length = sum(Text_Length),
                Mean_Length = mean(Text_Length) %>% round(digits = 2)) %>%
      ungroup() %>%
      right_join(Data_Full_Dates() %>% select(Day), by = c("Date" = "Day")) %>%
      complete(Date, Type, fill = list(Count = 0,
                                       Length = 0,
                                       Mean_Length = 0)) %>%
      arrange(Date) %>%
      distinct()
  })


  # Data: Heatmap / Boxplot ----
  Data_Heatmap_Boxplot <- reactive({
    Data_Raw_Date_Filter() %>%
      filter(Contact == input$Contact_Select) %>%
      transmute(
        Day = floor_date(Date, unit = "days"),
        Weekday = wday(Date, label = T, abbr = F),
        Time = hour(Date) + minute(Date) / 60,
        Hour = hour(Date),
        Type,
        Length = str_count(Text))
  })

  # Data: Me ----
  Data_Me <- reactive({

    Data_Raw_Date_Filter() %>%
      filter(Type == "Sent") %>%
      mutate(Date = floor_date(Date, "days") %>% as.Date()) %>%
      group_by(Date, Type) %>%
      summarise(
        Message_Count = n(),
        Message_Length = str_count(Text) %>% sum(),
        Mean_Length = str_count(Text) %>% mean() %>% round(digits = 2)
      ) %>%
      full_join(
        Data_Raw_Date_Filter() %>%
          filter(Type == "Sent") %>%
          mutate(Date = floor_date(Date, "day") %>% as.Date()) %>%
          distinct(Date, Contact) %>%
          group_by(Date) %>%
          summarise(Contacts = n())
      ) %>%
      select(Date, Type, Contacts,
             Message_Count, Message_Length, Mean_Length) %>%
      right_join(Data_Full_Dates() %>% select(Day),
                 by = c("Date" = "Day")) %>%
      replace_na(
        list(Type = "Sent",
             Contacts = 0,
             Message_Count = 0,
             Message_Length = 0,
             Mean_Length = 0))

  })

  # Data: Me Descriptive ----
  Data_Me_Descriptive <- reactive({

    Data_Me() %>%
      filter(Type == "Sent") %>%
      summarise(Total_Count = sum(Message_Count),
                Mean_Count_Daily = mean(Message_Count),
                Mean_Length_Daily = mean(Message_Length),
                Mean_Message_Length = mean(Mean_Length))

  })


  # Menu: Filter Contact ----
  observe({
    input$Top_Contacts_N

    updateSelectInput(session,
                      "Contact_Select",
                      choices = c("None Selected",
                                  Data_Top_Contacts() %>% distinct(Contact))

    )
  })


  # Value: Top N Order ----
  Order <- reactive({
    Data_Top_Contacts() %>% distinct(Contact)
  })



  # * Data View: Placeholder ----
  # output$Placeholder <- renderDataTable({
  #   datatable(
  #     SMS_Me_Hourly_Heatmap()
  #   )
  # })



  # Data View: Descriptives ----
  output$Data_Descriptives <- renderDataTable({
    datatable(Data_Descriptives(),
              caption = "Contact Details",
              options = list(pageLength = 10),
              colnames = c("Contact", "Message Type", "Message Count",
                           "Message Length", "Mean Message Length",
                           "Days Active", "Messages per Day",
                           "First Contact", "Last Contact"))
  })

  # Data View: Top Contacts ----
  output$Data_Top_Contacts <- renderDataTable({
    datatable(Data_Top_Contacts() %>% distinct(Contact, Total),
              rownames = T,
              options = list(paging = F,
                             searching = F))
  })

  # Data View: Contact Details ----
  output$Data_Contact_Details <- renderDataTable({
    datatable(
      Data_Descriptives() %>%
        filter(Contact == input$Contact_Select) %>%
        ungroup() %>%
        select(-Contact),

      rownames = F,
      options = list(paging = F,
                     searching = F),
      colnames = c("Message Type", "Message Count",
                   "Message Length", "Mean Message Length",
                   "Days Active", "Messages per Day",
                   "First Contact", "Last Contact")
    )
  })



  # Value View: Date Updated ----
  output$Date_Updated <- renderInfoBox({
    infoBox(
      "Date Updated",
      value = Data_Date_Range$Max,
      icon = icon("download"),
      color = "olive"
    )
  })

  # Value View: Contacts Recorded ----
  output$Contacts_Recorded <- renderInfoBox({
    infoBox(
      "Contacts Recorded",
      value = Data_Raw_Date_Filter() %>% distinct(Contact) %>% nrow(),
      icon = icon("users"),
      color = "olive"
    )
  })

  # Value View: Days Recorded ----
  output$Days_Recorded <- renderInfoBox({
    infoBox(
      "Days Recorded",
      value = Data_Raw_Date_Filter() %>%
        mutate(Date = as.Date(Date)) %>%
        distinct(Date) %>%
        nrow(),
      icon = icon("calendar"),
      color = "olive"
    )
  })

  # Value View: Text Count ----
  output$Texts_Number <- renderValueBox({
    valueBox(
      "Total Messages",
      value = Data_Raw_Date_Filter() %>% nrow() %>%
        formatC(format = "d", big.mark = ","),
      icon = icon("list"),
      color = "blue"
    )
  })

  # Value View: Sent Count ----
  output$Sent_Number <- renderValueBox({
    valueBox(
      "Messages Sent",
      value = Data_Raw_Date_Filter() %>% filter(Type == "Sent") %>% nrow() %>%
        formatC(format = "d", big.mark = ","),
      icon = icon("sign-out"),
      color = "blue"
    )
  })

  # Value View: Received Count ----
  output$Received_Number <- renderValueBox({
    valueBox(
      "Messages Received",
      value = Data_Raw_Date_Filter() %>% filter(Type == "Received") %>% nrow() %>%
        formatC(format = "d", big.mark = ","),
      icon = icon("sign-in"),
      color = "blue"
    )
  })

  # Value View: Selected Contact ----
  output$Info_Selected_Contact <- renderValueBox({
    infoBox(
      "Selected Contact",
      value = input$Contact_Select,
      icon = icon("user"),
      color = "olive"
    )
  })

  # Value View: Me Count ----
  output$Info_Me_Count <- renderValueBox(

    valueBox("Messages Sent",
            value =
              Data_Me_Descriptive() %>%
              transmute(Total_Count %>% formatC(format = "d", big.mark = ",")),
            icon = icon("sign-out"),
            color = "blue")

  )

  output$Info_Me_Mean_Count_Daily <- renderValueBox(

    valueBox("Messages per Day",
             value =
               Data_Me_Descriptive() %>%
               transmute(Mean_Count_Daily %>% round(digits = 2)),
             color = "blue",
             icon = icon("envelope"))

  )

  output$Info_Me_Mean_Length_Daily <- renderValueBox(

    valueBox("Characters per Day",
             value =
               Data_Me_Descriptive() %>%
               transmute(Mean_Length_Daily %>% round(digits = 2)),
             color = "blue",
             icon = icon("edit"))

  )

  output$Info_Me_Mean_Message_Length <- renderValueBox(

    valueBox("Characters per Message",
             value = Data_Me_Descriptive() %>%
               transmute(Mean_Message_Length %>% round(digits = 2)),
             color = "blue",
             icon = icon("edit"))

  )



  # Plotly: Top - Bar Charts ----

  output$Bar_Charts <- renderPlotly({
    Bar_Chart_Base <-
      Data_Top_Contacts() %>%

      group_by(Contact) %>%
      mutate(Prop_Count = round(Count / sum(Count), 2),
             Prop_Length = round(Length / sum(Length), 2)) %>%
      ungroup() %>%

      plot_ly(
        y = ~Contact,
        color = ~Type,
        colors = c("#FF6347", "#4682B4")) %>%

      layout(barmode = "stack",
             yaxis = list(title = "",
                          tickangle = 0,
                          categoryorder = "trace",
                          autorange = "reversed"),
             margin = list(l = 150,
                           b = 100,
                           pad = 10))

    subplot(
      Bar_Chart_Base %>% add_bars(x = ~Count) %>%
        layout(xaxis = list(title = "Count")),

      Bar_Chart_Base %>% add_bars(x = ~Length) %>%
        layout(xaxis = list(title = "Length")),
      shareX = FALSE,
      shareY = TRUE,
      titleX = T,
      nrows = 1
    )

  })


  # Plotly: Top - Line Charts ----
  output$Line_Chart <- renderPlotly({

    Plotly_Line_Group <-
      Data_Daily() %>%
      ungroup() %>%
      group_by(Contact) %>%
      plot_ly(x = ~Date, y = ~Count) %>%
      add_lines(alpha = 0.2,
                name = "All Contacts",
                hoverinfo = "none")

    Plotly_Line_Group %>%
      filter(Contact == input$Contact_Select) %>%
      add_lines(name = input$Contact_Select,
                line = list(shape = "spline")) %>%
      layout(showlegend = F,
             title = str_c("Selected Contact: ", input$Contact_Select))

  })



  # ggPlot: Top - Range Chart ----

  output$Range_Chart <- renderPlot(
    Data_Daily_Top() %>%
      ggplot(aes(x = Date, y = reorder(Contact, Date_Order_D))) +
      geom_point(shape = 15,
                 alpha = .4) +
      scale_x_date(date_labels = "%b '%y",
                   date_minor_breaks = "1 month",
                   position = "top",
                   name = NULL) +
      scale_y_discrete(name = NULL) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_line(linetype = 1, color = "gray"),
        panel.grid.major.x = element_line(linetype = 3, color = "black"),
        panel.grid.minor.x = element_line(linetype = 3, color = "gray"),
        axis.line.y = element_line(linetype = 1),
        plot.margin = unit(c(.5, 1, .5, .5), "cm"),
        axis.text = element_text(size = rel(1.1)),
        axis.title = element_text(size = rel(1.1))
      )
  )



  # ggPlot: Top - Difference Chart ----
  output$Difference_Chart <- renderPlot({

    # GG_Difference <-
      Data_Difference() %>%

      ggplot(aes(x = Mean_Length_Received, y = Mean_Length_Sent,
                 fill = abs(Count_Difference), size = abs(Length_Difference),
                 text = Contact)) +
      geom_point(shape = 22, alpha = .75) +
      scale_fill_viridis(name = "Count \nDifference", direction = -1, option = "plasma") +
      scale_size(name = "Length \nDifference", range = c(2, 12)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +

      theme_minimal() +
      labs(
        x = "Received",
        y = "Sent",
        title = "Average Number of Characters per Message"
      ) +
      theme(
        axis.text = element_text(size = rel(1.1)),
        axis.title.x = element_text(size = rel(1.1),
                                    face = "bold"),
        axis.title.y = element_text(size = rel(1.1),
                                    face = "bold")
      )

    # GG_Difference_Count %>% ggplotly()
  })

  # * ggPlot: Hover Info ----
  output$hover_info <- renderUI({

    hover <- input$plot_hover

    point <- nearPoints(Data_Difference(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)

    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)


    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)


    # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
    style <- str_c("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")


    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(str_c("<b> Contact: </b>", point$Contact, "<br/>",
                   "<b> Mean Received Length: </b>", point$Mean_Length_Received, "<br/>",
                   "<b> Mean Sent Length: </b>", point$Mean_Length_Sent, "<br/>",
                   "<b> Count Difference: </b>", abs(point$Count_Difference), "<br />",
                   "<b> Length Difference: </b>", abs(point$Length_Difference), "<br />"
      )))
    )
  })




  # ggPlot: Single - Echo Chart ----
  output$Echo_Chart <- renderPlot({
    Data_Echo_Chart() %>%

      group_by(Date) %>%
      summarise(
        Count = Count %>% sum()
        ) %>%

      ggplot(
        aes(
          x = Date,
          y = factor(1))) +
      geom_point(
        aes(size = Count),
        shape = 15,
        alpha = .1,
        color = "black") +
      scale_size_area(
        max_size = 24) +

      scale_x_date(
        name = NULL,
        date_labels = "%b %Y") +

      scale_y_discrete(
        name = NULL,
        breaks = NULL) +
      theme_minimal() +
      guides(size = F) +
      theme(
        panel.background = element_rect(color = "gray"),
        panel.grid.major.x = element_line(color = "gray", linetype = 2),
        panel.grid.minor.x = element_line(color = "gray", linetype = 2),
        axis.text = element_text(size = 12)
      )

  })



  # ggPlot: Single - Pie Chart ----
  output$Pie_Chart <- renderPlot({

    Data_Raw_Date_Filter() %>%
      ungroup() %>%
      filter(Contact == input$Contact_Select) %>%
      group_by(Contact, Type) %>%
      summarise(
        Count = n(),
        `Mean Length` = str_count(Text) %>% mean() %>% round(digits = 2)) %>%
      gather(Measure, Value, Count:`Mean Length`) %>%

      ggplot(aes(x = Measure,
                 y = Value,
                 fill = Type,
                 label = Value)) +

      geom_bar(stat = "identity",
               position = "fill",
               color = "black",
               width = 1) +

      geom_text(position = position_fill(vjust = .5),
                size = 5,
                fontface = "bold",
                color = "white") +

      facet_grid(. ~ Measure, scales = "free") +

      coord_polar(theta = "y", start = 0) +

      theme_minimal() +
      labs(x = NULL, y = NULL) +

      theme(axis.text = element_blank(),
            strip.text = element_text(face = "bold", size = 11),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.placement = "inside")
  })


    # ggPlot: Single - Line Chart - Mean Length ----
  output$Line_Chart_Single_Length <- renderPlot({

    Data_Single_Line_Chart() %>%
      ggplot(aes(x = Date, y = Mean_Length)) +

      geom_point(aes(fill = Type, size = Count),
                 alpha = .5,
                 color = "black",
                 shape = 22) +

      scale_size_area(max_size = 12) +

      geom_smooth(span = 0.25,
                  se = F,
                  method = "loess",
                  color = "black",
                  fullrange = T) +

      scale_x_date(name = NULL,
                   date_minor_breaks = "1 month") +

      scale_y_log10(name = NULL) +

      theme_minimal() +

      guides(size = guide_legend(title = "Message Count",
                                 title.position = "top"),

             fill = guide_legend(title = "Message Type",
                                 title.position = "top")) +

      theme(
        legend.position = "bottom"
      )
  })

  # ggPlot: Single - Line Chart - Length ----
  output$Line_Chart_Single_Count <- renderPlot({

    Data_Single_Line_Chart() %>%
    ggplot(aes(x = Date, y = Length)) +

      geom_point(aes(fill = Type,
                     size = Count),
                 alpha = .4,
                 shape = 22) +

      scale_size_area(max_size = 12) +

      geom_smooth(
        span = 0.25,
        se = F,
        method = "loess",
        color = "black") +

      scale_x_date(
        name = NULL,
        date_minor_breaks = "1 month") +

      guides(
        size = guide_legend(title = "Message Count",
                            title.position = "top"),
        fill = guide_legend(title = "Message Length",
                            title.position = "top")) +

      labs(y = NULL) +

      theme_minimal() +
      theme(
        legend.position = "bottom"
      )

  })


  # ggPlot: Single - Heatmap ----

  output$Heatmap <- renderPlot({
    Data_Heatmap_Boxplot() %>%
      group_by(Weekday, Hour, Type) %>%

      summarise(Hourly_Count = n(),
                Hourly_Length = sum(Length),
                Mean_Hourly_Length = mean(Length)) %>%


      full_join(tibble(Hour = seq(0, 23, by = 1))) %>% ungroup() %>%

      complete(Hour, Weekday, Type,
               fill = list(Hourly_Count = 0,
                           Hourly_Length = 0,
                           Mean_Hourly_Length = 0)) %>%

      mutate(Hour = as.factor(str_pad(Hour, 2, "left", "0"))) %>%

      ggplot(aes(x = Hour, y = Weekday, fill = Hourly_Length)) +
      geom_tile(color = "white", size = .5) +
      coord_equal() +
      facet_grid(Type ~ .) +
      scale_fill_viridis(name = "Message\nLength", option = "D", direction = -1) +
      theme_tufte(base_family = "Helvetica", ticks = F) +
      scale_y_discrete(limits = rev(levels(Data_Heatmap_Boxplot()$Weekday))) +
      labs(x = NULL, y = NULL, title = "Message Length by Weekday and Hour") +
      theme(
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(10, "mm"),
        legend.title.align = 1,
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12)
      )

  })


  # ggPlot: Single - Hour Scatter ----
  output$Hour_Scatter <- renderPlot({
    Data_Heatmap_Boxplot( ) %>%
      ggplot(aes(x = Type, y = Time, fill = Type,
                 alpha = Length, size = Length)) +

      geom_count(position = position_jitter(width = .4),
                 shape = 22) +
      scale_alpha_continuous(range = c(.25, .75)) +
      scale_size_area(max_size = 6) +
      facet_grid(~ Weekday) +
      labs(x = NULL, y = NULL, "Message Length by Weekday and Hour") +
      guides(size = guide_legend(title = "Message Length"),
             fill = guide_legend(title = "Message Type"),
             alpha = F) +
      scale_x_discrete(breaks = NULL) +
      scale_y_reverse(breaks = seq(0, 24, by = 6)) +
      theme_minimal() +
      theme(
        panel.background = element_rect(color = "gray"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(10, "mm"),
        legend.title.align = 1,
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12)
      )
  })

  # ggPlot: Single - Hour Line ----
  output$Hour_Line <- renderPlot({

    Data_Heatmap_Boxplot() %>%

      group_by(
        Weekday,
        Hour) %>%

      summarise(
        Count = n(),
        Length = sum(Length),
        Mean_Length = mean(Length)) %>%

      ggplot(
        aes(x = Hour,
            y = Length,
            alpha = Count)) +

      geom_pointrange(
        aes(ymin = 0, ymax = Length),
        fatten = 2.5,
        linetype = 1) +

      scale_alpha(
        range = c(.2, .8)) +

      guides(
        alpha = F) +

      scale_y_continuous(
        name = NULL,
        breaks = NULL) +

      theme_minimal() +

      theme(
        strip.text.y = element_text(size = 12, angle = 180, face = "bold"),
        panel.background = element_rect(color = "gray"),
        axis.text = element_text(size = 12)) +

      facet_grid(
        Weekday ~ .,
        switch = "both") +

      scale_x_continuous(
        name = NULL,
        breaks = seq(0, 24, by = 3))
  })

  # ggPlot: Me - Line ----
  output$Me_Line <- renderPlot({

    Data_Me() %>%
      filter(Type == "Sent") %>%
      ggplot(aes(x = Date, y = Message_Count)) +
      geom_point(shape = 22,
                 aes(fill = Message_Length,
                     size = Contacts),
                 alpha = .75) +
      geom_smooth(se = F, method = "loess", span = .4) +
      scale_fill_viridis(direction = -1) +
      guides(fill = guide_colorbar(title = "Message \nLength"),
             size = guide_legend(title = "Number of \nContacts")) +
      theme_minimal() +
      labs(x = NULL, y = "Count") +
      scale_x_date(date_minor_breaks = "1 month",
                   date_labels = "%b %Y") +
      theme(
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.title.align = .5,
        legend.key.height = unit(10, "mm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold")

      )
  })

  # ggPlot: Me - Over Time ----
  output$Me_Overtime <- renderPlot({

    Data_Me() %>%
      filter(Type == "Sent") %>%
      gather(Measure, Value, Contacts:Mean_Length) %>%

      ggplot(aes(x = Date, y = Value)) +

      facet_grid(Measure ~ ., scales = "free_y", switch = "y",
                 labeller = as_labeller(
                   c("Contacts" = "Number of \nContacts Messaged",
                     "Mean_Length" = "Average Length \nof Message",
                     "Message_Count" = "Number \nof Messages",
                     "Message_Length" = "Length \nof Messages"))) +

      geom_line(alpha = .5) +
      geom_smooth(se = F, method = "loess", span = .1) +

      scale_x_date(date_minor_breaks = "1 month",
                   date_labels = "%b %Y") +
      scale_y_continuous(expand = c(0.15, 0)) +

      theme_minimal() +
      theme(
        strip.text.y = element_text(angle = 180, face = "bold", size = 12),
        panel.background = element_rect(color = "gray"),
        panel.grid.major.x = element_line(color = "gray", linetype = 2),
        panel.grid.minor.x = element_line(color = "gray", linetype = 3),
        strip.placement = "outside",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12)
      )
  })



  # ggPlot: Me - Heatmap ----
  SMS_Me_Hourly <- reactive({
    Data_Raw_Date_Filter() %>%
      filter(Type == "Sent") %>%
      transmute(
        Day = floor_date(Date, unit = "days"),
        Weekday = wday(Date, label = T, abbr = F),
        Time = hour(Date) + minute(Date) / 60,
        Hour = hour(Date),
        Type,
        Length = str_count(Text))
  })

  SMS_Me_Hourly_Heatmap <- reactive({
    SMS_Me_Hourly() %>%
      group_by(Weekday, Hour) %>%
      summarise(Hourly_Count = n(),
                Hourly_Length = sum(Length),
                Mean_Hourly_Length = mean(Length)) %>%
      full_join(tibble(Hour = seq(0, 23, by = 1))) %>%
      complete(Hour,
               Weekday,
               fill = list(Mean_Hourly_Length = 0,
                           Hourly_Count = 0,
                           Hourly_Length = 0)) %>%
      mutate(Hour = as.factor(str_pad(Hour, 2, "left", "0"))) %>%
      ungroup() %>%
      distinct()
  })

  output$Me_Heatmap <- renderPlot({

    SMS_Me_Hourly_Heatmap() %>%
      ggplot(
        aes(x = Hour,
            y = Weekday,
            fill = Hourly_Count)) +

      geom_tile(
        color = "white",
        size = .5) +

      coord_equal() +

      scale_fill_viridis(
        name = "Message \nCount",
        option = "D",
        direction = -1) +

      theme_tufte(base_family = "Helvetica",
                  ticks = F) +

      scale_y_discrete(
        limits = rev(levels(SMS_Me_Hourly_Heatmap()$Weekday))) +

      labs(
        x = NULL,
        y = NULL,
        title = "By Weekday and Hour") +

      theme(
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title.align = .5,
        legend.key.height = unit(10, "mm"),
        axis.text = element_text(size = 12)
      )
  })
}


# --- Run the App ---
shinyApp(UI, Server)
