# code by Abigail Banas

library(shiny)
library(shinyjs)
library(quantmod)
library(plotly)
library(fpp3)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggeasy)
library(tidyquant)
library(ggthemes)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("cosmo"),
  
  useShinyjs(),
  
  h2(titlePanel("Google Search Trends on Airline Flights")),
  
  navbarPage("",
    
    tabPanel(h4("App Instructions!"), fluid = TRUE,
             
             column(6,
                    h3(p("Welcome to the Google Search Trends App for Airline Flights!")),
                    br(),
                    h4(p("This app is equipped with three total tabs, as you can see at the top of this page. The first tab, 
                    the one you are currently on, contains instructions and helpful tips for navagating this app. This app 
                    features Google Search trend data on the search phrase: \"flights to"), em("*insert selected city here*"),("\" 
                    for eight different cities. The 
                    features for this app include an interactive plot, a drop down list to customize your plots, and 
                    six different ways to see the data.")),
                    br(), 
                    h4(p("The second tab, titled", em("Full Time Series!"), (", is a tab that displays an interactive plot of 
                  Google Search data regarding flights to eight major cities in the U.S. In that tab, you are able to select
                  multiple cities to see their Google Search interest over time for the search phrase: \"flights to"), 
                         em("*insert selected city here*"),("\". The time frame for these search trends is January 1, 2010 to March 2022."))),
                    
                    h4(p("According to Google Trends, the numbers on the graph represent search interest \"relative to the highest 
                  point on the chart\". If the line on the plot hits 100, which is the highest point, this means that the search
                  term hit peak popularity at that time. A value of 50 on the chart means the search term was half its popularity 
                  at that time. The graph on this tab is interactive, so feel free to click and drag the chart, as well as focus in 
                  on specific time periods in the series that you are interested in.")),
                    br(),
                    h4(p("The third tab, titled" , em("Fun Graphs!"),(", is where you can find the graphs of each city's 
                    Seasonality, Seasonality with a Polar chart, Autocorrelation, Difference, and Decomposition. You are to select a city of
                    interest, and then select the type of plot you'd like to view. As you select your preferences, a description of 
                    the plot you chose will be displayed, sharing some information about the plot's features. ")))
             ),
             
             column(4, offset = 1,
                    br(),
                    br(),
                    img(src="https://i.gifer.com/JFi.gif", align = "right",height='350px',width='550px')
                    ),
    ),
    
    tabPanel(h4("Full Time Series!"), fluid = TRUE,
             
             sidebarLayout(
               sidebarPanel(
                 
                 checkboxGroupInput("checkGroup", label = h4("Select Your Cities:"), 
                                    choices = list("Atlanta" = "Atlanta", "Chicago" = "Chicago",  "Dallas" = "Dallas",
                                                   "Denver" = "Denver",
                                                   "Los Angeles" = "Los Angeles",
                                                   "New Orleans" = "New Orleans", 
                                                   "New York" = "New York", "Seattle" = "Seattle"),
                                    selected = c("Atlanta","Chicago"))
                 
               ),
               
               mainPanel(
                 plotlyOutput("plot1"),
                 
                 hr(),
                 h4(p("All of the full time series plots that you are able to interact with above have very similar characteristics. 
                      They display an upward trend for Interest, with the second five years (2015 to 2020) displaying a much steeper
                      trend than the first five years (2010 to 2015). The seasonality of the first five years is fairly consistent 
                      with each city, displaying that the seasonal cycle of Google Search Trends for flights to these cities typically
                      peaked in the first half of the year and quickly decreased towards the end of the year before picking up again to 
                      start a new cycle. ")),
                
                 h4(p("As the full time series of each city begins to have a steeper positive trend (2015-2020), we are able to see 
                      irregularities and variation in the seasonal patterns. For example, the interest in flights to New Orleans had
                      very extreme peaks out of no where happen in January of 2018, 2019, and 2020.")),
                 
                 h4(p("Going into the year 2020, we are able to see a serious drop in the interest of Google Search trends for flights 
                      to our eight cities. A drop like this one completely shattered any kind of upward trend in interest that was taking 
                      place beforehand. Each city's full time series displays this drop, however, the pattern after the drop follows a serious 
                      increase in Google Search trends for flights, and it continues to follow an upward trend as this year goes on."))
                 
               )
             )
    ),
    
    tabPanel(h4("Fun Graphs!"), fluid = TRUE,
             
             sidebarLayout(
               sidebarPanel(
              
                 radioButtons("buttonCity", label = h4("Select Your City:"), 
                                    choices = list("Atlanta" = "Atlanta", "Chicago" = "Chicago",  "Dallas" = "Dallas",
                                                   "Denver" = "Denver",
                                                   "Los Angeles" = "Los Angeles",
                                                   "New Orleans" = "New Orleans", 
                                                   "New York" = "New York", "Seattle" = "Seattle"),
                                    selected = "Atlanta"),
                 
                 
                 selectInput("selectPlot", label = h4("Select A Plot:"), 
                             choices = list("Seasonality" = "Seasonality", 
                                            "Seasonality - Polar" = "Seasonality - Polar", 
                                            "Autocorrelation" = "Autocorrelation",
                                            "Difference" = "Difference",
                                            "Decomposition" = "Decomposition"), 
                             selected = "Seasonality")
                 
               ),
               
               mainPanel(
                 plotOutput("yoPlots"),
                 
                 hr(),
                 h4(textOutput("yoText"))
               )
             )
    )
  )
)



server <- function(input, output) {

  flightdata <- reactive({
 
    file_Atlanta <- "flights_Atlanta.csv"
    Atlanta_gTrends <- read.csv(file_Atlanta, skip = 2)
    names(Atlanta_gTrends) <- c("Month", "Interest")
    Atlanta_gTrends$Month <- yearmonth(Atlanta_gTrends$Month)
    Atlanta_gTrends$City <- replicate(147, "Atlanta")
    

    file_Chigaco <- "flights_Chicago.csv"
    Chicago_gTrends <- read.csv(file_Chigaco, skip = 2)
    names(Chicago_gTrends) <- c("Month", "Interest")
    Chicago_gTrends$Month <- yearmonth(Chicago_gTrends$Month)
    Chicago_gTrends$City <- replicate(147, "Chicago")
    
    
    file_Dallas <- "flights_Dallas.csv"
    Dallas_gTrends <- read.csv(file_Dallas, skip = 2)
    names(Dallas_gTrends) <- c("Month", "Interest")
    Dallas_gTrends$Month <- yearmonth(Dallas_gTrends$Month)
    Dallas_gTrends$City <- replicate(147, "Dallas")
    
    file_Denver <- "flights_Denver.csv"
    Denver_gTrends <- read.csv(file_Denver, skip = 2)
    names(Denver_gTrends) <- c("Month", "Interest")
    Denver_gTrends$Month <- yearmonth(Denver_gTrends$Month)
    Denver_gTrends$City <- replicate(147, "Denver")
    
    
    file_LosAngeles <- "flights_LosAngeles.csv"
    LosAngeles_gTrends <- read.csv(file_LosAngeles, skip = 2)
    names(LosAngeles_gTrends) <- c("Month", "Interest")
    LosAngeles_gTrends$Month <- yearmonth(LosAngeles_gTrends$Month)
    LosAngeles_gTrends$City <- replicate(147, "Los Angeles")
    
    file_NewOrleans <- "flights_NewOrleans.csv"
    NewOrleans_gTrends <- read.csv(file_NewOrleans, skip = 2)
    names(NewOrleans_gTrends) <- c("Month", "Interest")
    NewOrleans_gTrends$Month <- yearmonth(NewOrleans_gTrends$Month)
    NewOrleans_gTrends$City <- replicate(147, "New Orleans")
    
    
    file_NewYork <- "flights_NewYork.csv"
    NewYork_gTrends <- read.csv(file_NewYork, skip = 2)
    names(NewYork_gTrends) <- c("Month", "Interest")
    NewYork_gTrends$Month <- yearmonth(NewYork_gTrends$Month)
    NewYork_gTrends$City <- replicate(147, "New York")
    
    
    file_Seattle <- "flights_Seattle.csv"
    Seattle_gTrends <- read.csv(file_Seattle, skip = 2)
    names(Seattle_gTrends) <- c("Month", "Interest")
    Seattle_gTrends$Month <- yearmonth(Seattle_gTrends$Month)
    Seattle_gTrends$City <- replicate(147, "Seattle")
    
    AllCity_gTrends <- bind_rows(Atlanta_gTrends,Chicago_gTrends,Dallas_gTrends,Denver_gTrends,
                                 LasVegas_gTrends,LosAngeles_gTrends,NewOrleans_gTrends,NewYork_gTrends,
                                Seattle_gTrends)
    AllCity_gTrends <- tsibble(AllCity_gTrends, key = "City", index = "Month")
    AllCity_gTrends %>% mutate(diff = difference(Interest))
    
    return(AllCity_gTrends[AllCity_gTrends$City%in%input$checkGroup,])
    
  })
  
  
  output$checkBox <- renderPrint({ input$checkGroup })
  
  output$dropDown <- renderPrint({ input$selectPlot })
  
  output$plot1 <- renderPlotly({
    
    autoplot(AllCity_gTrends[AllCity_gTrends$City%in%input$checkGroup,])+
      labs(title = "Plot of Google Search Trends for Selected Cities:", y = "Interest", x = "Date") + 
      easy_center_title() + theme_igray()
  })
  

  funplots <- reactive({
    
    if(input$selectPlot == "Seasonality") {
      x <- AllCity_gTrends %>% filter(City == input$buttonCity) %>% gg_season() + 
      labs(title = paste("Seasonality Plot of Google Search Trends for", input$buttonCity, "Flights:" ), y = "Interest", x = "Month") + easy_center_title() + theme_igray()
    }
    
    if(input$selectPlot == "Seasonality - Polar") {
      x <- AllCity_gTrends %>% filter(City == input$buttonCity) %>% gg_season(polar=1) + 
        labs(title = paste("Seasonality - Polar Plot of Google Search Trends for", input$buttonCity, "Flights:" ), y = "Interest", x = "Month") + easy_center_title() + theme_igray()
    }
    
    if(input$selectPlot == "Autocorrelation") {
      x <-     AllCity_gTrends %>% filter(City == input$buttonCity) %>% ACF() %>% autoplot() +
        labs(title = paste("Autocorrelation Plot of Google Search Trends for", input$buttonCity, "Flights:" ), y = "ACF", x = "Lag") + easy_center_title() + theme_igray()
    }
    
    if(input$selectPlot == "Difference") {
      x <-   AllCity_gTrends %>% mutate(diff = difference(Interest)) %>% filter(City == input$buttonCity) %>% autoplot(diff) +
        labs(title = paste("Difference Plot of Google Search Trends for", input$buttonCity, "Flights:" )) + easy_center_title() + theme_igray()
    }
    
    if(input$selectPlot == "Decomposition") {
     x <-   AllCity_gTrends %>% filter(City == input$buttonCity) %>% 
        model(classical_decomposition(Interest, type = "multiplicative")) %>% 
        components()  %>% autoplot() +
        labs(title = paste("Decompsition Plot of Google Search Trends for", input$buttonCity, "Flights:" )) + easy_center_title() + theme_igray()
      }
    
    return(x)
  })
  
  output$yoPlots <- renderPlot({
    funplots()
  })
  
  
  funtext <- reactive({
    #Atlanta
    if(input$selectPlot == "Seasonality" & input$buttonCity == "Atlanta") { text <- "The seasonality of Atlanta stays mostly 
    consistent, with the months following the same upwards and downwards trends. We are able to see the increase in Interest as each year
    is essentially stacked on top of the previous one. The year with the highest interest, 2019, hits its peak in the summer months before 
    following the slight downward trend into the end of the year. The year with the largest variation in seasonality is 2020, which we can see 
    cutting downward through the other years."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "Atlanta") { text <- "The seasonality of Atlanta stays mostly 
    consistent, with the months following the same upwards and downwards trends. We are able to see the increase in Interest as each year
    is essentially stacked on top of the previous one. The year with the highest interest, 2019, hits its peak in the summer months before 
    following the slight downward trend into the end of the year. The year with the largest variation in seasonality is 2020, which we can see 
    cutting downward through the other years."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "Atlanta") { text <- "As we can see in this autocorrelation plot, the data for 
    Atlanta is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "Atlanta") { text <- "With this multiplicative decomposition of Atlanta, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #Chicago
    if(input$selectPlot == "Seasonality" & input$buttonCity == "Chicago") { text <- "The seasonality of Chicago stays mostly 
    consistent, with the months following the same upwards and downwards trends. We are able to see the increase in Interest as each year
    is essentially stacked on top of the previous one. The year with the highest interest, 2019, hits its peak in June before 
    following the slight downward trend into the end of the year. The year with the largest variation in seasonality is 2020, which we can see 
    cutting downward through the other years. The year 2020 begins with the highest Interest of 100, but is quickly followed by a downward spike."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "Chicago") { text <- "The seasonality of Chicago stays mostly 
    consistent, with the months following the same upwards and downwards trends. We are able to see the increase in Interest as each year
    is essentially stacked on top of the previous one. The year with the highest interest, 2019, hits its peak in June before 
    following the slight downward trend into the end of the year. The year with the largest variation in seasonality is 2020, which we can see 
    cutting downward through the other years. The year 2020 begins with the highest Interest of 100, but is quickly followed by a downward spike."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "Chicago") { text <- "As we can see in this autocorrelation plot, the data for 
    Chicago is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "Chicago") { text <- "With this multiplicative decomposition of Chicago, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #Dallas
    if(input$selectPlot == "Seasonality" & input$buttonCity == "Dallas") { text <- "The seasonality of Dallas stays mostly 
    consistent, with the months following the same upwards and downwards trends. We are able to see the increase in Interest as each year
    is essentially stacked on top of the previous one. The year with the highest interest, 2019, hits its peak in June before 
    following the slight downward trend into the end of the year. The year with the largest variation in seasonality is 2020, which we can see 
    cutting downward through the other years. As we can see, the first months of 2022 started quite high in Interest and looks to be continuing 
    that trend next month."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "Dallas") { text <- "The seasonality of Dallas stays mostly 
    consistent, with the months following the same upwards and downwards trends. We are able to see the increase in Interest as each year
    is essentially stacked on top of the previous one. The year with the highest interest, 2019, hits its peak in June-July before 
    following the slight downward trend into the end of the year. The year with the largest variation in seasonality is 2020, which we can see 
    cutting downward through the other years. As we can see, the first months of 2022 started quite high in Interest and looks to be continuing 
    that trend next month."}
    if(input$selectPlot == "Autocorrelaiton" & input$buttonCity == "Dallas") { text <- "As we can see in this autocorrelation plot, the data for 
    Dallas is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "Dallas") { text <- "With this multiplicative decomposition of Dallas, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #Denver
    if(input$selectPlot == "Seasonality" & input$buttonCity == "Denver") { text <- "The seasonality of Denver stays very 
    consistent compared to the other cities, with the months following the same upwards and downwards trends. We are able to see
    the increase in Interest as each year is essentially stacked on top of the previous one. The year with the highest interest, 
    2019, hits its peak in June before following the slight downward trend into the end of the year. The year with the largest variation 
    in seasonality is 2020, which we can see cutting downward through the other years. The first month of 2020 started very high for 
    this city's Interest, but was hit by an extreme downward spike shortly after."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "Denver") { text <- "The seasonality of Denver stays very 
    consistent compared to the other cities, with the months following the same upwards and downwards trends. We are able to see
    the increase in Interest as each year is essentially stacked on top of the previous one. The year with the highest interest, 
    2019, hits its peak in June before following the slight downward trend into the end of the year. The year with the largest variation 
    in seasonality is 2020, which we can see cutting downward through the other years. The first month of 2020 started very high for 
    this city's Interest, but was hit by an extreme downward spike shortly after."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "Denver") { text <- "As we can see in this autocorrelation plot, the data for 
    Denver is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "Denver") { text <- "With this multiplicative decomposition of Denver, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #LA
    if(input$selectPlot == "Seasonality" & input$buttonCity == "Los Angeles") { text <-  "The seasonality of Los Angeles stays not as 
    consistent compared to the other cities, with the year 2021 creating variation in the seasonality. The time series still has the peak 
    in the summer months just as the other cities, and how the Interest of each year is essentially stacked on top of the previous one. 
    The year with the largest variation in seasonality is 2020 with a huge drop to April, which we can see cutting downward through the other years. The first month of 2020 started very high for 
    this city's Interest, but was hit by an extreme downward spike shortly after."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "Los Angeles") { text <- "The seasonality of Los Angeles stays not as 
    consistent compared to the other cities, with the year 2021 creating variation in the seasonality. The time series still has the peak 
    in the summer months just as the other cities, and how the Interest of each year is essentially stacked on top of the previous one. 
    The year with the largest variation in seasonality is 2020 with a huge drop to April, which we can see cutting downward through the other years. The first month of 2020 started very high for 
    this city's Interest, but was hit by an extreme downward spike shortly after."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "Los Angeles") { text <- "As we can see in this autocorrelation plot, the data for 
    Los Angeles is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "Los Angeles") { text <- "With this multiplicative decomposition of Los Angeles, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #NOLA
    if(input$selectPlot == "Seasonality" & input$buttonCity == "New Orleans") { text <- "The seasonality of New Orleans stays mostly 
    consistent compared to the other cities, with the year 2021 creating variation in the seasonality by starting very low and making its way
    back to the normality of the seasonality trend. The time series still has the peak in July, and how the Interest of each year is essentially stacked on top of the previous one. 
    The year with the largest variation in seasonality is 2020 with a huge drop to April, which we can see cutting downward through the other years. The first month of 2020 started very high for 
    this city's Interest, but was hit by an extreme downward spike shortly after."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "New Orleans") { text <- "The seasonality of New Orleans stays mostly 
    consistent compared to the other cities, with the year 2021 creating variation in the seasonality by starting very low and making its way
    back to the normality of the seasonality trend. The time series still has the peak in July, and how the Interest of each year is essentially stacked on top of the previous one. 
    The year with the largest variation in seasonality is 2020 with a huge drop to April, which we can see cutting downward through the other years. The first month of 2020 started very high for 
    this city's Interest, but was hit by an extreme downward spike shortly after."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "New Orleans") { text <- "As we can see in this autocorrelation plot, the data for 
    New Orleans is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "New Orleans") { text <- "With this multiplicative decomposition of New Orleans, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #NYC
    if(input$selectPlot == "Seasonality" & input$buttonCity == "New York") { text <- "The seasonality of New York stays fairly 
    consistent, with the year 2021 creating variation in the seasonality by starting very low and making its way
    back to the normality of the seasonality trend by the middle of the year. The time series still has the peak in July, and how the 
    Interest of each year is essentially stacked on top of the previous one. 
    The year with the largest variation in seasonality is 2020 with a huge drop to April, which we can see cutting downward through the other years. 
    The first month of 2020 started very high for January and again in March, but quickly spiked downward after that. The Interest for this year 
    stayed low compared to the other cities."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "New York") { text <- "The seasonality of New York stays fairly 
    consistent, with the year 2021 creating variation in the seasonality by starting very low and making its way
    back to the normality of the seasonality trend by the middle of the year. The time series still has the peak in July, and how the 
    Interest of each year is essentially stacked on top of the previous one. 
    The year with the largest variation in seasonality is 2020 with a huge drop to April, which we can see cutting downward through the other years. 
    The first month of 2020 started very high for January and again in March, but quickly spiked downward after that. The Interest for this year 
    stayed low compared to the other cities."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "New York") { text <- "As we can see in this autocorrelation plot, the data for 
    New York is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "New York") { text <- "With this multiplicative decomposition of New York, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #Seattle
    if(input$selectPlot == "Seasonality" & input$buttonCity == "Seattle") { text <- "The seasonality of Seattle remains 
    consistent, with the year 2021 creating variation in the seasonality by starting very low and making its way
    back to the normality of the seasonality trend by June. The time series still has the peak in June and July, and how the 
    Interest of each year is essentially stacked on top of the previous one. The year with the largest variation in seasonality is 
    2020 with a huge drop to April, which we can see cutting downward through the other years. 
    The Interest for 2020 stayed low compared to the other cities, and we can see that the beginning of 2022 is off to a high start."}
    if(input$selectPlot == "Seasonality - Polar" & input$buttonCity == "Seattle") { text <- "The seasonality of Seattle remains 
    consistent, with the year 2021 creating variation in the seasonality by starting very low and making its way
    back to the normality of the seasonality trend by June. The time series still has the peak in June and July, and how the 
    Interest of each year is essentially stacked on top of the previous one. The year with the largest variation in seasonality is 
    2020 with a huge drop to April, which we can see cutting downward through the other years. 
    The Interest for 2020 stayed low compared to the other cities, and we can see that the beginning of 2022 is off to a high start."}
    if(input$selectPlot == "Autocorrelation" & input$buttonCity == "Seattle") { text <- "As we can see in this autocorrelation plot, the data for 
    Seattle is highly autocorrelated, as every point is statistically significant. This data is heavily influenced by a seasonality trend, and 
    we can also note that the data is positively correlated."}
    if(input$selectPlot == "Decomposition" & input$buttonCity == "Seattle") { text <- "With this multiplicative decomposition of Seattle, we 
    are able to tell that the trend of the time series picks up the majority of the variation in the model. We know this because the reference 
    box for this component is the smallest between trend, seasonal, and random. We performed multiplicative decomposition on this time series 
    because of the variability in the seasonality and the slight heteroskedasticity as the time series progresses."}
    
    #Diffs
    if(input$selectPlot == "Difference" & input$buttonCity == "Atlanta") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of Atlanta. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "Chicago") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of Chicago. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "Dallas") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of Dallas. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "Denver") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of Denver. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "Los Angeles") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of Los Angeles. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "New Orleans") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of New Orleans. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "New York") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of New York. "}
    if(input$selectPlot == "Difference" & input$buttonCity == "Seattle") { text <- "This plot displays the difference in the Google Search 
    Interest monthly for the city of Seattle. "}
    
    
    return(text)
  })
  
  output$yoText <- renderText({
    funtext()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
