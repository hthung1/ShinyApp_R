
ui <- dashboardPage(
  skin = 'red',
  dashboardHeader(title = "Youtube dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-simple")),
      menuItem("Predict", tabName = "Predict", icon = icon("gears"))
    )
  ),
  
  
  
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                theme = shinytheme("cosmo"),
                titlePanel("Youtube Video Dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    width = 4,
                    tabsetPanel(
                      tabPanel("DATASET",
                               checkboxInput("cleanOption", "Clean NA", value = FALSE),
                               checkboxGroupInput("field",
                                                  "Column data:",
                                                  choices = list("VideoLength" = 1,
                                                                 "Views" = 2
                                                  )
                               ),
                      )
                    ),
                    #verbatimTextOutput("result")
                  ),
                  mainPanel(
                    width = 8,
                    tabsetPanel(
                      id = 'dataset',
                      tabPanel("Table",
                               dataTableOutput('table')
                      ),
                      tabPanel("Summary", verbatimTextOutput("summary")),
                    )
                  )
                )
              )
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "charts",
              fluidPage(
                theme = shinytheme("cosmo"),
                sidebarLayout(
                  sidebarPanel(
                    width = 4,
                    tabsetPanel(
                      tabPanel("Line",
                               selectInput("line", 
                                           label = "Choose a chart to display",
                                           choices = c( 
                                                       "Views",
                                                       "VideoLikesAdded", 
                                                       "VideoDislikesAdded",
                                                       "UserSubscriptionsAdded",
                                                       "UserSubscriptionsRemoved",
                                                       "AverageViewPercentage",
                                                       "AverageWatchTime"),
                                           selected = "Views"),
                               sliderInput("bins", 
                                           label = "Number of bins:",
                                           min = 1, max = 1523, value = 200)),
                      tabPanel("Bar",
                    selectInput("bar", 
                                label = "Choose chart of the week",
                                choices = c("View", 
                                            "Like",
                                            "Dislike", 
                                            "Subscription",
                                            "UnSubscription"),
                                selected = "View"))
                    )
                      

                  ),
                  mainPanel(
                  width = 8,
                  tabsetPanel(
                    tabPanel("Years",
                  plotlyOutput("plotline")),
                  
                    tabPanel("Weeks",
                  plotlyOutput("plotbar")
                    )
                  
                  ),
                ),
                
              )
            )
      ),
      tabItem(tabName = "Predict",
              fluidPage(
                theme = shinytheme("cosmo"),
                  sidebarLayout(
                    sidebarPanel(
                      width = 0
                    ),
                    mainPanel(
                      width = 8,
                      plotlyOutput("plotModel")
                    )
                  )
                
      ))
    )
  )
)

