d = read.csv("dataset/youtube.csv")

server = function(input, output, session) {
  
  data_temp = data.frame(d)
  
  updateCheckboxGroupInput(session, inputId = "field", choices = names(data_temp),selected = names(data_temp))
  output$result = renderPrint({
    paste(url, sep = "/", input$dataset)
  })
  
  output$table = renderDataTable(
    data_temp[, input$field , drop = FALSE],
    options = list(
      searching = TRUE,
      scrollX=TRUE
      #pageLength=25,
      #lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
    )
  )
  observeEvent(input$cleanOption, {
    if(input$cleanOption == FALSE) {
      output$table = renderDataTable(
        data_temp[, input$field, drop = FALSE],
        options = list(
          searching = TRUE,
          scrollX = TRUE
        )
      )
    } else if(input$cleanOption == TRUE) {
      d = na.omit(data_temp)
      output$table = renderDataTable(
        d[, input$field, drop = FALSE],
        options = list(
          searching = TRUE,
          scrollX = TRUE
        )
      )
    }
    
  })
  
  observeEvent(input$cleanOption, {
    if(input$cleanOption == FALSE) {
      output$summary = renderPrint(summary(data_temp))
    } else if(input$cleanOption == TRUE) {
      d = na.omit(data_temp)
      output$summary = renderPrint(summary(d))
    }
    
  })

  
  
    output$plotline <- renderPlotly({
      data <- d
      switch(input$line, 
             Views={
               data$Date <- as.Date(data$Date, '%d-%b-%y')
               topViewDay<-data%>%group_by(Date)%>%summarise(topViewDay = sum(Views), .groups = "keep")%>%ungroup()%>%arrange()
               Timeline<- head(topViewDay$Date,input$bins)
               View<- head(topViewDay$topViewDay,input$bins)
               data <- data.frame(Timeline,View)
               plot_ly(data, type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
                 add_trace(x = ~Timeline, y = ~View)%>%
                 layout(showlegend = F)%>%
                 layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6', width = 900)
             },
             VideoLikesAdded={
               data$Date <- as.Date(data$Date, '%d-%b-%y')
               like<-data%>%group_by(Date)%>%summarise(like = sum(VideoLikesAdded), .groups = "keep")%>%ungroup()%>%arrange()
               Timeline <- head(like$Date,input$bins)
               Like<- head(like$like,input$bins)
               data <- data.frame(Timeline,Like)
               plot_ly(data, type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
                 add_trace(x = ~Timeline, y = ~Like)%>%
                 layout(showlegend = F)%>%
                 layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6', width = 900)
             },
             VideoDislikesAdded={
               data$Date <- as.Date(data$Date, '%d-%b-%y')
               dislike<-data%>%group_by(Date)%>%summarise(dislike = sum(VideoDislikesAdded), .groups = "keep")%>%ungroup()%>%arrange()
               Timeline <- head(dislike$Date,input$bins)
               Dislike<- head(dislike$dislike,input$bins)
               data <- data.frame(Timeline,Dislike)
               plot_ly(data, type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
                 add_trace(x = ~Timeline, y = ~Dislike)%>%
                 layout(showlegend = F)%>%
                 layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6', width = 900)
             },
             UserSubscriptionsAdded={
               data$Date <- as.Date(data$Date, '%d-%b-%y')
               Subscribe<-data%>%group_by(Date)%>%summarise(Subscribe = sum(UserSubscriptionsAdded), .groups = "keep")%>%ungroup()%>%arrange()
               Timeline <- head(Subscribe$Date,input$bins)
               Subscribe<- head(Subscribe$Subscribe,input$bins)
               data <- data.frame(Timeline,Subscribe)
               plot_ly(data, type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
                 add_trace(x = ~Timeline, y = ~Subscribe)%>%
                 layout(showlegend = F)%>%
                 layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6', width = 900)
             },
             UserSubscriptionsRemoved={
               data$Date <- as.Date(data$Date, '%d-%b-%y')
               UnSubscribe<-data%>%group_by(Date)%>%summarise(UnSubscribe = sum(UserSubscriptionsRemoved), .groups = "keep")%>%ungroup()%>%arrange()
               Timeline <- head(UnSubscribe$Date,input$bins)
               UnSubscribe<- head(UnSubscribe$UnSubscribe,input$bins)
               data <- data.frame(Timeline,UnSubscribe)
               plot_ly(data, type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
                 add_trace(x = ~Timeline, y = ~UnSubscribe)%>%
                 layout(showlegend = F)%>%
                 layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6', width = 900)
             },
             {
               data$Date <- as.Date(data$Date, '%d-%b-%y')
               topViewDay<-data%>%group_by(Date)%>%summarise(topViewDay = sum(Views), .groups = "keep")%>%ungroup()%>%arrange()
               Timeline<- head(topViewDay$Date,input$bins)
               View<- head(topViewDay$topViewDay,input$bins)
               data <- data.frame(Timeline,View)
               plot_ly(data, type = 'scatter', mode = 'lines',fill = 'tozeroy')%>%
                 add_trace(x = ~Timeline, y = ~View)%>%
                 layout(showlegend = F)%>%
                 layout(
                   xaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   yaxis = list(zerolinecolor = '#ffff',
                                zerolinewidth = 2,
                                gridcolor = 'ffff'),
                   plot_bgcolor='#e5ecf6', width = 900)
             }
      )
      
      #barplot(head(totalLike$like,5*3))
    })
  
    output$plotbar <- renderPlotly({
    data <- d
    switch(input$bar, 
           View={
             data$Date <- as.Date(data$Date, '%d-%b-%y')
             day <- data$Date
             data1 <- data.frame(day)
             c <- data1
             data1$day <- strftime(data1$day, "%A")
             day<- data1$day
             data = data%>%mutate(data.frame(day))
             data = na.omit(data)
             topViewDay<-data%>%group_by(day)%>%summarise(topViewDay = sum(Views), .groups = "keep")%>%ungroup()%>%arrange(desc(day))
             Day <- topViewDay$day
             Views <-topViewDay$topViewDay
             data <- data.frame(Day, Views)
             plot_ly(data, x = ~Day, y = ~Views, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
           },
           Like={
             data$Date <- as.Date(data$Date, '%d-%b-%y')
             day <- data$Date
             data1 <- data.frame(day)
             c <- data1
             data1$day <- strftime(data1$day, "%A")
             day<- data1$day
             data = data%>%mutate(data.frame(day))
             data = na.omit(data)
             Like<-data%>%group_by(day)%>%summarise(Like = sum(VideoLikesAdded), .groups = "keep")%>%ungroup()%>%arrange(desc(Like))
             Day <- Like$day
             Like <-Like$Like
             data <- data.frame(Day, Like)
             View(data)
             plot_ly(data, x = ~Day, y = ~Like, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
           },
           Dislike={
             data$Date <- as.Date(data$Date, '%d-%b-%y')
             day <- data$Date
             data1 <- data.frame(day)
             c <- data1
             data1$day <- strftime(data1$day, "%A")
             day<- data1$day
             data = data%>%mutate(data.frame(day))
             data = na.omit(data)
             Dislike<-data%>%group_by(day)%>%summarise(Dislike = sum(VideoDislikesAdded), .groups = "keep")%>%ungroup()%>%arrange(desc(Dislike))
             Day <- Dislike$day
             Dislike <-Dislike$Dislike
             data <- data.frame(Day, Dislike)
             plot_ly(data, x = ~Day, y = ~Dislike, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
           },
           Subscription={
             data$Date <- as.Date(data$Date, '%d-%b-%y')
             day <- data$Date
             data1 <- data.frame(day)
             c <- data1
             data1$day <- strftime(data1$day, "%A")
             day<- data1$day
             data = data%>%mutate(data.frame(day))
             data = na.omit(data)
             Subscribe<-data%>%group_by(day)%>%summarise(Subscribe = sum(UserSubscriptionsAdded), .groups = "keep")%>%ungroup()%>%arrange(desc(Subscribe))
             Day <- Subscribe$day
             Subscribe <-Subscribe$Subscribe
             data <- data.frame(Day, Subscribe)
             plot_ly(data, x = ~Day, y = ~Subscribe, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
           },
           UnSubscription={
             data$Date <- as.Date(data$Date, '%d-%b-%y')
             day <- data$Date
             data1 <- data.frame(day)
             c <- data1
             data1$day <- strftime(data1$day, "%A")
             day<- data1$day
             data = data%>%mutate(data.frame(day))
             data = na.omit(data)
             UnSubscribe<-data%>%group_by(day)%>%summarise(UnSubscribe = sum(UserSubscriptionsRemoved), .groups = "keep")%>%ungroup()%>%arrange(desc(UnSubscribe))
             Day <- UnSubscribe$day
             UnSubscribe <-UnSubscribe$UnSubscribe
             data <- data.frame(Day, UnSubscribe)
             plot_ly(data, x = ~Day, y = ~UnSubscribe, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
           },
           {
             data$Date <- as.Date(data$Date, '%d-%b-%y')
             day <- data$Date
             data1 <- data.frame(day)
             c <- data1
             data1$day <- strftime(data1$day, "%A")
             day<- data1$day
             data = data%>%mutate(data.frame(day))
             data = na.omit(data)
             topViewDay<-data%>%group_by(day)%>%summarise(topViewDay = sum(Views), .groups = "keep")%>%ungroup()%>%arrange(desc(topViewDay))
             
             Day <- topViewDay$day
             
             Views <-topViewDay$topViewDay
             data <- data.frame(Day, Views)
             plot_ly(data, x = ~Day, y = ~Views, type = 'bar',
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
           }
    )
  })
  
  output$plotModel <- renderPlotly({
    data = d
    data = na.omit(d)
    data$Date <- as.Date(data$Date, '%d-%b-%y')
    day <- data$Date
    data1 <- data.frame(day)
    c <- data1
    data1$day <- strftime(data1$day, "%A")
    day<- data1$day
    data = data%>%mutate(data.frame(day))
    topViewDay<-data%>%group_by(day)%>%summarise(topViewDay = sum(Views), .groups = "keep")%>%ungroup()%>%arrange(desc(topViewDay))
    Views <-topViewDay$topViewDay
    view = unlist(Views)
    
    data$Date <- as.Date(data$Date, '%d-%b-%y')
    day <- data$Date
    data1 <- data.frame(day)
    c <- data1
    data1$day <- strftime(data1$day, "%A")
    day<- data1$day
    data = data%>%mutate(data.frame(day))
    Like<-data%>%group_by(day)%>%summarise(Like = sum(VideoLikesAdded), .groups = "keep")%>%ungroup()%>%arrange(desc(Like))
    Like <-Like$Like
    like =unlist(Like)
    
    data$Date <- as.Date(data$Date, '%d-%b-%y')
    day <- data$Date
    data1 <- data.frame(day)
    c <- data1
    data1$day <- strftime(data1$day, "%A")
    day<- data1$day
    data = data%>%mutate(data.frame(day))
    
    Dislike<-data%>%group_by(day)%>%summarise(Dislike = sum(VideoDislikesAdded), .groups = "keep")%>%ungroup()%>%arrange(desc(Dislike))
    Dislike <-Dislike$Dislike
    dislike = unlist(Dislike)
    
    data$Date <- as.Date(data$Date, '%d-%b-%y')
    day <- data$Date
    data1 <- data.frame(day)
    c <- data1
    data1$day <- strftime(data1$day, "%A")
    day<- data1$day
    data = data%>%mutate(data.frame(day))
    Subscribe<-data%>%group_by(day)%>%summarise(Subscribe = sum(UserSubscriptionsAdded), .groups = "keep")%>%ungroup()%>%arrange(desc(Subscribe))
    Subscribe <-Subscribe$Subscribe
    subscribe = unlist(Subscribe)
    
    data$Date <- as.Date(data$Date, '%d-%b-%y')
    day <- data$Date
    data1 <- data.frame(day)
    c <- data1
    data1$day <- strftime(data1$day, "%A")
    day<- data1$day
    data = data%>%mutate(data.frame(day))
    UnSubscribe<-data%>%group_by(day)%>%summarise(UnSubscribe = sum(UserSubscriptionsRemoved), .groups = "keep")%>%ungroup()%>%arrange(desc(UnSubscribe))
    UnSubscribe <-UnSubscribe$UnSubscribe
    unSubscribe = unlist(UnSubscribe)
    d1 = data.frame(like,dislike,subscribe,unSubscribe,view)
    d1 = cor(d1)
    ggcorr(d1, geom = "circle")
  })
}

