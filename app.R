
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(flexdashboard)
library(googleVis)
library(plyr)
library(dplyr)
library(readr)
library(plotly)
require(RColorBrewer)
library(treemap)
require(gridBase)
library(brotli)

devtools::install_github
devtools::install_github("gluc/data.tree")
devtools::install_github("timelyportfolio/d3treeR")
#install.packages("d3treeR")
library(d3treeR)
SONGS=read_csv('lyrics.csv')
songs=SONGS %>% filter(year!=1968 & year!=67 & year!=112 & year!=702 & year!=2038 & lyrics!="instrumental") %>%
  filter(!is.na(lyrics))

cc= songs%>% 
  group_by(genre,year) %>%
  summarise(njj=n())%>%
  collect

songs_compressed=songs %>%
  mutate(compress= sapply(lyrics, function(x)  memCompress(x, type="gzip")   ) ) %>%
  mutate(size=sapply(lyrics, object.size) )%>%
  mutate(size_compressed=sapply(compress, object.size) ) %>%
  mutate(ratio= 100-round((size_compressed/size)*100, 2)  ) %>%
  filter(ratio >10)

ll=songs_compressed %>%
  
  filter(ratio>93)

#songs_compressed= songss %>%
# mutate(compress= sapply(lyrics, function(x)  memCompress(x, type="bzip2")   ) ) %>%
#mutate(size=sapply(lyrics, object.size) )%>%
#mutate(size_compressed=sapply(compress, object.size) ) 


### Handle cliks on a treemap
tmLocate <-
  function(coor, tmSave) {
    tm <- tmSave$tm
    
    # retrieve selected rectangle
    rectInd <- which(tm$x0 < coor[1] &
                       (tm$x0 + tm$w) > coor[1] &
                       tm$y0 < coor[2] &
                       (tm$y0 + tm$h) > coor[2])
    
    return(tm[rectInd[1], ])
    
  }

ui <- dashboardPage( skin = "purple",
                     dashboardHeader(title= 'Songs' 
                     ),
                     dashboardSidebar(
                       
                       sidebarMenu(
                         
                         
                         menuItem("Dashboard",tabName="dash", icon=icon("tachometer") ),
                         
                         menuItem("Lyrics", tabName = "lyrics", icon=icon("file-audio-o ")),
                         menuItem("Compressed lyrics", tabName = "comp", icon=icon("wrench")),
                         menuItem("Most repetitive",tabName="fun", icon=icon("heart-o") )
                         
                       )
                     ),
                     
                     
                     dashboardBody( 
                       
                       tabItems(  
                         
                         
                         
                         tabItem(tabName = "dash",
                                 fluidPage(
                                   tabBox( id="tabset1", height = "100%",width = "100%" ,
                                           
                                           tabPanel("Statistics", 
                                                    fluidPage(
                                                      fluidRow( infoBoxOutput("numbsong"), infoBoxOutput("numbart"), infoBoxOutput("numbyear") ),
                                                      
                                                      box(width=12,title = "Number of songs per year",status = "danger",solidHeader = TRUE,
                                                          plotlyOutput('plotyear') ),
                                                      
                                                      box(width = 6, title="Number of songs per genre", status = "warning", solidHeader = F,
                                                          plotlyOutput("plotgenre")),
                                                      
                                                      box(width = 6, " The first plot is about the number of songs in the data per year.",
                                                          br(), 
                                                          "The second plot tells how many songs there are in each genre."),
                                                      
                                                      box(width=12,status = "danger" , solidHeader = TRUE, 
                                                          conditionalPanel(
                                                            condition = "output.condition1 == 1",
                                                            
                                                            plotOutput("threemap_population_country",height="600px",
                                                                       click="click_treemap_country")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "output.condition1 == 0",
                                                            br(),
                                                            br(),
                                                            plotlyOutput("population_country_time_series"),
                                                            uiOutput("zoomout")
                                                          ) )
                                                      
                                                      
                                                    ) 
                                           ),
                                           tabPanel("Songs' Myth",
                                                    fluidPage(
                                                      h1(" Are modern songs more repetitive? "),
                                                      p("How many times have we heard that old songs are 
                                                        better and more consistent compared to what we produce now? Is that right? "),
                                                      
                                                      p("Are modern songs more repetitive?") ,
                                                      br(),
                                                      p("With some math, statistics and of course R we can easily answer these questions.
                                                        So I'll be analyzing the repetitiveness of a dataset of 266551 songs that charted on the MetroLyrics
                                                        between 1968 and 2016.
                                                        
                                                        "),
                                                      br(),
                                                      br(),
                                                      h3("How will I proceed?"),
                                                      p("You may not have heard of the Lempel-Ziv algorithm, but you probably use it
                                                        every day. It's a lossless compression algorithm that powers gifs, pngs, and 
                                                        most archive formats (zip, gzip, rar...)."),
                                                      
                                                      h3("What does this have to do with pop music?"),
                                                      p(" The Lempel-Ziv algorithm works by exploiting repeated sequences. 
                                                        How efficiently LZ can compress a text is directly related to the number
                                                        and length of the repeated sections in that text."),
                                                      br(), 
                                                      p("For this proceedure I used the command memCompress in R from brotli package
                                                        choosing the gzip method"),
                                                      p("This application will allow you to see songs in a diffenrent way and discover the most repetitive songs in the world"),
                                                      h2("If you can't wait, Go to the MOST REPETITIVE!!!")
                                                      
                                                      
                                                      
                                                      
                                                      ) ),
                                           tabPanel("Data",
                                                    fluidPage(
                                                      
                                                      box(width=12,title="Summary of the variables" , status = "danger",  solidHeader = T, 
                                                          
                                                          
                                                          
                                                          collapsible = T,  DT::dataTableOutput('data') )
                                                      
                                                    )
                                           )
                                           
                                           
                                                      )
                                                      )
                         ),
                         tabItem(tabName = "lyrics",
                                 
                                 fluidPage(
                                   
                                   box(width=4,title = "Choose a genre ", status = "danger", solidHeader = TRUE, uiOutput("selectgenre")),
                                   box(width=4,title = "Choose an artist", status = "danger", solidHeader = TRUE, uiOutput("selectartist")),
                                   box(width=4,title = "Choose a song", status = "danger", solidHeader = TRUE, uiOutput("selectsong")),
                                   
                                   box(width = 8, verbatimTextOutput("lyr" ) ),
                                   
                                   box(width = 4,
                                       title = "Compression ratio", status = "primary", solidHeader = TRUE,
                                       flexdashboard::gaugeOutput("gauge_ratio")
                                       
                                   )
                                   
                                   
                                   
                                 )
                         ),
                         
                         tabItem(tabName = "comp", 
                                 fluidPage(
                                   box(width=12,title = "Average of ratio per year ", status = "danger", solidHeader = TRUE, plotlyOutput("ratio_per_year_plot")),
                                   box(width = 12, "In 1970, the average song is 54% compressible. However in 2015 and 2016 the average song 
                                       is almost 59%. if we took random songs from 2016 and 1970 having the same length, we'd expect the compressed 2016 
                                       song to be 15% smaller than the compressed 1970 songs. ", "In 1980 songs started changing, people discovered that
                                       repetitive songs were really liked so they started producing " ,br(),
                                       "The Myth is confirmed!!!!" 
                                   ),
                                   box(width=6,height=500, title = "Ratio's density ", status = "danger", solidHeader = TRUE, plotlyOutput("denscomp")),
                                   box(width=6,height = 500,solidHeader = TRUE,status = "danger",
                                       "We notice that most of the songs are 40 to 70 % compressible"),
                                   
                                   box(width = 6, status = "danger","We notice that Pop and Electronic music have an average ratio of 58 and 59%
                                       whic make their songs really repetitive since they are more likely listened for the music and beats
                                       however Metal songs have a low ratio. Metal artists and bands pay attention to their lyrics"),
                                   box(width=6,title = "Average of ratio for every genre ",
                                       status = "danger", solidHeader = TRUE, plotlyOutput("ratio_per_genre_plot")),
                                   
                                   box(width = 4, status = "danger", solidHeader = F, uiOutput("artist_vector"), background = "red"),
                                   
                                   box(width=12,title = "Ratios of different songs ", status = "danger", solidHeader = TRUE, htmlOutput("scatter_ratio_artist")),
                                   
                                   
                                   box(width = 4, status = "danger", solidHeader = F, uiOutput("genre_vector"), background = "red"),
                                   
                                   box(width=12,title = "Average ratios of artists for every genre ", status = "danger", solidHeader = TRUE, htmlOutput("scatter_ratio_genre"))
                                   
                                   
                                   )
                         ),
                         tabItem(tabName = "fun",
                                 fluidPage(
                                   
                                   box(width = 6,title="",  status = "danger", solidHeader = T,
                                       "The most repetitive songs in the world", br(), 
                                       uiOutput("mostsongs")
                                       
                                   ),
                                   box(width=6,solidHeader = T, status = "danger",
                                       flexdashboard::gaugeOutput("mostratio") )
                                   ,
                                   box(width = 8,
                                       "Genre:", br(),
                                       verbatimTextOutput("mostgenre"),br(),
                                       "Artist:", br(),
                                       verbatimTextOutput("mostartist"), br(),
                                       verbatimTextOutput("mostlyrics"))
                                   
                                   
                                   
                                   
                                   
                                 )
                                 
                         )
                         
                         )    
                         )
                     
)


server <- function(input, output) {
  numbs= reactive({
    songs %>%
      count %>%
      collect
    
    
  })
  output$numbsong= renderInfoBox({
    infoBox(
      numbs(), "Songs", icon = icon("music"),
      color = "navy", fill=TRUE
    )
  })
  
  
  
  numba= reactive({
    songs %>%
      group_by(artist) %>%
      summarise(nn=n()) %>%
      count %>%
      collect
    
    
  })
  
  output$numbart= renderInfoBox({
    infoBox(
      numba(), "Artists", icon = icon("user-o"),
      color = "teal", fill=TRUE
    )
  })
  
  
  numby=reactive({
    songs%>% 
      group_by(year) %>%
      summarise(years=n())%>%
      count%>%
      collect
  })
  
  
  output$numbyear= renderInfoBox({
    infoBox(
      numby(), "Years", icon = icon("hourglass-end"),
      color = "olive", fill=TRUE
    )
  })
  
  
  
  
  
  
  
  
  
  
  numb_song_year=reactive({
    songs%>% 
      group_by(year) %>%
      summarise(nyears=n())%>%
      collect
  })
  
  output$plotyear= renderPlotly({
    ll=  songs%>% 
      group_by(year) %>%
      summarise(nyears=n())%>%
      collect
    
    plot_ly(ll,x= ~year, y= ~nyears,  name='Number of songs', type = 'scatter', mode = 'lines+markers',
            line = list(color = 'teal', width = 3)) %>%
      
      layout( title='', xaxis = list(title = 'Years'), yaxis =list(title= 'Number of songs') )
    
  })
  
  
  
  output$plotgenre=renderPlotly({
    ss= songs %>%
      group_by(genre) %>%
      summarise(nn=n()) %>%
      collect
    plot_ly(ss, x = ~genre, y = ~nn, type = 'bar',
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%
      layout(title = "",
             xaxis = list(title = "Genres"),
             yaxis = list(title = "Number of songs"))
    
  })
  
  
  
  
  
  
  
  
  
  output$threemap_population_country <- renderPlot({ 
    
    
    
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    vps <- baseViewports()
    
    
    
    .tm <<- treemap(cc, 
                    index="genre", 
                    vSize="njj", 
                    vColor="njj",
                    type="value",
                    title = "",
                    palette="Blues",
                    border.col ="white",
                    position.legend="right",
                    fontsize.labels = 16,
                    title.legend="")
    
    
    
  })
  
  treemap_clicked_country <- reactiveValues(
    center = NULL,
    for_condition=NULL
  )
  
  
  # Handle clicks on treemap by country
  observeEvent(input$click_treemap_country, {
    x <- input$click_treemap_country$x
    y <- input$click_treemap_country$y
    treemap_clicked_country$center <- c(x,y)
    
    if(is.null(treemap_clicked_country$for_condition)){
      treemap_clicked_country$for_condition=c(x,y)
    }
    else{treemap_clicked_country$for_condition=NULL}
  })
  
  getRecord_population_country <- reactive({
    x <- treemap_clicked_country$center[1]
    y <- treemap_clicked_country$center[2]
    
    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
    
    
    l <- tmLocate(list(x=x, y=y), .tm)
    z=l[, 1:(ncol(l)-5)]
    
    
    if(is.na(z[,1]))
      return(NULL)
    
    col=as.character(z[,1])
    
    filter(cc,genre==col)
  })
  
  condition1<-reactive({
    
    refresh=refresh()
    
    if(is.null(treemap_clicked_country$for_condition) & refresh==0){
      result=1}else if((refresh%%2==0) & !is.null(treemap_clicked_country$for_condition)){
        result =0
      }else if((refresh%%2!=0) & !is.null(treemap_clicked_country$for_condition)){
        result =1
      }else if((refresh%%2!=0) & is.null(treemap_clicked_country$for_condition)){
        result =0
      }else if((refresh%%2==0) & is.null(treemap_clicked_country$for_condition)){
        result =1
      }
  })
  
  
  output$condition1 <- renderText({
    condition1()
  })
  
  outputOptions(output, 'condition1', suspendWhenHidden=FALSE)
  
  
  output$population_country_time_series<-renderPlotly({
    
    ttt=getRecord_population_country()
    
    
    
    
    plot_ly(ttt, x = ~year, y = ~njj , type = 'bar', color = I("orange")) %>%
      layout(title = "",
             xaxis = list(title = "Years"),
             yaxis = list(title = "Number of songs for this genre"))
    
    
  })
  
  
  output$zoomout = renderUI({
    actionButton("refresh", em("Go to the previous page",style="text-align:center;color:red;font-size:200%"))
  })
  
  refresh=reactive({
    input$refresh
  })
  
  
  
  
  
  #########################################
  #####new panel ###########
  output$data = DT::renderDataTable({
    
    songs
  },extensions="Responsive" ,options = list( lengthMenu = c(5, 10,25 ) ,pageLength = 10, lengthChange = TRUE, autoWidth = FALSE))
  
  
  ########################################
  ############ lyrics #################
  
  output$selectgenre=renderUI({
    genre_data= songs %>% 
      group_by(genre) %>%
      summarise(nsong=n())%>%
      collect
    
    
    selectizeInput("gg", label =NULL, choices = c("Choose a genre",genre_data$genre), selected="Choose a genre")
    
    
  })
  
  
  output$selectartist=renderUI({
    artist_data=songs %>%
      filter(genre==input$gg) %>%
      group_by(artist) %>%
      summarise(nsong=n()) %>%
      collect 
    selectizeInput("aa", label =NULL, choices = c( "Choose an artist",artist_data$artist), selected="Choose an artist")
    
  })
  
  output$selectsong=renderUI({
    song_data=songs %>%
      filter( genre==input$gg & artist==input$aa) %>%
      collect
    
    selectizeInput("ss", label =NULL, choices = c( "Choose a song",song_data$song), selected="Choose a song")
    
    
  })
  
  
  output$lyr=renderText({
    parole= songs %>%
      filter(genre==input$gg & artist==input$aa & song==input$ss)
    
    return(parole$lyrics)
  })
  
  
  output$gauge_ratio <- flexdashboard::renderGauge({
    
    parole= songs_compressed %>%
      filter(genre==input$gg & artist==input$aa & song==input$ss)
    
    gauge(parole$ratio, min = 0, max = 100, symbol = '%', label = paste(" "),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
    
  })
  
  ###########################################################################
  ############## compress ###########################################
  
  
  
  
  
  output$denscomp=renderPlotly({
    density1=density(songs_compressed$ratio)
    plot_ly(x= ~density1$x, y= ~density1$y ,name='Good rate', type= 'scatter', mode='lines')  %>%
      
      layout(title = '', yaxis = list(title = 'rate %'), barmode = 'group')
    
    
    
  })
  
  
  
  
  
  output$ratio_per_year_plot=renderPlotly({
    ss= songs_compressed %>%
      group_by(year) %>%
      summarise(meanratio=mean(ratio)) %>%
      collect
    
    plot_ly(ss, x = ~year, y = ~meanratio, type= 'scatter', mode='lines' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%
      layout(title = "",
             xaxis = list(title = "Genres"),
             yaxis = list(title = "Number of songs"))
    
    
  })
  
  output$ratio_per_genre_plot=renderPlotly({
    ss= songs_compressed %>%
      group_by(genre) %>%
      summarise(nn=mean(ratio), minimum=min(ratio), maximum=max(ratio)) %>%
      collect
    
    plot_ly(ss, x = ~genre, y = ~nn, type= 'bar',
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%
      layout(title = "",
             xaxis = list(title = "Genres"),
             yaxis = list(title = "Number of songs"))
    
  })
  
  years_vector=reactive({
    mm=songs %>% 
      group_by(year) %>%
      summarise(years=n())
    
    mm[,1]
    
  })
  
  
  
  output$artist_vector=renderUI({
    genre_data= songs_compressed %>% 
      group_by(artist) %>%
      summarise(nartist=n())%>%
      collect
    
    
    selectizeInput("nart", label =NULL, choices = c("Choose an artist", genre_data$artist), selected="Choose an artist")
    
    
  })
  
  
  
  
  
  
  
  output$scatter_ratio_artist=renderGvis({
    ratio_of_year= songs_compressed %>%
      filter(artist==input$nart)
    
    
    
    # Return the data and options
    gvisBubbleChart(ratio_of_year, idvar="song", 
                    xvar="ratio", yvar = "year", 
                    colorvar="ratio", sizevar="ratio",
                    options=list(
                      hAxis='{minValue:25, maxValue:100}',
                      vAxis=  '{minValue:1967, maxValue:2016}',        
                      height=700, width=850))
    
  })
  
  
  
  
  output$genre_vector=renderUI({
    genre_data= songs_compressed %>% 
      group_by(genre) %>%
      summarise(ngenre=n())%>% 
      filter (ngenre > 10)
    collect
    
    
    selectizeInput("ngenre", label =NULL, choices = c( "Choose a genre",genre_data$genre), selected="Choose a genre")
    
    
  })
  
  
  
  
  
  
  
  output$scatter_ratio_genre=renderGvis({
    ratio_of_genre= songs_compressed %>%
      filter(genre==input$ngenre) %>%
      group_by(artist) %>%
      summarise(nmean=mean(ratio), number=n()) %>%
      filter(number>15)
    
    
    
    # Return the data and options
    gvisColumnChart(ratio_of_genre, xvar="artist",
                    yvar=c("nmean"),
                    options=list(seriesType="bars",
                                 series='{1: {type:"line"}}'))
    
  })
  
  output$mostsongs=renderUI({
    selectizeInput("mosts", label =NULL, choices = c(ll$song) )
    
    
    
  })
  
  output$mostgenre = renderText({
    
    g=songs_compressed %>%
      filter(song==input$mosts & ratio>93)
    return(g$genre)
  })
  
  output$mostartist = renderText({
    a=songs_compressed %>%
      filter(song==input$mosts & ratio>93)
    return(a$artist)
    
  }) 
  
  
  output$mostlyrics= renderText({
    l=songs_compressed %>%
      filter(song==input$mosts & ratio>93 )
    return(l$lyrics)
  })
  
  output$mostratio <- flexdashboard::renderGauge({
    r=songs_compressed %>%
      filter(song==input$mosts & ratio>93 )
    
    
    gauge(r$ratio, min = 0, max = 100, symbol = '%', label = paste(" "),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
    
  })
  
}


shinyApp(ui = ui, server = server)
