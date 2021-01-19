#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(plotly)
library(reshape2)
# require(devtools)
# install_github("ramnathv/rCharts")
require(rCharts)

#### Load in the data ####
source('extract_data.R')

worldcountry = geojson_read("custom.geo.json", what = "sp")
countrycode <- read.csv('country code.csv') %>%
  select(name, alpha.3, alpha.2) %>%
  rename(Countrycode = alpha.3,
         Country = name)
covid19_cases %>%
  left_join(countrycode, by = 'Country') -> covid19_cases
plot_map <- worldcountry[worldcountry$adm0_a3 %in% covid19_cases$Countrycode, ]
covid19_cases = covid19_cases[match(plot_map$adm0_a3, covid19_cases$Countrycode),] %>%
  as.data.frame()

# top 10 countries--------------------
top10plot <- function(type){
  dat <- covid19_cases[, c('Country',type)]
  colnames(dat) <- c('Country', 'Cases')
  dat.graph <- dat[order(dat$Cases, decreasing = T),] %>%
    top_n(10, Cases)
  dat.graph$Country <- reorder(dat.graph$Country, dat.graph$Cases)
  graph <- ggplot(dat.graph, aes(x = Cases, y = Country)) +
    geom_point(size = 5, color = ifelse(type == 'Confirmed', 'darkseagreen4', ifelse(type == 'Deaths', 'goldenrod1', 'deepskyblue'))) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey60', linetype = 'dashed'),
          axis.text.y.left = element_text(size = 11, face = 'italic')) +
    xlab(type) +
    ylab('')
  return(graph)
}


  
# time series ################
form_timeseriesdata <- function(Country, Type){
  if(Type == 'Cumulative'){
    date.df <- data.frame(date = seq.Date(from = as.Date('2020-01-22'), 
                                          to = Time.default, by = 'day'))
    
    cases.df <- data.frame(Confirmed = as.numeric(aggregated_confirmed %>%
                                                    filter(`Country/Region` == Country)%>%
                                                    select(-1)),
                           Deaths = as.numeric(aggregated_deaths %>%
                                                 filter(`Country/Region` == Country)%>%
                                                 select(-1)),
                           Recover = as.numeric(aggregated_recover %>%
                                                  filter(`Country/Region` == Country)%>%
                                                  select(-1))) 
  }
  else{
    date.df <- data.frame(date = seq.Date(from = as.Date('2020-01-23'), 
                                          to = Time.default, by = 'day'))
    
    cases.df <- data.frame(Confirmed = diff(as.numeric(aggregated_confirmed %>%
                                                    filter(`Country/Region` == Country)%>%
                                                    select(-1))),
                           Deaths = diff(as.numeric(aggregated_deaths %>%
                                                 filter(`Country/Region` == Country)%>%
                                                 select(-1))),
                           Recover = diff(as.numeric(aggregated_recover %>%
                                                  filter(`Country/Region` == Country)%>%
                                                  select(-1)))) 
  }
  
  df <- bind_cols(date.df, cases.df)
  df <- df %>% reshape2::melt(id = 'date')
  return(df)
}
form_timeseriesdata('United States', 'Daily New') -> try1

 n1 <- nPlot(value ~ date, group = 'variable', type = 'lineWithFocusChart', data = try1)
 n1$xAxis(tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( d * 86400000 ));}!#" )
n1$params$width = 501
# Define UI for application ##-----------
ui <- navbarPage(title = 'Worldwide Covid-19 Data',
                 id = 'navBar',
                 position = 'fixed-top',
                 selected = 'home',
                 collapsible = TRUE,
                 theme = shinytheme('cyborg'),
                 windowTitle = 'CIDER Covid-19 Dashboard',
                 header = tags$style(type = 'text/css',
                                     "body {padding-top:70px;}"),
                 footer = includeHTML('./www/footer.html'),
                 tags$head(includeCSS('mystyle.css')),
                 tags$link(href = 'https://fonts.googleapis.com/css?family=Cinzel Decorative', rel='stylesheet'),
                 ####### Home #######
                 tabPanel(title = 'Home',
                          value = 'home',
                          icon = icon('home'),
                          # background-----
                          fluidRow(
                            HTML("
                                 <section class = 'banner'>
                                 <h2 class='parallax'> COVID-19 VISUALIZATION </h2>
                                 <p class='parallax_description'> A tool to look up COVID-19 statistics at country level. </p>
                                 </section>")
                          ),
                          # introduction ------------
                          tags$hr(),
                          fluidRow(
                            column(3),
                            column(6, br(), br(), h1('INTRODUCTION', style = 'text-align: center;'), br(),
                                   h5('The COVID-19 pandemic, also known as the coronavirus pandemic, 
                                      is an ongoing pandemic of coronavirus disease 2019 (COVID-19) caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2), 
                                      first identified in December 2019 in Wuhan, China. 
                                      The World Health Organization declared the outbreak a Public Health Emergency of International Concern in January 2020 and a pandemic in March 2020.',
                                      style = 'text-align: center;')
                                   ),
                            column(3)
                          ),
                          br(),
                          br(),
                          tags$hr(),
                          # navigation --------------
                          fluidRow(
                            column(3),
                            column(6,
                                   br(), br(), h1('What you will find here', style = 'text-align: center;'), br(),
                                   h5('The Covid-19 data at country level will be presented from two dimensions of space and time',
                                      style = 'text-align: center;'),
                                   br(), br(),
                                   tags$div(align = 'center',
                                            tags$div(class = 'center', id = 'map-1',
                                                     style = 'display: inline-block; width: 225px',
                                                     icon('map-marked-alt'),
                                                     br(),
                                                     br(),
                                                     tags$a(class = 'btn',
                                                            onclick="$('li:eq(3) a').tab('show')",
                                                            'View data on map')),
                                            tags$div(class = 'vertical'),
                                            tags$div(class = 'center',
                                                     style = 'display: inline-block; width: 225px',
                                                     icon('clock'),
                                                     br(),
                                                     br(),
                                                     tags$a(class = 'btn',
                                                            onclick="$('li:eq(4) a').tab('show')",
                                                            'View timeline graph')
                                            )
                                   )),
                            column(3)
                          
                                   )
                          ),
                 # Map -----------
                 tabPanel(title = 'Data on Map',
                          id = 'map',
                          icon = icon('globe-asia'),
                          fluidRow(tags$div(class = 'Map',
                                            leafletOutput('Map', width = '100%', height = '100%'),
                                            absolutePanel(class = 'apanel', top = 30, left = 50, draggable = TRUE,
                                                          width = 300, height = 'auto',
                                                          h3('Global Cases', align = 'left', class = 'Global_num'),
                                                          h4(textOutput('Globalconfirm'), align = 'left', class = 'Global_num'),
                                                          h5(textOutput('Globaldeaths'), align = 'left', class = 'Global_num'),
                                                          h6(textOutput('Globalrecover'), align = 'left', class = 'Global_num'),
                                                          p(em(paste('Cut off at', as.character(Time.default))), align = 'left'),
                                                          br()),
                                            absolutePanel(class = 'apanel', top = 30, right = 10, draggable = TRUE,
                                                          width = 350, height = 'auto',
                                                          selectInput(inputId = 'Index', label = 'Choose an indicator',
                                                                      choices = list('Confirmed Cases' = 'Confirmed',
                                                                                     'Death Cases' = 'Deaths',
                                                                                     'Recover Cases' = 'Recovered')),
                                                          h3('Top 10 Countries', align = 'right', class = 'Global_num'),
                                                          plotOutput('topten'),
                                                          br(),
                                                          actionButton('Draw', label = 'Click me to see choropleth', width = '100%'),
                                                          checkboxInput('legend', 'Show Legend', TRUE, width = '100%'),
                                                          helpText(strong('Instruction:'), br(),
                                                                   'Step1: Choose the indicator you are concerned. Then number of top 10 countries shows',
                                                                   br(), 
                                                                   'Step2: Click the button above to see choropleth on map',
                                                                   br(),
                                                                   'Step3: Move the cursor on the country area to see specific numbers')
                                                         )
                                            )

                                  )
                          ),
                 ###### Timeline #########
                 tabPanel(title = 'Timeline for Covid-19',
                          id = 'timeline',
                          icon = icon('chart-line'),
                          sidebarLayout(sidebarPanel = sidebarPanel(h3('Control Widget'),
                                                                    selectInput('Countryselect', label = 'Choose a country', choices = sort(covid19_cases$Country)),
                                                                    radioButtons('displaymethod', 'Choose a display method', choices = c('Cumulative', 'Daily New')),
                                                                    helpText(strong('Instruction:'), br(), 'Step1: Choose the country you want to look through', br(),
                                                                             'Step2: Choose the way data display, cumulative or daily new cases?', br(),
                                                                             'Step3: Interact with chart, click button on right top to see Confirme, Deaths or Recover number', br(),
                                                                             'Step4: Click the mini graph below and choose a time interval. Then you can drag it and focus on any specific period')
                                                                    ),
                                        mainPanel = mainPanel(h2(textOutput('Title')),
                                                              br(),
                                                              br(),
                                                              fluidRow(tags$div(class = 'linegrapharea',
                                                                                showOutput('linegraph', 'nvd3')
                                                                                )
                                                                       )
                                                              )
                                        )
                          )
                 
                 )

# Define server -----------------
server <- function(input, output, session) {
  ####### Reactive Data ########### 
  ####### Color for Map ###########
  colmap <- reactive({
    if(input$Index == 'Confirmed'){colorQuantile('Greens', domain = covid19_cases[, input$Index])}
    else if(input$Index == 'Deaths'){colorQuantile('Oranges', domain = covid19_cases[, input$Index])}
    else {colorQuantile('Blues', domain = covid19_cases[, input$Index])}
  })
  
  ####### Map Output ##################
  output$Map <- renderLeaflet({
     leaflet(plot_map) %>%
       addProviderTiles(provider = providers$CartoDB.Positron) %>%
       setView(-0, 30, zoom = 3) 
   })
   
  observeEvent(input$Draw, {
    pal <- colmap()
    leafletProxy('Map', data = plot_map) %>%
      clearShapes() %>%
      addPolygons(color = 'white',
                  weight = 1,
                  smoothFactor = 0.1,
                  fillOpacity = 0.5,
                  fillColor = ~pal(covid19_cases[, input$Index]),
                  label = sprintf('<strong>%s %s:</strong><br/>%i', covid19_cases$Country, input$Index, covid19_cases[, input$Index])  %>% lapply(htmltools::HTML),
                  highlightOptions = highlightOptions(color = 'black', weight = 2, bringToFront = T),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")) %>%
      addMiniMap(position = 'bottomleft')-> proxy
    proxy %>% clearControls()
    if (input$legend){
      proxy %>%
        addLegend(position = 'bottomright',
                  pal = pal,
                  values = covid19_cases[, input$Index],
                  opacity = 0.8)
    }
  })
  ######### Top 10 groph ########
  output$topten <- renderPlot({
    top10plot(input$Index)
  })
  ######### text ########
  output$Globalconfirm <- renderText({
    paste(prettyNum(sum(covid19_cases$Confirmed), big.mark = ','), 'Confirmed')
  })
  output$Globaldeaths <- renderText({
    paste(prettyNum(sum(covid19_cases$Deaths), big.mark = ','), 'Deaths')
  })
  output$Globalrecover <- renderText({
    paste(prettyNum(sum(covid19_cases$Recovered), big.mark = ','), 'Recover')
  })
  # timeline output------------
  output$Title <- renderText({
    paste('Timelines of Covid 19 in', input$Countryselect, '(', input$displaymethod, ')')
  })
  dat_timeline <- reactive({
    form_timeseriesdata(input$Countryselect, input$displaymethod)
  })
  output$linegraph <- renderChart2({
    p <- nPlot(value ~ date, group = 'variable', type = 'lineWithFocusChart', data = form_timeseriesdata(input$Countryselect, input$displaymethod),
               width = session$clientData[["output_plot1_width"]], height = session$clientData[["output_plot1_height"]])
    p$xAxis(tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date( d * 86400000 ));}!#")
    # p$params$width <- 1200
    # p$params$height <- 800
    return(p)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
