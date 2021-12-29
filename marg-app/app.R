require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(tmap)
require(leaflet)
require(sf)
require(tidyverse)
require(plotly)
require(highcharter)
require(DT)
library(wordcloud)
require(ggwordcloud)
require(tidytext)
require(wordcloud)
require(RColorBrewer)
require(magrittr)

pal <- brewer.pal(8,"Dark2")


## basemaps
africa = spData::world %>% 
  filter(continent == "Africa") %>% 
  select(country = name_long)

## data processing

### margs before 2020
margs = list()

for (i in 1:3){
  margs[[i]] = readxl::read_excel("Interactive Maps- MARG.xlsx", sheet = i)
}


marg1 = margs[[1]] %>% rename(Name = `Full Name`) %>% 
  separate(col = "Latitude", into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Longitude = Longitude %>% as.numeric(), 
         Latitude = Latitude %>% as.numeric(), focus = "to conduct research on")

marg2 = margs[[2]]   %>% 
  separate(col = "Latitude", into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Longitude = Longitude %>% as.numeric(), 
         Latitude = Latitude %>% as.numeric(), focus = "to analyse data on")

marg3 = margs[[3]]   %>% 
  separate(col = "Latitude", into = c("Latitude", "Longitude"), sep = ",") %>%
  mutate(Longitude = Longitude %>% as.numeric(), 
         Latitude = Latitude %>% as.numeric(), focus = "to attend conference on")

grantee = marg1 %>% select(2:3,5:11,13) %>%  
  bind_rows(marg2 %>% select(2:3,5:9, 11:14) %>% 
              rename("Name" = 3, "Institution" = 7), 
            marg3 %>% select(2:3,5:8, 10:14)%>% 
              rename("Name" = 3,"Research Title" = 4, "Institution" = 7)) %>%
  mutate(Nationality = replace(Nationality, Nationality == "TANZANIAN", 'Tanzania'),
         Nationality = replace(Nationality, Nationality == "Tanzanian", 'Tanzania'),
         Nationality = replace(Nationality, Nationality == "South African", 'South Africa'),
         Nationality = replace(Nationality, Nationality == "Malagasy", 'Madagascar'),
         Nationality = replace(Nationality, Nationality == "Zimbabwean", 'Zimbabwe'))

### margs of 2020
margs2020 = readxl::read_excel("MARG I 2020.xlsx", sheet = 1)

margs2020 = margs2020 %>% 
  separate(col = coordinate, into = c("Latitude", "Longitude"),
           sep = ",", remove = TRUE) %>%
  select(Year = 2,1, Name = 3, `Research Title` = 8, 6,
         Gender = 5, Institution = 10, Latitude, Longitude) %>%
  mutate(focus = NA, Publication = NA,
         focus = focus %>% as.character(),
         Longitude = Longitude %>% as.numeric(),
         Latitude = Latitude %>% as.numeric())

if (ncol(margs2020) == ncol(grantee))
  
  grantee = grantee %>%
  bind_rows(margs2020)

grantee = grantee %>% 
  mutate(Nationality = replace(Nationality, Nationality == "Mauritian", "Mauritius"),
         Nationality = replace(Nationality, Nationality == "Zimbabwe", "South Africa"),
         Nationality = replace(Nationality, Nationality == "Portugal", "Mozambique"),
         Nationality = replace(Nationality, Nationality == "British", "Tanzania"))

## we added the country iso3 code
grantee = grantee %>% 
  mutate(Nationality = case_when(Nationality == "Tanzania" ~ "Tanzania - TZA",
                                 Nationality == "Madagascar" ~ "Madagascar - MDG",
                                 Nationality == "Kenya" ~ "Kenya - KEN",
                                 Nationality == "Mozambique" ~ "Mozambique - MOZ",
                                 Nationality == "Mauritius" ~ "Mauritius - MUS",
                                 Nationality == "Seychellis" ~ "Seychellis - SEY",
                                 Nationality == "South Africa" ~ "South Africa - ZAF"))

### add the html column
htm_marg3_popup = paste0('<h2 style="color:#7A7A7A;">',marg3$`Full Name`,'</h2>', 
                         '<p style="color:#00688B;">',
                         " From ", "<b>", marg3$`Home Institution`, "</b>", 
                         "<br>",
                         " Attended the ", 
                         "<i>",  marg3$`Conference Name`,"</i>", 
                         " at ", marg3$`Conference City`, 
                         " in ", marg3$Year,
                         " through ", "<b>",marg3$`Grant Type`,"</b>", " support",'</p>',
                         title = "The conference proceding is linked here
                         <a href = 'https://ir.library.oregonstate.edu/concern/conference_proceedings_or_journals/xd07gt68r' target = '_blank'> (Community Participation in Fisheries Management in Tanzania) </a>",
                         "<br>",
                         "<img src='http://www.seascapemodels.org/images/intertidal_scene.JPG' 
                         style='width:280px;height:230px;'>",
                         "<br>",
                         "The intertidal zone at Hornby Island")

all.marg.html = paste0('<h3 style="color:#7A7A7A;">',grantee$Name,'</h3>', 
                       '<p style="color: black;">',
                       " From ", grantee$Institution, " in ", grantee$Nationality, 
                       " Received Financial support through ", grantee$`Grant Type`,
                       " ",
                       grantee$focus, 
                       " ",
                       '<i>', grantee$`Research Title`, '</i>','</p>',
                       title = "You can check the page at
                         <a href = 'https://ir.library.oregonstate.edu/concern/conference_proceedings_or_journals/xd07gt68r' target = '_blank'>  Google Scholar </a>",
                       "<br>",
                       "<img src='http://www.seascapemodels.org/images/intertidal_scene.JPG' 
                         style='width:280px;height:230px;'>",
                       "<br>",
                       "The intertidal zone at Hornby Island" )

grantee.link = grantee %>% mutate(link = all.marg.html)




## end data processing

## begin of User interface for client

ui = fluidPage(
  tags$img(src = "wiomsa.png", width = "200px", height = "130px"),
  # titlePanel(title = "Marine Research Grants"),
  br(),
  br(),
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"), # “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, “materia”, “minty”, “pulse”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “yeti”,
  useShinydashboard(), # added this to make use of the function of shinydashboard and shinywidget package in shiny like the infobox for indicators of key
  
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      br(),
      p("The Marine Research Grant (MARG) Programme seeks to enhance capacity to conduct research and increase our understanding on various aspects of marine sciences, and offer opportunities for researchers from the WIO to present their results in different regional and international fora. In addition provides opportunities for targeting wider audiences and opportunities to learn special skills."),
      hr(),
      p("MARG I provides opportunities for researchers to conduct research projects. MARG-I Research Grants are awarded for a duration of 1 year with a ceil of US$ 10,000."),
      hr(),
      p("MARG II is intended to help researchers gain technical experience by working in a specific laboratory or for data analysis and manuscript write-up. This grant is offered for maximum duration of three months and a ceiling of US$ 6,000."),
      hr(),
      p("MARG III is geared toward assisting researchers to travel to attend scientific meetings and conferences for the purpose of presenting their work and learning from others. The maximum amount offered is US$ 3,000. MARG III grants are provided for the purchase of return tickets, accommodation or daily subsistence allowance"),
      hr(),
      p("For detailed instructions for applying for these grants contact the WIOMSA secretariat: secretary@wiomsa.org"),
      hr(),
      p("Developed by Semba M.R"),
      tags$img(src = "wior.png", width = "150px", height = "169px"),
      tags$br(),
      tags$br()
    ),
    mainPanel(
      width = 10,
      fluidRow(
        h3("Grant Indicators"),
        sliderInput(inputId = "year", label = "Year of Grant", min = 2018, max = 2022,value = 2018, step = 1, round = 0, ticks = TRUE, animate = TRUE),
        helpText("By choosing the type of grant in the box, it allows you to interact with the application. Simply choose the grant type and allows the application to compute and provide the write answer for you based on the data"),
        br(),
        br(),
        infoBoxOutput(outputId = "total", width = 2),
        infoBoxOutput(outputId = "granted", width = 2),
        infoBoxOutput(outputId = "states", width = 2),
        infoBoxOutput(outputId = "margi", width = 2),
        infoBoxOutput(outputId = "margii", width = 2),
        infoBoxOutput(outputId = "margiii", width = 2)
        
        
      ),
      br(),
      br(),
      fluidRow(
        tags$h4("Maps & data"),
        column(width = 2, 
               radioButtons(inputId = "grant", label = "Grant Type", choices = c("MARG I", "MARG II", "MARG III")),
               helpText("By choosing the type of grant in the box, it allows you to interact with the application. Simply choose the grant type and allows the application to compute and provide the write answer for you based on the data")
        ),
        column(width = 4, tmapOutput(outputId = "grantMap")),
        column(width = 6, DT::DTOutput(outputId = "grantTab"))
      ),
      br(),
      br(),
      fluidRow(
        tags$h3("Dominance of Grants by countries"),
        column(width = 1, selectInput(inputId = "yeara", label = "Select year", choices = 2018:2021)),
        column(width = 3, helpText("MARG I"), plotlyOutput(outputId = "mgi")),
        column(width = 3, helpText("MARG II"),  plotlyOutput(outputId = "mgii")),
        column(width = 3, helpText("MARG III"), plotlyOutput(outputId = "mgiii"))
        
      ),
      br(),
      br(),
      fluidRow(
        tags$h3("Dominance of Grants by Gender"),
        column(width = 2, selectInput(inputId = "gender", label = "Select gender", choices = c("Female", "Male"), selected = "Female")),
        column(width = 3, helpText("MARG I"), highchartOutput(outputId = "mgi1")),
        column(width = 3, helpText("MARG II"),  highchartOutput(outputId = "mgii1")),
        column(width = 3, helpText("MARG III"), highchartOutput(outputId = "mgiii1"))
        
      ),
      fluidRow(
        tags$h3("WordCloud"),
        column(width = 3, selectInput(inputId = "grantis", label = "Title WordCloud", choices = c("MARG I", "MARG II", "MARG III"), selected = "MARG I", multiple = FALSE)),
        column(width = 9, plotOutput(outputId = "wordcloud"))
      ),
      br(),
      br(),
      # fluidRow(
      #   tags$h3("key indicators")
      # ),
      # br(),
      # br(),
      # fluidRow(
      #   tags$h3("key indicators")
      # ),
      # br(),
      # br(),
      # fluidRow(
      #   tags$h3("key indicators")
      # ),
      br(),
      br(),
      
    )
  )
  
  
)
## end of user interface

## begin of server

server = function(input, output, session){
  
  member.state = reactive({
    
    grantee.link %>% 
      janitor::clean_names() %>% 
      filter(year == input$year) %>% 
      distinct(nationality ) %>%
      nrow()
    
  })
  
  grant.year = reactive({
    
    grantee.link %>% 
      janitor::clean_names() %>% 
      filter(year == input$year) 
    
  })
  
    grant.results = reactive({
    
    grantee.link %>% 
      janitor::clean_names() %>% 
      filter(year == input$year) %>% 
      group_by(grant_type) %>% 
      summarise(count = n(), .groups = "drop") 

    })
    
    # if (nrow(grant.results()) == 1) next
    # 
    # grant.results() = grant.results() %>%
    #   add_row(grant_type = c("MARG II", "MARG III"), count = 0)

## begin of indicators infoboxes
  
  output$total <- renderInfoBox({
    infoBox(title = HTML("TOTAL Granted<br>"),
            value = HTML("<p style='font-size:40px'>",
                         grantee.link %>% 
                           nrow() ,"</p>"),
            color = "green",
            icon = shiny::icon(name = "clock"),
            fill = TRUE
    )
  })
  
  output$granted = renderInfoBox({
    infoBox(title = HTML("in ",input$year,"<br>"),
            value = HTML("<p style='font-size:50px'>",
                         grant.year() %>% 
                           nrow(),"</p>"),
            color = "maroon", 
            icon = shiny::icon(name = "calendar-check"),
            fill = TRUE)
  })
  
  output$states = renderInfoBox({
    infoBox(title = HTML("States ",input$year,"<br>"),
            value = HTML("<p style='font-size:50px'>",
                         member.state(),"</p>"),
            color = "blue",
            icon = shiny::icon(name = "building"),
            fill = TRUE)
  })
  
  output$margi = renderInfoBox({
    infoBox(title = HTML("MARG I<br>"),
            value = HTML("<p style='font-size:50px'>",
                         grant.results() %>% 
                           filter(grant_type=="MARG I") %>% 
                           pull(count) ,"</p>"),
            color = "yellow",
            icon = shiny::icon(name = "map-marked-alt"),
            fill = TRUE)
  })
  
  output$margii = renderInfoBox({
    infoBox(title = HTML("MARG II<br>"),
            value = HTML("<p style='font-size:50px'>",
                         grant.results() %>% 
                           filter(grant_type=="MARG II") %>% 
                           pull(count) ,"</p>"),
            color = "aqua",
            icon = shiny::icon(name = "microscope"),
            fill = TRUE)
  })
  
  output$margiii = renderInfoBox({
    infoBox(title = HTML("MARG III<br>"),
            value = HTML("<p style='font-size:50px'>",
                         grant.results() %>% 
                           filter(grant_type=="MARG III") %>% 
                           pull(count) ,"</p>"),
            color = "teal",
            icon = shiny::icon(name = "plane"),
            fill = TRUE)
  })
  
## end of indicators infoboxes
  
## begin of countries distribution map
  
  output$grantMap = renderTmap({
    tmap_mode(mode = "view")
    
     grant.country =   grantee.link %>% 
        janitor::clean_names() %>% 
        filter(!is.na(longitude) & !is.na(latitude)) %>% 
        filter(grant_type == input$grant) 
       
     
    
    grantee.sf = grant.country %>% 
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
      
      aa = grantee.sf %>% 
        st_join(africa, join = st_within) %>% 
        group_by(country) %>% 
        count() %>% 
        st_drop_geometry() %>% 
        as_tibble()
      
    
    
    bb = africa %>% 
      left_join(aa) 
    
    bb %>% 
      tm_shape() +
      tm_polygons(col = "n", id = "country", popup.vars = c("Grantee" = "n"), title = "Grantees")+
      tm_view(set.view = c(lon = 40, lat = -14, zoom = 3.2))
  })
  
## end of countries distribution map
  
output$grantTab = renderDT({
  
  grantee.link %>% 
    select(c(1:2,7)) %>% 
    janitor::clean_names() %>% 
    # filter(!is.na(longitude) & !is.na(latitude)) %>% 
    filter(grant_type == input$grant) %>% 
    DT::datatable(rownames = FALSE, options = list(pageLength = 8, autoWidth = TRUE))
  

})

output$mgi = renderPlotly({
  
  grantee.link %>% 
    janitor::clean_names() %>% 
    mutate(gender = if_else(gender == "M", "Male", gender), 
           gender = if_else(gender == "F", "Female",gender)) %>% 
    filter(year == input$yeara & grant_type == "MARG I") %>% 
    # filter(year == 2018) %>%
    separate(col = nationality, into = c("country", "code"), sep = " - ") %>% 
    group_by(country) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    plot_ly(type = "pie", 
            labels = ~country, 
            values = ~n, 
            hole = .5, 
            textinfo='label+percent',
            insidetextorientation='radial') %>% 
    layout(
      showlegend = F,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                   showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, 
                   zeroline = FALSE, showticklabels = FALSE)
    ) 
  
})


output$mgii = renderPlotly({
  
  grantee.link %>% 
    janitor::clean_names() %>% 
    mutate(gender = if_else(gender == "M", "Male", gender), 
           gender = if_else(gender == "F", "Female",gender)) %>% 
    filter(year == input$yeara & grant_type == "MARG II") %>% 
    # filter(year == 2018) %>%
    separate(col = nationality, into = c("country", "code"), sep = " - ") %>% 
    group_by(country) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    plot_ly(type = "pie", 
            labels = ~country, 
            values = ~n, 
            hole = .5, 
            textinfo='label+percent',
            insidetextorientation='radial') %>% 
    layout(
      showlegend = F,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                   showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, 
                   zeroline = FALSE, showticklabels = FALSE)
    ) 
  
})

output$mgiii = renderPlotly({
  
  grantee.link %>% 
    janitor::clean_names() %>% 
    mutate(gender = if_else(gender == "M", "Male", gender), 
           gender = if_else(gender == "F", "Female",gender)) %>% 
    filter(year == input$yeara & grant_type == "MARG III") %>% 
    # filter(year == 2018) %>%
    separate(col = nationality, into = c("country", "code"), sep = " - ") %>% 
    group_by(country) %>% 
    summarise(n = n(), .groups = "drop") %>% 
    plot_ly(type = "pie", 
            labels = ~country, 
            values = ~n, 
            hole = .5, 
            textinfo='label+percent',
            insidetextorientation='radial') %>% 
    layout(
      showlegend = F,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                   showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, 
                   zeroline = FALSE, showticklabels = FALSE)
    ) 
  
})
  
output$mgi1 = renderHighchart2({
  
  grantee.link %>% 
    janitor::clean_names() %>% 
    mutate(gender = if_else(gender == "M", "Male", gender), 
           gender = if_else(gender == "F", "Female",gender)) %>% 
    filter(gender == input$gender & grant_type == "MARG I") %>% 
    # filter(year == 2018) %>%
    separate(col = nationality, into = c("country", "code"), sep = " - ") %>% 
    group_by(country) %>% 
    summarise(n = n(), .groups = "drop")  %>%
    arrange(n)%>%
    hchart(type = "bar", hcaes(x = country, y = n))
  
  
})


output$mgii1 = renderHighchart2({
  
  grantee.link %>% 
    janitor::clean_names() %>% 
    mutate(gender = if_else(gender == "M", "Male", gender), 
           gender = if_else(gender == "F", "Female",gender)) %>% 
    filter(gender == input$gender & grant_type == "MARG II") %>% 
    # filter(year == 2018) %>%
    separate(col = nationality, into = c("country", "code"), sep = " - ") %>% 
    group_by(country) %>% 
    summarise(n = n(), .groups = "drop")  %>%
    arrange(n)%>%
    hchart(type = "bar", hcaes(x = country, y = n))
  
  
})

output$mgiii1 = renderHighchart2({
  
  grantee.link %>% 
    janitor::clean_names() %>% 
    mutate(gender = if_else(gender == "M", "Male", gender), 
           gender = if_else(gender == "F", "Female",gender)) %>% 
    filter(gender == input$gender & grant_type == "MARG III") %>% 
    separate(col = nationality, into = c("country", "code"), sep = " - ") %>% 
    group_by(country) %>% 
    summarise(n = n(), .groups = "drop") %>%
    arrange(n)%>%
    hchart(type = "bar", hcaes(x = country, y = n))
    
    
  
})

output$wordcloud = renderPlot({
  
  doc = grantee.link %>% 
    janitor::clean_names() %>% 
    filter(grant_type == input$grantis) %>%
    # filter(year == 2020) %>% 
    pull(research_title)%>% 
    tm::VectorSource() %>%
    tm::Corpus() %>% 
    # clean text data
    tm::tm_map(tm::removePunctuation) %>%
    tm::tm_map(tm::removeNumbers) %>%
    tm::tm_map(tolower)  %>%
    tm::tm_map(tm::removeWords, tm::stopwords("english")) %>%
    tm::tm_map(tm::stripWhitespace) %>%
    tm::tm_map(tm::PlainTextDocument)
  
  
  doc2 = doc$content$content %>% 
    as_tibble() %>% 
    stringr::str_split(pattern = " ") %>% 
    as.data.frame() %>% 
    rename(text = 1)
  
  doc.group = doc2 %>% 
    group_by(text) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    ungroup()
  
  doc.group  %$% 
    ggwordcloud::ggwordcloud(words = text,
                             freq = n, 
                             random.order = FALSE, 
                             max.words = 120, 
                             colors = pal)
})

}

## end of server

shinyApp(ui = ui, server = server)



