
load('whiskies.RData')
library(shiny)
library(tidyverse)

referenceList <- wh %>% distinct(Distillery) %>% arrange(Distillery) %>% pull(Distillery) %>% as.character()
whScore <- wh %>%
  gather(Character, Score, -Distillery)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Single Malt Discovery, by Bespoke Data Insights Ltd"),
  sidebarPanel(
    selectizeInput('refList', "Your favourite single malts", choices = referenceList, multiple = T, size = 10),
    numericInput('numReco', 'Max number of recommendations', 10)
  ),
  mainPanel(
    helpText(h2("We recommend")),
    tableOutput('recommendedWhiskies'),
    helpText('Data Source: http://outreach.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html')
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$recommendedWhiskies <- renderTable({
    refT <- whScore %>% 
      filter(Distillery %in% input$refList) %>%
      group_by(Character) %>%
      summarise(RefScore=mean(Score)) %>%
      ungroup() %>%
      mutate(RefScore=RefScore/sqrt(sum(RefScore^2)))
    whScore %>%
      filter(!Distillery %in% input$refList) %>%
      group_by(Distillery) %>%
      mutate(Score=Score/sqrt(sum(Score^2))) %>%
      inner_join(refT, by='Character') %>%
      group_by(Distillery) %>% 
      summarise(Similarity=sum(Score*RefScore)) %>%
      ungroup() %>% 
      arrange(-Similarity) %>% 
      mutate(Score=paste(round(Similarity*100,2),'%',sep='')) %>%
      select(-Similarity) %>% 
      head(input$numReco)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

