
# Import libraries
library(shiny)
library(DT)
library(magrittr)
library(dplyr)
library(ggplot2)

# Import data
bookData <- read.csv(file = 'data/bookData.csv')

# UI
ui <- fluidPage(
  
  navbarPage(
    
    "Matt's book data",
    id = "main_navbar",
    
    tabPanel(
      "Home",
      fluidRow(
        column(3, h3("")), 
        column(3, h1("Home"))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          h3("sidebar panel")
        ),
        mainPanel(
          h2("About the data"),
          h2("Browse the books"),
          DT::dataTableOutput('bookTable'),
          h2("Browse the authors"),
          DT::dataTableOutput('authorTable')
        )
      )
    ),
    
    tabPanel(
      "Titles",
      fluidRow(column(3, h3("")), 
               column(3, h1("Titles"))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          h3("sidebar panel")
        ),
        mainPanel(
          h2("Random Fantasy Title Generator"),
          actionButton("getRandTitle", "Generate my random title!"),
          textOutput("randomOfTitle"),
          tags$head(tags$style("#randomOfTitle{font-family: Copperplate, 'Copperplate Gothic Light', fantasy;
                                               font-size: 48px;}")),
          h2("Page counts over time"),
          plotOutput("pgsOverTime")
        )
      )
    ),
    
    tabPanel(
      "Authors",
      fluidRow(column(3, h3("")), 
               column(3, h1("Authors"))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          h3("sidebar panel")
        ),
        mainPanel(
          h2("About the authors"),
          plotOutput("authorCounts")
        )
      )
    ),
    
    tabPanel(
      "Gender",
      fluidRow(column(3, h3("")), 
               column(3, h1("Gender"))
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          h3("sidebar panel")
        ),
        mainPanel(
          h2("Gender of authors and main characters"),
          plotOutput("mcGenderbyAuthorGender"),
          h2("Gender of authors over time"),
          plotOutput("genderOverTime"),
          h2("Total ratings on Goodreads by Author's Gender"),
          plotOutput("ratingsByGender"),
          h2("Genre by Author's gender"),
          plotOutput("genreByGender")
        )
      )
    )
  )
)


################################# SERVER LOGIC #################################

server <- function(input, output) {
  
  recentBooks <- bookData %>%
    filter(Year >= 1950)
  
  # TITLES ---------------------------------------------------------------------
  
  bookOnlyData <- bookData %>%
    select(Title, Author, Year, Genre, X.N.F, Pgs, Goodreads, MC_gender)
  output$bookTable <- DT::renderDataTable({
    bookOnlyData
  })
  
  ofTitles <- dplyr::filter(bookData, grepl(' of ', Title))
  myRandomTitle <- eventReactive(input$getRandTitle, {
    randomOfTitles <- sample(1:nrow(ofTitles), 2, replace = F)
    beforeOf <- strsplit(ofTitles$Title[randomOfTitles[1]], " of ")[[1]][1]
    afterOf  <- strsplit(ofTitles$Title[randomOfTitles[2]], " of ")[[1]][2]
    paste(beforeOf, " of ", afterOf)
  })
  output$randomOfTitle <- renderText({
    myRandomTitle()
  })
    
  
  # AUTHORS --------------------------------------------------------------------
  
  authorOnlyData <- bookData %>%
    select(Author, A_Born, A_Origin, A_Gender, A_Ct) %>%
    unique()
  output$authorTable <- DT::renderDataTable({
    authorOnlyData
  })
  
  repeatAuthors <- bookData %>%
    filter(A_Ct > 1)
  output$authorCounts <- renderPlot({
    ggplot(repeatAuthors, 
           aes(Author)) + 
      geom_bar() +
      geom_text(stat='count', aes(label=..count..), vjust=-1) +
      labs(title = "Authors represented multiple times",
           x = "Authors", 
           y = "Books by each author") + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  # GENDER ---------------------------------------------------------------------
  
  output$mcGenderbyAuthorGender <- renderPlot ({
    ggplot(bookData, 
           aes(A_Gender, fill = MC_gender)) + 
      geom_bar(position = "dodge", stat ="count") +
      scale_fill_manual(values=c("#ff82c5", "#54afff", "#c987ff",
                                 "#a8a8a8", "#cccccc")) +
      labs(title = "Gender of MCs by gender of author",
           x = "Gender of author", 
           y = "Books with MCs of each gender") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$genderOverTime <- renderPlot({
    ggplot(data = recentBooks, 
           aes(x = Year,
               fill = A_Gender)) + 
      geom_histogram(binwidth = 5, col = "white", position ="dodge") +
      scale_fill_manual(values=c("#ff82c5", "#54afff")) +
      scale_x_continuous(breaks = seq(1950,2022, 5)) +
      labs(title = "Books by male and female authors since 1950",
           x = "Year of first publication (Binwidth = 5 years)", 
           y = "Books published") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$ratingsByGender <- renderPlot({
    ggplot(data = bookData, 
           aes(x = Goodreads,
               fill = A_Gender)) + 
      geom_histogram(binwidth = 100000, col = "white", position ="dodge") +
      scale_fill_manual(values=c("#ff82c5", "#54afff")) +
      scale_x_continuous(breaks = seq(0, 1400000, 100000)) +
      labs(title = "Total ratings on Goodreads by Gender",
           x = "Total ratings on Goodreads (binwidth = 100,000))", 
           y = "Books") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$genreByGender <- renderPlot ({
    ggplot(bookData, 
           aes(Genre, fill = A_Gender)) + 
      geom_bar(position = "dodge", stat ="count") +
      scale_fill_manual(values=c("#ff82c5", "#54afff")) +
      labs(title = "Genre by gender",
           x = "Genre by gender", 
           y = "Books") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  # OVER TIME ------------------------------------------------------------------
  
  output$pgsOverTime <- renderPlot({
    ggplot(data = recentBooks, 
           aes(x = Year, y = Pgs, color = Genre)) +
      geom_point(alpha = 0.7) +
      #coord_cartesian(xlim = c(1950, 2022)) +
      labs(title = "Page counts of books published since 1950",
           x = "Year of first publication", 
           y = "Pages in book") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$genderOverTime <- renderPlot({
    ggplot(data = recentBooks, 
           aes(x = Year,
               fill = A_Gender)) + 
      geom_histogram(binwidth = 5, col = "white", position ="dodge") +
      scale_x_continuous(breaks = seq(1950,2022, 5)) +
      labs(title = "Books by male and female authors since 1950",
           x = "Year of first publication (Binwidth = 5 years)", 
           y = "Books published") + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}

shinyApp(ui = ui, server = server)