
# Import libraries
library(shiny)
library(DT)
library(magrittr)
library(dplyr)
library(ggplot2)

# Import data
bookData <- read.csv(file = "data/bookData.csv")

# UI
ui <- fluidPage(
  navbarPage(
    title = "Matt's book data",
    theme = "www/styles.css",
    id = "main_navbar",
    
    tabPanel(
      title = "Home",
      h1("Welcome to Matt's bookshelf!"),
      HTML("<p class='subtitle'>This is an 
            <a href='https://shiny.rstudio.com/'>
						R Shiny app</a> 
						developed to share data visualizations of Matt's bookshelf.  
						The site draws data from a single spreadsheet of his books and 
						automatically creates visualizations of trends and relationships 
						in the data.  Click through the tabs to explore and interact with 
						the data.</p>"),
      h2("About the data"),
      HTML("<p>The data was collected by surveying Matt's bookshelves, then 
           collecting additional information on the books and authors from 
           sources like Goodreads, Amazon Books, and Wikipedia.  Some data, 
           such as genre designation, were subjective and assigned based on 
           my best judgement.  Others, such as authors' genders and places of 
           origin, are accurate 
           to the best of my ability, but may be subject to change or error.  
           If you spot incorrect information, please 
           <a href='mailto:cottrellkat@gmail.com'>
									email me</a>.</p>"),
      h2("How to use"),
      HTML("<p>Click through the tabs in the top navigation bar to explore and 
            interact with the data.  Visualizations are individually titled, 
           labelled, and described.  Some are interactive.  Unfortunately 
           R Shiny does not currently support hover-to-see-more for data points, 
           but if you are curious about a particular data point, note its 
           characteristics and look it up in the Catalog tab.</p>")
    ),
    
    tabPanel(
      title = "Titles",
      h1("Explore the titles"),
      HTML("<p class='subtitle'>Explore data pertaining to individual 
           titles.</p>"),
      h2("Browse books"),
      DT::dataTableOutput('bookTable'),
      h2("Random Fantasy Title Generator"),
      fluidRow(column(2,
                      actionButton("getRandTitle", "Generate my random title!")),
               column(9, offset = 1,
                      textOutput("randomOfTitle"))),
      tags$head(tags$style("#randomOfTitle{font-family: Copperplate, 'Copperplate Gothic Light', fantasy;
                                           font-size: 48px;}")),
      h2("Page counts over time"),
      plotOutput("pgsOverTime")
    ),
    
    tabPanel(
      title = "Authors",
      h1("Explore the authors"),
      HTML("<p class='subtitle'>Explore data pertaining to the authors.</p>"),
      h2("Browse authors"),
      DT::dataTableOutput('authorTable'),
      h2("Repeat authors"),
      plotOutput("authorCounts")
    ),
    
    tabPanel(
      title = "Gender",
      h1("Explore author and main character gender"),
      HTML("<p class='subtitle'>Explore data pertaining to the gender makeup of 
           the authors and main characters.</p>"),
      h2("Gender of authors and main characters"),
      plotOutput("mcGenderbyAuthorGender"),
      h2("Gender of authors over time"),
      plotOutput("genderOverTime"),
      h2("Total ratings on Goodreads by Author's Gender"),
      plotOutput("ratingsByGender"),
      h2("Genre by Author's gender"),
      plotOutput("genreByGender")
    ),
    
    tabPanel(
      title = "Genre",
      h1("Explore genre"),
      HTML("<p class='subtitle'>Explore data pertaining to the genres of 
           the books.</p>"),
    ),
    
    tabPanel(
      title = "Catalog",
      h1("Search the records"),
      HTML("<p class='subtitle'>Search for catalog records by title, author, 
           year, or other key criteria.</p>"),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          h3("Select your criteria"),
          fluidRow(
            column(width = 3,
                   p("Published:")),
            column(width = 4,
                   numericInput(inputId = "minYear",
                                label = NULL, 
                                value = min(bookData$A_Born), 
                                min = min(bookData$A_Born), 
                                max = max(bookData$A_Born), 
                                step = 1)),
            column(width = 1, p("-")),
            column(width = 4,
                   numericInput(inputId = "maxYear",
                                label = NULL, 
                                value = max(bookData$A_Born), 
                                min = min(bookData$A_Born), 
                                max = max(bookData$A_Born), 
                                step = 1)
            )
          ),
          fluidRow(
            column(width = 3,
                   p("Pages:")),
            column(width = 4,
                   numericInput(inputId = "minPgsInput",
                                label = NULL, 
                                value = min(bookData$Pgs), 
                                min = min(bookData$Pgs), 
                                max = max(bookData$Pgs), 
                                step = 1)),
            column(width = 1, p("-")),
            column(width = 4,
                   numericInput(inputId = "maxPgsInput",
                                label = NULL, 
                                value = max(bookData$Pgs),
                                min = min(bookData$Pgs), 
                                max = max(bookData$Pgs), 
                                step = 1)
            )
          ),
          fluidRow(
            column(width = 3,
                   p("Ratings:")),
            column(width = 4,
                   numericInput(inputId = "minRatingsInput",
                                label = NULL, 
                                value = min(bookData$Goodreads), 
                                min = min(bookData$Goodreads), 
                                max = max(bookData$Goodreads), 
                                step = 1)),
            column(width = 1, p("-")),
            column(width = 4,
                   numericInput(inputId = "maxRatingsInput",
                                label = NULL, 
                                value = max(bookData$Goodreads), 
                                min = min(bookData$Goodreads), 
                                max = max(bookData$Goodreads), 
                                step = 1)
            )
          ),
          fluidRow(
            column(width = 3,
                   p("Author born:")),
            column(width = 4,
                   numericInput(inputId = "minYrAuthorBornInput",
                                label = NULL, 
                                value = min(bookData$A_Born), 
                                # min = min(bookData$A_Born), 
                                # max = max(bookData$A_Born), 
                                step = 1)),
            column(width = 1, p("-")),
            column(width = 4,
                   numericInput(inputId = "maxYrAuthorBornInput",
                                label = NULL, 
                                value = max(bookData$A_Born), 
                                # min = min(bookData$A_Born), 
                                # max = max(bookData$A_Born), 
                                step = 1)
            )
          ),
          fluidRow(
            column(width = 3,
                   p("Author count:")),
            column(width = 4,
                   numericInput(inputId = "minYrAuthorCountInput",
                                label = NULL, 
                                value = min(bookData$A_Ct), 
                                # min = min(bookData$A_Ct), 
                                # max = max(bookData$A_Ct), 
                                step = 1)),
            column(width = 1, p("-")),
            column(width = 4,
                   numericInput(inputId = "maxYrAuthorCountInput",
                                label = NULL, 
                                value = max(bookData$A_Ct), 
                                # min = min(bookData$A_Ct), 
                                # max = max(bookData$A_Ct), 
                                step = 1)
            )
          ),
          fluidRow(
            column(width = 5,
                   checkboxGroupInput(inputId = "FNFInput", 
                                      label = "Fiction/non-fiction:",
                                      choiceNames = list("Fiction", "Nonfiction"),
                                      choiceValues = list("F", "NF")
                   ),
                   checkboxGroupInput(inputId = "genreInput", 
                                      label = "Genre:",
                                      choiceNames = list("Art", "Fantasy", "Fiction",
                                                         "History", "Science Fiction"),
                                      choiceValues = list("Art", "Fantasy", "Fiction",
                                                          "History", "Science Fiction")
                   )
                   
                   
            ),
            column(width = 7,
                   checkboxGroupInput(inputId = "authorGenderInput", 
                                      label = "Author gender:",
                                      choiceNames = list("Male", "Female"),
                                      choiceValues = list("M", "F")
                   ),
                   checkboxGroupInput(inputId = "mainCharGenderInput", 
                                      label = "Main character gender:",
                                      choiceNames = list("Male", "Female", 
                                                         "Multiple", 
                                                         "Other gender", 
                                                         "Gender not applicable"),
                                      choiceValues = list("M", "F", "MULT", "X", "NA")
                   )
            ),
            column(width = 12,
                   checkboxGroupInput(inputId = "authorOriginInput", 
                                      label = "Author place of origin:",
                                      choiceNames = list("A", "B"),
                                      choiceValues = list("A", "B"))
            )
          )
        ),
        mainPanel(
          DT::dataTableOutput('catalogTable')
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
  
  # CATALOG --------------------------------------------------------------------
  
  output$catalogTable <- renderDataTable({
    bookData
  })
  
}

shinyApp(ui = ui, server = server)