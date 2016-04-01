require(shiny)
require(shinydashboard)

createAssessment <-
  function(title, file, ..., assessmentIcon = icon("minus")) {
    data <- read.csv(file, ...)
    print("hi")
    print(data)
    sidebar <- menuItem(title, tabName = title, icon = assessmentIcon)
    main <- tabItem(tabName = title, h2(title))
    assessment <- list(sidebar, main)
    return(assessment)
  }