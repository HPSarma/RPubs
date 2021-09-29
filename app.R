### Exercise 04 ####

# 1. Revise app to provide a default view of most recent distributions 
#    by most recent assignment due date

# 2. Revise app to include a selector by one or more students

# 3. Revise app to include doughnut charts to show completion, 
#    late or uncompleted assessments by assignment

# 4. Revise app to include a student list 

# https://hnpsarma.shinyapps.io/Assignment/


#Read in data 


#getwd()

library(readr)
library(shiny)
library(dplyr)
library(lubridate)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)

nyuclasses <- read_csv("nyuclasses.csv")

head(nyuclasses,4)

# boxplot(nyuclasses$score[nyuclasses$assessment=='Assignment 1'],frame.plot=FALSE,
#         main = 'Assignment1', col='#4cbea3', horizontal = TRUE)

# filter the data
nyuclasses <- nyuclasses%>%
  filter(str_detect(assessment,'Assignment'))

# Convert into factor
nyuclasses$assessment <- as.factor(nyuclasses$assessment)
# nyuclasses$assessment <-as.factor(nyuclasses$assessment)
# nlevels(nyuclasses$assessment)
# levels(nyuclasses$assessment)

#Create a variable 'status' of the assignment
nyuclasses$status <- NA
rowCount <- nrow(nyuclasses)
for (i in 1:rowCount) {
  if(is.na(nyuclasses$sub_date[i])==TRUE){
    nyuclasses$status[i] <- "uncompleted"
  } else if (nyuclasses$sub_date[i] < nyuclasses$due_date[i]){
    nyuclasses$status[i] <- "completed"
  } else if (nyuclasses$sub_date[i]>nyuclasses$due_date[i]){
    nyuclasses$status[i] <- "late"}
}

# #Convert to factor
nyuclasses$status <- as.factor(nyuclasses$status)


head(nyuclasses,10)

nyuclasses[(nyuclasses$sub_date==nyuclasses$due_date),]

# #Arrange the data with - most recent distributions
# nyuclasses2 <- nyuclasses%>%
#     group_by(nyuclasses$score) %>% slice(which.max(nyuclasses$due_date))


# nrow(nyuclasses)
# sum(is.na(nyuclasses$score))
# 
# # drop na
# #nyuclasses <- nyuclasses %>% drop_na('score')
# 
# nrow(nyuclasses)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("NYU CLASS DISTRIBUTION"),
  #navbarMenu("View Options",
  navbarPage("Click a tab on the right >",
             tabPanel("Default View",
                      h3("Default view of most recent distributions", align = "center"),
                      #sidebarLayout(
                      sidebarPanel(width=0),
                      mainPanel(width = 11,flowLayout(
                          plotOutput("Plot1"),
                          plotOutput("Plot2"),
                          plotOutput("Plot3"),
                          plotOutput("Plot4"),
                          plotOutput("Plot5"), align = "center")
                        )
                      #)
               
             ),
             
             
             tabPanel("Student List",
                      h3("Multi-Student Distribution", align = "center"),
                      #sidebarLayout(
                      sidebarPanel(width=3,
                          selectInput("name","Select multiple Students",
                                      choices =   unique(nyuclasses$name), 
                                      selected= sort(unique(nyuclasses$name))[1],
                                      multiple = TRUE, selectize = FALSE, size = 30)

                        ),
                      mainPanel(width = 9,flowLayout(
                        plotOutput("studentList1"),
                        plotOutput("studentList2"),
                        plotOutput("studentList3"),
                        plotOutput("studentList4"),
                        plotOutput("studentList5"))
                      )
                      #)

             ),
             
             tabPanel("Doughnut Chart",
                      h3("Doughnut Charts on the Assignment Status on Student list", align = "center"),
                      #sidebarLayout(
                      sidebarPanel(width=3,
                                   selectInput("name","Select multiple Students",
                                               choices =   unique(nyuclasses$name), 
                                               selected= sort(unique(nyuclasses$name))[1],
                                               multiple = TRUE, selectize = FALSE, size = 30)
                                   
                      ),
                      mainPanel(width = 9,flowLayout(
                        plotOutput("Doughnut1"),
                        plotOutput("Doughnut2"),
                        plotOutput("Doughnut3"),
                        plotOutput("Doughnut4"),
                        plotOutput("Doughnut5"))
                      )
                      #)

             ),
             
             tabPanel("Assessment Dashboard",
                      h3("Assignment distributions", align = "center"),
                      #sidebarLayout(
                      sidebarPanel(
                        
                        # Input: Selector for variable to plot the grades for the selected assignment ----
                        selectInput("assessment", "Assessment:",
                                    c("Assignment 1" = "Assignment 1",
                                      "Assignment 2" = "Assignment 2",
                                      "Assignment 3" = "Assignment 3",
                                      "Assignment 4" = "Assignment 4",
                                      "Assignment 5" = "Assignment 5"))
                        
                      ),
                      mainPanel(
                        
                        # Output: Plot of the requested variable against grade ----
                        plotOutput("gradePlot")
                        
                      )
                      #)
                      
             )
             
             
             )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot1 <- renderPlot({
        # nyuclasses2 <- subset(nyuclasses, nyuclasses$assessment=="Assignment 1")
        # nyuclasses2 <- nyuclasses2%>%
        #      group_by(nyuclasses2$score) %>% slice(which.max(nyuclasses$due_date))

        boxplot(nyuclasses$score[nyuclasses$assessment=="Assignment 1"], frame.plot=FALSE, 
                horizontal=TRUE, col="#4cbea3", main="Assignment 1")
    })
    
    output$Plot2 <- renderPlot({

      boxplot(nyuclasses$score[nyuclasses$assessment=="Assignment 2"], frame.plot=FALSE, 
              horizontal=TRUE, col="slategray2", main="Assignment 2")
    })
    
    output$Plot3 <- renderPlot({

      boxplot(nyuclasses$score[nyuclasses$assessment=="Assignment 3"], frame.plot=FALSE, 
              horizontal=TRUE, col="plum2", main="Assignment 3")
    })
    
    output$Plot4 <- renderPlot({
      
      boxplot(nyuclasses$score[nyuclasses$assessment=="Assignment 4"], frame.plot=FALSE, 
              horizontal=TRUE, col="lightsalmon1", main="Assignment 4")
    })
    
    output$Plot5 <- renderPlot({

      boxplot(nyuclasses$score[nyuclasses$assessment=="Assignment 5"], frame.plot=FALSE, 
              horizontal=TRUE, col="khaki1", main="Assignment 5")
    })
    
    df_filtered <-reactive({
      student <- input$name
      df <- subset(nyuclasses, name %in% student)
      df <- subset(df, df$assessment=="Assignment 1")
      df
    }) 
    
    output$studentList1 <- renderPlot({
      
      #boxplot
      # boxplot(nyuclasses$score[nyuclasses$assessment==x],frame.plot=FALSE,
      #         main = x, col='', horizontal = TRUE)
       student <- input$name
       df <- subset(nyuclasses, name %in% student)
      # data.count <- length(unique(df$name)) #Number of selected students
      # df <- subset(df, df$assessment=="Assignment 1")
      # df <- df %>% summarize(count = n())
      
      boxplot(df$score[df$assessment=="Assignment 1"], frame.plot=FALSE, 
              horizontal=TRUE, col="#4cbea3", main="Assignment 1")
      
      
    })
    
    output$studentList2 <- renderPlot({

      student <- input$name
      df <- subset(nyuclasses, name %in% student)

      boxplot(df$score[df$assessment=="Assignment 2"], frame.plot=FALSE, 
              horizontal=TRUE, col="slategray2", main="Assignment 2")
      
      
    })
    
    output$studentList3 <- renderPlot({

      student <- input$name
      df <- subset(nyuclasses, name %in% student)

      boxplot(df$score[df$assessment=="Assignment 3"], frame.plot=FALSE, 
              horizontal=TRUE, col="plum2", main="Assignment 3")
      
      
    })
    
    output$studentList4 <- renderPlot({

      student <- input$name
      df <- subset(nyuclasses, name %in% student)

      boxplot(df$score[df$assessment=="Assignment 4"], frame.plot=FALSE, 
              horizontal=TRUE, col="lightsalmon1", main="Assignment 4")
      
      
    })
    
    output$studentList5 <- renderPlot({
      
      student <- input$name
      df <- subset(nyuclasses, name %in% student)

      boxplot(df$score[df$assessment=="Assignment 5"], frame.plot=FALSE, 
              horizontal=TRUE, col="khaki1", main="Assignment 5")
      
      
    })
    
    output$Doughnut1 <- renderPlot({
      
      student <- input$name
      df <- subset(nyuclasses, name %in% student)
      data.count <- length(unique(df$name)) #Number of selected students
      df <- subset(df, assessment=="Assignment 1")
      df <- df %>% group_by(status)
      df <- df %>% summarize(count = n())
      completed <- df$count[df$status=="completed"] 
      completed <- completed[1]
      uncompleted <- df$count[df$status=="uncompleted"] 
      uncompleted<- uncompleted[1]
      late <- df$count[df$status=="late"] 
      late <- late[1]
      df1 <- data.frame(completed,uncompleted,late)
      df1[is.na(df1)] <- 0
      #Plot with ggpubr library
      lab <- names(df1)
      val <- as.integer(df1[1,1:3])
      df2 <- data.frame(
        group = lab,
        value = val)
      
      ggdonutchart(df2, "value",color="black", label = "group",
                   lab.pos = "out", 
                   fill = "value", title="Assignment 1" ,
                   
                   )
      
    })
    
    
    output$Doughnut2 <- renderPlot({
      
      student <- input$name
      df <- subset(nyuclasses, name %in% student)
      data.count <- length(unique(df$name)) #Number of selected students
      df <- subset(df, assessment=="Assignment 2")
      df <- df %>% group_by(status)
      df <- df %>% summarize(count = n())
      completed <- df$count[df$status=="completed"] 
      completed <- completed[1]
      uncompleted <- df$count[df$status=="uncompleted"] 
      uncompleted<- uncompleted[1]
      late <- df$count[df$status=="late"] 
      late <- late[1]
      df1 <- data.frame(completed,uncompleted,late)
      df1[is.na(df1)] <- 0
      #Plot with ggpubr library
      lab <- names(df1)
      val <- as.integer(df1[1,1:3])
      df2 <- data.frame(
        group = lab,
        value = val)
      
      ggdonutchart(df2, "value",color="red", label = "group",
                   lab.pos = "out", 
                   fill = "value", title="Assignment 2" ,
                   
      )
      
    })
    
    output$Doughnut3 <- renderPlot({
      
      student <- input$name
      df <- subset(nyuclasses, name %in% student)
      data.count <- length(unique(df$name)) #Number of selected students
      df <- subset(df, assessment=="Assignment 3")
      df <- df %>% group_by(status)
      df <- df %>% summarize(count = n())
      completed <- df$count[df$status=="completed"] 
      completed <- completed[1]
      uncompleted <- df$count[df$status=="uncompleted"] 
      uncompleted<- uncompleted[1]
      late <- df$count[df$status=="late"] 
      late <- late[1]
      df1 <- data.frame(completed,uncompleted,late)
      df1[is.na(df1)] <- 0
      #Plot with ggpubr library
      lab <- names(df1)
      val <- as.integer(df1[1,1:3])
      df2 <- data.frame(
        group = lab,
        value = val)
      
      ggdonutchart(df2, "value",color="sienna4", label = "group",
                   lab.pos = "out", 
                   fill = "value", title="Assignment 3" ,
                   
      )
      
    })
    
    output$Doughnut4 <- renderPlot({
      
      student <- input$name
      df <- subset(nyuclasses, name %in% student)
      data.count <- length(unique(df$name)) #Number of selected students
      df <- subset(df, assessment=="Assignment 4")
      df <- df %>% group_by(status)
      df <- df %>% summarize(count = n())
      completed <- df$count[df$status=="completed"] 
      completed <- completed[1]
      uncompleted <- df$count[df$status=="uncompleted"] 
      uncompleted<- uncompleted[1]
      late <- df$count[df$status=="late"] 
      late <- late[1]
      df1 <- data.frame(completed,uncompleted,late)
      df1[is.na(df1)] <- 0
      #Plot with ggpubr library
      lab <- names(df1)
      val <- as.integer(df1[1,1:3])
      df2 <- data.frame(
        group = lab,
        value = val)
      
      ggdonutchart(df2, "value",color="indianred4", label = "group",
                   lab.pos = "out", 
                   fill = "value", title="Assignment 4" ,
                   
      )
      
    })
    
    output$Doughnut5 <- renderPlot({
      
      student <- input$name
      df <- subset(nyuclasses, name %in% student)
      data.count <- length(unique(df$name)) #Number of selected students
      df <- subset(df, assessment=="Assignment 5")
      df <- df %>% group_by(status)
      df <- df %>% summarize(count = n())
      completed <- df$count[df$status=="completed"] 
      completed <- completed[1]
      uncompleted <- df$count[df$status=="uncompleted"] 
      uncompleted<- uncompleted[1]
      late <- df$count[df$status=="late"] 
      late <- late[1]
      df1 <- data.frame(completed,uncompleted,late)
      df1[is.na(df1)] <- 0
      #Plot with ggpubr library
      lab <- names(df1)
      val <- as.integer(df1[1,1:3])
      df2 <- data.frame(
        group = lab,
        value = val)
      
      ggdonutchart(df2, "value",color="lawngreen", label = "group",
                   lab.pos = "out", 
                   fill = "value", title="Assignment 5" ,
                   
      )
      
    })
    
    output$gradePlot <- renderPlot({
      grade_ad = input$assessment
      boxplot(nyuclasses$score[nyuclasses$assessment==grade_ad], frame.plot=FALSE, horizontal=TRUE, col="#4cbea3", main=grade_ad)
      
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
