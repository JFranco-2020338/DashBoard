library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

montos<-ggplot(my.new.file[1:5,], aes(x =average_review_rating, y=product_name))+ 
  geom_bar(stat="identity", position="dodge", fill="white",col="steelblue")+
  labs(y="Name Product", x="Average Rating")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

graf<-ggplot(my.new.file[1:5,], aes(x =price, y=product_name))+ 
  geom_bar(stat="identity", fill="white",col="steelblue")+
  labs(y="Name Product", x="price ")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

  #GRAFICO DE BARRAS
 counts <- table(my.new.file$manufacturer)
 count <-barplot(counts, main="Tops Manufacturers",
        xlab="Manufacturer")
 
 #HISTOGRAMA DE FRECUENCIAS 
 His <-hist(my.new.file$price, main = "Histograma price",
      xlab = "price",
      ylab = "Frecuencia",
      col = "red",
      border = "black",
      xlim = c(18, 97),
      ylim = c(0, 150))
 
 #GRAFICO DE CAJAS 
 boxplot(my.new.file$number_of_answered_questions, main = "Number of answered questions",
         outline = TRUE)
 
 #
 df<-data.frame(zona=c("A","B","C"), outcome=c(11.07,24.71,39.47))
 ggplot(df,aes(zona,outcome))+geom_col()
 
 df


 
 
 
 
 ui<-dashboardPage(title= "Dashboard", skin= "green",
                  dashboardHeader(title="PROYECTO",
                                  
                                  
                                  dropdownMenu(type="tasks",
                                               taskItem(value=50,
                                                        text="Avance del DadhBoard",
                                                        color="blue"),
                                               taskItem(value=10,
                                                        text="Avance del DashBoard Header",
                                                        color="red")
                                  )
                                  
                  ),
                  dashboardSidebar(
                    sidebarSearchForm("searchText","buttonSearch","Buscar",icon = shiny::icon("amazon"))
                    ,
                    sidebarMenu(id="sidebarID",
                                menuItem("Primera ventana", tabName = "montos"),
                                menuItem("Segunda ventana",id = "chartsID",
                                         menuSubItem("DataSet", tabName = "datos", icon = shiny::icon("eye")),
                                         ##menuSubItem("video", tabName="video"),
                                         menuSubItem("Sub-ventana3",icon =icon("amazon"))
                                )
                    )
                  ),
                  dashboardBody(
                    
                    tabItems(tabItem(tabName = "datos", 
                                     DT::dataTableOutput("datos")
                                     
                                     
                    ),
                    tabItem(tabName = "montos", 
                            
                            fluidRow(
                              column(width=9,  
                                     valueBox("10000 Productos","Juguetes en stock",icon=icon("eye"),color="yellow"),
                                     valueBox("3.5 - 5.0 stars","Average review rating",icon=icon("star"),color="blue"),
                                     valueBoxOutput("valuebox"),
                              ),
                              column(width=3, imageOutput("figura", width="10%",height="150px")),

                              fluidRow(box(title="Most recommended products",plotOutput("graf"), width=9, status="primary", solidHeader=TRUE)),
                              fluidRow(box(title="Average review rating",plotOutput("montos"), width=9, status="primary", solidHeader=TRUE)))
                            
                    ),
                    tabItem(tabName="video", 
                            box(title="Video", tags$video(type = "video/mp4",src = "clase5.mp4", controls = "controls",
                                                          height="150px", width="400px"), width=12, status="primary", solidHeader=TRUE))
                    
                    )
                  ))

server <- function(input, output) { 
  
  redondeo <- function(x, k) as.numeric(trimws(format(round(x, k), nsmall=2)))
  output$datos<-DT::renderDataTable(my.new.file)
  output$montos<-renderPlot({montos})
  output$graf<-renderPlot({graf})
  output$count<-renderPlot({count})
  output$His<-renderPlot({His})
  output$valuebox<-renderInfoBox({valueBox(redondeo(sum((my.new.file)[6])),"Number of reviews!!", 
                                           icon=icon("money"),color="red")})
 
  
}

shinyApp(ui, server)