
# Generaciones sandwich y gransandwich

#___________________________________________________________________________________________________

# Fuente de la información utilizada en este tablero: 
# Alburez-Gutierrez, D., Mason, C., and Zagheni, E. (2021). 
# The 'Sandwich Generation' Revisited: Global Demographic Drivers of Care Time Demands. 
# Population and Development Review. 
# DOI:10.1111/padr.12436.
# Disponible en: https://github.com/alburezg/sandwich_clean

#___________________________________________________________________________________________________


# Configuracion ----

rm(list=ls())
gc()

# install.packages("pacman")

pacman::p_load(foreign, data.table, ggplot2, 
               RColorBrewer, dplyr, grid, gtable, 
               here, shiny, shinydashboard)

options(encoding = "utf-8")

options(scipen=999)

#___________________________________________________________________________________________________ 

# Material ----
data <- read.csv("tablex_full_country_results.csv")

paises <- unique(data$country)
cohorte <- unique(data$cohort)

tema <- theme_minimal() +
  theme(axis.text = element_text(size = 11, colour = "gray40"),
        axis.title = element_text(size = 13, colour = "gray40"), 
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) 


# Estructura de tablero ----

encabezado <- dashboardHeader(title = "Estado de sandwich")  

lateral <- dashboardSidebar(
  sidebarMenu(
    menuItem("Generación Sandwich",tabName="sandwich",icon=icon("person")),
    menuItem("Generación Gransandwich",tabName="gransandwich",icon=icon("people-group")),
    width = 300, collapsed = T))


# Contenido ----
f1 <- fluidRow(
  box(title = "País",
      solidHeader = F,
      background = "purple",
      collapsible = F,
      width = 3,
      selectInput(inputId="pais", label=" ", choices=paises, multiple=F)))


f2 <- fluidRow(
  box(
    title = "Años de vida en estado de sandwich",
    solidHeader = T,
    collapsible = F,
    plotOutput("graf1", height = "300px"),
    width = 5),
  
  box(
    title = "Porcentaje de vida en estado de sandwich",
    solidHeader = T,
    collapsible = F, 
    plotOutput("graf2", height = "300px"),
    width = 5))

f3 <- fluidRow(
  box(title = "Porcentaje de la cohorte en estado de sandwich",
      solidHeader = T,
      collapsible = F,
      tableOutput("tabla1"), 
      width = 6)
)

f4 <- fluidRow(
  box(title = "País",
      solidHeader = F,
      background = "purple",
      collapsible = F,
      width = 3,
      selectInput(inputId="pais2", label=" ", choices=paises, multiple=F)))


f5 <- fluidRow(
  box(
    title = "Años de vida en estado de gransandwich",
    solidHeader = T,
    collapsible = F,
    plotOutput("graf3", height = "300px"),
    width = 5),
  
  box(
    title = "Porcentaje de vida en estado de gransandwich",
    solidHeader = T,
    collapsible = F, 
    plotOutput("graf4", height = "300px"),
    width = 5))

f6 <- fluidRow(
  box(title = "Porcentaje de la cohorte en estado de gransandwich",
      solidHeader = T,
      collapsible = F,
      tableOutput("tabla2"), 
      width = 6)
)


# Cuerpo ----
cuerpo <-  dashboardBody(
  tags$head(
    tags$style(
      HTML('
           h3 {
           font-weight: bold;
           font-size: 22px;
           font-family: Tahoma, Geneva, sans-serif;
           }',
           ".small-box {height: 103px}",
           'h2 {
             font-size: 16px;
             font-family: Tahoma, Geneva, sans-serif;
             color: Gray;
           }',
           '#tabla1 {font-size: 18px;}',
           '#tabla2 {font-size: 18px;}'
      ))),
  tabItems(
    tabItem(tabName = "sandwich", f1, f2, f3), 
    tabItem(tabName = "gransandwich", f4, f5, f6)),
    tags$footer(
      HTML("Fuente de la información utilizada en este tablero: Alburez-Gutierrez, D., Mason, C., and Zagheni, E. (2021). The 'Sandwich Generation' Revisited: Global Demographic Drivers of Care Time Demands. <br>
      Population and Development Review. https://doi.org/10.1111/padr.12436.Datos disponibles en: https://github.com/alburezg/sandwich_clean"),
      align = "left",
      style = "
        position: absolute;
        bottom: 0;
        width: 100%;
        height: 60px;
        background-color: #ECF0F5;
        padding: 10px;
        color: #545454;
        font-size: 12.5px;"
    )
  )


# Servidor -----

server <- function(input, output) { 

  
## Pagina 1 ----
  
output$graf1 <- renderPlot({
    
    aux1<-data.frame(data[data$country==input$pais,c("cohort", 
                             "X3_sand.total.duration..years.")])
    
    aux1$label <- as.character(aux1$X3_sand.total.duration..years.)
    
    c <- max(aux1$X3_sand.total.duration..years.) + 1

    ggplot(aux1) +
      geom_col(aes(x=cohort, y=X3_sand.total.duration..years.), 
               fill="deepskyblue4") +
      geom_text(aes(x=cohort, y=X3_sand.total.duration..years. - 1, label=label),
                 colour="white", fontface="bold", size=5) +
      ylim(0,c) +
      xlab("Cohorte") + ylab("Años") + ggtitle(' ')  +
      tema
 }) 


output$graf2 <- renderPlot({
  
  aux2<-data.frame(data[data$country==input$pais,c("cohort", 
                                                   "X4_sand.share.of.lifespan....")])
  
  aux2$label <- as.character(aux2$X4_sand.share.of.lifespan....)
  
  c <- max(aux2$X4_sand.share.of.lifespan....) + 1
  
  ggplot(aux2) +
    geom_col(aes(x=cohort, y=X4_sand.share.of.lifespan....), 
             fill="deepskyblue4") +
    geom_text(aes(x=cohort, y=X4_sand.share.of.lifespan.... - 1, label=label),
              colour="white", fontface="bold", size=5) +
    ylim(0,c) +
    xlab("Cohorte") + ylab("Porcentaje") + ggtitle(' ')  +
    tema
}) 
  
  output$tabla1 <- renderTable({
    
    aux10<-data[data$country==input$pais, c("cohort", "X1_sand.size")]
    names(aux10) <- c("Cohorte", "Porcentaje")
    aux10})  


## Pagina 2 ----

output$graf3 <- renderPlot({
  
  aux3<-data.frame(data[data$country==input$pais2, c("cohort", 
                                                   "X7_gsand.total.duration..years.")])
  
  aux3$label <- as.character(aux3$X7_gsand.total.duration..years.)
  
  c <- max(aux3$X7_gsand.total.duration..years.) + 1
  
  ggplot(aux3) +
    geom_col(aes(x=cohort, y=X7_gsand.total.duration..years.), 
             fill="lightseagreen") +
    geom_text(aes(x=cohort, y=X7_gsand.total.duration..years. - 1, label=label),
              colour="white", fontface="bold", size=5) +
    ylim(0,c) +
    xlab("Cohorte") + ylab("Años") + ggtitle(' ')  +
    tema
}) 


output$graf4 <- renderPlot({
  
  aux4<-data.frame(data[data$country==input$pais2, c("cohort", 
                                                     "X8_gsand.share.of.lifespan....")])
  
  aux4$label <- as.character(aux4$X8_gsand.share.of.lifespan....)
  
  c <- max(aux4$X8_gsand.share.of.lifespan....) + 1
  
  ggplot(aux4) +
    geom_col(aes(x=cohort, y=X8_gsand.share.of.lifespan....), 
             fill="lightseagreen") +
    geom_text(aes(x=cohort, y=X8_gsand.share.of.lifespan.... - 1, label=label),
              colour="white", fontface="bold", size=5) +
    ylim(0,c) +
    xlab("Cohorte") + ylab("Porcentaje") + ggtitle(' ')  +
    tema
}) 

output$tabla2 <- renderTable({
  
  aux20<-data[data$country==input$pais2, c("cohort", "X5_gsand.size")]
  names(aux20) <- c("Cohorte", "Porcentaje")
  aux20})  

}

# UI ----
ui <- dashboardPage(title = 'Generación sandwich', encabezado, lateral, cuerpo, skin='purple')

shinyApp(ui, server)


