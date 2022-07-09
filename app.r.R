library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(lubridate)
library(plotly)
library(padr)

sales <- read_csv("sales_join.csv")

temp <- sales %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    quarter = date(floor_date(x = OrderDate, unit = "quarter"))
  ) %>% 
  group_by(quarter) %>% 
  summarise(
    Sales = sum(Sales),
    COS = sum(COS),
    Discount = sum(Discount),
    Freight = sum(Freight)
  ) %>%
  ungroup() %>% 
  tail(1)

topcat <- sales %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    quarter = date(floor_date(x = OrderDate, unit = "quarter"))
  ) %>% 
  group_by(quarter, CategoryName) %>% 
  summarise(
    Sales = sum(Sales)
  ) %>% 
  ungroup() %>% 
  arrange(desc(Sales)) %>% 
  filter(quarter == "2015-10-01") %>% 
  select(CategoryName, Sales)


ui <- shinyUI(
  
  dashboardPage(
    skin = "purple",
    title = "Uas Komstat",
    dashboardHeader(
      title = "Uas Komstat"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Menu", 
          tabName = "page1",
          icon = icon ("gear"),
          menuSubItem("Sales"),
          menuSubItem("COS"),
          menuSubItem("Discount"),
          menuSubItem("Freight")
        ),
        menuItem(
          text = "Penyusun",
          tabName = "page1",
          icon = icon ("group"),
          menuSubItem("Ikwan Prastyo"),
          menuSubItem("Dwi cahya Safitri")
        ),
        menuItem(
          text = "Visitus", 
          tabName = "page1",
          badgeLabel = "new", 
          badgeColor = "blue",
          icon = icon ("send")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          
          fluidRow(
            box(
              title = "Menu Dashboard",
              satus = "primary",
              solidHeader = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "Sales",
                width = 3
              ),
              infoBoxOutput(
                outputId = "COS",
                width = 3
              ),
              infoBoxOutput(
                outputId = "Discount",
                width = 3
              ),
              infoBoxOutput(
                outputId = "Freight",
                width = 3
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Pilih Nama Kategori", 
              closable = TRUE, 
              enable_label = TRUE,
              status = "primary", 
              solidHeader = FALSE,
              width = 12,
              
              selectInput(
                inputId = "catname", 
                label = "Kategori", 
                choices = c(topcat$CategoryName),
                selected = "Sportwear"
              ),
              
              dataTableOutput(
                outputId = "topcat"
              )
            ),
            
            fluidRow(
            box(
              title = "TOTAL PENJUALAN ", 
              closable = TRUE, 
              status = "success",
              solidHeader = FALSE,
              width = 12,
              height = "600px",
              
              plotlyOutput(
                outputId = "sale"
              )
            ),
            
            fluidRow(
              box(
                title = "TOTAL DISKON ", 
                closable = TRUE, 
                status = "success", 
                solidHeader = FALSE,
                width = 12,
                height = "600px",
                
                plotlyOutput(
                  outputId = "diskon"
              )
              )
            ),
            
            fluidRow(
              box(
                title = "TOTAL PENGIRIMAN KARGO", 
                closable = TRUE, 
                status = "success", 
                solidHeader = FALSE,
                width = 12,
                height = "600px",
                
                plotlyOutput(
                  outputId = "kargo" 
              )
              )
            )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$Sales <- renderInfoBox({
    
    infoBox(
      title = "Sales",
      value = temp %>% pull(Sales), 
      subtitle = "Total of Sales", 
      color = "green",
      icon = icon("search-dollar"),
      fill = TRUE
    )
  })
  
  output$COS <- renderInfoBox({
    
    infoBox(
      title = "COS",
      value = temp %>% pull(COS), 
      subtitle = "Cost Of Sale", 
      color = "blue",
      icon = icon("gbp"),
      fill = TRUE
    )
  })
  
  output$Discount <- renderInfoBox({
    
    infoBox(
      title = "Discount",
      value = temp %>% pull(Discount), 
      subtitle = "Total of Discount", 
      color = "red",
      icon = icon("dollar-sign"),
      fill = TRUE
    )
  })
  
  output$Freight <- renderInfoBox({
    
    infoBox(
      title = "Freight",
      value = temp %>% pull(Freight), 
      subtitle = "Total of Freight", 
      color = "yellow",
      fill = TRUE
    )
  })
  
 output$topcat <- renderDataTable({
   datatable(topcat)
 })
 
 ({
   output$sale <- renderPlotly({
     sales %>% 
       ggplot(aes(x = Sales, 
                  y = CategoryName, 
                  fill = CategoryName )) + geom_col()+
       ylab("")+
       labs(title = "Total Of Sales")
 })
 })
 
 ({
   output$diskon <- renderPlotly({
     sales %>% 
       ggplot(aes(x = Discount,
                  y = CategoryName, 
                  fill = CategoryName )) + geom_col()+
       ylab("")+
       labs(title = "Total Of Discount") 
   })
 })
 
 ({
   output$kargo <- renderPlotly({
     sales %>% 
       ggplot(aes(x = Freight,
                  y = CategoryName, 
                  fill = CategoryName )) + geom_col()+
       ylab("")+
       labs(title = "Total Of Freight")  
   })
 })

}

shinyApp(ui, server)