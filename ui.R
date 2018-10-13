library(dplyr)
library(varhandle)
library(shinydashboard)
library(shiny)
library(plotly)
patents <-  read.csv("msms-jun18-exp-data.csv")
prov <-  patents %>% filter(patents$Org_Level=="Provider")
#adding total 
prov$Value <-  unfactor(prov$Value) %>% as.numeric(prov$Value)

prov_booking <-  prov %>% filter(Org_Level== "Provider") %>% 
  filter(Dimension == "GestAgeFormalAntenatalBookingGroup") %>% filter(!is.na(Value)) %>% droplevels()

total <-  group_by(prov_booking, Org_Code) %>% summarise(Total=sum(Value))
prov_f <-  dplyr::inner_join(prov,total,c("Org_Code"="Org_Code"))


#country.choices <- setNames(nm=unique(patents$Org_Name))

ui <- dashboardPage(
  title = "NICE QS Maternity Indicator Explorer",
  dashboardHeader(title = "Marternity Indicator",
                  titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "main_menu",
      menuItem("Antenatal Care", tabName = "cs", icon = icon("bar-chart")),
      menuItem("Help", tabName = "hlp", icon = icon("question")),
      
      conditionalPanel("input.main_menu == 'cs'",
                       
                       selectizeInput("cs.trusts",
                                      label = "Select Peer group (trusts)",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = TRUE)#,

                       
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "cs",
              tabsetPanel(
                id = "cs_panel",
                tabPanel("Appointment in 10 Weeks", plotlyOutput("cs.quin10", height = "600px", width = "100%")),
                tabPanel("Appointment in 12 Weeks + 6 days", plotlyOutput("cs.quin12", height = "600px", width = "100%"))#,
                #tabPanel("Appointment in Weeks", plotlyOutput("cs.stack", height = "600px", width = "100%"))
              )
      ),
      
      
      
      tabItem(tabName = "hlp",
              column(fluidRow(box(htmlOutput("help"), width = 12)), width = 8, offset = 2)
              
      )
    )
  ),
  skin = "black"
)

