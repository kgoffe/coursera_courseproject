

server <- function(input, output,session) {
  patents <-  read.csv("msms-jun18-exp-data.csv")
  prov <-  patents %>% filter(patents$Org_Level=="Provider")
  prov$Value <-  unfactor(prov$Value) %>% as.numeric(prov$Value)
  
  prov_booking <-  prov %>% filter(Org_Level== "Provider") %>% 
    filter(Dimension == "GestAgeFormalAntenatalBookingGroup") %>% filter(!is.na(Value)) %>% droplevels()
  
  total <-  group_by(prov_booking, Org_Code) %>% summarise(Total=sum(Value))
  prov_f <-  dplyr::inner_join(prov,total,c("Org_Code"="Org_Code"))
  
  
  updateSelectizeInput(session,'cs.trusts',choices=prov_f$Org_Name)
  

  output$cs.quin10 <- renderPlotly({
    upto10 <-  prov_f %>% filter(Measure=="0 to 70 days")
    upto10p <- mutate(upto10, upto10wks = Value/Total)
    upto10p_q <- mutate(upto10p, quintile_rank=ntile(upto10p$upto10wks,5))
    q10_sel <- upto10p_q %>% filter(Org_Name %in% input$cs.trusts)
    g <- ggplot() + geom_col(data=upto10p_q, aes(x=reorder(Org_Name, upto10wks),y=upto10wks,fill=quintile_rank,color=quintile_rank))+
                          geom_col(data=q10_sel,aes(x=Org_Name,y=upto10wks),fill="yellow",color="yellow")+
      theme_classic() + theme (axis.text.x = element_blank(), axis.ticks.x=element_blank(), legend.position = "none") + scale_y_continuous(labels= scales::percent, limits=c(0,1))+
      xlab("Provider") + ylab("% women booking by 10 weeks") + ggtitle("% of Women with Booking Appointment in 10 weeks (70 days)")
    
    ggplotly(g)
    
  })
  
  output$cs.quin12 <- renderPlotly({
    upto12 <-  prov_f %>% filter(Measure=="71 to 90 days")
    upto12p <- mutate(upto12, upto12wks = Value/Total)
    upto12p_q <- mutate(upto12p, quintile_rank=ntile(upto12p$upto12wks,5))
    q12_sel <- upto12p_q %>% filter(Org_Name %in% input$cs.trusts)
    g <- ggplot() + geom_col(data=upto12p_q, aes(x=reorder(Org_Name, upto12wks),y=upto12wks,fill=quintile_rank,color=quintile_rank))+
      geom_col(data=q12_sel,aes(x=Org_Name,y=upto12wks),fill="yellow",color="yellow")+
      theme_classic() + theme (axis.text.x = element_blank(), axis.ticks.x=element_blank(), legend.position = "none") + scale_y_continuous(labels= scales::percent, limits=c(0,1))+
      xlab("Provider") + ylab("% women booking by 12 weeks + 6 days") + ggtitle("% of Women with Booking Appointment in 12 weeks and 6 days (90 days)")
    
    ggplotly(g)
    
  })
  
  
  
  
  
  output$help <- renderText({
    "
    <div>
    
    <h2>
    Description
    </h2>
    
    <p>
    NICE Quality Standards (QS) provide evidence-based standards for the provision of various aspects of antenatal, intrapartum and postnatal care.
    Achievment of many of the QS requires care to be co-ordinated across services involved in the maternity. Profiling against the QS does not requirements for population and casemix,
    as the good practice identified should be standard across all providers. 
    </p>

    <p>
    The Maternity Services Dataset (MSDS) has been implemented from April 2015. 
    It is intended to provide comprehensive data for national and local monitoring of antenatal, intrapartum and postnatal care, outcomes of maternity care, health inequalities, and to support payment systems, but progress with implementation and data quality / completeness varies between Trusts. 
    </p>
    <p>
    This dashboard is showing quintile chart of antenatal booking appointment in NHS trusts in England.

    </p>
    <p>
    More information on the variables can be found directly on the <a href='https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/maternity-services-data-set'> NHS Digital website </a>
    </p>
    
    <h2>
    How to use the dashboard
    </h2>
    <p>
    <ul>
    <li> You need <strong> shiny, dplyr,shinydashboard </strong> libraries to run this dashboard
    <li> When the dashboard loaded, two quintile charts will be shown. 
    <li> Type trust that you want to highlight in yellow to <strong>'Select Peer group (trusts)'</strong> 
    <li> <strong>'Appointment 10 weeks'</strong> and <strong>'Appointment in 12 weeks + 6 days'</strong> charts will be updated and show in yellow with the trust that you select 

   
    
    </ul>
    </p>
    </div>
    "

  })
}



