library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(lubridate)
library(summarytools)
library(DT)
library(plotly)
library(ggplot2)
library(rhandsontable)
library(scales)
library(dplyr)
library(reshape2)
library(shinyWidgets)
library(googlesheets4)

title<-tags$img(src="https://i.ibb.co/5Lt2w6M/ppmit.jpg",
                width='150px')


header<-dashboardHeader(title=title,
                        tags$li(a(img(src = "https://i.ibb.co/tc2wcv0/Whats-App-Image-2022-02-26-at-11-14-05-AM.jpg",
                                      title = "Nadya Yuniar", height = "50px"),
                                  style = "padding-top:1px; padding-bottom:1px;"),
                                class = "dropdown"),
                        tags$li(a(img(src = "https://i.ibb.co/QHPwtVf/Whats-App-Image-2022-02-26-at-11-13-05-AM.jpg",
                                      title = "Rizki Adi Wijaya", height = "50px"),
                                  style = "padding-top:1px; padding-bottom:1px;"),
                                class = "dropdown") )

sidebar<-dashboardSidebar()
body<-dashboardBody()
ui<-dashboardPage(header,sidebar,body)

options(gargle_oauth_cache = ".secrets", email = "Yuniarnadya4@gmail.com")



gs4_auth(cache = ".secrets", email = "Yuniarnadya4@gmail.com")

as_sheets_id("https://docs.google.com/spreadsheets/d/1s9IKtMdmCzTRx8FPP4YsvkRihgjiSum43rSOKjMIfKM/edit#gid=0")
sheet_id <- "1s9IKtMdmCzTRx8FPP4YsvkRihgjiSum43rSOKjMIfKM"

# importing data
penits <- data.frame(read_sheet(ss = sheet_id,sheet = "Data Penelitian Dosen ITS"))
pengits <-data.frame(read_sheet(ss = sheet_id,sheet = "Data Pengmas Dosen ITS")) 
penstat <- data.frame(read_sheet(ss = sheet_id,sheet = "Data Penelitian Dosen Departemen Statistika ITS")) 
pengstat <- data.frame(read_sheet(ss = sheet_id,sheet = "Data Pengmas Dosen Departemen Statistika ITS")) 
database <- data.frame(read_sheet(ss = sheet_id,sheet = "Database Dosen ITS")) 


resetFormpenits <- function(session){
    updateSelectInput(session,"Sk1",selected=character(0))
    updateTextInput(session,"Na1",value="")
    updateTextInput(session,"Ju1",value="")
    updateTextInput(session,"DP1",value="")
    updateSelectInput(session,"SU1",selected=character(0))
    updateSelectInput(session,"De1",selected=character(0))
    updateTextInput(session,"Ta1",value="")
    updateSelectInput(session,"Sp1",selected=character(0))
    updateTextInput(session,"DD1",value="")
}

resetFormpenstat <- function(session){
    updateSelectInput(session,"Sk2",selected=character(0))
    updateTextInput(session,"Na2",value="")
    updateTextInput(session,"Ju2",value="")
    updateTextInput(session,"DP2",value="")
    updateSelectInput(session,"SU2",selected=character(0))
    updateSelectInput(session,"De2",selected=character(0))
    updateTextInput(session,"Ta2",value="")
    updateSelectInput(session,"Sp2",selected=character(0))
    updateTextInput(session,"DD2",value="")
}

resetFormpengits <- function(session){
    updateSelectInput(session,"Sk3",selected=character(0))
    updateTextInput(session,"Na3",value="")
    updateTextInput(session,"Ju3",value="")
    updateSelectInput(session,"SU3",selected=character(0))
    updateSelectInput(session,"De3",selected=character(0))
    updateTextInput(session,"Ta3",value="")
    updateSelectInput(session,"Sp3",selected=character(0))
    updateTextInput(session,"DD3",value="")
}

resetFormpengstat <- function(session){
    updateSelectInput(session,"Sk4",selected=character(0))
    updateTextInput(session,"Na4",value="")
    updateTextInput(session,"Ju4",value="")
    updateSelectInput(session,"SU4",selected=character(0))
    updateSelectInput(session,"De4",selected=character(0))
    updateTextInput(session,"Ta4",value="")
    updateSelectInput(session,"Sp4",selected=character(0))
    updateTextInput(session,"DD4",value="")
}


count_database <- database %>% count(JENIS, NAMA,DEPARTEMEN,STATUS.USULAN, SKEMA, TAHUN, SUMBER.PENDANAAN)
count_penits <- penits %>% count(NAMA,DEPARTEMEN,STATUS.USULAN, SKEMA, TAHUN, SUMBER.PENDANAAN)
count_pengits <- pengits %>% count(NAMA,DEPARTEMEN,STATUS.USULAN, SKEMA, TAHUN, SUMBER.PENDANAAN)
count_penstat <- penstat %>% count(NAMA,DEPARTEMEN,STATUS.USULAN, SKEMA, TAHUN, SUMBER.PENDANAAN)
count_pengstat <- pengstat %>% count(NAMA,DEPARTEMEN,STATUS.USULAN, SKEMA, TAHUN, SUMBER.PENDANAAN)


# Add Sidebar
sidebarItem<-dashboardSidebar(
    sidebarMenu(
        menuItem("Home Page",tabName="HP"),
        menuItem("Pencarian",tabName="Pencarian",
                 menuSubItem("Penelitian",tabName="PI"),
                 menuSubItem("Pengabdian Masyarakat",tabName="PMI")),
        menuItem("Performa",tabName="Pe"),
        menuItem("Infografis",tabName="Infografis",
                 menuSubItem("Penelitian",tabName="IPI"),
                 menuSubItem("Pengabdian Masyarakat",tabName="IPMI")),
        menuItem("Input Data",tabName="In",
                 menuSubItem("Penelitian",tabName="IDP"),
                 menuSubItem("Pengabdian Masyarakat",tabName="IDPI"))
    )
)


# add bodyitem
bodyItem<-dashboardBody(
    tabItems(
        tabItem(tabName = "HP",
                h2(strong("Home Page"),align = "center"),
                h2(),
                p("Lembaga Penelitian dan Pengabdian kepada Masyarakat (LPPM) ITS adalah unsur pelaksana akademik yang melaksanakan sebagian tugas pokok dan fungsi ITS di bidang Penelitian dan Pengabdian Kepada Masyarakat.", style = "font-family: 'times'; font-si10pt", align="justify"),
                br(),
                p("LPPM sebagaimana tertuang dalam Organisasi dan Tata Kerja ITS dalam Peraturan Rektor Nomor 10 Tahun 2016 mempunyai tugas mengelola dan mengembangkan jejaring penelitian dan pengabdian kepada masyarakat", style = "font-family: 'times'; font-si10pt",align="justify"),
                br(),
                p("Dalam melaksanakan tugas , LPPM menyelenggarakan fungsi:", style = "font-family: 'times'; font-si10pt",align="justify"),
                p("1. Penyusunan dan sosialisasi peta jalan penelitian dan pengabdian kepada masyarakat sesuai kebijakan Rektor", style = "font-family: 'times'; font-si10pt"),
                p("2. Penyusunan perencanaan, pengorganisasian, dan pemantauan program penelitian, termasuk penjaminan mutu, baik program monodisiplin maupun program lintas disiplin yang dilaksanakan oleh pusat penelitian dan laboratorium", style = "font-family: 'times'; font-si10pt"),
                p("3. Penyusunan perencanaan, pengorganisasian, dan pemantauan program pengabdian kepada masyarakat", style = "font-family: 'times'; font-si10pt"),
                p("4. Penyediaan informasi kegiatan penelitian dan pengabdian kepada masyarakat", style = "font-family: 'times'; font-si10pt"),
                br(),
                p("LPPM dipimpin oleh seorang Kepala LPPM, yang dalam menjalankan tugasnya bertanggung jawab kepada Rektor", style = "font-family: 'times'; font-si10pt"),
                br(),
                p("LPPM terdiri atas:", style = "font-family: 'times'; font-si10pt"),
                p("1. Sekretariat LPPM", style = "font-family: 'times'; font-si10pt"),
                p("2. Bagian Tata Usaha", style = "font-family: 'times'; font-si10pt"),
                p("3. Pusat Studi", style = "font-family: 'times'; font-si10pt"),
                p("4. Pusat Lain-lain", style = "font-family: 'times'; font-si10pt"),
                p("5. Laboratorium", style = "font-family: 'times'; font-si10pt"),
                br(),
                p("LPPM ITS memiliki 7 Pusat Studi, 3 Pusat lainnya, dan 2 Laboratorium yakni :", style = "font-family: 'times'; font-si10pt"),
                br(),
                p("Pusat Studi", style = "font-family: 'times'; font-si10pt"),
                p("1. Pusat Studi Energi", style = "font-family: 'times'; font-si10pt"),
                p("2. Pusat Studi Kelautan", style = "font-family: 'times'; font-si10pt"),
                p("3. Pusat Studi TIK dan Robotika", style = "font-family: 'times'; font-si10pt"),
                p("4. Pusat Studi Sains, Material, dan Nanoteknologi", style = "font-family: 'times'; font-si10pt"),
                p("5. usat Studi Potensi Daerah dan Pemberdayaan Masyarakat", style = "font-family: 'times'; font-si10pt"),
                p("6. Pusat Studi Pemukiman, Lingkungan, dan Infrastruktur", style = "font-family: 'times'; font-si10pt"),
                p("7. Pusat Studi Kebumian, Bencana, dan Perubahan Iklim", style = "font-family: 'times'; font-si10pt"),
                br(),
                p("Pusat Lainnya", style = "font-family: 'times'; font-si10pt"),
                p("1. Pusat Pengelolaan Hak Kekayaan Intelektual", style = "font-family: 'times'; font-si10pt"),
                p("2. Pusat Publikasi Ilmiah", style = "font-family: 'times'; font-si10pt"),
                p("3. Pusat Kajian Halal", style = "font-family: 'times'; font-si10pt"),
                br(),
                p("Laboratorium", style = "font-family: 'times'; font-si10pt"),
                p("1. Laboratorium Energi dan Lingkungan", style = "font-family: 'times'; font-si10pt"),
                p("2. Laboratorium Robotika", style = "font-family: 'times'; font-si10pt")
        ),
        tabItem(tabName="PI",
                h2(strong("Pencarian Penelitian Dosen ITS"),align = "center"),
                h2(),
                tabsetPanel(
                    tabPanel("ITS",
                             basicPage(
                                 dataTableOutput("tab1")
                             )),
                    tabPanel("Departemen Statistika ITS",
                             basicPage(
                                 dataTableOutput("tab2")
                             )))
        ),
        tabItem(tabName="PMI",
                h2(strong("Pencarian Pengabdian Masyarakat Dosen ITS"),align = "center"),
                h2(),
                tabsetPanel(
                    tabPanel("ITS",
                             basicPage(
                                 dataTableOutput("tab3")
                             )),
                    tabPanel("Departemen Statistika ITS",
                             basicPage(
                                 dataTableOutput("tab4")
                             )))
        ),
        tabItem(tabName = "Pe",
                h2(strong("Performa Dosen ITS"),align = "center"),
                h2(),
                fluidRow(
                    box(background = "black",
                        selectInput("namdos","Nama Dosen",
                                    choices = unique(database$NAMA),
                                    selected = unique(database$NAMA)),width = 12),
                    box(title = strong("Jumlah Penelitian dan Pengabdian Masyarakat Per Tahun"),plotOutput("graf1performa"),width = 12),
                    box(title = strong("SKEMA Penelitian yang Terdanai Per Tahun"),plotOutput("graf2performa"),width = 12),
                    box(title = strong("SKEMA Pengabdian Masyarakat yang Terdanai Per Tahun"),plotOutput("graf3performa"),width = 12)
                )
        ),
        tabItem(tabName="IPI", 
                h2(strong("Infografis Penelitian Dosen ITS"),align = "center"),
                h2(),
                tabsetPanel(
                    tabPanel("ITS",
                             fluidRow(
                                 box(background = "black",
                                     checkboxGroupInput("thnpenits","Tahun",choices = unique(penits$TAHUN),selected = unique(penits$TAHUN)),
                                     checkboxGroupInput("dptpenits","Departemen",choices = unique(penits$DEPARTEMEN),selected = unique(penits$DEPARTEMEN)),width = 2),
                                 box(title = strong("Jumlah Penelitian Terdanai"),plotOutput("graf1penits"),width = 10),
                                 box(title = strong("Status Usulan"),plotOutput("graf2penits"),width = 10),
                                 box(title = strong("SKEMA Penelitian"),plotOutput("graf3penits"),width = 10))
                    ),
                    tabPanel("Departemen Statistika ITS",
                             fluidRow(
                                 box(background = "black",
                                     checkboxGroupInput("thnpenstat","Tahun",choices = unique(penstat$TAHUN),selected = unique(penstat$TAHUN)),width = 12),
                                 box(title = strong("Jumlah Penelitian Terdanai"),plotOutput("graf1penstat"),width = 6),
                                 box(title = strong("Status Usulan"),plotOutput("graf2penstat"),width = 6),
                                 box(title = strong("SKEMA Penelitian"),plotOutput("graf3penstat"),width = 12))
                    ))
        ),
        tabItem(tabName="IPMI",
                h2(strong("Infografis Pengabdian Masyarakat Dosen ITS"),align = "center"),
                h2(),
                tabsetPanel(
                    tabPanel("ITS",
                             fluidRow(
                                 box(background = "black",
                                     checkboxGroupInput("thnpengits","Tahun",choices = unique(pengits$TAHUN),selected = unique(pengits$TAHUN)),
                                     checkboxGroupInput("dptpengits","Departemen",choices = unique(pengits$DEPARTEMEN),selected = unique(pengits$DEPARTEMEN)),width = 2),
                                 box(title = strong("Jumlah Pengabdian Masyarakat Terdanai"),plotOutput("graf1pengits"),width = 10),
                                 box(title = strong("SKEMA Pengabdian Masyarakat"),plotOutput("graf2pengits"),width = 10))
                    ),
                    tabPanel("Departemen Statistika ITS",
                             fluidRow(
                                 box(background = "black",
                                     checkboxGroupInput("thnpengstat","Tahun",choices = unique(pengstat$TAHUN),selected = unique(pengstat$TAHUN)),width = 12),
                                 box(title = strong("Jumlah Pengabdian Masyarakat Terdanai"),plotOutput("graf1pengstat"),width = 12),
                                 box(title = strong("SKEMA Pengabdian Masyarakat"),plotOutput("graf2pengstat"),width = 12))
                    ))
        ),
        tabItem(tabName="IDP", 
                h2(strong("Input Data Penelitian Dosen ITS"),align = "center"),
                h2(),
                tabsetPanel(
                    tabPanel("ITS",
                             fluidRow(column(12,
                                             selectInput("Sk1","SKEMA:",choices=unique(penits$SKEMA),selected=character(0))),
                                      column(12,
                                             textInput("Na1","Nama :",value="")),
                                      column(12,
                                             textInput("Ju1","Judul:",value="")),
                                      column(12,
                                             textInput("DP1","Durasi Penelitian (Thn) :",value="")),
                                      column(12,
                                             selectInput("SU1", "Status Usulan :",choices=unique(penits$STATUS.USULAN),selected=character(0))),
                                      column(12,
                                             selectInput("De1","Departemen :",choices=unique(penits$DEPARTEMEN),selected=character(0))),
                                      column(12,
                                             textInput("Ta1","Tahun :",value="")),
                                      column(12,
                                             selectInput("SP1","Sumber Pendanaan :",choices=unique(penits$SUMBER.PENDANAAN),selected=character(0))),
                                      column(12,
                                             textInput("DD1","Dana Disetujui (Rp) :",value="")),
                                      actionButton("submit1", "Submit"),
                                      actionButton("clear1","Clear Form")
                             )),
                    tabPanel("Departemen Statistika ITS",
                             fluidRow(column(12,
                                             selectInput("Sk2","SKEMA:",choices=unique(penits$SKEMA),selected=character(0))),
                                      column(12,
                                             textInput("Na2","Nama :",value="")),
                                      column(12,
                                             textInput("Ju2","Judul:",value="")),
                                      column(12,
                                             textInput("DP2","Durasi Penelitian (Thn) :",value="")),
                                      column(12,
                                             selectInput("SU2", "Status Usulan :",choices=unique(penits$STATUS.USULAN),selected=character(0))),
                                      column(12,
                                             selectInput("De2","Departemen :",choices=unique(penstat$DEPARTEMEN),selected=character(0))),
                                      column(12,
                                             textInput("Ta2","Tahun :",value="")),
                                      column(12,
                                             selectInput("SP2","Sumber Pendanaan :",choices=unique(penits$SUMBER.PENDANAAN),selected=character(0))),
                                      column(12,
                                             textInput("DD2","Dana Disetujui (Rp) :",value="")),
                                      actionButton("submit2", "Submit"),
                                      actionButton("clear2","Clear Form")
                             )))),
        tabItem(tabName="IDPI",
                h2(strong("Input Data Pengabdian Masyarakat Dosen ITS"),align = "center"),
                h2(),
                tabsetPanel(
                    tabPanel("ITS",
                             fluidRow(column(3,
                                             selectInput("Sk3","SKEMA:",choices=unique(pengits$SKEMA),selected=character(0))),
                                      column(3,
                                             textInput("Na3","Nama :",value="")),
                                      column(3,
                                             textInput("Ju3","Judul:",value="")),
                                      column(3,
                                             selectInput("SU3", "Status Usulan :",choices=unique(pengits$STATUS.USULAN),selected=character(0))),
                                      column(3,
                                             selectInput("De3","Departemen :",choices=unique(pengits$DEPARTEMEN),selected=character(0))),
                                      column(3,
                                             textInput("Ta3","Tahun :",value="")),
                                      column(3,
                                             selectInput("SP3","Sumber Pendanaan :",choices=unique(pengits$SUMBER.PENDANAAN),selected=character(0))),
                                      column(3,
                                             textInput("DD3","Dana Disetujui (Rp) :",value="")),
                                      actionButton("submit3", "Submit"),
                                      actionButton("clear3","Clear Form")
                             )),
                    tabPanel("Departemen Statistika ITS",
                             fluidRow(column(3,
                                             selectInput("Sk4","SKEMA:",choices=unique(pengits$SKEMA),selected=character(0))),
                                      column(3,
                                             textInput("Na4","Nama :",value="")),
                                      column(3,
                                             textInput("Ju4","Judul:",value="")),
                                      column(3,
                                             selectInput("SU4", "Status Usulan :",choices=unique(pengits$STATUS.USULAN),selected=character(0))),
                                      column(3,
                                             selectInput("De4","Departemen :",choices=unique(pengstat$DEPARTEMEN),selected=character(0))),
                                      column(3,
                                             textInput("Ta4","Tahun :",value="")),
                                      column(3,
                                             selectInput("SP4","Sumber Pendanaan :",choices=unique(pengits$SUMBER.PENDANAAN),selected=character(0))),
                                      column(3,
                                             textInput("DD4","Dana Disetujui (Rp) :",value="")),
                                      actionButton("submit4", "Submit"),
                                      actionButton("clear4","Clear Form")
                             ))))
    )
)

ui<-dashboardPage(skin="black",header = header,
                  sidebar = sidebarItem,
                  body = bodyItem)


server<-function(input,output,session){
    
    output$tab1 = renderDataTable({
        penits
    })
    
    output$tab2 = renderDataTable({
        penstat
    })
    
    output$tab3 = renderDataTable({
        pengits
    })
    
    output$tab4 = renderDataTable({
        pengstat
    })
    
    #Infografis Performa
    output$graf1performa <- renderPlot({
        ggplot(subset(count_database,NAMA==input$namdos), aes(x = TAHUN, y = n, fill=JENIS)) + geom_col() + scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf2performa <- renderPlot({
        ggplot(subset(count_penits,NAMA==input$namdos), aes(x = TAHUN, y = n, fill = SKEMA)) + geom_col() + scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf3performa<- renderPlot({
        ggplot(subset(count_pengits,NAMA==input$namdos), aes(x = TAHUN, y = n, fill = SKEMA)) + geom_col() + scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    #Infografis Penits
    output$graf1penits <- renderPlot({
        ggplot(subset(subset(count_penits,TAHUN== input$thnpenits),DEPARTEMEN == input$dptpenits), aes(x = TAHUN, y = n, fill=DEPARTEMEN)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf2penits<- renderPlot({
        ggplot(subset(subset(count_penits,TAHUN== input$thnpenits),DEPARTEMEN == input$dptpenits), aes(x = TAHUN, y = n, fill=STATUS.USULAN)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf3penits <- renderPlot({
        ggplot(subset(subset(count_penits,TAHUN== input$thnpenits),DEPARTEMEN == input$dptpenits), aes(x = TAHUN, y = n, fill=SKEMA)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    #Infografis Penstat
    output$graf1penstat <- renderPlot({
        ggplot(subset(count_penstat,TAHUN==input$thnpenstat), aes(x = TAHUN, y = n, fill=DEPARTEMEN)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf2penstat<- renderPlot({
        ggplot(subset(count_penstat,TAHUN==input$thnpenstat), aes(x = TAHUN, y = n, fill=STATUS.USULAN)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf3penstat <- renderPlot({
        ggplot(subset(count_penstat,TAHUN==input$thnpenstat), aes(x = TAHUN, y = n, fill=SKEMA)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    #Infografis Pengits
    output$graf1pengits <- renderPlot({
        ggplot(subset(subset(count_pengits,TAHUN== input$thnpengits),DEPARTEMEN == input$dptpengits), aes(x = TAHUN, y = n, fill=DEPARTEMEN)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf2pengits<- renderPlot({
        ggplot(subset(subset(count_pengits,TAHUN== input$thnpengits),DEPARTEMEN == input$dptpengits), aes(x = TAHUN, y = n, fill=SKEMA)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    #Infografis Pengstat
    output$graf1pengstat <- renderPlot({
        ggplot(subset(count_pengstat,TAHUN==input$thnpengstat), aes(x = TAHUN, y = n, fill=DEPARTEMEN)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    output$graf2pengstat<- renderPlot({
        ggplot(subset(count_pengstat,TAHUN==input$thnpengstat), aes(x = TAHUN, y = n, fill=SKEMA)) + geom_col()+ scale_x_continuous(breaks = seq(2017, 2020, by = 1)) 
    })
    
    observeEvent(input$submit1,{
        to_be_done_at_submit1()
        # saveDatapenits(input)
        resetFormpenits(session)
        response<-paste0("Submited Success","!")
        showNotification(response,duration=0,type="message")
        shinyjs::reset("form")
    })
    
    
    observeEvent(input$clear1,{
        resetFormpenits(session)
    })
    
    observeEvent(input$submit2,{
        to_be_done_at_submit2()
        # saveDatapenits(input)
        resetFormpenits(session)
        response<-paste0("Submited Success","!")
        showNotification(response,duration=0,type="message")
        shinyjs::reset("form")
    })
    
    observeEvent(input$clear2,{
        resetFormpenstat(session)
    })
    
    observeEvent(input$submit3,{
        to_be_done_at_submit3()
        # saveDatapenits(input)
        resetFormpenits(session)
        response<-paste0("Submited Success","!")
        showNotification(response,duration=0,type="message")
        shinyjs::reset("form")
    })
    
    observeEvent(input$clear3,{
        resetFormpengits(session)
    })
    
    observeEvent(input$submit4,{
        to_be_done_at_submit4()
        # saveDatapenits(input)
        resetFormpenits(session)
        response<-paste0("Submited Success","!")
        showNotification(response,duration=0,type="message")
        shinyjs::reset("form")
    })
    
    observeEvent(input$clear4,{
        resetFormpengstat(session)
    })
    
    to_be_done_at_submit1 <- eventReactive(input$submit1, {          #Collect data
        dt1 <- data.frame(as.character(input$Sk1),as.character(input$Na1),as.character(input$Ju1), as.integer(input$DP1),
                          as.character(input$SU1),as.character(input$De1),as.character(input$Ta1), as.character(input$SP1),
                          as.integer(input$DD1))
        sheet_append(ss = sheet_id, data = dt1, sheet = "Data Penelitian Dosen ITS")
        sheet_append(ss = sheet_id, data = dt1, sheet = "Database Dosen ITS")
        
        #Say thankyou
        h5("Thanks for entering data. (Terimakasih sudah memasukkan data. Anda bisa melihat dan mengunduh semua data ",
           
        )
    })
    
    to_be_done_at_submit2 <- eventReactive(input$submit2, {          #Collect data
        dt1 <- data.frame(as.character(input$Sk2), as.character(input$Na2), as.character(input$Ju2), as.integer(input$DP2),
                          as.character(input$SU2),as.character(input$De2), as.character(input$Ta2), as.character(input$SP2),
                          as.integer(input$DD2))
        
        sheet_append(ss = sheet_id, data = dt1, sheet = "Data Penelitian Dosen Departemen Statistika ITS")
        sheet_append(ss = sheet_id, data = dt1, sheet = "Data Penelitian Dosen ITS")
        sheet_append(ss = sheet_id, data = dt1, sheet = "Database Dosen ITS")
        
        
        #Say thankyou
        h5("Thanks for entering data. (Terimakasih sudah memasukkan data. Anda bisa melihat dan mengunduh semua data ",
           
        )
    })
    
    to_be_done_at_submit3 <- eventReactive(input$submit3, {          #Collect data
        dt1 <- data.frame(as.character(input$Sk3), as.character(input$Na3), as.character(input$Ju3), as.integer(input$DP3),
                          as.character(input$SU3),as.character(input$De3), as.character(input$Ta3), as.character(input$SP3),
                          as.integer(input$DD3))
        
        sheet_append(ss = sheet_id, data = dt1, sheet = "Data Pengmas Dosen ITS")
        sheet_append(ss = sheet_id, data = dt1, sheet = "Database Dosen ITS")
        
        
        #Say thankyou
        h5("Thanks for entering data. (Terimakasih sudah memasukkan data. Anda bisa melihat dan mengunduh semua data ",
           
        )
    })
    
    to_be_done_at_submit4 <- eventReactive(input$submit4, {          #Collect data
        dt1 <- data.frame(as.character(input$Sk4), as.character(input$Na4), as.character(input$Ju4), as.integer(input$DP4),
                          as.character(input$SU4),as.character(input$De4), as.character(input$Ta4), as.character(input$SP4),
                          as.integer(input$DD4))
        
        sheet_append(ss = sheet_id, data = dt1, sheet = "Data Pengmas Dosen Departemen Statistika ITS")
        sheet_append(ss = sheet_id, data = dt1, sheet = "Data Pengmas Dosen ITS")
        sheet_append(ss = sheet_id, data = dt1, sheet = "Database Dosen ITS")
        
        
        #Say thankyou
        h5("Thanks for entering data. (Terimakasih sudah memasukkan data. Anda bisa melihat dan mengunduh semua data ",
           
        )
    })
}
shinyApp(ui,server)
