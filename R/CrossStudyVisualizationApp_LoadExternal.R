
#This R File Makes an App that allows for visualization of cross-study analysis
#of SEND Data

#Need to do:
#* Add Detailed Scoring Controls (i.e. user can determine scoring ranges) > In-Progress GM
      #* Goal is to have this as another blue tab like the "data load" tab 
#* Ability to load different studies > SB (Done)
#* Add User control to facet by route, duration, treatment or species



################################################################################
## this code is for creating shiny app
##
## History:
## -----------------------------------------------------------------------------
## Date         Programmer            Note
## ----------   --------------------  ------------------------------------------
## 2023-05-22   Susan Butler,           Initial version
##              Yousuf Ali
################################################################################

#' @title Run sendCrossStudy app
#' @param database_path Optional, character\cr
#'    file path for database
#'
#' @return function run the app.
#'
#' @export
#' @examples
#' \dontrun{
#' send_cross_study_app()
#' }

#' @import ggplot2
#' @import data.table
#' @import shiny
#' @importFrom cowplot ggdraw plot_grid get_legend
#' @importFrom ggpattern geom_tile_pattern scale_pattern_density_manual scale_pattern_fill_brewer
#' @importFrom gridExtra grid.arrange
#' @importFrom Hmisc sasxport.get
#' @importFrom httr authenticate content GET
#' @importFrom reshape2 dcast
#' @importFrom sendigR genericQuery getStudiesSDESIGN initEnvironment
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage dashboardSidebar menuItem sidebarMenu
#' @importFrom shinyFiles shinyDirButton shinyDirChoose
#' @importFrom shinyTree get_selected renderTree shinyTree
#' @importFrom shinyWidgets actionBttn pickerInput
#' @importFrom tools file_path_sans_ext
#' @importFrom magrittr %>% %<>%
#' @importFrom stringr str_count str_detect str_replace_all str_which word str_match
#' @importFrom tidyr replace_na pivot_longer
#' @importFrom dplyr arrange case_when first group_by mutate summarise_at vars lag summarise
#' @importFrom grDevices rgb
#' @importFrom stats aggregate lm sd na.omit setNames complete.cases dist hclust
#' @importFrom fmsb radarchart
#' @importFrom graphics legend par
#' @importFrom utils download.file read.csv head
#' @importFrom ggplot2 ggplot
#' @importFrom plotly ggplotly renderPlotly plotlyOutput




send_cross_study_app <- function(database_path) {

#Set File Path to Biocelerate Data
## homePath <- dirname(this.path())
## setwd(homePath)

##### Setup Values and Functions ####

#Loading Initial Datasets
summaryResults <- list()
CompileDataSummary <- list()
BodyWeightSummary <- list()
FWDataSummary <- list()
defaultVal <- 350
SEX <- 'M'

#Database Load
db_path <- database_path
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = db_path,
                                    dbCreate = FALSE)


RptDoseStudyID <- sendigR::getStudiesSDESIGN(dbtoken,studyDesignFilter = "PARALLEL")
MIStudyIDS <- sendigR::genericQuery(dbtoken, "SELECT STUDYID FROM MI") #limiting to MI because it is the least likely to be filled
dbStudyIDs <- intersect(RptDoseStudyID$STUDYID,MIStudyIDS$STUDYID)
rm(MIStudyIDS) #Freeing Memory
rm(RptDoseStudyID) #Freeing Memory
dbStudyIDS <- unique(dbStudyIDs)
#Pull INDs and Study Titles for those StudyIDs
APPID <- sendigR::genericQuery(dbtoken, "SELECT * FROM ID WHERE STUDYID in (:1)", dbStudyIDs)
STITLE <- sendigR::genericQuery(dbtoken, 'SELECT STUDYID, TSVAL FROM TS WHERE TSPARMCD = "STITLE" and STUDYID in (:1)', dbStudyIDs)
dbStudys <- merge(APPID, STITLE, by = "STUDYID")
dbStudys$CombinedName <- paste0(dbStudys$APPID,"-",dbStudys$STUDYID,": ",dbStudys$TSVAL)
rm(APPID) #Freeing Memory
rm(STITLE) #Freeing Memory


#Standardizing Terminology
MITESTCDlist <- list('LIVER' = c('LIVER'),
                     'KIDNEY' = c('KIDNEY'),
                     'HEMATOPOIETIC' = c('BONE MARROW', 'SPLEEN', 'THYMUS','LYMPH NODE, MESENTERIC', 'LYMPH NODE, MANDIBULAR'),
                     'ENDOCRINE' = c('GLAND, THYROID', 'GLAND, ADRENAL', 'GLAND, PITUITARY',
                                                        'GLAND, PARATHYROID', 'PANCREAS'),
                     'REPRODUCTIVE' = c('CERVIX','EPIDIDYMIS','GLAND, PROSTATE','GLAND, MAMMARY',
                                                      'OVARY','PROSTATE','TESTIS','UTERUS','VAGINA'))
OMTESTCDlist <- MITESTCDlist
organTESTCDlist <- list('LIVER' = c('SERUM | ALT',
                                    'SERUM | AST',
                                    'SERUM | ALP',
                                    'SERUM | GGT',
                                    'SERUM | BILI',
                                    'SERUM | ALB'),
                        'KIDNEY' = c('SERUM | CREAT',
                                     'SERUM | UREAN',
                                     'SERUM | ALB',
                                     'SERUM | CL',
                                     'SERUM | K',
                                     'SERUM | PHOS',
                                     'SERUM | SODIUM',
                                     'URINE | CL',
                                     'URINE | K',
                                     'URINE | SODIUM',
                                     'URINE | GLUC',
                                     'URINE | SPGRAV',
                                     'URINE | VOLUME',
                                     'URINE | PROT',
                                     'URINE | UROBIL'),
                        'HEMATOPOIETIC' = c( 'WHOLE BLOOD | RBC',
                                             'WHOLE BLOOD | HCT',
                                             'WHOLE BLOOD | MCHC',
                                             'WHOLE BLOOD | MCH',
                                             'WHOLE BLOOD | MCV',
                                             'WHOLE BLOOD | RDW',
                                             'WHOLE BLOOD | WBC',
                                             'WHOLE BLOOD | MONO',
                                             'WHOLE BLOOD | BASO',
                                             'WHOLE BLOOD | EOS',
                                             'WHOLE BLOOD | LYM',
                                             'WHOLE BLOOD | PLAT',
                                             'WHOLE BLOOD | MPV'),
                        'ENDOCRINE' = c('URINE | CL',
                                        'URINE | K',
                                        'URINE | SODIUM',
                                        'URINE | GLUC',
                                        'URINE | SPGRAV',
                                        'URINE | VOLUME',
                                        'URINE | PROT'),
                        'REPRODUCTIVE' = c('SERUM | GNRH',
                                           'SERUM | LH',
                                           'SERUM | FSH',
                                           'SERUM | DHT',
                                           'SERUM | DOXMTST',
                                           'SERUM | FAI',
                                           'SERUM | MTESTOS',
                                           'SERUM | NANDRLN',
                                           'SERUM | TESTOS',
                                           'SERUM | TESTOSBA',
                                           'SERUM | TESTOSFR',
                                           'SERUM | TESTOSWB',
                                           'SERUM | TSTFTSTT',
                                           'SERUM | TSTFWTST',
                                           'SERUM | TST4OH',
                                           'SERUM | ESTROGEN'))
doseRanks <- c('Vehicle', 'LD', 'MD', 'HD')

############################## UI #############################################
ui <- dashboardPage (
  dashboardHeader(title="Cross-Study Analysis", titleWidth = 350),
  dashboardSidebar(
    tags$head(tags$style(type = "text/css", "
              #loadmessage {
                position: relative;
                top: 0px;
                left: 0px;
                width: 100%;
                padding: 5px 0px 5px 0px;
                text-align: center;
                font-weight: bold;
                font-size: 100%;
                color: #000000;
                background-color: #eef66c;
                z-index: 1000;
              }
                ")),
    #Coloring "Input" tabs as Midnight Blue, "Output" Tabs as White
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: Floralwhite;  color:black}
    .tabbable > .nav > li > a[data-value='Data Source'] {background-color: MidnightBlue;   color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: DimGray; color:white}
  ")),
    
    
    actionBttn("PLOT", label = "Generate Visuals", color = "primary",
               style = "jelly"),
    sidebarMenu(id='sidebar', 
                radioButtons("sex", "Sex Selection:",
                             c("Male" = "M",
                               "Female" = "F",
                               "Male/Female" = "M/F"), selected = 'M'),
                
                radioButtons(inputId = "dose", label = "Dose Selection:",
                             c("HD" = "HD", 
                               "MD" = "MD",
                               "LD" = "LD"), selected = 'HD'),
                menuItem('Detailed Control', startExpanded = TRUE,
                         h5('Detailed Test Selection:')
                         ,shinyTree::shinyTree("tree",checkbox = TRUE),
                         pickerInput(inputId = "AGGMethod",
                                     label = "Radar Aggregation Method",
                                     c('mean', 'animalMax','endpointMax'),
                                     selected = 'animalMax'),
                         pickerInput(inputId = 'bwMethod',
                                     label = "BW Method",
                                     c("BW","TERMBW"), selected = 'BW'),
                         pickerInput(inputId = 'omMethod',
                                     label = "OM Ratio Method", 
                                     c("BrainRatio", "BWRatio", "Organ"), selected = 'BrainRatio'))
         )
    ),

  dashboardBody(
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div("Loading...", id="loadmessage")
    ),
    ## shinyjs::useShinyjs(),
    ## shinyjs::runcodeUI(),

    tabsetPanel(
      tabPanel("Data Source", 
              column(width = 5,pickerInput(inputId = 'DataSourceChoice',
                                            label = "Choose data source:", 
                                            c("Local","Database"), selected = "Database")),
              conditionalPanel(condition = "input.DataSourceChoice == 'Local' ",
                               titlePanel("Select Local Folder for Repeat-Dose Study"),
                               shinyDirButton('folder','Select Folder Studies are in:', 'Please Select a Folder', FALSE)
              ),
              conditionalPanel(condition = "input.DataSourceChoice == 'Database' ",
                               titlePanel("Select Database StudyID"),
                               h4('List is limited to Parallel Studies with a MI Domain.'),
                               column(width=5, offset = 5,
                                      uiOutput("Database.select.study"))),
              column(width=5, offset = 5,conditionalPanel(condition = "input.DataSourceChoice == 'Local' & is.list(input.folder) == FALSE",
                                              uiOutput("select.folder")))
              ),
      tabPanel('Overall',
               tabsetPanel(
                 tabPanel('Overall Radarplot',
                          uiOutput('ReactSummaryRadar')),
                 tabPanel('Overall Barplot',
                          column(width=10,fluidRow(cellWidths = c("50%","50%"),
                                                               uiOutput('ReactSummaryBar'), 
                                                               uiOutput('ReactdisplayStudiesALL'))))
               )),
      tabPanel('Body Weight',
               column(width = 5,pickerInput(inputId = 'bwMetric',
                           label = "BW Metric", 
                           c("zScore","PercentChange"), selected = 'PercentChange')),
               column(width = 5,pickerInput(inputId = "FWTime",
                           label = "FW Time Duration",
                           c("Week","Day"), selected = "Week")),
               column(width = 10,plotOutput('FWplot')),
               column(width = 10,plotOutput('BWplot'))
      ),
      tabPanel('Kidney',
               tabsetPanel(
                 tabPanel('Overall',
                          uiOutput('ReactKidneyRadar')),
                 tabPanel('Kidney Barplot',
                          column(width=10,fluidRow(cellWidths = c("50%","50%"),
                                                   uiOutput('ReactKidneyBar'), 
                                                   uiOutput('ReactdisplayStudiesKidney')))),
                 tabPanel('Clinical Chemistry',
                          plotOutput('KSERLBplot')),
                 tabPanel('Urinalysis',
                          plotOutput('KURILBplot')),
                 tabPanel('Organ Weights',
                          plotOutput('OMplot')),
                 tabPanel('Histopathology',
                          column(width = 5,pickerInput(inputId = 'KMIClustY',
                                      label = "Cluster the Y Axis?", 
                                      c("NO","YES"), selected = 'NO')),
                          column(width = 5,pickerInput(inputId = 'KMIClustX',
                                      label = "Cluster the X Axis?", 
                                      c("NO","YES"), selected = 'NO')),
                          column(width = 10,uiOutput('KMIplotreactive')))
                 
               )
      ),
      tabPanel('Liver',
               tabsetPanel(
                 tabPanel('Overall',
                          uiOutput('ReactLiverRadar')),
                 tabPanel('Liver Barplot',
                          column(width=10,fluidRow(cellWidths = c("50%","50%"),
                                                   uiOutput('ReactLiverBar'), 
                                                   uiOutput('ReactdisplayStudiesLiver')))),
                 tabPanel('Clincal Chemistry',
                          plotOutput('LSERLBplot')),
                 tabPanel('Organ Weights',
                          plotOutput('LOMplot')),
                 tabPanel('Histopathology',
                          column(width = 5,pickerInput(inputId = 'LMIClustY',
                                                       label = "Cluster the Y Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width = 5,pickerInput(inputId = 'LMIClustX',
                                                       label = "Cluster the X Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width = 10, uiOutput('LMIplotreactive')))),
      ),
      tabPanel('Hematopoietic',
               tabsetPanel(
                 tabPanel('Overall',
                          uiOutput('ReactHemaRadar')),
                 tabPanel('Hematopoietic Barplot',
                          column(width=10,fluidRow(cellWidths = c("50%","50%"),
                                                   uiOutput('ReactHemaBar'), 
                                                   uiOutput('ReactdisplayStudiesHema')))),
                 tabPanel('Hematology',
                          plotOutput('HHEMELBplot')),
                 tabPanel('Organ Weights',
                          plotOutput('HOMplot')),
                 tabPanel('Histopathology',
                          column(width = 5,pickerInput(inputId = 'HMIClustY',
                                                       label = "Cluster the Y Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width = 5,pickerInput(inputId = 'HMIClustX',
                                                       label = "Cluster the X Axis?", 
                                                       c("NO","YES"), selected = 'NO')),
                          column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                               uiOutput('HMIplotreactive'), 
                                   uiOutput('HMIplotreactive2')))),
                          column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                               uiOutput('HMIplotreactive4'), 
                                                               uiOutput('HMIplotreactive5')))),
                          column(width=10,fluidRow(uiOutput('HMIplotreactive3'))))
               )),
      tabPanel('Endocrine',
               tabsetPanel(
                  tabPanel('Overall',
                           uiOutput('ReactEndoRadar')),
                  tabPanel('Endocrine Barplot',
                           column(width=10,fluidRow(cellWidths = c("50%","50%"),
                                                    uiOutput('ReactEndocrineBar'), 
                                                    uiOutput('ReactdisplayStudiesEndocrine')))),
                  tabPanel('Clinical Chemistry',
                           plotOutput('ESERLBplot')),
                  tabPanel('Organ Weights',
                           plotOutput('EOMplot',height = 600)),
                  tabPanel('Histopathology',
                           column(width = 5,pickerInput(inputId = 'EMIClustY',
                                                        label = "Cluster the Y Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width = 5,pickerInput(inputId = 'EMIClustX',
                                                        label = "Cluster the X Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('EMIplotreactive'),
                                                uiOutput('EMIplotreactive2')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('EMIplotreactive3'),
                                                uiOutput('EMIplotreactive4')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('EMIplotreactive5'))))
                  ))),
      tabPanel('Reproductive',
               tabsetPanel(
                  tabPanel('Overall',
                           uiOutput('ReactReproRadar')),
                  tabPanel('Reproductive Barplot',
                           column(width=10,fluidRow(cellWidths = c("50%","50%"),
                                                    uiOutput('ReactReproductiveBar'), 
                                                    uiOutput('ReactdisplayStudiesReproductive')))),
                  tabPanel('Organ Weights',
                           plotOutput('ROMplot',height = 600)),
                  tabPanel('Histopathology',
                           column(width = 5,pickerInput(inputId = 'RMIClustY',
                                                        label = "Cluster the Y Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width = 5,pickerInput(inputId = 'RMIClustX',
                                                        label = "Cluster the X Axis?", 
                                                        c("NO","YES"), selected = 'NO')),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive'),
                                                uiOutput('RMIplotreactive2')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive3'),
                                                uiOutput('RMIplotreactive4')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive5'),
                                                uiOutput('RMIplotreactive6')))),
                           column(width=10,fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                                uiOutput('RMIplotreactive7'),
                                                uiOutput('RMIplotreactive8')))))
               ))
    )
  ),
)

############################### Server #######################################
  server <- function(input, output, session) {
  
    shinyDirChoose(input, 'folder', roots=c(wd="."), filetypes = c('', '.xpt'))
    plotHeight <- reactiveValues(X=defaultVal)
    numSEX <- reactiveValues(X = 1)

    output$select.folder <- renderUI ({
      if (input$DataSourceChoice == "Local" && is.list(input$folder) == TRUE){
        selectInput(inputId = "LocalStudies",
          label = "Choose Studies",
          choices = list.dirs(path = paste0(homePath,
            paste(unlist(input$folder$path),
              collapse = '/')),
            full.names = FALSE),
          multiple = TRUE)
      }
    })

    output$Database.select.study <- renderUI({
      if (input$DataSourceChoice == "Database"){

        shiny::tagList(
          shiny::selectizeInput(inputId = 'ind', label= 'Select IND',
            choices= dbStudys$APPID,
            multiple= TRUE),

          selectInput(inputId = "DatabaseStudies",
            label = "Choose Database StudyIDs",
            ## NEED TO REWRITE BELOW
            ## choices = dbStudys$CombinedName,
            choices = NULL,
            multiple = TRUE,
            ## selected = two_works
            )

        )
      }
    })

## update studyid selection when IND changes

    shiny::observeEvent(input$ind, {
      ind_selected <- input$ind
      print(ind_selected)
      index <- which(dbStudys$APPID %in% ind_selected)
      df <- dbStudys[index, ]

      studies <- unique(df[['STUDYID']])

      studies_four_treatment_group <- get_treatment_group(studies=studies,
                                                          db_path=database_path)
      studies_four <- studies_four_treatment_group[['four_trtm_group']]
      df_four <- df[STUDYID %in% studies_four,]
      chc <- df_four$CombinedName
      shiny::updateSelectInput(session = session, inputId = 'DatabaseStudies',
                               choices = chc)

    })



   observeEvent(input$PLOT,{
      #Remake the plot values based on User Selection
      ####Controllable Variables###############
     
     dataSource <- input$DataSourceChoice
       #Convert Concatenated Names back to STUDYIDs
       DatabaseStudies <- dbStudys$STUDYID[which(dbStudys$CombinedName %in% input$DatabaseStudies)]
       
       #Find number of StudyIDs
       numstudies <- length(DatabaseStudies)
       
       for (j in 1:numstudies){
         Name <- paste0('SENDStudy',as.character(j))
         #Pull relevant domain data for each domain
         bw <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM BW WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         dm <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM DM WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         ex <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM EX WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         fw <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM FW WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         lb <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM LB WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         mi <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM MI WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         om <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM OM WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         ts <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TS WHERE STUDYID = (:1)",
                                     queryParams = DatabaseStudies[j])
         ta <-sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TA WHERE STUDYID = (:1)",
                                    queryParams = DatabaseStudies[j])

         pooldef <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM POOLDEF WHERE STUDYID = (:1)",
                                          queryParams = DatabaseStudies[j])
         #Combine into list of assigned name
         assign(Name, list('bw' = bw, 'dm' = dm,'ex' = ex, 'fw' = fw, 'pooldef'=pooldef,
                           'lb' = lb, 'mi' = mi, 'om'=om, 'ts'=ts, 'ta'=ta))
         print(paste0(Name, " = ", input$DatabaseStudies[j]))
       }  
       #Check that SEND studies loaded are repeat-dose and have 4 doses (vehicle to high-dose)
       for (nj in 1:numstudies){
         Name <- paste0('SENDStudy',as.character(nj))
         #Get Study Type
         SSTYP <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "SSTYP")]
         #Error if SSTYP is not Repeat Dose Toxicity
         if (SSTYP %in% c("REPEAT DOSE TOXICITY", "REPEAT-DOSE TOXICITY",
                          "Repeat-Dose Toxicity", "Repeat Dose Toxicity") == FALSE){
           showNotification(paste0(input$LocalStudies[nj], " is not a Repeat-Dose Toxicity Study"), type = "error")
           return()
         }
         #Get Number of Doses
         StudyARMCDs <- unique(get(Name)$ta$ARMCD)
         StudyARMCDs <- StudyARMCDs[which(grepl("R",StudyARMCDs) == FALSE)] #Remove Recovery Permutations
       }

      #Check two or more studies chosen Studies Chosen 
      if (numstudies<2){
         showNotification("Two Datasets Must be selected", type = "error")
      } else {
         chosenstudies <- input$studies
         #organSystems for grouping of tests
         organSystems <- c('LIVER', 'KIDNEY', 'HEMATOPOIETIC','ENDOCRINE','REPRODUCTIVE')
         
         #Find Selected Tests from shinyTree ouput
         TreeSelect <- input$tree
         SelectedDomainTests <- names(unlist(shinyTree::get_selected(input$tree, format = "slices")))
         #Check Which Domains are Selected and update organSystems
         systemsselected <- toupper(unlist(strsplit(SelectedDomainTests,"[.]"))) 
         organSystems <- toupper(unique(systemsselected[which(systemsselected %in% organSystems)]))
         #Convert Selected Tests to usable format
         SelectedLBTests <- SelectedDomainTests[which(grepl("Laboratory",SelectedDomainTests) == TRUE)]
         SelectedOMTests <- SelectedDomainTests[which(grepl("Weight",SelectedDomainTests) == TRUE)]
         SelectedMITests <- SelectedDomainTests[which(grepl("Histopathology",SelectedDomainTests) == TRUE)]
         #Remove all but Lowest Levels of Tree (Values which have 3 '.')
         SelectedLBTests <- SelectedLBTests[which(stringr::str_count(SelectedLBTests, "[.]") == 3)]
         SelectedMITests <- SelectedMITests[which(stringr::str_count(SelectedMITests, "[.]") == 2)]
         SelectedOMTests <- SelectedOMTests[which(stringr::str_count(SelectedOMTests, "[.]") == 2)]
         SelectedLBTests <- as.data.frame(strsplit(SelectedLBTests,"[.]"))
         SelectedMITests <- as.data.frame(strsplit(SelectedMITests,"[.]"))
         SelectedOMTests <- as.data.frame(strsplit(SelectedOMTests,"[.]"))
         SelectedLBTests <- SelectedLBTests[c(1,4),]
         SelectedMITests <- SelectedMITests[c(1,3),]
         SelectedOMTests <- SelectedOMTests[c(1,3),]
         SelectedLBTests <- t(SelectedLBTests)
         SelectedOMTests <- t(SelectedOMTests)
         SelectedMITests <- t(SelectedMITests)
         rownames(SelectedLBTests) <- NULL
         rownames(SelectedOMTests) <- NULL
         rownames(SelectedMITests) <- NULL
         #Update organTESTCDlist by separating by organsystem 
         for (organSystem in organSystems){
            #Limit to organ System
            Tests <- SelectedLBTests[which(toupper(SelectedLBTests[,1]) %in% organSystem),2]
            MITESTS <- SelectedMITests[which(toupper(SelectedMITests[,1]) %in% organSystem),2]
            OMTESTS <- SelectedOMTests[which(toupper(SelectedOMTests[,1]) %in% organSystem),2]
            #update relavant organTESTCDlist
            organTESTCDlist[[organSystem]] <- Tests
            ## Update MITESTCDlist FOR OM AND MI using dataframes made above
            MITESTCDlist[[organSystem]] <- as.character(MITESTS)
            OMTESTCDlist[[organSystem]] <- as.character(OMTESTS)
         }



         
         #End point aggregation Method (for Radar): 'mean', 'animalMax', or 'endpointMax'
         aggregationMethod <- input$AGGMethod
         
         #BW Control Variables
         BWMethod <- input$bwMethod
         
         #OM Metric: BrainRatio, BWRatio, or Organ
         OMMetric <- input$omMethod
         
         SEX <- input$sex
         if (SEX == "M/F"){
            SEX <- c("M","F")
            plotHeight$X <- 700
            numSEX$X <- 2
         } else {
            plotHeight$X <- 350
            numSEX$X <- 1
         }
         #Check that Gender and Detailed Test Work
         if ('M' %in% SEX & 'REPRODUCTIVE' %in% organSystems){
            if (any(c('EPIDIDYMIS','GLAND, PROSTATE','PROSTATE','TESTIS') %in% c(MITESTCDlist$REPRODUCTIVE,OMTESTCDlist$REPRODUCTIVE)) == FALSE){
               showNotification("REPRODUCTIVE must include MI/OM from Sex Selected", type = "error")
               return(NULL)
               stop()
      } 
   } 
   if ('F' %in% SEX & 'REPRODUCTIVE' %in% organSystems) {
      if (any(c('CERVIX','GLAND, MAMMARY','OVARY','UTERUS','VAGINA') %in% c(MITESTCDlist$REPRODUCTIVE,OMTESTCDlist$REPRODUCTIVE)== FALSE)){
         showNotification("REPRODUCTIVE must include MI/OM from Sex Selected", type = "error")
         return(NULL)
         stop()
      } 
   } 
   #Select Dose Option for Visualization: 'Vehicle', 'LD','MD','HD
   Dose <- input$dose
  
   #Clear RDS Data
   summaryResults <- list()
   MIresults <- list()
   LBresults <- list()
   OMresults <- list()
   summaryData <- data.frame()
  ##### Load in Data ##########
   #Load Data Pertaining to all organSystems
   for (Gender in SEX){
     #Create Shared Data Frame with StudyID, Species, USUBJID, SEX, and DOSES 
# datasource database ###############
       #Make CompileData with DM of all animals
       CompileData <- data.frame(StudyID = NA, Species = NA, USUBJID = NA, SEX = NA, ARMCD = NA)
       SENDStudyLookup <- data.frame(Name = NA, StudyID = NA)
       for (nj in 1:numstudies){
         Name <- paste0('SENDStudy',as.character(nj))
         #Pull all of the relevant DM Data
         Species <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "SPECIES")]
         TRTName <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "TRT")]
         Duration <-get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "DOSDUR")]
         DelMethod <-get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "ROUTE")]
         #Convert duration to days
         if (grepl("W",Duration) ==TRUE){
           days <- as.numeric(gsub("\\D","",Duration))*7
         } else if (grepl("M",Duration) == TRUE){
           days <- as.numeric(gsub("\\D","",Duration))*7*30
         } else {
           days <- as.numeric(gsub("\\D","",Duration))
         }
         Duration <- paste0(days,"D")
         #Make StudyID
         StudyID <- paste0(Species, " ", TRTName," ",DelMethod," ",Duration)
         SENDStudyLookup <- rbind(SENDStudyLookup, c(Name,StudyID))
         DMData <- data.frame(StudyID = rep(StudyID, length(get(Name)$dm$USUBJID)),
                              Species = rep(Species, length(get(Name)$dm$USUBJID)),
                              USUBJID = get(Name)$dm$USUBJID,
                              SEX = get(Name)$dm$SEX,
                              ARMCD = get(Name)$dm$ARMCD)
         #Remove T from ARMCD for groups that include it for "treatment"
         DMData$ARMCD <- gsub('T','',DMData$ARMCD)
         #Add to CompileData
         CompileData <- rbind(CompileData, DMData)
       }
       #remove NA from first line
       CompileData <- na.omit(CompileData)
       
       #CHeck for TK Animals
       if ("RAT" %in% CompileData$Species){
         #Check which SENDStudies are Rat
         RatStudies <- CompileData[which(CompileData$Species == "RAT"), ]
         StoreTKIndv <- list()
        #Go back to SENDStudy# dataframe and pull pp poolIDs
         for (studies in 1:length(unique(RatStudies$StudyID))){
           study <- unique(RatStudies$StudyID)[studies]
           Name <- SENDStudyLookup$Name[which(SENDStudyLookup$StudyID == study)]
           TKPools <- unique(get(Name)$pp$POOLID)
           TKIndv <- get(Name)$pooldef$USUBJID[which(get(Name)$pooldef$POOLID %in% TKPools)]
           #Check if all treated animals are TK Animals >> Include them if true
           StudyData <- CompileData[which(CompileData$StudyID %in% study),]
           StudyData <- StudyData[!(StudyData$USUBJID %in% c(TKIndv)),]
           if (length(unique(StudyData$ARMCD)) == 1){
             
           } else {
             StoreTKIndv <- append(StoreTKIndv, TKIndv, after = length(StoreTKIndv))
             #Remove TK animals from CompileData
             CompileData <- CompileData[!(CompileData$USUBJID %in% c(TKIndv)),] 
           }
         }
       }
     #Remove Recovery Animals and Recode Treatment Ranks
     ## saveRDS(CompileData, file='compile.rds')
     AllData <- CompileData
     CompileData <- CompileData[!stringr::str_detect(CompileData$ARMCD, "R"),]
     CompileData$ARMCD <- as.numeric(CompileData$ARMCD)
     CompileData$ARMCD <- factor(CompileData$ARMCD)
     if (length(unique(CompileData$ARMCD))>4){
       #error catch for accidental inclusion of other arm numbers
       CompileData <- CompileData[which(CompileData$ARMCD %in% c(1,2,3,4)),]
       CompileData$ARMCD <- as.factor(as.numeric(CompileData$ARMCD))
     }
     levels(CompileData$ARMCD) <- doseRanks
     if (Gender == 'M') {
       CompileData <- CompileData[which(CompileData$SEX == Gender),]
     } else if (Gender == 'F') {
       CompileData <- CompileData[which(CompileData$SEX == Gender),]
     }
     # BW-
     #Find Terminal and Initial BW for all studies
     #Make InitialWeight Formatting Data Frame
       InitialWeight <- data.frame("STUDYID" = NA, "USUBJID" = NA,
                                   "BWSTRESN" = NA,"VISITDY" = NA)
       BodyWeight <- data.frame("STUDYID" = NA, "USUBJID" = NA,
                                   "BWSTRESN" = NA,"VISITDY" = NA)
       for (nj in 1:numstudies){
         ## browser()
         Name <- paste0('SENDStudy',as.character(nj))
         #Check that "VISITDY" is used, 
         if (any(get(Name)$bw$VISITDY[which((get(Name)$bw$VISITDY <= 1))])){
         # if there are pre-study weights include them
         StudyInitialWeight <- get(Name)$bw[which((get(Name)$bw$VISITDY <= 1)),
                                       c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")]
         StudyBodyWeight <- get(Name)$bw[which(get(Name)$bw$BWTESTCD == "BW"),
                                  c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")]
         } else {
           #Change BWDY to VISIDY
           StudyInitialWeight <- get(Name)$bw[which((get(Name)$bw$BWDY <= 1)),
                                              c("STUDYID", "USUBJID", "BWSTRESN","BWDY")]
           StudyBodyWeight <- get(Name)$bw[which(get(Name)$bw$BWTESTCD == "BW"),
                                           c("STUDYID", "USUBJID", "BWSTRESN","BWDY")]
           colnames(StudyInitialWeight) <- c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")
           colnames(StudyBodyWeight) <- c("STUDYID", "USUBJID", "BWSTRESN","VISITDY")
         }
         #Store Values
         InitialWeight <- rbind(InitialWeight,StudyInitialWeight)
         BodyWeight <- rbind(BodyWeight, StudyBodyWeight)
       }
       #Remove NA Values at the start 
       InitialWeight <- na.omit(InitialWeight)
       BodyWeight <- na.omit(BodyWeight)
       #If RAT Studies included remove TK Values
       if ("RAT" %in% CompileData$Species){
         InitialWeight <- InitialWeight[!(InitialWeight$USUBJID %in% c(StoreTKIndv)),]
         BodyWeight <- BodyWeight[!(BodyWeight$USUBJID %in% c(StoreTKIndv)),]
       }
       #Load in TermBW
       if (BWMethod == "TERMBW"){
         TermBodyWeight <- data.frame(STUDYID = NA, USUBJID = NA, BWSTRESN = NA)
         for (nj in 1:numstudies){
         Name <- paste0('SENDStudy',as.character(nj))
         TermBodyWeight <- get(Name)$bw[which(get(Name)$bw$BWTESTCD == "TERMBW"),
                                        c("STUDYID", "USUBJID", "BWSTRESN")]
         }
         TermBodyWeight <- na.omit(TermBodyWeight)
         #If RAT Studies included remove TK Values
         if ("RAT" %in% CompileData$Species){
         TermBodyWeight <- TermBodyWeight[!(TermBodyWeight$USUBJID %in% c(StoreTKIndv)),]
         }
       } else if (BWMethod == "BW"){
         TermBodyWeight <- data.frame(USUBJID = unique(BodyWeight$USUBJID),
                                      BWSTRESN = NA)
         for (Indv in unique(BodyWeight$USUBJID)){
           IndvTerm <- BodyWeight[which(BodyWeight$USUBJID == Indv),]
           InitialW <- IndvTerm[which(IndvTerm$VISITDY <= 0),c("BWSTRESN","VISITDY")]
           if (nrow(InitialW) == 0){ #error catch for animals missing pre-study
             InitialW <- IndvTerm[which(IndvTerm$VISITDY <= 1),c("BWSTRESN","VISITDY")]
           }
           InitialW <- as.numeric(InitialW$BWSTRESN[which(InitialW$VISITDY == max(InitialW$VISITDY),)])
           IndvTerm$Diff <- abs(as.numeric(IndvTerm$BWSTRESN) - InitialW)
           maxdiff <- max(as.numeric(IndvTerm$Diff), na.rm = TRUE)  ## Find largest difference from initial
           index <- which(TermBodyWeight$USUBJID == Indv)
           TermBodyWeight$BWSTRESN[index] <- unique(IndvTerm$BWSTRESN[which(as.numeric(IndvTerm$Diff) == maxdiff)])
         }
         TermBodyWeight$BWSTRESN <- as.numeric(TermBodyWeight$BWSTRESN)
         #If RAT Studies included remove TK Values
         if ("RAT" %in% CompileData$Species){
           TermBodyWeight <- TermBodyWeight[!(TermBodyWeight$USUBJID %in% c(StoreTKIndv)),]
         }
       }
     ## }
     #Make Numeric
     if (is.factor(TermBodyWeight$BWSTRESN)) {
       TermBodyWeight$BWSTRESN <- as.numeric(levels(TermBodyWeight$BWSTRESN)[TermBodyWeight$BWSTRESN])
       InitialWeight$BWSTRESN <- as.numeric(levels(InitialWeight$BWSTRESN)[InitialWeight$BWSTRESN])
     } else {
       TermBodyWeight$BWSTRESN <- as.numeric(TermBodyWeight$BWSTRESN)
       InitialWeight$BWSTRESN <- as.numeric(InitialWeight$BWSTRESN)
       BodyWeight$BWSTRESN <- as.numeric(BodyWeight$BWSTRESN)
     }
     CompileData <- merge(CompileData, TermBodyWeight, by = "USUBJID")
     #Subtract Starting Weight
     CompileData$BWSub <- NA
     CompileData$BWBaseline <- NA
     BodyWeight$BWSub <- NA
     BodyWeight$BaselinePercentChange <- NA
     BodyWeight$Baseline <- NA
     for (indv in unique(InitialWeight$USUBJID)){
       Relvdata <- CompileData[which(CompileData$USUBJID == indv),]
       inx <- which(CompileData$USUBJID == indv)
       BWinx <- which(BodyWeight$USUBJID == indv)
       InitialW <- InitialWeight[which(InitialWeight$USUBJID == indv),c("BWSTRESN","VISITDY")]
       InitialW <- as.numeric(InitialW$BWSTRESN[which(InitialW$VISITDY == max(InitialW$VISITDY),)])
       CompileData$BWSub[inx] <- CompileData$BWSTRESN[inx]-InitialW
       CompileData$BWBaseline[inx] <- InitialW
       BodyWeight$Baseline[BWinx] <- InitialW
       BodyWeight$BWSub[BWinx] <- as.numeric(BodyWeight$BWSTRESN[BWinx]) - InitialW
       BodyWeight$BaselinePercentChange[BWinx] <- BodyWeight$BWSub[BWinx]/InitialW*100
     }

     #Make Compile Data BWzScore for Scoring
     CompileData$BWzScore <- NA
     for (study in unique(CompileData$StudyID)){
       stdyidx <- which(CompileData$StudyID %in% study)
       Controlanimals <- CompileData[which(CompileData$ARMCD == "Vehicle" & CompileData$StudyID %in% study),]
       ControlBaselinemean <- mean(Controlanimals$BWSub)
       ControlBaseSD <- sd(Controlanimals$BWSTRESN)
       CompileData$BWzScore[stdyidx] <- (CompileData$BWSub[stdyidx] - ControlBaselinemean )/ControlBaseSD
     }
     if (BWMethod == "BW"){
       BodyWeight <- merge(BodyWeight, CompileData[,c('USUBJID','SEX','StudyID','ARMCD')], by='USUBJID')
       BodyWeightSummary[[Gender]] <- BodyWeight
     }
     if (BWMethod == "TERMBW"){
       BodyWeight <- NA
       BodyWeight <- merge(TermBodyWeight[,c("USUBJID","BWSTRESN")], CompileData[,c("USUBJID","Species","ARMCD","SEX", "StudyID", "BWSub","BWBaseline")],
                            by='USUBJID')
       BodyWeightSummary[[Gender]] <- BodyWeight
     }

     #FW-
       #Check Number of Species in Compile Data
       NumSpecies <- unique(CompileData$Species)
       #Make Blank Data Frame
       DailyFood <- data.frame(ARMCD = "0", FWDY = 0, FWSTRESN = 0, Compound = '0',
                               Species = NA, Diff = 0)
       for (TypeSpecies in NumSpecies){
         #Get StudyIDs for Studies in this species
         SpeciesIDs <- CompileData$StudyID[which(CompileData$Species == TypeSpecies)]
         SpeciesFood <- data.frame(ARMCD = "0", FWDY = 0, FWSTRESN = 0, Compound = '0')
         ##Load in Studies FW
         for (study in unique(SpeciesIDs)){
           Name <- SENDStudyLookup$Name[which(SENDStudyLookup$StudyID == study)]
           TRTName <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "TRT")]
           Species <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "SPECIES")]
           #Check that there is a FW Domain
           if (is.null(get(Name)$fw) == TRUE){
             if (is.null(get(Name)$cl) == TRUE){
               #Skips Study if it has neither cl or fw
               
             } else {
               #Converting cl into fw 
               FoodData <- merge(CompileData[which(CompileData$StudyID == study),c("USUBJID","StudyID","Species","SEX","ARMCD")],
                                  get(Name)$cl[,c("USUBJID", "CLDY","CLSTRESC")])
               #Convert Categorical to Numeric %
               FoodDataSum <- FoodData %>%
                 dplyr::mutate(CLSTRESC = case_when(CLSTRESC == "NORMAL" ~ 1.0
                                                    ,CLSTRESC == "Food Consumption, reduced" ~ 0.5
                                                    ,CLSTRESC == "Food Consumption, minimal" ~ 0.25
                                                    ,TRUE ~ 0
                 ))
               #Rename Column names to match Dog6576
               names(FoodDataSum)[6] <- "FWDY"
               names(FoodDataSum)[7] <- "FWSTRESN"
               FoodDataSum <- FoodDataSum %>% #Average by Treatment Group per Day
                 group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
               FoodDataSum$FWDY <- as.numeric(FoodDataSum$FWDY)
               FoodDataSum$Compound <- rep(TRTName, nrow(FoodDataSum))
               SpeciesFood <-rbind(SpeciesFood,FoodDataSum)
             }
           } else {
           #Check if study is a rat study due to pooled housing
           if (Species %in% c("RAT","Rat","rat")){
             if ("POOLID" %in%  colnames(get(Name)$fw)){
             PoolFoodData <- merge(get(Name)$pooldef,
                               get(Name)$fw[,c("POOLID","FWDY","FWSTRESN")],
                               allow.cartesian = TRUE,
                               by = "POOLID")
             } else { #Accounting for studies that don't pool rats
               PoolFoodData <- get(Name)$fw[,c("USUBJID","FWDY","FWSTRESN")]
             }
             FoodData <- merge(CompileData[which(CompileData$StudyID == study),c("USUBJID","StudyID","Species","SEX","ARMCD")],
                               PoolFoodData[,c("USUBJID", "FWDY","FWSTRESN")], by = "USUBJID") 
             #Remove TK Animals
             FoodData <- FoodData[!(FoodData$USUBJID %in% c(StoreTKIndv)),]
             #Average by Treatment Group per Day
             FoodDataSum <- FoodData %>%  
               group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
             FoodDataSum$Compound <- rep(TRTName,nrow(FoodDataSum))
             SpeciesFood <- rbind(SpeciesFood,FoodDataSum)
           } else {
             #Non Pooled Rat Load
             FoodData <- merge(CompileData[which(CompileData$StudyID == study), c("USUBJID","StudyID","Species","SEX","ARMCD")],
                               get(Name)$fw[,c("USUBJID", "FWDY","FWSTRESN")], by = "USUBJID")
             FoodDataSum <- FoodData %>%  
               group_by(ARMCD,FWDY) %>% summarise_at(vars(FWSTRESN),mean)
             FoodDataSum$Compound <- rep(TRTName,nrow(FoodDataSum))
             SpeciesFood <- rbind(SpeciesFood,FoodDataSum)
           }
           }
         }
         #Remove formatting
         SpeciesFood$FWSTRESN <- as.numeric(SpeciesFood$FWSTRESN)
         SpeciesFood <- na.omit(SpeciesFood)
         #Take Average consumption of Pre-Study Animals
         PreStudy <- SpeciesFood[which(SpeciesFood$FWDY <=0),]
         MeanFC <- mean(PreStudy$FWSTRESN, na.rm = TRUE)
         if (MeanFC == 0){ #IF there is no prestudy only use the first day
           PreStudy <- SpeciesFood[which(SpeciesFood$FWDY <=1),]
           MeanFC <- mean(PreStudy$FWSTRESN, na.rm = TRUE)
         }
         #Normalize FWStresn for comparisson using Average
         SpeciesFood$FWSTRESN <- (SpeciesFood$FWSTRESN/MeanFC)
         ##Make DataFrame per Species
         FoodDaily <- SpeciesFood %>%
           group_by(FWDY, Compound) %>%
           arrange(FWDY) %>%
           mutate(Diff = FWSTRESN - lag(FWSTRESN, default = first(FWSTRESN)))
         FoodDaily$Species <- rep(TypeSpecies, nrow(FoodDaily))
         #Store Values in DailyFood
         DailyFood <- rbind(DailyFood, FoodDaily)
         }
       #Remove Formatting line
       DailyFood <- na.omit(DailyFood)
       #Spit out FWData dataframe of all species
       FWData <- DailyFood
     ## }
     FWDataSummary[[Gender]] <- FWData
     CompileDataPrime <- CompileData
  
     ##################### Organ System Specific Graphs ######################
     #Will Run through all of the possible organSystems to generate graphs and fill CompileData
     for (organSystem in organSystems) {
       MIDOMAIN <- organSystem
       Organ <- MIDOMAIN #For OM, will automatically include brain for brain ratio
       #Make LB Data Data Frame to Hold Information
       # LB-
         LBData <- data.frame("USUBJID" = NA,"LBSPEC" = NA,"LBTESTCD" = NA,
                              "LBSTRESN" = NA, "VISITDY" = NA)
         for (nj in 1:numstudies){
           Name <- paste0('SENDStudy',as.character(nj))
           #Pull all of the relevant LB Data
           if ("LBDY" %in% colnames(get(Name)$lb) == TRUE){
             LBD <- get(Name)$lb[which((get(Name)$lb$LBDY >= 1)),
                                 c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "LBDY")]
             colnames(LBD) <- c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")
             if (all(is.na(LBD$LBSPEC)) == TRUE){
               #Convert LBCAT to LBSPEC if no LBSPEC
               LBD$LBSPEC <- get(Name)$lb[which((get(Name)$lb$LBDY >= 1)),
                                         c("LBCAT")]
               if (c("HEMATOLOGY", "Hematology","hematology") %in% levels(LBD$LBSPEC)){
                 levels(LBD$LBSPEC)[match(c("HEMATOLOGY", "Hematology","hematology"),
                                          levels(LBD$LBSPEC))] <- "WHOLE BLOOD"
               }
               if (c("CLINICAL CHEMISTRY","Clinical Chemistry") %in% levels(LBD$LBSPEC)){
                 levels(LBD$LBSPEC)[match(c("CLINICAL CHEMISTRY","Clinical Chemistry"),
                                          levels(LBD$LBSPEC))] <- "SERUM" 
               }
               if (c("URINALYSIS","Urinalysis") %in% levels(LBD$LBSPEC)){
                 levels(LBD$LBSPEC)[match(c("URINALYSIS","Urinalysis"),
                                          levels(LBD$LBSPEC))] <- "URINE" 
               }
             }
           } else {
             LBD <- get(Name)$lb[which((get(Name)$lb$VISITDY >= 1)),
                                 c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")] 
           }
           #Add to CompileData
           LBData <- rbind(LBData, LBD)
         }
         LBData <- na.omit(LBData)
       # Concatenate LBSPEC and LBTESTCD
       LBData$LBTESTCD <- paste(LBData$LBSPEC, LBData$LBTESTCD, sep = ' | ')
       #Remove Not Included Tests
       organIndex <- which(LBData$LBTESTCD %in% organTESTCDlist[[organSystem]])
       LBData <- LBData[organIndex,]
       #Check if LBData has become empty
       if (nrow(LBData) == 0){
         #Removes empty LBData from FINALDAYS
         #print(paste0("No LB TESTS SELECTED in ", organSystem))
       } else {
       #Make list of Recovery Animals
       RecoveryAnimals<-unique(subset(LBData$USUBJID, !(LBData$USUBJID %in% CompileData$USUBJID)))
       #Find Final Day for Before Recovery for Recovery Animals
       RecovData <- LBData[which(LBData$USUBJID %in% RecoveryAnimals),]
       RecovData <- RecovData[which(RecovData$VISITDY < 40),] #set manually for Biocelerate Studies
       FinalDays <- NA
       for (indv in unique(LBData$USUBJID)){
         indvtests <- LBData[which(LBData$USUBJID == indv), "LBTESTCD"]
         for (TEST in unique(indvtests)){
           if (indv %in% RecoveryAnimals){
             Indv <- which(RecovData$USUBJID == indv)
             IndvData <- RecovData[Indv,]
             IndvData <- IndvData[which(IndvData$LBTESTCD == TEST),]
             maxday <- suppressWarnings(max(IndvData$VISITDY))
           } else {
             Indv <- which(LBData$USUBJID == indv)
             IndvData <- LBData[Indv,]
             IndvData <- IndvData[which(IndvData$LBTESTCD == TEST),]
             maxday <- suppressWarnings(max(IndvData$VISITDY))
           }
           LastTest <- which(LBData$USUBJID == indv & LBData$VISITDY == maxday)
           FinalDays <- append(FinalDays, LastTest)
         }
       }
       FinalDays <- unique(FinalDays) #Removes accidentally created duplicates in FINAL DAYS
       LBData <- LBData[FinalDays, c("USUBJID","LBSPEC","LBTESTCD","LBSTRESN")]
       }
       # MI-
       #MI Values included in grouped systems replace MIDOMAIN and Organ for analysis
       Group <- MIDOMAIN
       MIDOMAIN <-MITESTCDlist[[organSystem]]
       Organ <- OMTESTCDlist[[organSystem]]
       #Make DataFrame to hold MI Information 
         #Add Load of MIData
         MIData <- data.frame("USUBJID" = NA,"MISTRESC" = NA,"MISEV" = NA,
                              "MISPEC" = NA)
         for (nj in 1:numstudies){
           Name <- paste0('SENDStudy',as.character(nj))
           #Pull all of the relevant MI Data using Grepl for language variances (such as added (s))
           MBD <- get(Name)$mi[which(grepl(MIDOMAIN, get(Name)$mi$MISPEC) == TRUE),
                               c("USUBJID", "MISTRESC","MISEV","MISPEC")]
           #Add to CompileData
           MIData <- rbind(MIData, MBD)
         }
         #MAKE NA Sev's a 0
         MIData$MISEV = MIData$MISEV %>% tidyr::replace_na("0")
         MIData <- na.omit(MIData)
       ## }
       MIData$MISTRESC <- toupper(MIData$MISTRESC)
       #Convert Severity
       MIData$MISEV <- stringr::str_replace_all(MIData$MISEV, "1 OF 5", "1")
       MIData$MISEV <- stringr::str_replace_all(MIData$MISEV, "MINIMAL", "1")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "2 OF 5", "2")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "MILD", "2")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "3 OF 5", "3")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "MODERATE", "3")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "4 OF 5", "4")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "MARKED", "4")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "5 OF 5", "5")
       MIData$MISEV <-  stringr::str_replace_all(MIData$MISEV, "SEVERE", "5")
       MIData$MISEV <- ordered(MIData$MISEV, levels= c("0","1", "2", "3", "4","5"))
       MIData$MISEV = MIData$MISEV %>% tidyr::replace_na("0")
       
       #Compile OM Data for Organlist
       #Find Brain indexes
         OrganWeights <- data.frame(USUBJID = NA,OMSPEC = NA,OMSTRESN = NA, OMTEST = NA)
         for (nj in 1:numstudies){
           Name <- paste0('SENDStudy',as.character(nj))
           #Pull idx of the brain and desired organs
           Studyidx <- stringr::str_which(get(Name)$om$OMSPEC, "BRAIN")
           for (i in 1:length(Organ)){
             Studyidx <- append(Studyidx, stringr::str_which(get(Name)$om$OMSPEC, Organ[i]))
           }
           #Pull relevant OM Data
           OMD <- get(Name)$om[Studyidx,c("USUBJID", "OMSPEC", "OMSTRESN","OMTEST")]
           #Add to CompileData
           OrganWeights <- rbind(OrganWeights, OMD)
         }
         OrganWeights <- na.omit(OrganWeights)
       ## }
       OrganWeights <- OrganWeights[which(OrganWeights$OMTEST == "Weight"), c("USUBJID", "OMSPEC", "OMSTRESN")]
       CompileData <- merge(CompileData, OrganWeights, by = "USUBJID", all.x = T) 
       CompileData <- CompileData[which(is.na(CompileData$OMSPEC) == FALSE),]
       
      #Combine Levels on Findings
      MIData$MISTRESC <- as.factor(MIData$MISTRESC)
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "CELL DEBRIS"] <- "CELLULAR DEBRIS"
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Infiltration, mixed cell"] <- "Infiltrate"
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Infiltration, mononuclear cell"] <- "Infiltrate"
      levels(MIData$MISTRESC)[levels(MIData$MISTRESC) == "Fibrosis"] <- "Fibroplasia/Fibrosis"
  ######### OM Graph ###############

      #Calculate Organ to Terminal BW Ratio
      CompileData$OrgantoTermBW <- CompileData$OMSTRESN/CompileData$BWSTRESN
      #Calculate Organ to Brain Weight Ratio
      CompileData$OrgantoBrainOW <- NA
      for (sub in unique(CompileData$USUBJID)){
        SubjectData <- CompileData[which(CompileData$USUBJID == sub),]
        Subidx <-which(CompileData$USUBJID == sub)
        SubBrainWeight <- SubjectData$OMSTRESN[which(SubjectData$OMSPEC == "BRAIN")]
        CompileData$OrgantoBrainOW[Subidx] <- CompileData$OMSTRESN[Subidx]/SubBrainWeight
      }
       ## browser()
      #Calculate zScore
      CompileData$OrganzScore <- NA
      CompileData$BWRatiozScore <- NA
      CompileData$BrainRatiozScore <- NA
      for (sex in c('M','F')) {
        ## Restrict Gender of compile data based on sex
        GenderData <- CompileData[which(CompileData$SEX == sex),]
        #Calculate Z Score for straight Organ Weight, Terminal BW Ratio, and Brain Ratio
        for (study in unique(CompileData$StudyID)){
          StudyData <- GenderData[which(GenderData$StudyID == study),]
          StudyIdx <- which(CompileData$StudyID == study)
          StudyIdx <- intersect(StudyIdx, which(CompileData$SEX == sex))
          #Find Control Animals Per Study and Calculate Mean and SD per Organ
          ControlAnimals <- StudyData[which(StudyData$ARMCD == "Vehicle"),]
          for (organ in unique(StudyData$OMSPEC)){
            organdata <- ControlAnimals[which(ControlAnimals$OMSPEC == organ),]
            MeansSd <- data.frame(ORGAN = organ,
                                  ORGANMEAN = mean(organdata$OMSTRESN, na.rm = TRUE),
                                  ORGANSd = sd(organdata$OMSTRESN, na.rm = TRUE),
                                  OrgantoBWmean = mean(organdata$OrgantoTermBW, na.rm = TRUE),
                                  OrgantoBWSd = sd(organdata$OrgantoTermBW, na.rm = TRUE),
                                  OrgantoBrainmean = mean(organdata$OrgantoBrainOW, na.rm = TRUE),
                                  OrgantoBrainSd = sd(organdata$OrgantoBrainOW, na.rm = TRUE))
            Idx<- which(StudyData$OMSPEC == organ)
            StudyData$OrganzScore[Idx] <-(StudyData$OMSTRESN[Idx] - MeansSd$ORGANMEAN)/MeansSd$ORGANSd
            StudyData$BWRatiozScore[Idx] <-(StudyData$OrgantoTermBW[Idx] - MeansSd$OrgantoBWmean)/MeansSd$OrgantoBWSd
            StudyData$BrainRatiozScore[Idx] <-(StudyData$OrgantoBrainOW[Idx] -MeansSd$OrgantoBrainmean)/MeansSd$OrgantoBrainSd
          }
          #Reconcile Study into CompileData with Zscores
          CompileData$OrganzScore[StudyIdx] <- StudyData$OrganzScore
          CompileData$BWRatiozScore[StudyIdx] <- StudyData$BWRatiozScore
          CompileData$BrainRatiozScore[StudyIdx] <- StudyData$BrainRatiozScore
        }
      }

      #GRAPH OM ZScore
      OMGraphData <- CompileData[,c("USUBJID","StudyID","ARMCD","Species","SEX","OMSPEC","BrainRatiozScore",
                                    "BWRatiozScore", "OrganzScore")]
      OMGraphData$Compound <- word(OMGraphData$StudyID, 2)

      #Remove Brain and limit to Dose Chosen
      OMGraphData <- OMGraphData[which(OMGraphData$OMSPEC %in% Organ),]
      OMresults[[Group]][[Gender]] <-OMGraphData

      ###### LB GRAPHS #######################################################
      
      if (nrow(LBData) == 0){
        #Removes empty LBData from zScore Calcuation as it cannot calculate
      } else {
      #Calculate Z Score per LBTESTCD
      LBData <- merge(LBData, unique(AllData[,c("USUBJID", "ARMCD","StudyID","SEX")]), by = "USUBJID")
      #Recode Arms for LBData
      LBData$ARMCD <- as.factor(LBData$ARMCD)
        # Find Levels of Study
        ARMS <- levels(LBData$ARMCD)
        ARMS <- gsub("R","",ARMS)
        levels(LBData$ARMCD) <- ARMS #Remove leading 0s
        levels(LBData$ARMCD)[levels(LBData$ARMCD) == "01"] <- "1"
        levels(LBData$ARMCD)[levels(LBData$ARMCD) == "02"] <- "2"
        levels(LBData$ARMCD)[levels(LBData$ARMCD) == "03"] <- "3"
        levels(LBData$ARMCD)[levels(LBData$ARMCD) == "04"] <- "4"
      ## }
      levels(LBData$ARMCD) <- doseRanks
      if (length(LBData$LBTESTCD) == 0){
      } else {
        LBData$zscore <- NA
        for (sex in c('M','F')) {
          ## Restrict Gender of compile data based on sex
          GenderData <- LBData[which(LBData$SEX == sex),]
          for (study in unique(LBData$StudyID)){
            StudyData <- GenderData[which(GenderData$StudyID == study),]
            for (TEST in unique(StudyData$LBTESTCD)){
              TESTData <- StudyData[which(StudyData$LBTESTCD == TEST),]
              ControlTESTData <- TESTData[which(TESTData$ARMCD == "Vehicle"),]
              SIdx <- which(LBData$StudyID == study)
              SIdx <- intersect(SIdx, which(LBData$SEX == sex))
              index <- intersect(which(LBData$LBTESTCD == TEST),SIdx)
              LB.mean.C <- mean(ControlTESTData$LBSTRESN, na.rm = TRUE)
              LB.sd.C <- sd(ControlTESTData$LBSTRESN, na.rm = TRUE)
              if (is.na(LB.sd.C) == TRUE){
                #print(paste0(TEST, "For ", study, sex))
              }
              LBData$zscore[index] <- (LBData$LBSTRESN[index]- LB.mean.C)/LB.sd.C
            }
          }
        }
      }
      #Remove Recovery Animals from LBData
      LBData2 <- LBData[which(LBData$USUBJID %in% CompileData$USUBJID),]
      if (length(LBData2$LBTESTCD) == 0){
        
      } else{
        CompileData <- merge(CompileData, LBData2[,c("USUBJID","zscore","LBTESTCD")], by = "USUBJID")
        CompileData <- reshape2::dcast(CompileData, USUBJID+StudyID+Species+SEX+ARMCD+OMSPEC+BrainRatiozScore+BWSTRESN+BWzScore~LBTESTCD, value.var = "zscore", fun.aggregate = mean)
      }
      }

      #Save Respective LBData per OrganSystem and Gender
      LBresults[[Group]][[Gender]] <- LBData

  ########## MI Data ###############
      
      #Merge Severity MI Data into Compile Data
       ## print('MIDATA')
       ## print(str(MIData))
      MIIncidencePRIME <-MIData[,c(1,2,4)]

      ## print('MIIncidencePRIME')
       ## print(head(MIIncidencePRIME))
      Severity <- merge(MIData, CompileData[,c("USUBJID", "StudyID", "Species", "ARMCD")])
      MIData <- reshape2::dcast(MIData, USUBJID ~ MISTRESC, value.var = "MISEV")
      MIData[is.na(MIData)] <- "0" #Fill NAs with Zero
      CompileData<- merge(CompileData, MIData, by = "USUBJID")
       print('compile data')
       print(str(CompileData))

      # Remove Normal MI Results
      normalIndex <- which(colnames(CompileData) == 'NORMAL')
      if (length(normalIndex) > 0) {
        CompileData <- CompileData[, -normalIndex]
      }

      #Calculate Incidence per group for MI Data
      MIIncidencePRIME <- merge(MIIncidencePRIME, unique(CompileData[,c("USUBJID", "StudyID","ARMCD")]), by = "USUBJID")
      groups <- paste0(MIIncidencePRIME$StudyID," ", MIIncidencePRIME$ARMCD)
      #HAVE IT THROUGH ORGANS HERE
      for (MISPEC in unique(MIIncidencePRIME$MISPEC) ){
      MIIncidence <- MIIncidencePRIME[which(MIIncidencePRIME$MISPEC %in% MISPEC),c("USUBJID","MISTRESC", "StudyID","ARMCD")]
      
      GroupIncid <- data.frame(Treatment = NA,
                               Sex = NA,
                               Finding = NA,
                               Count = NA)
      print('GroupIncid')
      print(str(GroupIncid))
      GroupIncid2 <- GroupIncid
      for (Study in unique(MIIncidence$StudyID)){
        for (sex in c('M','F')) {
          StudyMI <- MIIncidence[which(MIIncidence$StudyID==Study),]
          StudyGroupIncid <- data.frame(Treatment = NA,
                                        Sex = NA,
                                        Finding = NA,
                                        Count = NA)
          StudyNonBaselineIncid <- StudyGroupIncid
          sexSubjects <- CompileData$USUBJID[which(CompileData$SEX == sex)]
          sexIndex <- which(StudyMI$USUBJID %in% sexSubjects)
          StudyMI <- StudyMI[sexIndex,]
          print('StudyMI')
          print(str(StudyMI))
          for(dose in unique(StudyMI$ARMCD)){
            doseMI <- StudyMI[which(StudyMI$ARMCD == dose),]
            print('doseMI')
            print(str(doseMI))


            Incid <- data.frame(table(toupper(doseMI$MISTRESC))/length(unique(doseMI$USUBJID)))
            names(Incid)[2] <- "Count"
            names(Incid)[1] <- "Finding"
            Incid$Treatment <- paste0(Study, " ",dose)
            Incid$Sex <- sex

            StudyGroupIncid <- rbind(StudyGroupIncid,Incid)
            StudyNonBaselineIncid <- rbind(StudyNonBaselineIncid, Incid)
          }
          #Removing of Vehicle Baseline
          for (finding in unique(StudyGroupIncid$Finding)) {
            findingIndex <- which(StudyGroupIncid$Finding == finding)
            vehicleIndex <- grep('Vehicle', StudyGroupIncid$Treatment[findingIndex])
            if (length(vehicleIndex) > 0) {
              baseline <- StudyGroupIncid$Count[findingIndex][vehicleIndex]
              StudyGroupIncid$Count[findingIndex] <- StudyGroupIncid$Count[findingIndex] - baseline
            }
          }
          negativeIndex <- which(StudyGroupIncid$Count < 0)
          if (length(negativeIndex) > 0) {
            StudyGroupIncid$Count[negativeIndex] <- 0
          }

          GroupIncid <- rbind(GroupIncid, StudyGroupIncid)
          GroupIncid2 <- rbind(GroupIncid2, StudyNonBaselineIncid) #Non-Baseline Removed for graph
        }
      }
      removeIndex <- which(is.na(GroupIncid$Treatment))
      removeIndex2 <- which(is.na(GroupIncid2$Treatment))
      if (length(removeIndex) > 0) {
        GroupIncid <- GroupIncid[-removeIndex,]
        GroupIncid2 <- GroupIncid2[-removeIndex2,]
      }
      MIIncidence <- GroupIncid
      MIIncidence2 <- GroupIncid2
      #Make MIplotData by Merging MI Incidence with Severity
      Severity$Treatment <- paste0(Severity$StudyID," ", Severity$ARMCD)
      names(Severity)[2] <- "Finding"
      MIplotData <- merge(MIIncidence2,Severity[,c("Treatment","MISEV","Finding")], by = c("Treatment","Finding"))
      MIplotData <- aggregate(MISEV ~Treatment+Finding+Sex+Count,FUN = max, data = MIplotData)
      #Reconcile names for Heatmap
      rep_str <- c("NORMAL" = "UNREMARKABLE", "INFILTRATION, MIXED CELL"= "INFILTRATE",
                   "INFILTRATION, MONONUCLEAR CELL" = "INFILTRATE")
      MIplotData$Finding <- stringr::str_replace_all(MIplotData$Finding, rep_str)
      #Filter for Parameters
      MIplotData$Dose <- word(MIplotData$Treatment,-1)
      
      #SAVE MIplotData Per Gender and Dose AND ORGAN
      MIresults[[Group]][[Gender]][[MISPEC]]<- MIplotData
      }
      CompileDataSummary[[Gender]] <- CompileData
  ####################################### Scoring Portion #########################

      ScoredData <- CompileData[,1:6] # On Local COmpile data is empty here D:
      ScoredData$BWzScore <- abs(CompileData$BWzScore)
      for (Study in unique(ScoredData$StudyID)){
        ScoredData[ScoredData$StudyID == Study,] %<>%
          dplyr::mutate(BWzScore = case_when(BWzScore > 3 ~ 3
                                             ,BWzScore >= 2 ~ 2
                                             ,BWzScore >= 1 ~ 1
                                             ,TRUE ~ 0
          ))

      }

      #Score the OM Domain Brain Ratios
      ScoredData$OMBrainRatio <- abs(CompileData$BrainRatiozScore)
      for (Study in unique(ScoredData$StudyID)){
        ScoredData[ScoredData$StudyID == Study,] %<>%
          dplyr::mutate(OMBrainRatio = case_when(OMBrainRatio > 3 ~ 3
                                                 ,OMBrainRatio  >= 2 ~ 2
                                                 ,OMBrainRatio  >= 1~ 1
                                                 ,TRUE ~ 0
          ))
      }

      #Score LB Data
      colIndex <- which(colnames(CompileData) %in% organTESTCDlist[[organSystem]])
      if (identical(integer(0),colIndex) == TRUE){
        ScoredData$OMSPEC <- CompileData$OMSPEC
        #Removes Scoring if there are no LB Tests
        colIndex <- which(colnames(CompileData) == "BWRatiozScore")
        print(paste0("No LB TESTS SELECTED in ", organSystem)) 
      } else {
        for (i in colIndex) {
          colName <- colnames(CompileData)[i]
          ScoredData[[colName]] <- NA
          for (Study in unique(ScoredData$StudyID)){
            j <- which(CompileData$StudyID == Study)
            StudyData <- CompileData[which(CompileData$StudyID == Study),]
            
            x <- ifelse(StudyData[,i]>3 | StudyData[,i]<(-3),3,
                        ifelse(StudyData[,i]>2 | StudyData[,i]<(-2),2,
                               ifelse((StudyData[,i]> 1 | StudyData[,i]< (-1)),1,0)))
            ScoredData[j,colName] <-x
            # colnames(ScoredData)[i] <- colnames(CompileData)[i+1]
          }
        }
      }
      IncidenceOverideCount <- 0 
      #Score MI Data
      ## print('colIndex')
       ## print(colIndex)
       ## print(str(CompileData))
      colIndex <- seq((colIndex[length(colIndex)]+1), ncol(CompileData))
      for (i in colIndex){
        colName <- colnames(CompileData)[i]
        ScoredData[[colName]] <- NA 
        #Score Severity
        x <- ifelse(CompileData[,i]>3,3,
                    ifelse(CompileData[,i]==3,2,
                           ifelse(CompileData[,i]>0,1,0)))
        ScoredData[,colName] <-x
        #Check the Incidence percentage for each group
        for (Study in unique(ScoredData$StudyID)){
          for (sex in c('M','F')) {
            studyDataStudyIndex <- which(CompileData$StudyID == Study)
            studyDataSexIndex <- which(CompileData$SEX == sex)
            studyDataIndex <- intersect(studyDataStudyIndex, studyDataSexIndex)
            StudyData <- CompileData[studyDataIndex,]

            MIIncidStudyIndex <- grep(Study, MIIncidence$Treatment)
            MIIncidSexIndex <- which(MIIncidence$Sex == sex)
            MIIncidIndex <- intersect(MIIncidStudyIndex, MIIncidSexIndex)
            MIIncidStudy <- MIIncidence[MIIncidIndex,]

            for (Dose2 in unique(StudyData$ARMCD)){
              DoseSevIndex <- which(StudyData$ARMCD == Dose2)
              DoseSev <- StudyData[DoseSevIndex,]
              DoseIncid <- MIIncidStudy[which(word(MIIncidStudy$Treatment, -1) == Dose2),]
              if (colName %in% DoseIncid$Finding) {
                findingIndex <- which(DoseIncid$Finding == colName)
                Incid <- DoseIncid$Count[findingIndex]
                Incid <- ifelse(Incid>=0.5,3,
                                ifelse(Incid>=0.25,2,
                                       ifelse(Incid>=0.1,1,0)))
                swapIndex <- which(DoseSev[[colName]] < Incid & DoseSev[[colName]] > 0)
                if (length(swapIndex) > 0) {
                  DoseSev[swapIndex, colName] <- Incid
                  ScoredData[studyDataIndex[DoseSevIndex], colName] <- DoseSev[, colName]
                  IncidenceOverideCount <- IncidenceOverideCount + 1
                }

              }
            }
          }
        }
      }

      #Check if there is more than just OMSPEC of Brain per Study ID
      Rmxidx <- list()
      for (Study in unique(ScoredData$StudyID)){
        idx <- which(ScoredData$StudyID %in% Study)
       Data <- ScoredData[idx,]
       Val <- unique(Data$OMSPEC)
       if (length(Val) == 1){
  
       } else {
         #Add Rows with "BRAIN" to the remove list
         idx2 <- which(ScoredData$StudyID %in% Study & ScoredData$OMSPEC == "BRAIN")
         Rmxidx <-append(Rmxidx, idx2)
        }
      }
      Rmxidx <- unlist(Rmxidx)
      ScoredData <- ScoredData[-Rmxidx,]
      CompileData <- CompileData[-Rmxidx,]

      #   #Return MI DOMAIN to Initial Terminology if needed to be changed for groupings
      if (Group == 'HEMATOPOIETIC'){
        MIDOMAIN <- 'HEMATOPOIETIC'
      } else if (Group == 'ENDOCRINE'){
        MIDOMAIN <- 'ENDOCRINE'
      } else if (Group == 'REPRODUCTIVE'){
        MIDOMAIN <- 'REPRODUCTIVE'
      }

      summaryResults[[organSystem]]$BW <- NULL
      summaryResults[[organSystem]]$OM <- NULL
      summaryResults[[organSystem]]$LB$SERUM <- NULL
      summaryResults[[organSystem]][['LB']][['WHOLE BLOOD']] <- NULL
      summaryResults[[organSystem]]$LB$URINE <- NULL
      summaryResults[[organSystem]]$MI <- NULL

      organEndPoints <- c('Organ Weights', 'Clinical Pathology', 'Histopathology', 'Body Weights')
      for (sex in Gender) {
        for (Study in unique(ScoredData$StudyID)) {
          ScoredDataTmp <- ScoredData[which(ScoredData$StudyID == Study & ScoredData$SEX == sex & ScoredData$ARMCD == Dose),]
          for (organEndPoint in organEndPoints) {
            if (organEndPoint == 'Body Weights') {
              BWtmp <- mean(ScoredDataTmp$BWzScore, na.rm = T)
              summaryResults[[organSystem]]$BW <- c(summaryResults[[organSystem]]$BW, BWtmp)
            } else if (organEndPoint == 'Organ Weights') {
              OMtmp <- mean(ScoredDataTmp$OMBrainRatio, na.rm = T)
              summaryResults[[organSystem]]$OM <- c(summaryResults[[organSystem]]$OM, OMtmp)
            } else if (organEndPoint == 'Clinical Pathology') {
              for (endpoint in c('SERUM', 'WHOLE BLOOD', 'URINE')) {
                endpointIndex <- grep(endpoint, colnames(ScoredDataTmp))
                if (length(endpointIndex) > 0) {
                  if (aggregationMethod == 'mean') {
                    maxEndpointData <- apply(ScoredDataTmp[,endpointIndex], MARGIN=1, FUN=mean, na.rm = T)
                  } else if (aggregationMethod == 'animalMax') {
                    maxEndpointData <- apply(ScoredDataTmp[,endpointIndex], MARGIN=1, FUN=max, na.rm = T)
                  } else if (aggregationMethod == 'endpointMax') {
                    maxEndpointData <- apply(ScoredDataTmp[,endpointIndex], MARGIN=2, FUN=max, na.rm = T)
                  }
                  infIndex <- which(is.infinite(maxEndpointData))
                  if (length(infIndex) > 0) {
                    maxEndpointData[infIndex] <- NA
                  }
                  meanEndpointValue <- mean(maxEndpointData, na.rm = T)
                  summaryResults[[organSystem]][['LB']][[endpoint]] <- c(summaryResults[[organSystem]][['LB']][[endpoint]],
                                                                         meanEndpointValue)
                }
              }
            } else if (organEndPoint == 'Histopathology') {
              preMIindex <- grep('|', colnames(ScoredDataTmp), fixed = T)
              if (nrow(LBData) == 0) {
                preMIindex <- which(colnames(ScoredDataTmp) == "OMSPEC") #account for loss of LB Variables
              }
              ScoredDataTmp2 <- ScoredDataTmp[which(ScoredDataTmp$OMSPEC %in% MITESTCDlist[[organSystem]]),]
              MIindex <- seq((max(preMIindex)+1), ncol(ScoredDataTmp))
              if (aggregationMethod == 'mean') {
                 if (length(MIindex) ==1){
                  maxEndpointData <- mean(ScoredDataTmp2[,MIindex], na.rm = T)  
                 } else {
                  maxEndpointData <- apply(ScoredDataTmp2[,MIindex], MARGIN=1, FUN=mean, na.rm = T)  
                 }
              } else if (aggregationMethod == 'animalMax') {
                 if (length(MIindex) ==1){
                  maxEndpointData <- max(ScoredDataTmp2[,MIindex], na.rm = T)     
                 } else {
                  maxEndpointData <- apply(ScoredDataTmp2[,MIindex], MARGIN=1, FUN=max, na.rm = T)  
                 }
              } else if (aggregationMethod == 'endpointMax') {
                 if (length(MIindex) == 1){
                    maxEndpointData <- max(ScoredDataTmp2[,MIindex], na.rm = T) 
                 } else {
                    maxEndpointData <- apply(ScoredDataTmp2[,MIindex], MARGIN=2, FUN=max, na.rm = T)  
                 }
              }
              if (identical(maxEndpointData, integer(0)) == TRUE){ #Error Catch for missing values in studies
                 maxEndpointData <- 0
              }
              meanEndpointValue <- mean(as.numeric(maxEndpointData))
              summaryResults[[organSystem]]$MI <- c(summaryResults[[organSystem]]$MI, meanEndpointValue)
            }
          }
        }
      }
      #Clear for Reset
      CompileData <- CompileDataPrime
    } 
    Results <- as.data.frame(summaryResults)
      # Generate list of names from order loaded
      StudyNames <- list()
      for (nj in 1:numstudies){
        Name <- paste0('SENDStudy',as.character(nj))
        #Pull all of the relevant DM Data
        Species <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "SPECIES")]
        TRTName <- get(Name)$ts$TSVAL[which(get(Name)$ts$TSPARMCD == "TRT")]
        StudyID <- paste0(Species, " ", TRTName, " ", Gender)
        #Store Study ID
        StudyNames <- append(StudyNames, StudyID)
      }
      #replace Results rownames with generated StudyIDs
      rownames(Results) <- StudyNames
    ## }
    summaryData <- rbind(summaryData,Results)
  }
  summaryData <- cbind(summaryData, setNames( lapply(c(organSystems, 'BW'), function(x) x= NA),c(organSystems, 'BW')  ))

  for (organSystem in c(organSystems, 'BW')) {
    Tmp <- rowMeans(summaryData[,grep(organSystem, colnames(summaryData))], na.rm = TRUE)
    # assign(organSystem, Tmp)
    summaryData[,organSystem] <- Tmp
  }
  summaryData <- t(summaryData)
  #Rename 'BW' as 'Body Weight' and add 'LB' to single LB source names to include LB
  rownames(summaryData)[which(rownames(summaryData) =="BW")] <- "BODY WEIGHT"
  rownames(summaryData)[which(rownames(summaryData) =="LIVER.SERUM")] <- "LIVER.LB.SERUM"
  rownames(summaryData)[which(rownames(summaryData) =="ENDOCRINE.SERUM")] <- "ENDOCRINE.LB.SERUM"
  rownames(summaryData)[which(rownames(summaryData) =="HEMATOPOIETIC.WHOLE.BLOOD")] <- "HEMATOPOIETIC.LB.WHOLE.BLOOD"

  print('DONE')
  showNotification("Analysis is Done", type = "message")
  
  #Re-Render Plots that Need Data Changes
  output$FWplot <- renderPlot({
    if (length(SEX) == 2){
      FWPM <-makeFWplot(FWDataSummary$M, input$FWTime, input$dose, 'M')
      FWPF <- makeFWplot(FWDataSummary$F, input$FWTime, input$dose,'F')
      print(plot(cowplot::plot_grid(FWPM,FWPF, nrow = 1,ncol = 2)))
    } else {
      FWP <- makeFWplot(FWData, input$FWTime,input$dose,SEX)
      print(FWP) 
    }
  })
  
  output$BWplot <- renderPlot({
    if (length(SEX) == 2){
      q <- makeBWplot(BodyWeightSummary$M,input$bwMethod,input$bwMetric,input$dose,'M')
      t <- makeBWplot(BodyWeightSummary$F,input$bwMethod,input$bwMetric,input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(q,t)))
    } else {
      q <- makeBWplot(BodyWeight,input$bwMethod,input$bwMetric,input$dose,SEX)
      print(q) 
    }
  })
  ## OM Plots ##
  
  output$OMplot <- renderPlot({ #Kidney Organ Weights Graph
    if (length(SEX) == 2){
      KOM <- makeOMplot(OMresults,'KIDNEY',input$omMethod,input$dose,'M')
      KOM2 <- makeOMplot(OMresults,'KIDNEY',input$omMethod,input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(KOM,KOM2)))
    } else{
      KOM <- makeOMplot(OMresults,'KIDNEY',input$omMethod,input$dose,SEX)
      print(KOM) 
    }
  })
  
  output$LOMplot <- renderPlot({ #Liver Organ Weights Graph
    if (length(SEX) == 2){
      LOM <- makeOMplot(OMresults,'LIVER',input$omMethod,input$dose,'M')
      LOM2 <- makeOMplot(OMresults,'LIVER',input$omMethod,input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(LOM,LOM2)))
    } else{
      LOM <- makeOMplot(OMresults,'LIVER',input$omMethod,input$dose,SEX)
      print(LOM) 
    }
  })
  
  output$HOMplot <- renderPlot({ #Hematopoietic Organ Weights Graph
    if (length(SEX) == 2){
      HOM <- makeOMplot(OMresults,'HEMATOPOIETIC',input$omMethod,input$dose,'M')
      HOM2 <- makeOMplot(OMresults,'HEMATOPOIETIC',input$omMethod,input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(HOM,HOM2)))
    } else{
      HOM <- makeOMplot(OMresults,'HEMATOPOIETIC',input$omMethod,input$dose,SEX)
      print(HOM) 
    }
  })
  
  output$EOMplot <-renderPlot({ #Endocrine Organ Weights Graph
    if (length(SEX) == 2){
      EOM <- makeOMplot(OMresults,'ENDOCRINE',input$omMethod,input$dose,'M')
      EOM2 <- makeOMplot(OMresults,'ENDOCRINE',input$omMethod,input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(EOM,EOM2)))
    } else{
      EOM <- makeOMplot(OMresults,'ENDOCRINE',input$omMethod,input$dose,SEX)
      print(EOM) 
    }
  })
  
  output$ROMplot <-renderPlot({ #Reproductive Organ Weights Graph
    if (length(SEX) == 2){
      ROM <- makeOMplot(OMresults,'REPRODUCTIVE',input$omMethod,input$dose,'M')
      ROM2 <- makeOMplot(OMresults,'REPRODUCTIVE',input$omMethod,input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(ROM,ROM2)))
    } else{
      ROM <- makeOMplot(OMresults,'REPRODUCTIVE',input$omMethod,input$dose,SEX)
      print(ROM) 
    }
  })
  
  ## LB Plots##
  output$KSERLBplot <- renderPlot({ #Kidney Clinical Chemistry LB Graph
    if (length(SEX) ==2){
      KERB <- makeLBplot(LBresults, 'KIDNEY','SERUM',input$dose,'M')
      KERB2 <- makeLBplot(LBresults, 'KIDNEY','SERUM',input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(KERB,KERB2)))
    } else {
      KERB <- makeLBplot(LBresults, 'KIDNEY','SERUM',input$dose,SEX)
      print(KERB) 
    }
  })
  
  output$KURILBplot <- renderPlot({ #Kidney Urinalysis LB Graph
    if (length(SEX) ==2){
      KURILB <- makeLBplot(LBresults, 'KIDNEY','URINE',input$dose,'M')
      KURILB2 <- makeLBplot(LBresults, 'KIDNEY','URINE',input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(KURILB,KURILB2)))
    } else {
      KURILB <- makeLBplot(LBresults, 'KIDNEY','URINE',input$dose,SEX)
      print(KURILB) 
    }
  })
  
  
  output$LSERLBplot <- renderPlot({ #Liver Clinical Chemistry LB Graph
    if (length(SEX) ==2){
      LSERLB <- makeLBplot(LBresults, 'LIVER','SERUM',input$dose,'M')
      LSERLB2 <- makeLBplot(LBresults, 'LIVER','SERUM',input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(LSERLB,LSERLB2)))
    } else {
      LSERLB <- makeLBplot(LBresults, 'LIVER','SERUM',input$dose,SEX)
      print(LSERLB) 
    }
  })
  
  output$HHEMELBplot <- renderPlot({ #Hematopoietic Hematology LB Graph
    if (length(SEX) ==2){
      HHEMELB <- makeLBplot(LBresults, 'HEMATOPOIETIC','WHOLE BLOOD',input$dose,'M')
      HHEMELB2 <- makeLBplot(LBresults, 'HEMATOPOIETIC','WHOLE BLOOD',input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(HHEMELB,HHEMELB2)))
    } else {
      HHEMELB <- makeLBplot(LBresults, 'HEMATOPOIETIC','WHOLE BLOOD',input$dose,SEX)
      print(HHEMELB) 
    }
  })
  
  output$ESERLBplot <- renderPlot({ #Endocrine Clinical Chemistry LB Graph
    if (length(SEX) ==2){
      ESERLB <- makeLBplot(LBresults, 'ENDOCRINE','SERUM',input$dose,'M')
      ESERLB2 <- makeLBplot(LBresults, 'ENDOCRINE','SERUM',input$dose,'F')
      print(cowplot::ggdraw(cowplot::plot_grid(ESERLB,ESERLB2)))
    } else {
      ESERLB <- makeLBplot(LBresults, 'ENDOCRINE','SERUM',input$dose,SEX)
      print(ESERLB) 
    }
  })

  ##Radar Plots ##
     for (i in 1:length(SEX)) { #Overall Summary Radar
        local({
        genders <- SEX[i]
        print('genders')
        print(genders)
        print(summaryData)
        output[[paste('SummaryRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'ALL',genders)
         return(plotData)
        },height = 700, width = 1600)
        
        output[[paste('LRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Liver',genders)
           return(plotData)
        },height = 700, width = 1600)
        output[[paste('KRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Kidney',genders)
           return(plotData)
        },height = 700, width = 1600)
        output[[paste('HRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'HEMATOPOIETIC',genders)
           return(plotData)
        },height = 700, width = 1600)
        output[[paste('ERadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Endocrine',genders)
           return(plotData)
        },height = 700, width = 1600)
        output[[paste('RRadar',i)]] <- renderPlot({ 
           plotData <- makeRadar(summaryData,'Reproductive',genders)
           return(plotData)
        },height = 700, width = 1600)
       
     }) 
   }
 

# display Studies ----  
 output$display_Studies_ALL <- shiny::renderUI({
   shiny::req(summaryData)
   # shiny::req(input$clinDosing)
   df <- displayIndic(summaryData, 'ALL')
   print(df)
   choice <- unique(df$Indicators)
   #input$selectData
   #input$selectStudy
   #shiny::isolate(Data <- getData())
   #studyList <- names(Data[['Nonclinical Information']])
   #studyList <- studyList[-which(studyList=='New Study')]
   #studyList <- stringr::str_sort(studyList, numeric = T)
   addUIDep(shiny::selectizeInput("displayStudiesALL",
                                  label = "Select and Order Organ Systems to Display:", choices = choice,
                                  selected = choice,
                                  multiple = TRUE,
                                  width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
   ))
 })
  output$SummaryBar <- renderPlotly({
    #shiny::renderPlot({
    testvar<-input$displayStudiesALL
    barData <- makeBarPlot(summaryData, 'ALL', testvar)
    barData
    ## print(barData)
  })
 
 
 output$display_Studies_Kidney <- shiny::renderUI({
   shiny::req(summaryData)
   df <- displayIndic(summaryData, 'Kidney')
   print(df)
   choice <- unique(df$Indicators)
  
   addUIDep(shiny::selectizeInput("displayStudiesKidney",
                                  label = "Select and Order Indications to Display:", choices = choice,
                                  selected = choice,
                                  multiple = TRUE,
                                  width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
   ))
 })
 output$KidneyBar <- renderPlotly({
   testvar<-input$displayStudiesKidney
   barData <- makeBarPlot(summaryData, 'Kidney', testvar)
   barData
   ## print(barData)
 }) 
 
 output$display_Studies_Liver <- shiny::renderUI({
   shiny::req(summaryData)
   df <- displayIndic(summaryData, 'Liver')
   ## print(df)
   choice <- unique(df$Indicators)
   
   addUIDep(shiny::selectizeInput("displayStudiesLiver",
                                  label = "Select and Order Indications to Display:", choices = choice,
                                  selected = choice,
                                  multiple = TRUE,
                                  width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
   ))
 })
 output$LiverBar <- renderPlotly({
   testvar<-input$displayStudiesLiver
   barData <-makeBarPlot(summaryData, 'Liver', testvar)
   barData
   ## print(barData)
 })
 
 
 output$display_Studies_Hema <- shiny::renderUI({
   shiny::req(summaryData)
   df <- displayIndic(summaryData, 'HEMATOPOIETIC')
   ## print(df)
   choice <- unique(df$Indicators)
   
   addUIDep(shiny::selectizeInput("displayStudiesHema",
                                  label = "Select and Order Indications to Display:", choices = choice,
                                  selected = choice,
                                  multiple = TRUE,
                                  width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
   ))
 })
 output$HemaBar <- renderPlotly({
   testvar<-input$displayStudiesHema
   barData <-makeBarPlot(summaryData, 'HEMATOPOIETIC', testvar)
   barData
   ## print(barData)
 })
 
 
 output$display_Studies_Endocrine <- shiny::renderUI({
   shiny::req(summaryData)
   df <- displayIndic(summaryData, 'Endocrine')
   print(df)
   choice <- unique(df$Indicators)
   
   addUIDep(shiny::selectizeInput("displayStudiesEndocrine",
                                  label = "Select and Order Indications to Display:", choices = choice,
                                  selected = choice,
                                  multiple = TRUE,
                                  width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
   ))
 })
 output$EndocrineBar <- renderPlotly({
   testvar<-input$displayStudiesEndocrine
   barData <-makeBarPlot(summaryData, 'Endocrine',testvar)
   barData
   ## print(barData)
 })
 
 
 
 output$display_Studies_Reproductive <- shiny::renderUI({
   shiny::req(summaryData)
   df <- displayIndic(summaryData, 'Reproductive')
   ## print(df)
   choice <- unique(df$Indicators)
   
   addUIDep(shiny::selectizeInput("displayStudiesReproductive",
                                  label = "Select and Order Indications to Display:", choices = choice,
                                  selected = choice,
                                  multiple = TRUE,
                                  width = "100%", options = list(plugins = list("drag_drop", "remove_button"))
   ))
 })
 output$ReproductiveBar <- renderPlotly({
   testvar<-input$displayStudiesReproductive
   barData <-makeBarPlot(summaryData, 'Reproductive',testvar)
   barData
   ## print(barData)
 })
 
 
 
 
 
 ## MI Plots ##
  output$KMIplot <- renderPlot({ #KIDNEY MI PLOT
    if (length(SEX) == 2){
      KM <- makeMIplot(MIresults,'KIDNEY','KIDNEY',input$dose,'M',
                       input$KMIClustY, input$KMIClustX)
      KM2 <- makeMIplot(MIresults,'KIDNEY','KIDNEY',input$dose,'F',
                        input$KMIClustY, input$KMIClustX)
      print(gridExtra::grid.arrange(KM,KM2))
    } else {
      KM <- makeMIplot(MIresults,'KIDNEY','KIDNEY',input$dose,SEX,
                       input$KMIClustY, input$KMIClustX)
      print(KM)
    }
  }, height = plotHeight$X)
  
  output$LMIplot <- renderPlot({ #LIVER MI PLOT
    if (length(SEX) == 2){
      LM <- makeMIplot(MIresults,'LIVER','LIVER',input$dose,'M',
                       input$LMIClustY, input$LMIClustX)
      LM2 <- makeMIplot(MIresults,'LIVER','LIVER',input$dose,'F',
                        input$LMIClustY, input$LMIClustX)
      print(gridExtra::grid.arrange(LM,LM2))
    } else {
      LM <- makeMIplot(MIresults,'LIVER','LIVER',input$dose,SEX,
                       input$LMIClustY, input$LMIClustX)
      print(LM)
    }
  }, height = plotHeight$X)
  
  output$HMIplotSpleen <- renderPlot({ #HEMATOPOIETIC SPLEEN MI PLOT
    if (length(SEX) == 2){
       if ("SPLEEN" %in% MITESTCDlist$HEMATOPOIETIC){
      HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"SPLEEN",input$dose,'M',
                       input$HMIClustY, input$HMIClustX)
      HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"SPLEEN",input$dose,'F',
                        input$HMIClustY, input$HMIClustX)
      print(gridExtra::grid.arrange(HM,HM2))
       }
    } else {
      if ("SPLEEN" %in% MITESTCDlist$HEMATOPOIETIC){
         HMS <- makeMIplot(MIresults,'HEMATOPOIETIC',"SPLEEN",input$dose,SEX,
                           input$HMIClustY, input$HMIClustX)  
         print(HMS)
      }
    }
  },height = plotHeight$X)
  
  output$HMIplotBM <- renderPlot({ #HEMATOPOIETIC BONE MARROW MI PLOT
     if (length(SEX) == 2){
        if ("BONE MARROW" %in% MITESTCDlist$HEMATOPOIETIC){
        HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"BONE MARROW",input$dose,'M',
                         input$HMIClustY, input$HMIClustX)
        HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"BONE MARROW",input$dose,'F',
                          input$HMIClustY, input$HMIClustX)
        print(gridExtra::grid.arrange(HM,HM2))
        }
     } else {
        if ("BONE MARROW" %in% MITESTCDlist$HEMATOPOIETIC){
           HMB <- makeMIplot(MIresults,'HEMATOPOIETIC',"BONE MARROW",input$dose,SEX,
                             input$HMIClustY, input$HMIClustX) 
           print(HMB)
        }
     }
  },height = plotHeight$X)
  
  output$HMIplotThymus <- renderPlot({ #HEMATOPOIETIC THYMUS MI PLOT
     if (length(SEX) == 2){
        if ("THYMUS" %in% MITESTCDlist$HEMATOPOIETIC){
           HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"THYMUS",input$dose,'M',
                            input$HMIClustY, input$HMIClustX)
           HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"THYMUS",input$dose,'F',
                             input$HMIClustY, input$HMIClustX)
           print(gridExtra::grid.arrange(HM,HM2))
        }
     } else {
        if ("THYMUS" %in%  MITESTCDlist$HEMATOPOIETIC){
           HMT <- makeMIplot(MIresults,'HEMATOPOIETIC',"THYMUS",input$dose,SEX,
                             input$HMIClustY, input$HMIClustX)
           print(HMT)
        }
     }
  },height = plotHeight$X)
  
  output$HMIplotLympMand <- renderPlot({ #HEMATOPOIETIC LYMPH NODE, MANDIBULAR MI PLOT
    if (length(SEX) == 2){
      if ("LYMPH NODE, MANDIBULAR" %in% MITESTCDlist$HEMATOPOIETIC){
        HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MANDIBULAR",input$dose,'M',
                         input$HMIClustY, input$HMIClustX)
        HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MANDIBULAR",input$dose,'F',
                          input$HMIClustY, input$HMIClustX)
        print(gridExtra::grid.arrange(HM,HM2))
      }
    } else {
      if ("LYMPH NODE, MANDIBULAR" %in%  MITESTCDlist$HEMATOPOIETIC){
        HMT <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MANDIBULAR",input$dose,SEX,
                          input$HMIClustY, input$HMIClustX)
        print(HMT)
      }
    }
  },height = plotHeight$X)
  output$HMIplotLympMESEN <- renderPlot({ #HEMATOPOIETIC LYMPH NODE, MESENTERIC MI PLOT
    if (length(SEX) == 2){
      if ("LYMPH NODE, MESENTERIC" %in% MITESTCDlist$HEMATOPOIETIC){
        HM <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MESENTERIC",input$dose,'M',
                         input$HMIClustY, input$HMIClustX)
        HM2 <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MESENTERIC",input$dose,'F',
                          input$HMIClustY, input$HMIClustX)
        print(gridExtra::grid.arrange(HM,HM2))
      }
    } else {
      if ("LYMPH NODE, MESENTERIC" %in%  MITESTCDlist$HEMATOPOIETIC){
        HMT <- makeMIplot(MIresults,'HEMATOPOIETIC',"LYMPH NODE, MESENTERIC",input$dose,SEX,
                          input$HMIClustY, input$HMIClustX)
        print(HMT)
      }
    }
  },height = plotHeight$X)
  
  output$EMIplot <- renderPlot({ #ENDOCRINE GLAND, ADRENAL MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, ADRENAL" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, ADRENAL",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, ADRENAL",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(gridExtra::grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, ADRENAL" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, ADRENAL",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot2 <- renderPlot({ #ENDOCRINE GLAND, PITUITARY MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, PITUITARY" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PITUITARY",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PITUITARY",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(gridExtra::grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, PITUITARY" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PITUITARY",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot3 <- renderPlot({ #ENDOCRINE  GLAND, PARATHYROID MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, PARATHYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PARATHYROID",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PARATHYROID",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(gridExtra::grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, PARATHYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, PARATHYROID",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot4 <- renderPlot({ #ENDOCRINE  GLAND, THYROID MI PLOT
     if (length(SEX) == 2){
        if ("GLAND, THYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, THYROID",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, THYROID",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(gridExtra::grid.arrange(EM,EM2))
        }
     } else {
        if ("GLAND, THYROID" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "GLAND, THYROID",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
  
  output$EMIplot5 <- renderPlot({ #ENDOCRINE  PANCREAS MI PLOT
     if (length(SEX) == 2){
        if ("PANCREAS" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "PANCREAS",input$dose,'M',
                            input$EMIClustY, input$EMIClustX)
           EM2 <- makeMIplot(MIresults,'ENDOCRINE', "PANCREAS",input$dose,'F',
                             input$EMIClustY, input$EMIClustX)
           print(gridExtra::grid.arrange(EM,EM2))
        }
     } else {
        if ("PANCREAS" %in%  MITESTCDlist$ENDOCRINE){
           EM <- makeMIplot(MIresults,'ENDOCRINE', "PANCREAS",input$dose,SEX,
                            input$EMIClustY, input$EMIClustX)
           print(EM)
        }
     }
  },height = plotHeight$X)
 
  #MI Plots for Reproductive set by SEX they occur in
  output$RMIplot <- renderPlot({ #REPRODUCTIVE GLAND, PROSTATE MI PLOT
      if ("GLAND, PROSTATE" %in% MITESTCDlist$REPRODUCTIVE){
         RM <- makeMIplot(MIresults,'REPRODUCTIVE',"GLAND, PROSTATE",input$dose,'M',
                          input$RMIClustY, input$RMIClustX)
         print(RM)   
      }
  },height = 350)
  
  output$RMIplot2 <- renderPlot({ #REPRODUCTIVE EPIDIDYMIS MI PLOT
     if ("EPIDIDYMIS" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"EPIDIDYMIS",input$dose,'M',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot3 <- renderPlot({ #REPRODUCTIVE TESTIS MI PLOT
     if ("TESTIS" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"TESTIS",input$dose,'M',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot4 <- renderPlot({ #REPRODUCTIVE UTERUS MI PLOT
     if ("UTERUS" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"UTERUS",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot5 <- renderPlot({ #REPRODUCTIVE CERVIX MI PLOT
     if ("CERVIX" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"CERVIX",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot6 <- renderPlot({ #REPRODUCTIVE GLAND, MAMMARY MI PLOT
     if ("GLAND, MAMMARY" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"GLAND, MAMMARY",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot7 <- renderPlot({ #REPRODUCTIVE VAGINA MI PLOT
     if ("VAGINA" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"VAGINA",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$RMIplot8 <- renderPlot({ #REPRODUCTIVE OVARY MI PLOT
     if ("OVARY" %in% MITESTCDlist$REPRODUCTIVE){
        RM <- makeMIplot(MIresults,'REPRODUCTIVE',"OVARY",input$dose,'F',
                         input$RMIClustY, input$RMIClustX)
        print(RM)   
     }
  },height = 350)
  
  output$tree <- shinyTree::renderTree(TreeSelect)
  
  }})
  
  
  #Dynamic Sizing of MI and Radar Plots
  
  ###new addition - barplot###
   output$ReactSummaryBar<- renderUI({
     #lapply(paste('SummaryBar',1:numSEX$X),plotOutput)
     #shiny::plotOutput("Barplot")
     plotlyOutput('SummaryBar')
    })
  ###test - selection ###
   output$ReactdisplayStudiesALL <- renderUI({
     uiOutput('display_Studies_ALL')
    })
   
   
   output$ReactKidneyBar <- renderUI({
     plotlyOutput('KidneyBar')
   })
   output$ReactdisplayStudiesKidney <- renderUI({
     uiOutput('display_Studies_Kidney')
   })
   
   
   
   output$ReactLiverBar <- renderUI({
     plotlyOutput('LiverBar')
   })
   output$ReactdisplayStudiesLiver <- renderUI({
     uiOutput('display_Studies_Liver')
   })
   
   
   output$ReactHemaBar <- renderUI({
     plotlyOutput('HemaBar')
   })
   output$ReactdisplayStudiesHema <- renderUI({
     uiOutput('display_Studies_Hema')
   })
   
   
   
   output$ReactEndocrineBar <- renderUI({
     plotlyOutput('EndocrineBar')
   })
   output$ReactdisplayStudiesEndocrine <- renderUI({
     uiOutput('display_Studies_Endocrine')
   })
   
   
   output$ReactReproductiveBar <- renderUI({
     plotlyOutput('ReproductiveBar')
   })
   output$ReactdisplayStudiesReproductive <- renderUI({
     uiOutput('display_Studies_Reproductive')
   })
   
   
   
  output$ReactSummaryRadar <- renderUI({ #Generate one plot per Gender
    lapply(paste('SummaryRadar',1:numSEX$X),plotOutput)
  })
  output$ReactLiverRadar <- renderUI({
     lapply(paste('LRadar',1:numSEX$X),plotOutput)
  })
  output$ReactKidneyRadar <- renderUI({
     lapply(paste('KRadar',1:numSEX$X),plotOutput)
  })
  
  output$ReactHemaRadar <- renderUI({
     lapply(paste('HRadar',1:numSEX$X),plotOutput)
  })
  output$ReactEndoRadar <- renderUI({
     lapply(paste('ERadar',1:numSEX$X),plotOutput)
  })
  output$ReactReproRadar <- renderUI({
     lapply(paste('RRadar',1:numSEX$X),plotOutput)
  })
  
  output$KMIplotreactive <- renderUI({
    plotOutput('KMIplot',height = plotHeight$X)
  })
  
  output$LMIplotreactive <- renderUI({
    plotOutput('LMIplot', height = plotHeight$X)
  })
  
  output$HMIplotreactive <- renderUI({
    plotOutput('HMIplotSpleen', height = plotHeight$X)
  })
  output$HMIplotreactive2 <- renderUI({
     plotOutput('HMIplotBM', height = plotHeight$X)
  })
  output$HMIplotreactive3 <- renderUI({
     plotOutput('HMIplotThymus', height = plotHeight$X)
  })
  output$HMIplotreactive4<- renderUI({
    plotOutput('HMIplotLympMand', height = plotHeight$X)
  })
  output$HMIplotreactive5<- renderUI({
    plotOutput('HMIplotLympMESEN', height = plotHeight$X)
  })
  
  output$EMIplotreactive <- renderUI({
    plotOutput('EMIplot', height = plotHeight$X)
  })
  
  output$EMIplotreactive2 <- renderUI({
     plotOutput('EMIplot2', height = plotHeight$X)
  })
  
  output$EMIplotreactive3 <- renderUI({
     plotOutput('EMIplot3', height = plotHeight$X)
  })
  
  output$EMIplotreactive4 <- renderUI({
     plotOutput('EMIplot4', height = plotHeight$X)
  })
  
  output$EMIplotreactive5 <- renderUI({
     plotOutput('EMIplot5', height = plotHeight$X)
  })
  
  output$RMIplotreactive <- renderUI({
    plotOutput('RMIplot', height = plotHeight$X)
  })
  
  output$RMIplotreactive2 <- renderUI({
     plotOutput('RMIplot2', height = plotHeight$X)
  })
  
  output$RMIplotreactive3 <- renderUI({
     plotOutput('RMIplot3', height = plotHeight$X)
  })
  
  output$RMIplotreactive4 <- renderUI({
     plotOutput('RMIplot4', height = plotHeight$X)
  })
  
  output$RMIplotreactive5 <- renderUI({
     plotOutput('RMIplot5', height = plotHeight$X)
  })
  
  output$RMIplotreactive6 <- renderUI({
     plotOutput('RMIplot6', height = plotHeight$X)
  })
  
  output$RMIplotreactive7 <- renderUI({
     plotOutput('RMIplot7', height = plotHeight$X)
  })
  
  output$RMIplotreactive8 <- renderUI({
     plotOutput('RMIplot8', height = plotHeight$X)
  })
  
  output$tree <- shinyTree::renderTree ({
     organSystemList <- list( 
        'Kidney' = list(
           'Laboratory Values(LB)' = list("Clinical Chemistry" = structure(list('SERUM | CREAT'= 'SERUM | CREAT',
                                                                                'SERUM | UREAN' = 'SERUM | UREAN',
                                                                                'SERUM | ALB' = 'SERUM | ALB',
                                                                                'SERUM | CL' = 'SERUM | CL',
                                                                                'SERUM | K' = 'SERUM | K',
                                                                                'SERUM | PHOS' = 'SERUM | PHOS',
                                                                                'SERUM | SODIUM' = 'SERUM | SODIUM'), stselected = TRUE),
                                          'Urinanlysis' = structure(list('URINE | K'='URINE | K',
                                                                         'URINE | SODIUM' = 'URINE | SODIUM',
                                                                         'URINE | GLUC' = 'URINE | GLUC',
                                                                         'URINE | SPGRAV' = 'URINE | SPGRAV',
                                                                         'URINE | VOLUME' = 'URINE | VOLUME',
                                                                         'URINE | PROT' = 'URINE | PROT'),stselected = TRUE)),
           'Histopathology(MI)' = structure(list('KIDNEY' = structure('KIDNEY',stselected = TRUE),
                                                 'URETER' = structure('URETER'),
                                                 'URINARY BLADDER' = structure('URINARY BLADDER'),
                                                 'URETHRA' = structure('URETHRA'))),
           'Organ Weight(OM)' = structure(list('KIDNEY' = structure('KIDNEY',stselected = TRUE)))),
        'Liver' = list(
           'Laboratory Values(LB)' = list("Clinical Chemistry" = structure(list('SERUM | ALT'= 'SERUM | ALT',
                                                                                'SERUM | AST' = 'SERUM | AST',
                                                                                'SERUM | ALP' = 'SERUM | ALP',
                                                                                'SERUM | GGT' = 'SERUM | GGT',
                                                                                'SERUM | BILI' = 'SERUM | BILI',
                                                                                'SERUM | ALB' = 'SERUM | ALB'), stselected = TRUE)),
           'Histopathology(MI)' = structure(list('LIVER' = structure('LIVER',stselected = TRUE),
                                                 'GALLBLADDER' = structure('GALLBLADDER'))),
           'Organ Weight(OM)' = structure(list('LIVER' = structure('LIVER',stselected = TRUE),
                                               'GALLBLADDER' = structure('GALLBLADDER')))
        ),
        'Hematopoietic' = list(
           'Laboratory Values(LB)' = list("Hematology" = structure(list('WHOLE BLOOD | RBC'= 'WHOLE BLOOD | RBC',
                                                                        'WHOLE BLOOD | HCT' = 'WHOLE BLOOD | HCT',
                                                                        'WHOLE BLOOD | MCHC' = 'WHOLE BLOOD | MCHC',
                                                                        'WHOLE BLOOD | MCH' = 'WHOLE BLOOD | MCH',
                                                                        'WHOLE BLOOD | MCV' = 'WHOLE BLOOD | MCV',
                                                                        'WHOLE BLOOD | RDW' = 'WHOLE BLOOD | RDW',
                                                                        'WHOLE BLOOD | WBC' = 'WHOLE BLOOD | WBC',
                                                                        'WHOLE BLOOD | MONO' = 'WHOLE BLOOD | MONO',
                                                                        'WHOLE BLOOD | BASO' = 'WHOLE BLOOD | BASO',
                                                                        'WHOLE BLOOD | EOS' = 'WHOLE BLOOD | EOS',
                                                                        'WHOLE BLOOD | LYM' = 'WHOLE BLOOD | LYM',
                                                                        'WHOLE BLOOD | PLAT' = 'WHOLE BLOOD | PLAT',
                                                                        'WHOLE BLOOD | MPV' = 'WHOLE BLOOD | MPV'), stselected = TRUE)),
           'Histopathology(MI)' = structure(list('BONE MARROW' = "BONE MARROW",
                                                 'SPLEEN' = 'SPLEEN',
                                                 'THYMUS' = 'THYMUS',
                                                 "LYMPH NODE, MANDIBULAR" = "LYMPH NODE, MANDIBULAR",
                                                 "LYMPH NODE, MESENTERIC" = "LYMPH NODE, MESENTERIC"), stselected = TRUE),
           'Organ Weight(OM)' = structure(list('SPLEEN' = 'SPLEEN',
                                               'THYMUS' = 'THYMUS'), stselected = TRUE)
        ),
        'Endocrine' = list(
           # 'Laboratory Values(LB)' = list("Clinical Chemistry" = structure(list('SERUM | ALB'= 'SERUM | ALB',
           #                                                                      'SERUM | CL' = 'SERUM | CL',
           #                                                                      'SERUM | PHOS' = 'SERUM | PHOS',
           #                                                                      'SERUM | SODIUM' = 'SERUM | SODIUM',
           #                                                                      'SERUM | GLUC' = 'SERUM | GLUC',
           #                                                                      'SERUM | CA' = 'SERUM | CA'), stselected = TRUE)),
           'Histopathology(MI)' = structure(list('GLAND, THYROID' = 'GLAND, THYROID',
                                                 'GLAND, ADRENAL' = 'GLAND, ADRENAL',
                                                 'GLAND, PITUITARY'='GLAND, PITUITARY',
                                                 'GLAND, PARATHYROID'='GLAND, PARATHYROID',
                                                 'PANCREAS'='PANCREAS'), stselected = TRUE),
           'Organ Weight(OM)' = structure(list('GLAND, THYROID' = 'GLAND, THYROID',
                                               'GLAND, ADRENAL' = 'GLAND, ADRENAL',
                                               'GLAND, PITUITARY'='GLAND, PITUITARY',
                                               'GLAND, PARATHYROID'='GLAND, PARATHYROID',
                                               'PANCREAS'='PANCREAS'), stselected = TRUE)
        ),
        'Reproductive' = list(
           #Remove LB Values
           # 'Laboratory Values(LB)' = list('Hematology' = structure(list('WHOLE BLOOD | RBC'='WHOLE BLOOD | RBC',
           #                                                              'WHOLE BLOOD | HGB' = 'WHOLE BLOOD | HGB'),stselected = TRUE)),
           'Histopathology(MI)' = structure(list('GLAND, PROSTATE' = structure('GLAND, PROSTATE',stselected = TRUE),
                                                 'EPIDIDYMIS' = structure('EPIDIDYMIS',stselected = TRUE),
                                                 'TESTIS' = structure('TESTIS',stselected = TRUE),
                                                 'CERVIX' = structure('CERVIX'),
                                                 'GLAND, MAMMARY' = structure('GLAND, MAMMARY'),
                                                 'OVARY' = structure("OVARY"),
                                                 'UTERUS' = structure('UTERUS'),
                                                 'VAGINA' = structure('VAGINA'))),
           'Organ Weight(OM)' = structure(list('GLAND, PROSTATE' = structure('GLAND, PROSTATE',stselected = TRUE),
                                               'TESTIS' = structure('TESTIS',stselected = TRUE),
                                               'OVARY' = structure("OVARY")))
        )
     )
  })
  outputOptions(output,"tree", suspendWhenHidden = FALSE)
  ## shinyjs::runcodeServer()

}
shinyApp(ui = ui, server = server)

}
