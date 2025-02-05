#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Feb-03-2025    Md Yousuf Ali (MdYousuf.Ali@fda.hhs.gov)

#' @title Run sendSummarizer shiny app
#' @param db_path Optional, character\cr
#'    file path for database
#'
#' @return function run the app.
#'
#' @export
#' @examples
#' \dontrun{
#' sendSummarizer_app()
#' }


sendSummarizer_app <- function(db_path){
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
   query <- 'SELECT *  FROM ID'
    all_ids <- DBI::dbGetQuery(conn = conn, query)
    all_ids <- data.table::setDT(all_ids)
    dbStudyIDS <- unique(all_ids$APPID)
    ## print(all_ids)

    ## data.table::setDT(dt)
    ## APPID <- DBI::dbGetQuery(conn=conn, query="")

www_path <- system.file("", package = "sendSummarizer")

ui <- fluidPage(
  titlePanel("Cross-Study Analysis"),
  shiny::checkboxInput('show_side',label = 'hide/show sidebar'),
  ## shiny::div(class='side_bar',

  sidebarLayout(
    ## column(width = 3,
    div(id='side_bar',
        sidebarPanel(width = 2,

                     shiny::selectizeInput(inputId = 'ind',
                                           label= 'Select IND',
                                           choices= NULL,
                                           multiple= TRUE),

                     selectizeInput(inputId = "DatabaseStudies",
                                 label = "Choose Database StudyIDs",
                                 choices = NULL,
                                 multiple = TRUE
                                 ## selected = two_works
                                 ),
                     radioButtons("sex", "Sex Selection:",
                                  c("Male" = "M",
                                    "Female" = "F",
                                    "Male/Female" = "M/F"), selected = 'M'),

                     radioButtons(inputId = "dose", label = "Dose Selection:",
                                  c("HD" = "HD",
                                    "MD" = "MD",
                                    "LD" = "LD"), selected = 'HD'),
                     shiny::br(),
                     h5('Detailed Test Selection:'),
                     shinyTree::shinyTree("tree",checkbox = TRUE),
                     shiny::br(),
                     ## pickerInput(inputId = "AGGMethod",
                     ##             label = "Radar Aggregation Method",
                     ##             c('mean', 'animalMax','endpointMax'),
                     ##             selected = 'animalMax'),

                     selectizeInput(inputId = "AGGMethod",
                                    label = "Radar Aggregation Method",
                                choices= c('mean', 'animalMax','endpointMax'),
                                 selected = 'animalMax'),
                     selectizeInput(inputId = 'bwMethod',
                                 label = "BW Method",
                                 c("BW","TERMBW"), selected = 'BW'),
                     selectizeInput(inputId = 'omMethod',
                                 label = "OM Ratio Method",
                                 c("BrainRatio", "BWRatio", "Organ"), selected = 'BrainRatio'),
                     shiny::br(),
                     shiny::actionButton('PLOT',label = "Generate Visuals")

    )),

    mainPanel(
      htmltools::includeScript(paste0(www_path, "/www/hide.js")),
      plotOutput("distPlot")
    ))
  ## )
)

server <- function(input, output,session) {
# hide sidebar panel
        shiny::observeEvent(input$show_side, {
        session$sendCustomMessage(type = "toggle_side", input$show_side)
      })


  shiny::observe({
    shiny::updateSelectizeInput(session = session, inputId = 'ind',
                                choices = dbStudyIDS,
                                server = TRUE)


  })


  get_studyid <- shiny::eventReactive(input$ind,{
    shiny::req(input$ind)
    ind <- input$ind
    get_title <- get_studyid_title(conn=conn, ind,all_ids)
    get_title



  })

  shiny::observeEvent(input$ind,{
    shiny::updateSelectizeInput(session = session,
                                inputId = 'DatabaseStudies',
                                choices = get_studyid(),
                                selected = NULL
                                )

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


}
shiny::shinyApp(ui = ui, server = server)


}
