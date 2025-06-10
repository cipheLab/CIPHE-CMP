library(shinydashboard)
library(shiny)
library(shinyFiles)
library(rhandsontable)
library(DT)
library(shinybusy)
library(shinyWidgets)
library(bslib)
library(DiagrammeR)
library(shinyjs)

ui <- dashboardPage( 

  skin = "blue",
  dashboardHeader(title = 'CIPHE CMP '),
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem("Data Management", tabName = "dataManagmnt"),
      menuItem("Preprocessing", tabName = "preprocess"),
      menuItem("Debarcoding", tabName = "debarcoding"),      
      menuItem("Annotation", tabName = "annotation"),
      menuItem("Infinity Flow", tabName = "infinityFlow")
 
    ),
    div(
      style = "padding: 10px; text-align: center;",
      actionButton("unTransformData", "Detransform data",
                   style = "background-color:#F0FFF0; font-weight: bold;
                        border-width: 2px; border-color: #F0FFF0; width: 90%;"),
      actionButton("decompData", "Decompensate data",
                   style = "background-color:#F0FFF0; font-weight: bold;
                        border-width: 2px; border-color: #F0FFF0; width: 90%;"),
      actionButton("openExportModal", "Export FCS",
                   style = "background-color: #BCEE68; font-weight: bold; 
                        border-width: 2px; border-color: #BCEE68; width: 90%;"))
    
  ),
  dashboardBody(
    conditionalPanel(
      condition = "input.tab == 'dataManagmnt'",
                fluidRow(
                  column(12, align = "center",
                         uiOutput("plateIndicators")
                  )
                )
                ,
                box(width = 12,HTML("<h4> <span style='font-weight: bold;'>DATA MANAGEMENT </span></h4>" )
                    
                ),
                box(width = 12,
                    title = div(style = "display: flex; align-items: center; justify-content: space-between;",
                                span("UPLOADING", style = "font-size: 18px; font-weight: bold;"),
                                actionButton("HELP_UPLOADING", "", icon("question-circle"), class = "btn btn-xs btn-info")
                    ),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    # Experiment name input
                    column(6, 
                           textInput("experimentName", "Experiment Name:", 
                                     placeholder = "Leave blank for an automatic name"),
                           
                           radioButtons(
                             
                             
                             inputId = "experienceType",
                             label = HTML("<h4><span style='background-color: #fdfd96;'>Select :</span></h4>" ),
                             choices = c("Pop","Bc + Pop","Bc"),
                             selected="Pop"
                           ),
                           HTML("<h4 style='background-color: #fdfd96; padding: 5px; border-radius: 5px; display: inline-block;'> Upload Files </h4>"),
                           
                           selectInput(
                             inputId = "selector",
                             label = "How many plates?",
                             choices = 1:10,
                             width = '30%'
                           ),
                           
                           uiOutput("info"),
                           uiOutput("folder_inputs"),
                           tags$br(),
                           actionButton("loadData", 'Load data',  
                                        style = "background-color: #BCEE68; color: black; 
                                            font-weight: bold; padding: 10px 15px; 
                                            border-radius: 5px; border-width: 2px; border-color: #BCEE68;"),
                           tags$br(), tags$br(),
                           
                    ),
                    
                    # Upload RData file
                    column(6, 
                           tags$br(),
                           shinyFilesButton(
                             "backup",
                             "Upload backup analysis ...",
                             "Choisir un projet en cours d'analyse",
                             multiple = FALSE,
                             filetype=list(rds = c('rds','RDS','Rds'))
                           ),
                           
                           grVizOutput("organigramme", width = "100%", height = "500px")
                           
                    ),
                    
                    
                    
                    
                    
                    uiOutput("unMatchMarkerOutput")
                )),
                
          
          ################################################################################
          ##################### PREPROCESSING  ###########################################
          conditionalPanel(
            condition = "input.tab == 'preprocess'",
            
            uiOutput("fcsTransformed"),
            
            fluidPage(
              
              box(width = 12,HTML("<h4> <span style='font-weight: bold;'>PREPROCESSING </span></h4>" ),
                  
                  
              ),
              box(width = 3,HTML("<h4>  <span style='background-color: #fdfd96;'> 1 - Normalization </span> </h4>" )),
              tags$br(),
              uiOutput("RDataFile1_ui"),
              
              ####### STEP 1 : PREPROCESSING
              
              fluidRow(
                
                column(
                  width = 12,
                  
                  box(width = 12,
                      title = p("Separate beads from cells ",actionButton("HELP_SEPARATE_BEADS", "", icon("question-circle"), class = "btn-xs")),
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      
                      tags$br(),
                      fluidRow(
                     
                        column(2,selectInput("marker_x2", "X-axis Marker", choices = c(""), selected = NULL, width = 200)),
                        column(2, selectInput("marker_y2", "Y-axis Marker", choices = c(""), selected = NULL, width = 200)),
                        
                      ),
                      fluidRow(
                        column(6,
                               
                               plotOutput("plot_clusters", width='500px', height='500px',brush = brushOpts(id = "brush_area", resetOnNew = TRUE))),
                        column(6,
                               plotOutput("plot_histo_clusters", height = "500px", width = "500px")
                        ),
                        column(5,actionButton(inputId = "beadAttribution", label = "Valide gate (beads)", style = " background-color: #BCEE68;border-width: 2px; border-color: #BCEE68;"))
                      )
                      
                  ),
                  
                  box(
                    width = 12,
                    
                    title =p( "Beads Control Quality",actionButton("HELP_BEAD_QC", "", icon("question-circle"), class = "btn-xs")), 
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    fluidRow(
                      box(width=12,
                          HTML("<h4><strong>    STEP 1 : Remove doublets </strong></h4>"),
                          column(5,
                                 selectizeInput("groupQC", "Visualization group :", choices = c(""), multiple = FALSE, width="60%"),
                                 plotOutput("plot_QC", height = "400px", width = "400px")),
                          column(
                            2,
                            selectInput(
                              "marker_x3", 
                              "X-axis Marker", 
                              choices = c(""), 
                              selected = NULL, 
                              width = "100%"
                            ),
                            selectInput(
                              "marker_y3", 
                              "Y-axis Marker", 
                              choices = c(""), 
                              selected = NULL, 
                              width = "100%"
                            ),
                            tags$br(),
                            
                            
                            sliderInput("X_scale", "X scale",
                                        min = 1, max = 1000000,
                                        value = c(1,500000), step=100),
                            
                            sliderInput("Y_scale", "Y scale",
                                        min = 1, max = 1000000,
                                        value = c(1,500000), step=100),
                            
                            actionButton(
                              inputId = "removeDoublets", 
                              label = "Remove doublets (All plates)", 
                              style = "background-color: #BCEE68; border-width: 2px; border-color: #BCEE68;", 
                              width = "85%"
                            ),
                            tags$br(),
                            tags$br(),
                          ),
                      )
                    ),
                    
                    box(width=12,
                        HTML("<h4><strong>  STEP 2 : Select mode </strong></h4>"),
                        tags$br(),
                        column(3, 
                               actionButton("transformVizu", "Transform data (All plates)", style = "background-color: #BCEE68; border-width: 2px; border-color: #BCEE68;"),
                               tags$br(),
                               tags$br(),
                               selectInput(
                                 "marker_x4", 
                                 "X-axis Marker", 
                                 choices = c(""), 
                                 selected = NULL, 
                                 width = "100%"
                               ),
                               numericInput("nbPeaks", "nb of Peaks", min=1, max=20, value=2),
                               actionButton("mclustMode", "Launch mclust (All plates)", style = "background-color: #BCEE68; border-width: 2px; border-color: #BCEE68;"),
                               tags$br(),
                               uiOutput("peakChoice")),
                        
                        
                        column(4,  plotOutput("plot_histogram_peak2", height = "400px", width = "400px")),
                        column(4,  plotOutput("plot_histogram_peak3", height = "400px", width = "400px")),
                        
                    ),
                    
                    box(width=12,
                        HTML("<h4><strong> STEP 3 : Re-gate  </strong></h4>"),
                        column(1), selectizeInput("groupTransfo", "Select group", choices = c(""), multiple = FALSE, width="60%")),
                    tags$br(), 
                    
                    fluidRow(
                      
                      column(
                        3,
                        
                        selectInput(
                          "marker_x5", 
                          "X-axis Marker", 
                          choices = c(""), 
                          selected = NULL, 
                          width = "100%"
                        ),
                        
                        actionButton("reset_gate", "Reset Gate")
                        
                      ),
                      column(
                        3,
                        
                        selectInput(
                          "marker_y4", 
                          "Y-axis Marker", 
                          choices = c(""), 
                          selected = NULL, 
                          width = "100%"
                        ),
                        
                        
                        tags$br(),
                        
                        
                        
                      ),
                      
                    ),
                    
                    
                    
                    
                    tags$br(),
                    fluidRow(
                      
                      column(
                        4,
                        plotOutput("plot_peak", 
                                   brush = brushOpts(
                                     id = "plot_brush",  
                                     resetOnNew = TRUE      
                                   ),height = "400px", width = "400px"),
                  
                      ),
                      column(
                        4,
                        plotOutput("plot_histogram_peak", height = "400px", width = "400px"),
                      
                      )
                      
                      
                      
                      
                      
                      
                      
                    ),
                    box(width=12,
                        HTML("<h4><strong> STEP 4: validate Bead Gating  </strong></h4>"),
                        tags$br(),
                        column(4,actionButton("UnTransformVizu", "unTransform data (All plates)", style = "background-color: #BCEE68; border-width: 2px; border-color: #BCEE68;"),
                               tags$br(),tags$br(),actionButton("valideBeads", "Validate beads (All plates)", style = "background-color: #BCEE68; border-width: 2px; border-color: #BCEE68;")))
                    
                    
                    
                    
                  )
                  
                ),
                
                box(
                  width = 12,
                  title = p("Normalization", actionButton("HELP_NORMALIZE", "", icon("question-circle"), class = "btn-xs")),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  fluidRow(
                    column(
                      10,
                      uiOutput("matchMarkerOutput"),
                      tags$br(),
                      actionButton(
                        inputId = "Normalize",
                        label = "Normalize",
                        style = "background-color:  #BCEE68;border-width: 2px; border-color:  #BCEE68;",
                        width = '30%'
                      ),
                      tags$br()
                    )
                  ),
                  
                  fluidRow(
                    tags$br(),
                    column(3,
                           tags$br(),
                           uiOutput("groupVizTable"),
                           uiOutput("harmTableVizu")),
                    column(2,
                           tags$br(),
                           selectInput("cellsOrcellsAndBeads", "Choose", choices = c(""), selected = NULL, width = 200),
                           selectInput("marker_x6", "X-axis Marker", choices = c(""), selected = NULL, width = 200),
                           selectInput("groupVerif", "Group", choices = c(""), selected = NULL, multiple = TRUE, width = 200)),
                    column(4,
                           plotOutput("vizAfterNorm", height = "500px", width = "500px"))
                  ),
                  
                  fluidRow(
                    column(4,
                           textInput("zipFileName", "Enter ZIP file name (without extension):", value = "NormalizedFCS")),
                    column(4,
                           
                           tags$br(),
                           downloadButton("downloadHarmTable", "Download harmonized Table"))
                  )
                ),
                
                
                
                fluidRow(
                  box(width = 3, HTML("<h4><span style='background-color: #fdfd96;'> 2 - FlowQC</span></h4>")),
                  
                  box(
                    width = 12,
                    title = p("FlowQC", actionButton("HELP_FLOWQC", "", icon("question-circle"), class = "btn-xs")),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    fluidRow(
                      column(3,
                             plotOutput("plotQC", width = "400px", height = "400px"),
                             sliderInput("X_scale2", "X scale", min = 1, max = 1000000, value = c(1, 500000), step = 100),
                             sliderInput("Y_scale2", "Y scale", min = 1, max = 1000000, value = c(1, 500000), step = 100)
                      ),
                      
                      
                      
                      
                      
                      
                      column(4,
                             tabsetPanel(
                               tabPanel("FlowQC",
                                        class = "tabset-panel",
                                        tags$br(), tags$br(),
                                        HTML("
                                   <strong>Preprocessing includes also:</strong>
                                   <ul style='padding-left: 20px;'>
                                     <li>Remove margin events</li>
                                     <li>Remove doublets</li>
                                   </ul>
                                   "),
                                        tags$br(), tags$br(),
                                        checkboxInput("enableFlowCut", "Include flowCut step", value = TRUE),
                                        
                                        conditionalPanel(
                                          condition = "input.enableFlowCut == true",
                                          uiOutput("selectMaxPercCut"),
                                          uiOutput("selectFlowCutMarkers"),
                                          tags$br()
                                        ),
                                        
                                        actionButton("runflowCut", "Run Cleaning", icon = icon("cogs"),
                                                     style = "background-color: #BCEE68; border-width: 2px; border-color: #BCEE68;")
                               )
                             )
                      )
                    ),
                    
                    fluidRow(
                      column(2,
                             selectInput("marker_x10", "X-axis Marker", choices = c(""), selected = NULL, width = "80%")
                      ),
                      column(2,
                             selectInput("marker_y10", "Y-axis Marker", choices = c(""), selected = NULL, width = "80%")
                      )
                    ),
                    
                    fluidRow(
                      tags$br(),
                      column(12,
                             HTML("<h4><span style='font-weight: bold;'>Results QC :</span></h4>"),
                             uiOutput("flowQCResult_ui")
                      )
                    )
                  )
                )
                ,
                
                fluidRow(box(width = 3,HTML("<h4>  <span style='background-color: #fdfd96;'> 3 - Concatenate plates </span> </h4>" ))),
                box(width=12,
                    title=p("Concatenate plates",actionButton("HELP_CONCATENATE", "", icon("question-circle"), class = "btn-xs") ),
                    status='primary',
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    column(
                      4,
                      selectInput("plateForConcatenation", "Choose plates to concatenate", choices = c(""), selected = NULL, width = "80%", multiple=TRUE)
                    ),
                    column(2, actionButton(inputId = "RunPlateConcatenation", label = "Run concatenation", style = "background-color: #BCEE68; border-width: 2px; border-color:  #BCEE68; margin-top: 10px;"),
                           
                    )
                    
                    
                    
                    
                    
                ),
                fluidRow(box(width = 3,HTML("<h4>  <span style='background-color: #fdfd96;'> 4 - Comp and transfo </span> </h4>" ))),
                box(
                  width = 12,
                  title = p("Comp and Transfo ",actionButton("HELP_COMP_TRANSF", "", icon("question-circle"), class = "btn-xs")),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  
                  
                  # Option to apply comp
                  checkboxInput("compensation", "Apply spillover matrix", value= TRUE),
             
                  
                  uiOutput("compensation_message"),
                  
                  
                  fluidRow(
                    
                    column(
                      6, 
                      fileInput(
                        inputId = "uploadTransformation",
                        label ="(Option) Upload CSV transformation file",
                        buttonLabel = "Upload CSV",
                        placeholder = "No file selected",
                        multiple = FALSE,
                        accept = c(".csv"),
                        width = "100%",
                        
                      ),
                      selectInput("transfo", "Transformation", choices = list("arcsinh","None"), selected = "None", width = 200)
                    ), 
                    tags$br(),
                    
                  ),
                  
                  fluidRow( 
                    column(
                      4,  
                      div(
                        style = "margin-left: 40px;",  
                        rHandsontableOutput('table_group_transfo')
                        
                      ),
                      tags$div(id = "status_message"),
                      tags$br(),
                      
                      downloadButton("exportTransformation", "Export CSV"),
                      
                      actionButton(inputId = "validTransformation", label = "Validate transformation (Apply to all files)", style = "background-color: #BCEE68; border-width: 2px; border-color:  #BCEE68; margin-top: 10px;"),
                      column(12,uiOutput("X_scale_min_ui"),  uiOutput("Y_scale_min_ui")),
                      column(12,uiOutput("X_scale_max_ui"),  uiOutput("Y_scale_max_ui"))
                      
                    ),
                    
                    column(
                      8,
                      fluidRow(
                        column(
                          3,
                          selectInput("marker_x", "X-axis Marker", choices = c(""), selected = NULL, width = "100%"),
                          actionButton(inputId = "applyTransformation", label = "Transform", style = "background-color: #BCEE68; border-width: 2px; border-color:  #BCEE68; margin-top: 10px;")
                        ),
                        column(
                          3,
                          selectInput("marker_y", "Y-axis Marker", choices = c(""), selected = NULL, width = "100%")
                        ),
                        column(4, sliderInput(
                          inputId = "num_breaks",
                          label = "Number of Breaks:",
                          min = 5,
                          max = 100,
                          value = 30,
                          step = 5))
                      ),
                      fluidRow(
                        column(
                          6,
                          plotOutput("plot_density", height = "500px", width = "500px")
                        ),
                        column(
                          6,
                          plotOutput("plot_barplot", height = "500px", width = "500px")
                        )
                      )
                    )
                    
                    
                    
                  )
                )
                
                
                
              )
            )
          ),
          
          
          ################################################################################
          ####################### DEBARCODING  ###########################################
          
          conditionalPanel(
            condition = "input.tab == 'debarcoding'",
            
            uiOutput("fcsTransformed"),
            
            fluidPage(
              
              box(width = 3,HTML("<h4> <span style='font-weight: bold;'>DEBARCODING </span></h4>" )),
              tags$br(),
              uiOutput("RDataFile2_ui"),
              box(width = 12,
                  title = p("VAEVICTIS",actionButton("HELP_VAEVICTIS", "", icon("question-circle"), class = "btn-xs")),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  fluidRow(column(6,
                                  HTML("<h4> <span style='background-color: #fdfd96;'font-weight: bold;'>STEP 1 : </span> Run vaevictis</h4>" ))),
                  
                  fluidRow(column(4,selectInput("markers_analyse", "Select markers", choices=c(""), multiple=TRUE))),
                  tags$br(),
       
                  tags$br(),
                  fluidRow(column(1,numericInput("nclust", "N. of clusters",15)),
                           column(1,numericInput("ncells", "N.cells/clust",500)),
                           column(1,sliderTextInput("tsneW", "t-SNE regularization weight", choices=c(0,0.1, 0.5, 1, 2, 5, 10), selected=10, grid=T)),
                           column(1,sliderTextInput("ivisW", "ivis pn loss weight", choices=c(0,0.1, 0.5, 1, 2, 5, 10), selected=10, grid=T)),
                           column(1,sliderTextInput("vaeW", "reconstruction error weight", choices=c(0,0.1, 0.5, 1, 2, 5, 10), selected=2, grid=T)),
                           column(1,sliderTextInput("klW", "KL-divergence weight", choices=c(0,0.1, 0.5, 1, 2, 5, 10), selected=1, grid=T)),
                           column(1,sliderTextInput("umapW", "UMAP regularization weight", choices=c(0,0.1, 0.5, 1, 2, 5, 10), selected=10, grid=T)),
                           column(2,checkboxInput("useSaved", "Use previously saved model", value=FALSE),
                                  conditionalPanel(condition="input.useSaved==1",
                                                   shinyFilesButton(
                                                     "browseSaved", 
                                                     "Browse saved models...",
                                                     "Choose a saved model to apply on the data.",
                                                     multiple = FALSE,
                                                     filetype=list(json = c('json'))
                                                   ),
                                                   textOutput("savedModelName")
                                  )
                                  
                                 
                           ),
                           column(1,actionButton("runVaev", "Run", style = "background-color: #BCEE68; border-width: 2px; border-color:  #BCEE68; margin-top: 10px;"))
                           
                  ),
                  
                  fluidRow(column(4,uiOutput("choosePlatevae_ui"),uiOutput("chooseFilevae_ui"),
                           
                                  plotOutput("vaevPlot", width='500px',height='500px')),
                           tags$br(),
                           tags$br(),
                           column(2, checkboxInput("saveModel", "Save this model", value=TRUE), tags$br(),actionButton("runVaevAll", "Apply (Run on all Files)", style = "background-color: #BCEE68; border-width: 2px; border-color:  #BCEE68; margin-top: 10px;")),
                         
                  ),
                  
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  fluidRow(column(6,HTML("<h4> <span style='background-color: #fdfd96;'>STEP 2 : </span> Attribute barcodes</h4>" ))),
                  tags$br(),
                  
                  fluidRow(column(
                    2,
                    
                    selectInput(
                      "marker_x7",
                      "X-axis Marker",
                      choices = c(""),
                      selected = NULL,
                      width = "100%"
                    )),
                    column(
                      2,
                      
                      selectInput(
                        "marker_y7",
                        "Y-axis Marker",
                        choices = c(""),
                        selected = NULL,
                        width = "100%"
                      )),
                    
                    uiOutput("marker_x8_ui"),
                    uiOutput("marker_y8_ui"),
                    uiOutput("marker_x9_ui"),
                    uiOutput("marker_y9_ui"),
                    
                  ),
                  fluidRow(column(
                    4,
                    
                    actionButton("apply_gate", "Apply Gate",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                    actionButton("reset_gates", "Reset Gates", style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                    plotOutput("scatterPlot", click = "plot_click",  hover = "plot_hover",width='500px',height='500px')
                    
                  ),
                  
                  
                  column(
                    4,
                    actionButton("apply_gate2", "Apply Gate",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                    actionButton("reset_gates2", "Reset Gates", style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                    tags$br(),
                    plotOutput("plot_debarcoding",click = "plot_click2", width='500px',height='500px')
                    
                    
                    
                    
                  ),
                  column(
                    4,
                    tags$br(),
                    tags$br(),
                    
                    
                    plotOutput("plot_debarcodingBis", width='500px',height='500px'),
                  ),
                  
                  
                  ),
                  
                  fluidRow(
                    column(
                      1,    uiOutput("gate_legend")),
                    column(2,tags$br(),uiOutput("gate_checkboxes_debarcoding")),
                    column(4, tags$br(), uiOutput("renameBarcode_ui"),uiOutput("tableMatchBarcodeId_ui")), column(1,      tags$br(),
                                                                                                        tags$br(),tags$br(), uiOutput("exportBarcodeMatch_ui"),tags$br(),uiOutput("validateBarcode_ui")) , column(1),column(3,tags$br(),tags$br(),uiOutput("informationForDebarcoding"))
                    
                    
                    
                    
                  ),
                  tags$br(),
                  tags$br(),
                  tags$br()
              ))
            
          ),
          
          ##################################################################################
          #####################   ANNOTATION ###############################################
          
          
          #### ANNOTATION SCYAN
          
          conditionalPanel(
            
            condition = "input.tab == 'annotation'",
            
            fluidPage(box(width=10, uiOutput("informationAnnotation")
            
            ), 
            tags$br(),
            tags$br(),
            
            uiOutput("RDataFile3_ui"),
            
            box(width = 12,status = "primary",
                
                title=p("Scyan Annotation :",actionButton("HELP_SCYAN", "", icon("question-circle"), class = "btn-xs")),
                
                solidHeader = TRUE,
                
                collapsible = TRUE,
                
                # Load or build Scyan knowledge table  
                box(width = 12 , status="info",title='Load or build scyan knowledge table',
                    
                    
                    fileInput("knowledgeTable", "Upload knowledge Table (CSV or Excel) :", multiple = FALSE, buttonLabel = "Upload...", accept = c('.xlx','.xlsx','.csv'), width="50%"),
                    
                    
                    uiOutput("messageKnowledgeTable"),
                    tags$br(),
                    DTOutput("knowledgeTable"),
                    fluidRow(column(3,shinyFilesButton(
                      "landmarkFCSScyan",
                      "OR upload landmark FCS ...",
                      "Choose landmark FCS",
                      multiple = TRUE
                    ),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    DTOutput("matchPopId")
                    
                    ) ,
                    
                    uiOutput("knowledgeTableBuilder_ui")
                    # column(2,   selectizeInput("markersPresentInKnowledgeTableBuilding", "Choose core pannel markers", choices = c(""), multiple = TRUE),actionButton("buildLandmarkFCSScyan", "Build knowledge table",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"), uiOutput("exportTable_ui"))
                    )
                    
                ),
                
                #fileInput("buildKnowledgeTable", "   OR build knowledge table :", multiple = TRUE, buttonLabel = "Upload...", accept = c('.csv','.txt')),
                
                box(width = 12 , status="info",title='Match FCS markers with landmark markers',
                    
                    layout_columns(
                      
                      # Markers used for prediction
                      selectizeInput("markersUsedForPredictionsScyan", "Markers in FCS", choices = c(""), multiple = TRUE),
                      
                      # Markers used for train the model
                      selectizeInput("markersPresentInKnowledgeTable", "Markers in knowledge table", choices = c(""), multiple = TRUE),
                      
                    )
                    
                ),
                
                # Annotation part 
                box(width = 12, status="info",title=' Annotation',
                    fluidRow(column(4,selectInput(inputId = "popToAnnotate", "Select column that contains annotation labels", choices=c(""))),
                             column(2, numericInput("std", "std", value=0.25)),
                             column(2,numericInput("lr","lr", value=0.001))),
                    # Button to run Scyan annotation data
                    actionButton("annotate_data_with_Scyan", "Run Scyan annotation",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;")),
                
            ),
            box(width = 12,  title = p("Annotation Scaffold  ",actionButton("HELP_SCAFFOLD", "", icon("question-circle"), class = "btn-xs")),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                
                # CLARA clustering
                box(width = 12 , status="info", title = 'STEP 1 - Clustering',
                    
                    # Button to choose markers used for clustering
                    fluidRow(column(4,selectInput("marker_clustering", "Markers used for clustering", choices = c(""), multiple = TRUE)),
                             
                             column(2,numericInput("clustering_parameter", "k_parameter", value = 300)),
                             
                             column(3, tags$br(),actionButton("clustering", "Run CLARA clustering",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                                    uiOutput("messageClustering"),
                                
                             )),
                      
                    
                     # Action button to run clustering
                    
                    
                    # Choose if you want a model XGBoost already existing
                    fluidRow(checkboxInput("alreadyClustered", "My file(s) is(are) already clustered :", value=FALSE))),
                
                conditionalPanel(
                  
                  condition = "input.alreadyClustered == true",
                  
                  column(6,selectInput("clusteringColumn", "Choose name of column that contain clusters", choices = c(""), multiple = FALSE, width=500),
                         tags$br())
                  
                ),
                
                
                
                
                
                # Scaffold Map
                box(width = 12 , status="info", title = 'STEP 2 - Build or load scaffold Map',
                    
                    column(2, tags$br(), shinyFilesButton(
                      "loadlandmark",
                      "Upload landmark FCS ...",
                      "Choose landmark FCS",
                      multiple = TRUE),
                      tags$br(),uiOutput("scaffoldMap_ui"),
                      uiOutput("messageBuildScaffoldMap")
                   ),
                    
                    column(2,tags$br(),
                           shinyFilesButton(
                             "oldScaffoldMap",
                             "OR upload scaffold map ...",
                             "Choose landmark FCS",
                             multiple = TRUE
                           ))
                    
                    
                    
                    
                    
                    
                    
                ),
                
                # Scaffold predictions
                box(width = 12 , status="info", title = 'STEP 3 - Annotation',
                    
                    # Action Button to run scaffold predictions
                    actionButton("RunScaffoldAnnotation", "Run Scaffold Annotation",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                    
                    uiOutput("messageRunScaffoldAnnotation")
                    
                    
                )
                
            ),
            
            box(width = 12,status = "primary",
                
                title=p("QC Annotation :",actionButton("HELP_ANNOTATION_QC", "", icon("question-circle"), class = "btn-xs")),
                
                solidHeader = TRUE,
          
                collapsible = TRUE,
                fluidRow(column(4,selectizeInput("markersUMAP", "Markers for UMAP", choices = c(""), multiple = TRUE),
                                actionButton("runUMAP","Run UMAP", width='20%', style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                                
                                
                                
                                
                ),
                column(4, tags$br(),
                       uiOutput("colorBy_ui"),
                       uiOutput("QCAnnotation_ui")
                      
                       
                       
                       
                ),
                column(2, uiOutput("gate_legend2"))),
                
                
    
                fluidRow(
                  column(12,
                         uiOutput("selected_annotation_column_ui"),
                      
                         tags$br(),
                         tabsetPanel(
                           tabPanel("Annotation Viewer",
                                    fluidRow(
                                   
                                      column(3,
                                             tags$br(),
                                             DT::dataTableOutput("annotation_table", width = '100%') ),
                                         
                                     
                                      column(4,
                                             column(4, uiOutput("marker_x11_ui")),
                                             column(4, uiOutput("marker_y11_ui")),
                                             plotOutput("verifAnnotationPlot", width = "500px", height = "500px")
                                      ),
                                      column(2,  tags$br(),uiOutput("gate_legend3")),
                                    ),
                                    tags$br(), tags$br()
                           )
                         )
                  )
                )
                ,
                fluidRow(column(4, tags$br(), tags$br(),actionButton("runScyanAll","Run Scyan on all files",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;"),
                                actionButton("runScaffoldAll","Run Scaffold on all files",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;")))
                
            ))  ),
    
    ##################################################################################
    #####################   PYINFINITY FLOW ##########################################
    
          fluidPage(conditionalPanel(
            
            condition = "input.tab == 'infinityFlow'",
            
            
            
            box(width = 12,status = "primary",
                
                title=p("pyInfinityFLow :",actionButton("HELP_PYINFINITYFLOW", "", icon("question-circle"), class = "btn-xs")),
                
                solidHeader = TRUE,
                
                collapsible = TRUE,
                
                
                fluidRow(
                  column(3, HTML("<h4> <span style='background-color: #fdfd96;' >STEP 1 : </span>  Please upload Infinity Markers </h4>" ), tags$br(),uiOutput("uploadPlate_ui"),
                         DTOutput("plate_ui"), tags$br(), tags$br()),
                  column(4, HTML("<h4> <span style='background-color: #fdfd96;' > STEP 2 : </span> Please select backbone and exploratory markers : </h4>" ), tags$br(),  rHandsontableOutput('selection_table', width='80%'), tags$br()),
                  column(3, HTML("<h4> <span style='background-color: #fdfd96;' >STEP 3 : </span>  Run pyInfinityFlow </h4>" ),tags$br(), tags$br(), sliderInput("nCores", "n cores", min=1, max=38, step=2, value=15, width='70%'),
                         checkboxInput("TransformLogiclePy", "Transform with pyInfinity flow method (est.logicle)", value=FALSE),
                         checkboxInput("statsByPop", "Populations present in my FCS ", value=TRUE),
                         
                         conditionalPanel(
                           condition = "input.statsByPop == true",
                           selectizeInput("annotationColumn", "Select column that contain annotation", choices = c(""), multiple = FALSE, width="60%"),
                           tags$br()
                         ),
                         checkboxInput("statsByBarcode", "Barcodes present in my FCS ", value=TRUE),
                         conditionalPanel(
                           condition = "input.statsByBarcode == true",
                           selectizeInput("barcodeColumn", "Select column that contain barcodes", choices = c(""), multiple = FALSE, width="60%"),
                           tags$br()
                         ),
                         
                         actionButton("runPyInfinityFlow", "Run (py) InfinityFlow" ,style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;", width='50%'), tags$br(),uiOutput("exportFCSFinal_ui"),
                         tags$br(),
                         uiOutput("informationForSummaryCounts"),
                         uiOutput("min_cell_ui"),
                        
                         
                         tags$br(),
                         DT::DTOutput("summaryPopExport"),
                         tags$br(),
                         
                         uiOutput("export_pyInfinity_ui")
                         
                  )
                  
                )
                
                
                
                
            ),
            
            box(width = 12,status = "primary",
                
                title=p("Stats PyInfinityFlow :",actionButton("HELP_PYINFINITYFLOW", "", icon("question-circle"), class = "btn-xs")),
                
                solidHeader = TRUE,
                
                collapsible = TRUE,
                
                column(5,
                       tabsetPanel(
                         
                         tabPanel("ISO threshold",
                                  class = "tabset-panel", 
                                  tags$br(), tags$br(),
                                  
                                  uiOutput("isoPyInfinityFlowPlate_ui"),
                                  uiOutput("isoPyInfinityFlow_ui"),
                                  tags$br(),plotOutput("plotIso", height='500px', width='500px'),
                                  tags$br(),
                                  uiOutput("threshold_ui"),
                  
                                  tags$br(),
                                  tags$br(), actionButton("calcStats", "Calculate stats (All plates)",style = "background-color: #BCEE68; color: black; 
                                            font-weight: bold; padding: 10px 15px; 
                                            border-radius: 5px; border-width: 2px; border-color: #BCEE68;",width='30%')
                         )
                         
                       )),
                column(1),
                column(5,
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       uiOutput("statsHtml1_ui"),
                       uiOutput("statsHtml_ui"),
                       tags$br(),
                       uiOutput("mesured_estimated_choices_ui"),
                       DTOutput("statsResult"),
                       uiOutput("exportStats_ui")
                       
                )       
                
                
            )
            
          )
          )
        
        
  )
        
    )



  
      
  
