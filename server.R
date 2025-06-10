
# R libraries import
library(shiny)
library(parallel)
library(RColorBrewer)
library(mclust)
library(FlowCIPHE)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(reticulate)
library(tools)
library(readxl)
library(sp)
library(scaffold)
library(Rcpp)
library(MASS)
library(sf)
library(uwot)
library(flowCore)
library(igraph)
library(rhandsontable)
library(dplyr)
library(DiagrammeR)
library(DBI)
library(duckdb)
library(shinyFiles)
library(ellipse)
library(pracma)
library(openxlsx)
library(plotly)
library(future)
library(future.apply)
library(plyr)
plan(multisession)  
suppressWarnings(library(flowCut))
suppressWarnings(library(flowDensity))

# Scripts import
source("/mnt/md0/CMP/scripts/FUNCTIONS/help.R")
source("/mnt/md0/CMP/scripts/FUNCTIONS/functions.R")

source_python("/mnt/md0/CMP/scripts/FUNCTIONS/scyanFunctions.py")

# Select conda env
use_python("/home/admincyto/miniconda3/envs/scyan/bin/python3.9")
s <- import("scyan")
f<-import('fcsparser')

# Set working dir 
setwd("/mnt/md0/CMP/scripts")

## PATHS 
workingdir.root<-c(wd='/mnt/md0/CMP/')
vae.root <- c(wd='/media/data/html/Database/VAE.MODELS/')
scaffold.root <- c(wd='/mnt/md0/CMP/scaffold_models/')
save.root <- c(wd='/mnt/md0/CMP/backup/')
pyInf.root <- c(wd='/mnt/md0/CMP/pyInfinityFlow')

# Increase max size of uploading files
options(expressions = 5e5, shiny.maxRequestSize = 100 * 1024^3)

shinyServer(function(input, output,session) {
  max_gates <- 9
  gate_color_map <- setNames(c("#FF0000" , "#FF9900", "#FFE500", "#80FF00" ,"#00FFFF" ,"#001AFF" ,"#7F00FF" ,"#CC00FF" ,"#008B45" )  , as.character(1:max_gates))

 ## HELP BUTTON
  observeEvent(input$HELP_UPLOADING, {
    showModal(modalDialog(
      title = "Help - Uploading",
      HTML("
      <div style='text-align: justify;'>
        <p>Use this section to import your data files into the application. You can upload either raw FCS files or a previously saved analysis in RDS format.</p>
        <ul>
          <li><strong>FCS files:</strong> 
            <ul>
              <li>Click on <em>Select folder</em> for each plate you want to include.</li>
              <li>Then click on <strong>Load Data</strong> to begin the import process.</li>
            </ul>
          </li>
          <li><strong>RDS backup:</strong> 
            <ul>
              <li>Click on <strong>Upload backup analysis</strong>.</li>
              <li>Select the RDS project file you previously saved.</li>
            </ul>
          </li>
          <li><strong>Experiment name:</strong> 
            <ul>
              <li>You may enter a custom experiment name.</li>
              <li>If left blank, a name will be generated automatically.</li>
            </ul>
          </li>
        </ul>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  

  observeEvent(input$HELP_SEPARATE_BEADS, {
    showModal(modalDialog(
      title = "Help - Separate Beads from Cells",
      HTML("
      <div style='text-align: justify;'>
        <p>This step allows you to separate beads from cells using manual gating.</p>
        <ul>
          <li>Use your mouse to draw a gate around the bead population on the scatter plot.</li>
          <li>Once selected, click <strong>Validate Gate (beads)</strong> to confirm your selection.</li>
          <li>These beads can then:
            <ul>
              <li>Be removed from the FCS files if not needed, or</li>
              <li>Be removed from the FCS and used for normalization during preprocessing.</li>
            </ul>
          </li>
        </ul>
        <p>If you want to use the beads for standardization, don't worry if there's debris or anything, you can do QC afterwards.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  observeEvent(input$HELP_BEAD_QC, {
    showModal(modalDialog(
      title = "Help - Bead Quality Control",
      HTML("
      <div style='text-align: justify;'>
        <p>This section is used to perform quality control on beads before using them for normalization.</p>
        <ol>
          <li><strong>STEP 1 – Remove doublets:</strong>
            <ul>
              <li>Click on <strong>Remove doublets (All plates)</strong> to eliminate potential double events from the dataset.</li>
            </ul>
          </li>
          <li><strong>STEP 2 – Select mode:</strong>
            <ul>
              <li>Click on <strong>Transform data (All plates)</strong> to apply a logicle transformation for better peak visibility.</li>
              <li>Select a marker where bead peaks are clearly distinguishable.</li>
              <li>Indicate the number of expected peaks.</li>
              <li>Click <strong>Launch mclust (All plates)</strong> to automatically detect peak modes.</li>
              <li>Select peak you want to used for normalization</li>
            </ul>
          </li>
          <li><strong>STEP 3 – Manual re-gating (optional):</strong>
            <ul>
              <li>If the beads are not well-separated, manually gate them for each plate.</li>
              <li>Recommended markers for gating:
                <ul>
                  <li><strong>X-axis:</strong> G-PE-A</li>
                  <li><strong>Y-axis:</strong> BV650</li>
                </ul>
              </li>
            </ul>
          </li>
        </ol>
        <p style='margin-top:10px;'><strong>Final step:</strong> After QC is complete, click <strong>UnTransform data (All plates)</strong> followed by <strong>Validate beads (All plates)</strong>.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_NORMALIZE, {
    showModal(modalDialog(
      title = "Help - Normalization",
      HTML("
      <div style='text-align: justify;'>
        <p>This step allows you to normalize marker intensities across plates using selected bead populations.</p>
        <ul>
          <li><strong>1. Select markers:</strong> For each plate, choose the markers you want to normalize using the available dropdown menu.</li>
          <li><strong>2. Launch normalization:</strong> Once your selections are complete, click the <strong>Normalize</strong> button to perform the normalization process.</li>
        </ul>
        <p>This ensures consistent signal intensity across all your samples and reduces batch effects.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_CONCATENATE, {
    showModal(modalDialog(
      title = "Help - Concatenation",
      HTML("
      <div style='text-align: justify;'>
        <p>This section allows you to concatenate multiple plates into a single dataset.</p>
        <ul>
        
          <li>The parameters and structure used for concatenation will be taken from the <strong>first plate</strong> you select.</li>
          <li>You can select multiple plates to be merged together.</li>
        </ul>
        <div style='margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 5px solid #ffc107;'>
          <strong>⚠️ Important:</strong> Repeat this action for <strong>each group of plates</strong> you want to concatenate. Results are based on the first plate's format.
         <strong> Please, click on EXPORT FCS after concatenation and export the plate that contain concat files and export it. Close the app and reimport the files</strong> 
       
        </div>
        
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_FLOWQC, {
    showModal(modalDialog(
      title = "Help - FlowQC",
      HTML("
      <div style='text-align: justify;'>
        <p>This section is designed to clean your FCS files by removing technical artifacts and unwanted events.</p>
        <ul>
          <li><strong>Remove margin events:</strong> These are extreme events that fall outside the detector range and may distort analysis.</li>
          <li><strong>Remove doublets:</strong> These are overlapping events (e.g., two cells passing together) that should be excluded from single-cell analysis.</li>
          <li><strong>Use FlowCut:</strong> 
            FlowCut is an automated quality control algorithm that detects and removes abnormal regions in flow cytometry data over time. 
            It uses statistical and signal drift detection to identify unstable acquisition periods, improving data consistency.
          </li>
        </ul>
        <p>Configure FlowCut thresholds and select relevant markers before launching the cleaning process on all files.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  

  observeEvent(input$HELP_COMP_TRANSF, {
    showModal(modalDialog(
      title = "Help - Compensation and Transformation",
      HTML("
      <div style='text-align: justify;'>
        <p>This section allows you to apply a <strong>spillover compensation matrix</strong> and perform <strong>data transformation</strong> on your FCS files.</p>
        <ul>
          <li><strong>Compensation:</strong> Use this to correct fluorescence channel overlaps using a spillover matrix.</li>
          <li><strong>Transformation:</strong> Apply transformations (e.g., arcsinh) to rescale fluorescence signals and improve visualization.</li>
          <li><strong>CSV upload:</strong> 
            If you already have a transformation file, you can upload it here.
            The CSV file should contain two columns:
            <ul>
              <li><strong>Fluo</strong> — the marker name,</li>
              <li><strong>Arg</strong> — the transformation argument (e.g., cofactor).</li>
            </ul>
            Make sure the file is comma-separated.
          </li>
        </ul>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_CSV_UPLOAD, {
    showModal(modalDialog(
      title = "Aide - Fichier CSV de transformation",
      "Chargez un fichier CSV définissant la transformation souhaitée (e.g. arcsinh).",
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })

  observeEvent(input$HELP_VAEVICTIS, {
    showModal(modalDialog(
      title = "Help - VAEVICTIS",
      HTML("
      <div style='text-align: justify;'>
        <p><strong>VAEVICTIS RUN:</strong></p>
        <ul>
          <li><strong>1. Marker selection:</strong> Choose the markers you want to use for analysis. These should be relevant for distinguishing cell populations.</li>
          <li><strong>2. Run on subset:</strong> Click <strong>Run</strong> to apply VAEVICTIS on a subset of all your files (combined from all plates).</li>
          <li><strong>3. Visualize results:</strong> Review the plots generated to assess cluster separation and quality.</li>
          <li><strong>4. Apply to all:</strong> If satisfied with the preview, click <strong>Apply (Run on all files)</strong> to perform the full clustering process.</li>
        </ul>
        
        <p><strong>DEBARCODING:</strong></p>
        <p>Once VAEVICTIS has been applied to all your files, manually gate your barcodes and assign names (or numbers) to the resulting clusters. This will add a <code>barcodeID</code> column to your FCS files, which can be used in downstream analysis.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_SCYAN, {
    showModal(modalDialog(
      title = "Help - Scyan Annotation",
      HTML("
      <div style='text-align: justify;'>
        <p>Scyan allows you to annotate all your FCS files using a reference knowledge table.</p>
        
        <p><strong>What is the knowledge table?</strong></p>
        <ul>
          <li>It is a matrix where:
            <ul>
              <li>Each <strong>column</strong> corresponds to a marker (core panel).</li>
              <li>Each <strong>row</strong> corresponds to a known cell population.</li>
              <li>Each value should be between <strong>-1</strong> and <strong>+1</strong>:
                <ul>
                  <li><strong>-1</strong>: the population does <em>not</em> express the marker</li>
                  <li><strong>+1</strong>: the population <em>does</em> express the marker</li>
                </ul>
              </li>
            </ul>
          </li>
        </ul>

        <p><strong>How to provide it?</strong></p>
        <ul>
          <li><strong>Option 1:</strong> Upload a CSV or Excel file. The first column must contain population names in the format: <code>1_TCD4</code> (a number, underscore, then the population name).</li>
          <li><strong>Option 2:</strong> Upload individual FCS files — each file must represent a single population. The file names must follow the same format: <code>1_CD4.fcs</code>.</li>
        </ul>

        <p><strong>Marker Matching:</strong> After the table is loaded, you will need to match the markers from your FCS files with those from the core panel used in the knowledge table.</p>

        <p><strong>⚙️ Parameters:</strong></p>
        <ul>
          <li><strong>std (standard deviation):</strong> Controls how strictly the model follows the knowledge table. A lower value enforces stricter adherence to the expected marker expressions, while a higher value allows more flexibility.</li>
          <li><strong>lr (learning rate):</strong> Determines how fast the model updates during training. A very low value may result in slow convergence; a very high value might make training unstable.</li>
        </ul>
        <p>Default values generally work well, but you may adjust them depending on your data quality or model performance.</p>

        <p>Once configured, Scyan will train a model using the table and annotate your ungated FCS files accordingly.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_SCAFFOLD, {
    showModal(modalDialog(
      title = "Help - Scaffold Annotation",
      HTML("
      <div style='text-align: justify;'>
        <p>Scaffold is a method for annotating cells based on a reference map built from known landmark populations.</p>

        <p><strong>Step 1 – Clustering:</strong></p>
        <ul>
          <li>By default, the clustering algorithm used is <strong>CLARA</strong>.</li>
          <li>Select the markers you want to use for clustering and click <strong>Run CLARA clustering</strong>.</li>
          <li>If your file is already clustered, check the box <strong>“My files are already clustered”</strong> and select the column that contains the cluster labels.</li>
        </ul>

        <p><strong> Step 2 – Build or Load Scaffold Map:</strong></p>
        <ul>
          <li>You can build a Scaffold reference map from annotated FCS files.</li>
          <li>Each FCS file must contain a single cell population, and the filename should follow the format: <code>1_TCD4.fcs</code> (ID number + underscore + population name).</li>
          <li>Alternatively, you can upload a previously built Scaffold map if you already have one.</li>
        </ul>

        <p><strong>Step 3 – Annotation:</strong></p>
        <ul>
          <li>Once the map is ready, click <strong>Run Scaffold Annotation</strong> to project your clustered data onto the reference and assign population labels.</li>
        </ul>

        <p>Scaffold is particularly useful when you have well-defined reference populations and want to annotate new datasets consistently.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$HELP_ANNOTATION_QC, {
    showModal(modalDialog(
      title = "Help - QC Annotation",
      HTML("
      <div style='text-align: justify;'>
        <p>This section allows you to visually assess the quality of your annotations using UMAP projection.</p>
        <ul>
          <li>A subset of all FCS files is used to generate a UMAP embedding.</li>
          <li>Annotations (from Scyan or Scaffold) are overlaid on the UMAP for inspection.</li>
          <li>If the clustering and annotation look correct, you can apply the annotation to all files by clicking the corresponding button.</li>
        </ul>
        <p>This step helps verify that the model-based annotations align with known population structures before full application.</p>
      </div>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
observeEvent(input$HELP_PYINFINITYFLOW, {
  showModal(modalDialog(
    title = "Help - pyInfinityFlow",
    HTML("
      <div style='text-align: justify;'>
        <p>The <strong>pyInfinityFlow</strong> module allows you to predict unmeasured markers based on backbone markers using a deep learning model.</p>

        <p><strong>Step 1 – Data setup:</strong></p>
        <ul>
          <li>For each plate, upload a TXT file containing plate information.</li>
          <li>This file must contain two tab-separated columns:
            <ul>
              <li><code>Infinity_target</code></li>
              <li><code>Infinity_isotype</code></li>
            </ul>
          </li>
        </ul>

        <p><strong>Step 2 – Marker selection:</strong></p>
        <ul>
          <li>Select core panel markers as <strong>backbone</strong> markers (used for prediction).</li>
          <li>Designate the markers you want to impute as <strong>exploratory</strong>.</li>
        </ul>

        <p><strong>Step 3 – Run pyInfinityFlow:</strong></p>
        <ul>
          <li>Select the number of cores to use, based on your machine's capabilities.</li>
          <li>Click <strong>Run pyInfinityFlow</strong> to start the prediction process.</li>
        </ul>

        <p><strong>Step 4 – Statistics:</strong></p>
        <ul>
          <li>This part allows you to compute statistics either on:
            <ul>
              <li>The original measured values, or</li>
              <li>The predicted values from pyInfinityFlow.</li>
            </ul>
          </li>
          <li>First, define the positivity threshold for each ISO file to interpret marker expression.</li>
        </ul>
      </div>
    "),
    easyClose = TRUE,
    footer = modalButton("Close")
  ))
})

  
  listObjects <- reactiveValues()

  listObjectsUpdated <- reactiveVal(0)
  # Automatically update all UI elements when listObjects is updated
  observeEvent(listObjectsUpdated(), {
    # Update dropdown menus
    updateSelectizeInput(session, "groupTransfo", choices = names(listObjects))
    updateSelectizeInput(session, "groupQC", choices = names(listObjects))
    updateSelectizeInput(session, "referenceGroup", choices = names(listObjects))
    updateSelectInput(session, "plateForConcatenation", choices = names(listObjects))
    
    # Refresh the plate indicators UI
    output$plateIndicators <- renderUI({
      colors <- c("#FF5733", "#33B5E5", "#33D17A", "#FFC107", "#9C27B0", 
                  "#FF9800", "#8BC34A", "#3F51B5", "#E91E63", "#009688")
      
      plates <- names(listObjects)[grepl("^plate_\\d+$", names(listObjects))]
      
      plate_boxes <- lapply(seq_along(plates), function(i) {
        plate_name <- plates[i]
        num_files <- length(listObjects[[plate_name]]$original_filenames)
        
        div(
          style = paste0(
            "display: inline-block; width: 200px; height: 50px;",
            "background-color: ", colors[i %% length(colors) + 1], ";",
            "color: white; text-align: center;",
            "border-radius: 10px; font-weight: bold;",
            "margin: 5px; padding: 10px; line-height: 30px;",
            "border: 2px solid black;"
          ),
          paste(plate_name, "-", num_files, "files")
        )
      })
      
      do.call(tagList, plate_boxes)
    })
  })
  
  # Function to increment the trigger when listObjects changes
  triggerListObjectsUpdate <- function() {
    listObjectsUpdated(listObjectsUpdated() + 1)
  }
  
  # Set reactive value
  listObject <- reactiveValues(
    listFCS = NULL,
    flow.frames = NULL,
    CSTFile=NULL,
    path=NULL,
    listCST=NULL,
    markerFCS=NULL,
    flow.frames.CST=NULL,
    flow.frames.PCA=NULL,
    flow.frames.clusters=NULL,
    data.table=NULL,
    csvFileTransfo=NULL,
    beads=NULL,
    flow.frames.beads=NULL,
    flow.frames.cells=NULL,
    gate=NULL,
    bead_gated=NULL,
    flow.frames.bead_peak=NULL,
    groups=NULL,
    concat=NULL,
    dfMatchBarcode=NULL,
    mapping_table=NULL,
    markerCurrentGroup=NULL,
    markerRefGroup=NULL,
    knowledgeTable=NULL,
    landmarkMeta=NULL,
    landmarkFCS=NULL,
    flow.frames.subset=NULL,
    umap=NULL,
    data.table=NULL,
    csvFileTransfo=NULL,
    vae.filename=NULL,
    vae.config =NULL,
    vae.weights=NULL,
    vae.finish=NULL,
    dico_pop=NULL,
    scyan_table=NULL,
    flow.frames.subset.pop=NULL,
    Map=NULL,
    plate=NULL,
    backbone_specification=NULL,
    experimentName=NULL,
    brushed_points=NULL,
    stats_df=NULL,
    stats_df_estimated=NULL,
    FCSXGboostExport=NULL,
    pyInfinityFlowFCS=NULL,
    pop_dict=NULL)

  
  
  restoreReactiveValues <- function(lst) {
    rv <- reactiveValues()
    for (n in names(lst)) {
      rv[[n]] <- lst[[n]]
    }
    return(rv)
  }
  # Open export modal when clicking the sidebar button
  observeEvent(input$openExportModal, {
    showModal(modalDialog(
      title = "Export deconcatenated FCS files",
      
      HTML("<p>Select the plates you want to export. You can either export them as a ZIP file locally or save them on the server in /mnt/md0/CMP/backupFCS.</p>"),
      
      checkboxGroupInput("selectedPlatesForExport", "Plates to export:",
                         choices = names(listObjects),
                         selected = names(listObjects)),
      
      radioButtons("exportDestination", "Export to:",
                   choices = c("Local ZIP" = "local", "Server Folder" = "server"),
                   selected = "server"),
      
      textInput("exportFolderName", "Folder name (used only for server export):", 
                value = gsub(" ", "_", if (is.null(listObject$experimentName)) "My_Export" else listObject$experimentName)),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("startExportFCS", "Start Export", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })

  # Logic server for FCS export
  observeEvent(input$startExportFCS, {
    showModal(modalDialog(
      title = "Exporting FCS files...",
      HTML("<p>Please wait while the files are being processed...</p>"),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Trigger the export logic 
    
    isolate({
      export_type <- input$exportDestination
      selected_groups <- input$selectedPlatesForExport
      export_name <- gsub("[^a-zA-Z0-9_]", "_", input$exportFolderName)
      
      if (length(selected_groups) == 0) {
        removeModal()
        showNotification("No plates selected!", type = "error")
        return(NULL)
      }
      
      if (export_type == "local") {
        removeModal()
        showModal(modalDialog(
          title = "Download FCS ZIP",
          HTML("<p>Your files are ready. Click below to download:</p>"),
          downloadButton("downloadExportFCS", "Download ZIP", class = "btn-success"),
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      } else {
        export_path <- file.path("/mnt/md0/CMP/backupFCS", export_name)
        if (!dir.exists(export_path)) dir.create(export_path, recursive = TRUE)
        
        for (group_name in selected_groups) {
          group <- listObjects[[group_name]]
          deconcat <- group$flow.frames.deconcatenate
          if (is.null(deconcat)) next
          
          group_dir <- file.path(export_path, group_name)
          dir.create(group_dir, showWarnings = FALSE, recursive = TRUE)
          
          for (fcs_name in names(deconcat)) {
            clean_name <- gsub(paste0("^", group_name, "_"), "", fcs_name)
            clean_name <- gsub("[^a-zA-Z0-9_\\.]", "_", clean_name)
            
            
            clean_name <- sub("\\.fcs$", "", clean_name, ignore.case = TRUE)
            clean_name <- paste0(clean_name, ".fcs")
            
            fcs_file_path <- file.path(group_dir, clean_name)
            write.FCS(deconcat[[fcs_name]], fcs_file_path)
          }
          
        }
        
        removeModal()
        showNotification(paste("Exported to:", export_path), type = "message", duration = 6)
      }
    })
  })
  
 
  # Logic for Button Export FCS 
  output$downloadExportFCS <- downloadHandler(
    filename = function() {
      paste0("FCS_Export_", input$experimentName, ".zip")
    },
    content = function(file) {
      removeModal()
      showModal(modalDialog(
        title = "Download FCS ZIP",
        HTML("<p>Please wait ...</p>"),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
      temp_dir <- tempdir()
      all_files <- c()
      
      selected_groups <- isolate(input$selectedPlatesForExport)
      
      for (group_name in selected_groups) {
        group <- listObjects[[group_name]]
        deconcat <- group$flow.frames.deconcatenate
        if (is.null(deconcat)) next
        
        group_dir <- file.path(temp_dir, group_name)
        dir.create(group_dir, showWarnings = FALSE, recursive = TRUE)
        
        for (fcs_name in names(deconcat)) {
          clean_name <- gsub(paste0("^", group_name, "_"), "", fcs_name)
          clean_name <- gsub("[^a-zA-Z0-9_\\.]", "_", clean_name)
          fcs_file_path <- file.path(group_dir, paste0(clean_name, ".fcs"))
          write.FCS(deconcat[[fcs_name]], fcs_file_path)
          all_files <- c(all_files, fcs_file_path)
        }
      }
      
      # Correct zip: only include what you created
      zipfile <- file.path(temp_dir, "export.zip")
      old_wd <- getwd()
      setwd(temp_dir)
      zip(zipfile, files = selected_groups, flags = "-r9X")
      setwd(old_wd)
      
      file.copy(zipfile, file)
      removeModal()
    }
  )
  
  ################################################################################
  ##################### UPLOAD DATA ##############################################
 

  shinyFileChoose(input, "backup", roots = save.root, session = session, filetypes=c("rds"))

  observe({
    req(input$backup)
    isolate({  
      
      fileinfo <- parseFilePaths(roots = save.root, input$backup)
      req(nrow(fileinfo) > 0)
      
      filepath <- as.character(fileinfo$datapath)
      if (!file.exists(filepath)) {
        showNotification("Fichier introuvable.", type = "error")
        return(NULL)
      }
      
      tryCatch({
        showModal(modalDialog(
          title = "Loading project...",
          tags$div(
            style = "text-align: center;",
            tags$p("Please wait.")
          ),
          footer = NULL,
          easyClose = FALSE
        ))
        
        loaded <- readRDS(filepath)
        
        # Reset objects
        for (n in names(listObject)) listObject[[n]] <- NULL
        for (n in names(listObjects)) listObjects[[n]] <- NULL
        
        # Load new ones
        for (n in names(loaded$listObject)) listObject[[n]] <- loaded$listObject[[n]]
        for (n in names(loaded$listObjects)) listObjects[[n]] <- loaded$listObjects[[n]]
        

        if (!is.null(loaded$gate_coords2)) gate_coords2(loaded$gate_coords2)
        if (!is.null(loaded$gate_applied2)) gate_applied2(loaded$gate_applied2)
        if (!is.null(loaded$all_gates)) all_gates(loaded$all_gates)
        if (!is.null(loaded$gate_to_confirm))  gate_to_confirm(loaded$ gate_to_confirm)
        if (!is.null(loaded$gate_coords)) gate_coords(loaded$gate_coords)
      
        removeModal()
        
        updateSelectizeInput(session, "groupTransfo", choices = names(listObjects))
        updateSelectizeInput(session, "groupQC", choices = names(listObjects))
        updateSelectizeInput(session, "referenceGroup", choices = names(listObjects))
        updateTextInput(session, "experimentName", value = listObject$experimentName)  
       
   
        showNotification("Project successfully completed", type = "message")
        
      }, error = function(e) {
        removeModal()
        showNotification(paste("Erreur de lecture :", e$message), type = "error")
        NULL
      })
      
    }) 
  })
  
 

    
    
    output$organigramme<- renderGrViz({
      
    
      graph_code <- switch(input$experienceType,
                           
                           "Pop" = "
        digraph organigramme {
          node [shape=box, style=filled, fontname=Arial, color=black]
          Pop [label='Pop', fillcolor=pink]
          Normalisation [label='Normalization', fillcolor=lightgray]
          FlowCut [label='FlowCut', fillcolor=lightgray]
          Comp_Transf [label='Comp / Transf', fillcolor=lightgray]
           Annotation [label='Annotation', fillcolor=lightgray]
          Equal_Sampling [label='Equal Sampling', fillcolor=lightgray]
          Enriched_Files [label='Enriched Files', fillcolor=green]
          Infinity_Flow [label='Infinity Flow', fillcolor=lightgray]
          Stats [label='Stats', fillcolor=green]

          Pop -> Normalisation [color=deeppink, penwidth=2]
          Normalisation -> FlowCut [color=deeppink, penwidth=2]
          FlowCut -> Comp_Transf [color=deeppink, penwidth=2]
          Comp_Transf -> Annotation [color=deeppink, penwidth=2]
          Annotation -> Equal_Sampling [color=deeppink, penwidth=2]
          Equal_Sampling ->Infinity_Flow [color=deeppink, penwidth=2]


          Infinity_Flow -> Stats [color=black, penwidth=2]
          Infinity_Flow ->   Enriched_Files [color=black, penwidth=2]
        }
      ",
                           
                           "Bc + Pop" = "
        digraph organigramme {
          node [shape=box, style=filled, fontname=Arial, color=black]
          BC_Pop [label='BC + Pop', fillcolor=orange]
          Normalisation [label='Normalization', fillcolor=lightgray]
          FlowCut [label='FlowCut', fillcolor=lightgray]
          Comp_Transf [label='Comp / Transf', fillcolor=lightgray]

          VAEVICTIS [label='VAEVICTIS', fillcolor=lightgray]
          Debarcoding [label='Debarcoding', fillcolor=lightgray]
          Annotation [label='Annotation', fillcolor=lightgray]

          Equal_Sampling [label='Equal Sampling', fillcolor=lightgray]
          Infinity_Flow [label='Infinity Flow', fillcolor=lightgray]
          Stats [label='Stats', fillcolor=green]
          Enriched_Files [label='Enriched Files', fillcolor=green]
          BC_Pop -> Normalisation [color=orange, penwidth=2]
          Normalisation -> FlowCut [color=orange, penwidth=2]
          FlowCut -> Comp_Transf [color=orange, penwidth=2]
          Comp_Transf -> VAEVICTIS [color=orange, penwidth=2]
          VAEVICTIS -> Debarcoding [color=orange, penwidth=2]
          Debarcoding -> Annotation [color=orange, penwidth=2]
          Annotation -> Equal_Sampling [color=orange, penwidth=2]
          Equal_Sampling -> Infinity_Flow [color=orange, penwidth=2]
          Infinity_Flow -> Stats [color=black, penwidth=2]
          Infinity_Flow ->   Enriched_Files [color=black, penwidth=2]
        }
      ",
                           
                           "Bc" = "
        digraph organigramme {
          node [shape=box, style=filled, fontname=Arial, color=black]
          BC [label='BC', fillcolor=red]
          Normalisation [label='Normalization', fillcolor=lightgray]
          FlowCut [label='FlowCut', fillcolor=lightgray]
          Comp_Transf [label='Comp / Transf', fillcolor=lightgray]
          VAEVICTIS [label='VAEVICTIS', fillcolor=lightgray]
          Debarcoding [label='Debarcoding', fillcolor=lightgray]

          Infinity_Flow [label='Infinity Flow', fillcolor=lightgray]
          Stats [label='Stats', fillcolor=green]
          Enriched_Files [label='Enriched Files', fillcolor=green]
          BC -> Normalisation [color=red, penwidth=2]
          Normalisation -> FlowCut [color=red, penwidth=2]
          FlowCut -> Comp_Transf [color=red, penwidth=2]
          Comp_Transf -> VAEVICTIS [color=red, penwidth=2]
          VAEVICTIS -> Debarcoding [color=red, penwidth=2]
          Debarcoding -> Infinity_Flow [color=red, penwidth=2]
          Infinity_Flow -> Stats [color=black, penwidth=2]
          Infinity_Flow -> Enriched_Files [color=black, penwidth=2]
        }
      "
      )
      
      DiagrammeR::grViz(graph_code)
    })
  

  
  output$file_inputs <- renderUI({
    req(input$selector)
    
    lapply(seq_len(input$selector), \(i) {
      fileInput(
        inputId = paste0("file_input_", i),
        label = paste("Upload plate:", i),
        multiple=TRUE
      )
    })
  })
  
  folder_paths <- reactiveValues(paths = list())
  output$folder_inputs <- renderUI({
    req(input$selector)
    
    lapply(seq_len(input$selector), \(i) {
      shinyDirButton(paste0("folder_input_",i),
                     
                     label=paste0("Select folder plate : ", i),
                     title=paste0(" Select folder plate : ", i),
                     icon = icon("folder"))
      
      
      
    })
  })
  
  
  
  observe({
    lapply(seq_len(input$selector), function(i) {
      name <- paste0("folder_input_", i)
      
      shinyDirChoose(input, name, roots = c(home = "/mnt/md0/"), session = session)
      
      observeEvent(input[[name]], {
        folder_paths$paths[[name]] <- parseDirPath(c(home = "/mnt/md0"), input[[name]])
      })
    })
  })
  
  output$selected_paths <- renderPrint({
    folder_paths$paths
  })
  
  
  ## Import FCS files
  plan(multisession, workers = parallel::detectCores() - 1)
  
  observeEvent(input$loadData, {
    
    req(input$folder_input_1)
    
    if (input$experimentName == "") {  
      auto_name <- paste0("Experiment_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      updateTextInput(session, "experimentName", value = auto_name)  
      listObject$experimentName <- auto_name
      
    } else {
      listObject$experimentName <- input$experimentName
    }
    
    for (i in seq_len(input$selector)) {
      showModal(modalDialog(
        title = paste0("Reading files plate ", i, "..."),
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait.")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      listFCS <- list.files(folder_paths$paths[[paste0("folder_input_", i)]], full.names = TRUE)
      
      original_names <- list.files(folder_paths$paths[[paste0("folder_input_", i)]])
      
      
      original_names <- paste0("plate_", i, "_", original_names)
      
      if (is.null(listObjects[[paste0("plate_", i)]])) {
        listObjects[[paste0("plate_", i)]] <- list(
          listFCS = NULL,
          flow.frames = NULL,
          markerFCS = NULL,
          data.table = NULL,
          original_filenames = NULL
        )
      }
      
      currentGroup <- listObjects[[paste0("plate_", i)]]
      currentGroup$original_filenames <- as.vector(original_names)
      currentGroup$listFCS <- listFCS
      

      validated_files.all <- lapply(currentGroup$listFCS, function(file) {
        
        flow_data <- read.FCS(file, truncate_max_range = FALSE, ignore.text.offset = TRUE)
        path<-dirname(file)
        
        path<-dirname(path)
        listObject$path<-path
        if (!is.matrix(exprs(flow_data))) stop("Invalid FCS file format")
        flow_data
      })
      
      validated_files <- lapply(validated_files.all, function(file) {
        if (nrow(file) > 1000) {
          flow_data <- sample(1:nrow(file), 1000)
          exprs(file) <- exprs(file)[flow_data, ]
        }
        return(file)
      })
      
      concatenatedFCS <- if (length(validated_files) == 1) {
        validated_files.all[[1]]
      } else {
        concatenate.FCS.CIPHE(validated_files, "idAcquisition")
      }
      exprs_data <- exprs(concatenatedFCS)
      
      
      idAcq_cols <- which(colnames(exprs_data) == "idAcquisition")
      
      if (length(idAcq_cols) > 1) {
      
        exprs_data <- exprs_data[, -idAcq_cols[-length(idAcq_cols)]]
        
   
        exprs(concatenatedFCS) <- exprs_data
      }
      currentGroup$markerFCS <- concatenatedFCS@parameters@data$name
      names(currentGroup$markerFCS) <- NULL
      currentGroup$flow.frames <- concatenatedFCS
      currentGroup$flow.frames.deconcatenate <- validated_files.all
      names(currentGroup$flow.frames.deconcatenate) <- as.vector(original_names)
      
      listObject$data.table <- data.frame(
        Fluo = currentGroup$markerFCS,
        Arg = rep("none", length(currentGroup$markerFCS))
      )
      
      listObjects[[paste0("plate_", i)]] <- currentGroup
      
      updateSelectizeInput(session, "groupTransfo", choices = names(listObjects))
      updateSelectizeInput(session, "groupQC", choices = names(listObjects))
      updateSelectizeInput(session, "referenceGroup", choices = names(listObjects))
      
      removeModal()
    }
    if(is.null(listObject$flow.frames.subset)){
      concatenated_list_subset<-list()
      for (plate_name in names(listObjects)) {
        concatenated_list_subset[[plate_name]]<-listObjects[[plate_name]]$flow.frames
      }
      
      listObject$flow.frames.subset<-concatenate.FCS.CIPHE(concatenated_list_subset,"idPlate")
      
    }
  })
  

  output$plateIndicators <- renderUI({
    req(listObjects)
    colors <- c("#FF5733", "#33B5E5", "#33D17A", "#FFC107", "#9C27B0", 
                "#FF9800", "#8BC34A", "#3F51B5", "#E91E63", "#009688")  
    

    plates <- names(listObjects)[grepl("^plate_\\d+$", names(listObjects))]
    
  
    plate_boxes <- lapply(seq_along(plates), function(i) {
      plate_name <- plates[i]
      num_files <- length(listObjects[[plate_name]]$original_filenames)
      
      div(
        style = paste0(
          "display: inline-block; width: 200px; height: 50px;",
          "background-color: ", colors[i %% length(colors) + 1], ";",
          "color: white; text-align: center;",
          "border-radius: 10px; font-weight: bold;",
          "margin: 5px; padding: 10px; line-height: 30px;",
          "border: 2px solid black;"
        ),
        paste(plate_name, "-", num_files, "files")
      )
    })
    
    do.call(tagList, plate_boxes)
  })
  
  
  observe({
    lapply(seq_len(input$selector), function(i) {
      observeEvent(input[[paste0("folder_input_", i)]], {
        tryCatch({
          progress <- Progress$new()
          progress$set(message = paste("Processing plate", i), value = 0.5)
          
          files <- input[[paste0("folder_input_", i)]]
          
          progress$close()
          
        }, error = function(e) {
          progress$close()
          showNotification(paste("Erreur :", e$message), type = "error")
        })
      })
    })
  })
  
  # Transform data in logicle to better visualization
  observeEvent(input$transformVizu, {
    tryCatch({
      
      
      if (!is.null(listObjects)) {
        showModal(modalDialog(
          title = "Transform bead (only for visualization)...",
          tags$div(
            style = "text-align: center;",
            tags$p("Please wait."),
          ),
          footer = NULL,
          easyClose = FALSE
        ))
        for (group_name in names(listObjects)) {
          
          currentGroup <- listObjects[[group_name]]
          req(currentGroup)
          req(currentGroup$flow.frames.beads)
          
          
          currentGroup$flow.frames.beads <- FlowCIPHE::logicle.CIPHE(currentGroup$flow.frames.beads, value = 500, markers=colnames(currentGroup$flow.frames.beads))
          
          
          if (!is.null(currentGroup$flow.frames.beads.gated.regate)) {
            currentGroup$flow.frames.beads.gated.regate <- FlowCIPHE::logicle.CIPHE(currentGroup$flow.frames.beads.gated.regate, value = 500, markers=colnames(currentGroup$flow.frames.beads))
          }
          
          
          if (!is.null(currentGroup$flow.frames.beads.gated)) {
            currentGroup$flow.frames.beads.gated <- FlowCIPHE::logicle.CIPHE(currentGroup$flow.frames.beads.gated, value = 500, markers=colnames(currentGroup$flow.frames.beads))
          }
          
          listObjects[[group_name]] <- currentGroup
        }
        removeModal()
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  # Initialize markers
  observeEvent(input$groupTransfo,{
    req(input$groupTransfo)
    if (!is.null(listObjects)) {
      if (!is.null(listObjects[[input$groupTransfo]])) {
        currentGroup <- listObjects[[input$groupTransfo]]
        
        # Update UI elements for the group
        if ("FSC-A" %in% currentGroup$markerFCS && "SSC-A" %in% currentGroup$markerFCS) {
          updateSelectInput(session, "marker_x",  choices = currentGroup$markerFCS, selected = "FSC-A")
          updateSelectInput(session, "marker_y",  choices = currentGroup$markerFCS, selected = "SSC-A")
          updateSelectInput(session, "marker_x2", choices = currentGroup$markerFCS, selected = "FSC-A")
          updateSelectInput(session, "marker_y2", choices = currentGroup$markerFCS, selected = "SSC-A")
          
        } else {
          updateSelectInput(session, "marker_x",  choices = currentGroup$markerFCS)
          updateSelectInput(session, "marker_y",  choices = currentGroup$markerFCS)
          updateSelectInput(session, "marker_x2", choices = currentGroup$markerFCS)
          updateSelectInput(session, "marker_y2", choices = currentGroup$markerFCS)
          
        }
        
        if ("FSC-A" %in% currentGroup$markerFCS && "FSC-H" %in% currentGroup$markerFCS) {
          updateSelectInput(session, "marker_x10", choices = currentGroup$markerFCS, selected = "FSC-A")
          updateSelectInput(session, "marker_y10", choices = currentGroup$markerFCS, selected = "FSC-H")
          
        }else{
          
          updateSelectInput(session, "marker_x10", choices = currentGroup$markerFCS)
          updateSelectInput(session, "marker_y10", choices = currentGroup$markerFCS)
          
        }
        updateSelectizeInput(session, "lifeMarker", choices = c("None", currentGroup$markerFCS), selected=c("None"))
        updateSelectInput(session, "markers_analyse", choices = currentGroup$markerFCS, selected=currentGroup$markerFCS)
        
      }
      
    }
    
    
  })
  
  
  
  
  
  # Display common Markers between ref and group
  
  observeEvent(input$groupNorm, {
    req(input$groupNorm)
    tryCatch({
      
      currentGroup <- listObjects[[input$groupNorm]]
      refGroup <- listObjects[["plate_1"]]
      req(currentGroup$flow.frames)
      
      if (!is.null(currentGroup$flow.frames)) {
        markerCurrentGroup <- colnames(currentGroup$flow.frames)
      } else {
        markerCurrentGroup <- colnames(currentGroup$flow.frames.beads)
      }
      
      if (!is.null(refGroup$flow.frames)) {
        markerRefGroup <- colnames(refGroup$flow.frames)
      } else if (!is.null(refGroup$flow.frames.beads)) {
        markerRefGroup <- colnames(refGroup$flow.frames.beads)
      }
      
      listObject$markerRefGroup <- markerRefGroup
      listObject$markerCurrentGroup <- markerCurrentGroup
      
      commonMarkers <- intersect(markerCurrentGroup, markerRefGroup)
      
      
      if (!is.null(currentGroup$commonMarkers)) {
        selectedMarkers <- currentGroup$commonMarkers
      } else {
        selectedMarkers <- commonMarkers
        currentGroup$commonMarkers <- selectedMarkers
        listObjects[[input$groupNorm]] <- currentGroup
      }
      
      updateSelectizeInput(session, "commonMarkers",
                           selected = selectedMarkers,
                           choices = commonMarkers)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Display common Markers between ref and group
  observeEvent(input$commonMarkers, {
    req(input$groupNorm)
    currentGroup <- listObjects[[input$groupNorm]]
    req(input$commonMarkers)
    
    
    currentGroup$commonMarkers <- input$commonMarkers
    listObjects[[input$groupNorm]] <- currentGroup
  })
  

  
  output$messageReference<-renderUI({
    if(input$selector >=2){
      HTML("<h4> <span style='color: green;font-weight: bold;'> GROUP 1 ->  </span><span style='font-weight: bold;'>REFERENCE PLATE </span></h4>" )
      
    }
  })
  
  
  
  
  # With unmatch column
  
  diff_colnames <- reactive({
    req(listObject$markerRefGroup)
    req(listObject$markerCurrentGroup)
    setdiff(listObject$markerRefGroup,listObject$markerCurrentGroup)
  })
  
  
  output$dynamic_selectors <- renderUI({
    req(input$groupNorm)
    req(diff_colnames())
    
    selected_group_name <- input$groupNorm
    marker_unmatch <- setdiff(listObject$markerCurrentGroup, listObject$markerRefGroup)
    
    lapply(seq_along(diff_colnames()), function(i) {
      ref_marker <- diff_colnames()[i]
      
      selected_value <- if (!is.null(listObjects[[selected_group_name]]$mapping) &&
                            !is.null(listObjects[[selected_group_name]]$mapping[[ref_marker]])) {
        listObjects[[selected_group_name]]$mapping[[ref_marker]]
      } else {
        NULL
      }
      
      selectizeInput(paste0("slider_", i),
                     label = paste("Ref", ref_marker, "match with :"),
                     choices = c("", marker_unmatch),
                     selected = selected_value,
                     width = "150px")
    })
  })
  
  
  # mapping marker ref/group
  observe({
    
    req(input$groupNorm)
    req(diff_colnames())
    
    selected_group_name <- input$groupNorm
    
    if (is.null(listObjects[[selected_group_name]]$mapping)) {
      listObjects[[selected_group_name]]$mapping <- list()
    }
    if (is.null(listObjects[["plate_1"]]$mapping)) {
      listObjects[["plate_1"]]$mapping <- list()
    }
    
    mapping_group <- listObjects[[selected_group_name]]$mapping
    mapping_ref <- if (!is.null(listObjects[["plate_1"]]$mapping[[selected_group_name]])) {
      listObjects[["plate_1"]]$mapping[[selected_group_name]]
    } else {
      list()
    }
    
    for (i in seq_along(diff_colnames())) {
      ref_marker <- diff_colnames()[i]
      selected_value <- input[[paste0("slider_", i)]]
      
      if (!is.null(selected_value) && selected_value != "") {
        mapping_group[[ref_marker]] <- selected_value
        mapping_ref[[selected_value]] <- ref_marker
      }
    }
    
    listObjects[[selected_group_name]]$mapping <- mapping_group
    listObjects[["plate_1"]]$mapping[[selected_group_name]] <- mapping_ref
    
    output$result <- renderPrint({
      list(
        group = listObjects[[selected_group_name]]$mapping
      )
    })
  })
  
  output$table_group_transfo <- renderRHandsontable({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    req(listObject$data.table)
    
    df <- listObject$data.table
    
    rhandsontable::rhandsontable(
      df,
      rowHeaders = NULL,  
      useTypes = TRUE,
      stretchH = "all"
    ) %>%
      rhandsontable::hot_table(allowFillHandle = TRUE, fillHandle = list(direction = "vertical", autoInsertRow = FALSE)) %>%
      rhandsontable::hot_col("Fluo", readOnly = TRUE)%>%
      rhandsontable::hot_col("Arg", readOnly = FALSE)
  })
  
  observeEvent(input$table_group_transfo, {
    
    req(input$table_group_transfo)
    updated_table <- hot_to_r(input$table_group_transfo)
    listObject$data.table<-updated_table
  })
  
  # update group QC
  
  observeEvent(input$groupTransfo,{
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    updateSelectInput(session, "marker_x4", choices = currentGroup$markerFCS)
    updateSelectInput(session, "marker_y4", choices = currentGroup$markerFCS)
    updateSelectInput(session, "marker_x5", choices = currentGroup$markerFCS)
    updateSelectInput(session, "marker_y5", choices = currentGroup$markerFCS)
    
    if ("FSC-A" %in% currentGroup$markerFCS && "FSC-H" %in% currentGroup$markerFCS) {
      updateSelectInput(session, "marker_x3", choices = currentGroup$markerFCS, selected = "FSC-A")
      updateSelectInput(session, "marker_y3", choices = currentGroup$markerFCS, selected = "FSC-H")
    } else {
      updateSelectInput(session, "marker_x3", choices = currentGroup$markerFCS)
      updateSelectInput(session, "marker_y3", choices = currentGroup$markerFCS)
    }
    if ("vaevictis1" %in% currentGroup$markerFCS && "vaevictis2" %in% currentGroup$markerFCS) {
      updateSelectInput(session, "marker_x7", choices = currentGroup$markerFCS,selected="vaevictis1")
      updateSelectInput(session, "marker_y7", choices = currentGroup$markerFCS, selected="vaevictis2")
      
    }else{
      updateSelectInput(session, "marker_x7", choices = currentGroup$markerFCS)
      updateSelectInput(session, "marker_y7", choices = currentGroup$markerFCS)
    }
    
  })
  
  
  
  ################################################################################
  ##################### BEAD EXTRACTION ##########################################
  
  
  ####### STEP 1 : PREPROCESSING
  
  observeEvent(input$uploadTransformation,{
    tryCatch({
      
      listObject$csvFileTransfo<-read.table(input$uploadTransformation$datapath, row.names = 1, header=TRUE, sep=",", quote="")
      listObject$data.table<-listObject$csvFileTransfo
      
      
    }, error = function(e) {
      
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
    
    
  })
  
  
  # Export transformed FCS
  output$exportFCSTransfo <- downloadHandler(
    filename = function() {
      paste0(input$groupTransfo, "_", Sys.Date(), "_transf", ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      file_paths <- list()
      
      for (id in 1:length(listObjects[[input$groupTransfo]]$flow.frames.deconcatenate)) {
        name <- basename(listObjects[[input$groupTransfo]]$original_filenames[id])
        new_name <- paste0("transf_", name)
        file_path <- file.path(temp_dir, new_name)
        
        write.FCS(listObjects[[input$groupTransfo]]$flow.frames.deconcatenate[[id]], file_path)
        file_paths <- c(file_paths, file_path)
      }
      
      zip_path <- file.path(temp_dir, "custom.zip")
      zip(zip_path, files = file_paths, flags = "-j")
      
      file.copy(zip_path, file)
    }
  )
  
  
  groupData <- reactive({
    req(input$groupTransfo)
    
    currentGroup <- listObjects[[input$groupTransfo]]
    req(currentGroup)
    
    if (!is.null(currentGroup$flow.frames.transformed)) {
      return(currentGroup$flow.frames.transformed)
    } else {
      return(currentGroup$flow.frames)
    }
  })
  
  
  output$plot_barplot <- renderPlot({
    tryCatch({
      
      data <- groupData()
      req(data)
      
      channel_data <- data@exprs[, input$marker_x]
      
      hist(
        channel_data,
        col = "#BCEE68",
        border = "#BCEE68",
        xlab = paste("Fluo Expression :",input$marker_x),
        ylab = "nCells",
        main = input$groupTransfo,
        breaks = input$num_breaks
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot no available", cex = 1.2)
    })
  })
  
  
  output$plot_density <- renderPlot({
    tryCatch({
      req(input$marker_x)
      data <- groupData()
      req(data)
      
      if (nrow(data@exprs) > 10000) {
        subset_indices <- sample(nrow(data@exprs), 10000)
        exprs(data) <- data@exprs[subset_indices, ]
      }
      
      plotDens(
        data,
        channels = c(input$marker_x, input$marker_y),
        main = input$groupTransfo
      )
    },  error = function(e) {
      plot.new()
      text(0.5, 0.5, "Plot no available", cex = 1.2)
      
    })
  })
  
  
  # Transformation
  observeEvent(input$applyTransformation, {
    tryCatch({
      req(listObjects)
      
      for (group_name in names(listObjects)) {
        currentGroup <- listObjects[[group_name]]
        
        if(!(is.null(currentGroup) )){
          if (input$compensation) {
            currentGroup$flow.frames <- FlowCIPHE::compensate.CIPHE(currentGroup$flow.frames)
          }
          
          if (input$transfo == "arcsinh") {
            showModal(modalDialog(
              title = "Apply Transformation...",
              tags$div(
                style = "text-align: center;",
                tags$p("Please wait."),
              ),
              footer = NULL,
              easyClose = FALSE
            ))
            arg <- c(listObject$data.table[, 2])
            numeric_rows <- grepl("^[0-9]+(\\.[0-9]+)?$", arg)
            
            filtered_table <- listObject$data.table[
              suppressWarnings(!is.na(as.numeric(as.character(listObject$data.table$Arg)))), 
              , 
              drop = FALSE
            ]
            marker <- c(filtered_table$Fluo)
            
            
            arg <- as.numeric(filtered_table[, 2])  
        
            currentGroup$flow.frames.transformed <- arcsinhCIPHE(currentGroup$flow.frames, marker = marker, arg)
            
            fluo <- c(listObject$data.table[, 1])
            
            for(i in fluo){
              table<-listObject$data.table[which(listObject$data.table$Fluo==i),]
              arg<-table[,2]
    
              if(arg =="none"){
           
                currentGroup$flow.frames.transformed@exprs[,i] <- currentGroup$flow.frames@exprs[,i]
                
              }
              
              
            }
            
          } else if (input$transfo == "None") {
            currentGroup$flow.frames.transformed <- currentGroup$flow.frames
          }
          
          listObjects[[group_name]] <- currentGroup
        }
      }
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  observeEvent(input$validTransformation, {
    tryCatch({
      req(input$groupTransfo)
      
      for (group_name in names(listObjects)) {
        currentGroup <- listObjects[[group_name]]
        
        if(!(is.null(currentGroup) )){
         
          
          if (input$transfo == "arcsinh") {
            showModal(modalDialog(
              title = "Apply Transformation on all files...",
              tags$div(
                style = "text-align: center;",
                tags$p("Please wait."),
              ),
              footer = NULL,
              easyClose = FALSE
            ))
            arg <- c(listObject$data.table[, 2])
            numeric_rows <- grepl("^[0-9]+(\\.[0-9]+)?$", arg)
            filtered_table <- listObject$data.table[
              suppressWarnings(!is.na(as.numeric(as.character(listObject$data.table$Arg)))), 
              , 
              drop = FALSE
            ]
            marker <- filtered_table$Fluo
            arg <- as.numeric(filtered_table[, 2])
            
            currentGroup$flow.frames <- currentGroup$flow.frames.transformed
            
            currentGroup$flow.frames.transformed<- NULL
            
            if (!is.null(currentGroup$flow.frames.deconcatenate)) {
              for (sub_name in names(currentGroup$flow.frames.deconcatenate)) {
                if (input$compensation) {
                  currentGroup$flow.frames.deconcatenate[[sub_name]] <- FlowCIPHE::compensate.CIPHE(currentGroup$flow.frames.deconcatenate[[sub_name]])
                }
                currentGroup$flow.frames.deconcatenate[[sub_name]] <- arcsinhCIPHE(
                  currentGroup$flow.frames.deconcatenate[[sub_name]], marker = marker, arg
                )
              }
            }
          } else if (input$transfo == "None") {
          
            if (!is.null(currentGroup$flow.frames.deconcatenate)) {
              for (sub_name in names(currentGroup$flow.frames.deconcatenate)) {
                if (input$compensation) {
                  currentGroup$flow.frames.deconcatenate <- FlowCIPHE::compensate.CIPHE(currentGroup$flow.frames.deconcatenate)
                }
                currentGroup$flow.frames.deconcatenate[[sub_name]] <- currentGroup$flow.frames.deconcatenate[[sub_name]]
              }
            }
          }
          for (plate_name in names(listObjects)) {
            current_plate <- listObjects[[plate_name]]
            ff_list <- current_plate$flow.frames.deconcatenate
            
            sampled_list <- list()
            
            for (i in seq_along(ff_list)) {
              fcs <- ff_list[[i]]
              total_cells <- nrow(fcs@exprs)
              n <- min(1000, total_cells) 
              
              sampled_indices <- sample(seq_len(total_cells), n)
              sampled_exprs <- fcs@exprs[sampled_indices, , drop = FALSE]
              
              sampled_fcs <- fcs
              sampled_fcs@exprs <- sampled_exprs
              
              sampled_list[[i]] <- sampled_fcs
            }
            
            
            listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
          }
          
          # concat all plates
          concatenated_list_subset <- list()
          
          for (plate_name in names(listObjects)) {
            plate <- listObjects[[plate_name]]
            
            if (!is.null(plate$flow.frames)) {
              concatenated_list_subset[[plate_name]] <- plate$flow.frames
            }
          }
          
          if (length(concatenated_list_subset) > 0) {
            listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
          } else {
            showNotification("Aucun flow.frames valide trouvé pour la concaténation.", type = "error")
          }
          
          listObjects[[group_name]] <- currentGroup
        }
      }
      concatenated_list_subset<-list()
      for (plate_name in names(listObjects)) {
        concatenated_list_subset[[plate_name]]<-listObjects[[plate_name]]$flow.frames
      }
      
      listObject$flow.frames.subset<-concatenate.FCS.CIPHE(concatenated_list_subset,"idPlate")
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  ####### SEPARATE BEADS FROM CELLS
  
  output$plot_histo_clusters <- renderPlot({
    tryCatch({
      req(listObject$flow.frames.subset, input$marker_x2, input$marker_y2)
      data <- listObject$flow.frames.subset
      
      data<-as.data.frame(data@exprs)
      hist(
        data[, input$marker_x2],
        breaks = 100,
        col = "#BCEE68",
        border = "#BCEE68",
        xlab = input$marker_x2,
        main =""
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No histogram available.", cex = 1.2)
    })
  })
  
  output$plot_clusters <- renderPlot({
    tryCatch({
      
      
      
      req(listObject$flow.frames.subset, input$marker_x2, input$marker_y2)
      data <- listObject$flow.frames.subset
      
      if (nrow(data@exprs) > 10000) {
        subset_indices <- sample(nrow(data@exprs), 10000)
        exprs(data) <- data@exprs[subset_indices, ]
      }
      
      df <- as.data.frame(exprs(data))
      x_vals <- df[[input$marker_x2]]
      y_vals <- df[[input$marker_y2]]
      
      plot(x_vals, y_vals, 
           pch = 16, 
           xlab = input$marker_x2, ylab = input$marker_y2,
           col = 'black',
           main = "bead gating", 
           cex = 0.5)
      
      # Dessiner le rectangle si sélection active
      if (!is.null(listObject$brushed_rect)) {
        rect_data <- listObject$brushed_rect
        rect(rect_data$xmin, rect_data$ymin, 
             rect_data$xmax, rect_data$ymax, 
             border = "green", lwd = 2)
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No clusters available.", cex = 1.2)
    })
  })
  
  
  # Gating beads
  observeEvent(input$brush_area, {
    req(input$brush_area)
    
    listObject$brushed_rect <- list(
      xmin = input$brush_area$xmin,
      xmax = input$brush_area$xmax,
      ymin = input$brush_area$ymin,
      ymax = input$brush_area$ymax
    )
    
  })
  
  
  # 
  # 
  # observeEvent(input$table_group_transfo_cells_filled, {
  #   req(input$groupTransfo)
  #   currentGroup <- listObjects[[input$groupTransfo]]
  #   cells_filled <- input$table_group_transfo_cells_filled
  # 
  #   for (cell in 1:nrow(cells_filled)) {
  # 
  #     row <- cells_filled[cell,"row"]
  #     col <- cells_filled[cell,"col"]
  #     value <- cells_filled[cell,"value"]
  # 
  # 
  #     listObject$data.table[row, col] <- value
  # 
  #   }
  # 
  # 
  #   listObjects[[input$groupTransfo]] <- currentGroup
  # })
  
  observeEvent(input$beadAttribution, {
    tryCatch({
      req(listObject$brushed_rect, input$marker_x2, input$marker_y2)
      showModal(modalDialog(
        title = "Bead gating on all files...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait.")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      rect_data <- listObject$brushed_rect
      
      for (name in names(listObjects)) {
        currentGroup <- listObjects[[name]]
        flow_frame <- currentGroup$flow.frames
        
        if (!is.null(flow_frame)) {
          df <- as.data.frame(exprs(flow_frame))
          
          if (input$marker_x2 %in% colnames(df) && input$marker_y2 %in% colnames(df)) {
            gated_df <- df[
              df[[input$marker_x2]] >= rect_data$xmin &
                df[[input$marker_x2]] <= rect_data$xmax &
                df[[input$marker_y2]] >= rect_data$ymin &
                df[[input$marker_y2]] <= rect_data$ymax,
            ]
            
            if (nrow(gated_df) > 0) {
              beads_ff <- flowFrame(as.matrix(gated_df))
              currentGroup$flow.frames.beads <- beads_ff
              
              full_exprs <- exprs(flow_frame)
              keep_idx <- !(
                df[[input$marker_x2]] >= rect_data$xmin &
                  df[[input$marker_x2]] <= rect_data$xmax &
                  df[[input$marker_y2]] >= rect_data$ymin &
                  df[[input$marker_y2]] <= rect_data$ymax
              )
              exprs(flow_frame) <- as.matrix(full_exprs[keep_idx, , drop = FALSE])
              currentGroup$flow.frames <- flow_frame
            } else {
              currentGroup$flow.frames.beads <- NULL
            }
            
          }
        }
        
        deconcat_list <- currentGroup$flow.frames.deconcatenate
        
        if (!is.null(deconcat_list)) {
          for (i in seq_along(deconcat_list)) {
            ff <- deconcat_list[[i]]
            df <- as.data.frame(exprs(ff))
            
            if (!(input$marker_x2 %in% colnames(df)) || !(input$marker_y2 %in% colnames(df))) next
            
            keep_idx <- !(
              df[[input$marker_x2]] >= rect_data$xmin &
                df[[input$marker_x2]] <= rect_data$xmax &
                df[[input$marker_y2]] >= rect_data$ymin &
                df[[input$marker_y2]] <= rect_data$ymax
            )
            
            if (any(keep_idx)) {
              exprs(ff) <- exprs(ff)[keep_idx, ]
            } else {
              exprs(ff) <- exprs(ff)[0, , drop = FALSE]
            }
            
            deconcat_list[[i]] <- ff
          }
          
          currentGroup$flow.frames.deconcatenate <- deconcat_list
        }
        
        listObjects[[name]] <- currentGroup
      }
      removeModal()
      showNotification("Gate applied : beads extracted et removed from files.", type = "message", duration = 4)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Erreur",
        paste("Une erreur est survenue :", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  ########################################################################
  ##################### BEAD QC ##########################################
  
  groupBeads <- reactive({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    req(currentGroup)
    req(currentGroup$flow.frames.beads)
    
    return(currentGroup$flow.frames.beads)
    
  })
  groupBeadsGated <- reactive({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    req(currentGroup)
    req(currentGroup$flow.frames.beads.gated)
    return(currentGroup$flow.frames.beads.gated)
    
  })
  output$plot_QC <- renderPlot({
    tryCatch({
      beads <- groupBeads()
      req(beads)
      bead_data <- exprs(beads)
      
      
      if (nrow(bead_data) > 10000) {
        sampled_indices <- sample(nrow(bead_data), 10000)
        bead_data <- bead_data[sampled_indices, ]
      }
      
      beads_sampled <- flowFrame(bead_data)
      
      plotDens(
        beads_sampled,
        channels = c(input$marker_x3, input$marker_y3),
        main = paste("Beads for Group:", input$groupTransfo),
        xlim=input$X_scale,
        ylim=input$Y_scale
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No beads available ", cex = 1.2)
    })
  })
  
  
  # remove doublets
  observeEvent(input$removeDoublets, {
    tryCatch({
      req(input$groupTransfo)
      
      for (group_name in names(listObjects)) {
        currentGroup <- listObjects[[group_name]]
        req(currentGroup)
        req(currentGroup$flow.frames.beads)
        
        debris_gate <- flowDensity(
          currentGroup$flow.frames.beads,
          channels = c("FSC-A", "SSC-A"),
          position = c(TRUE, TRUE),
          gates = c(quantile(exprs(currentGroup$flow.frames.beads)[,"FSC-A"], 0.02),
                    quantile(exprs(currentGroup$flow.frames.beads)[,"SSC-A"], 0.02))
        )
        currentGroup$flow.frames.beads <- getflowFrame(debris_gate)
        
        # viability_gate <- flowDensity(
        #   currentGroup$flow.frames.beads,
        #   channels = c("Live_Dead"),
        #   position = FALSE
        # )
        # currentGroup$flow.frames.beads <- getflowFrame(viability_gate)
        
        gate.singlet <- openCyto:::.singletGate(currentGroup$flow.frames.beads, channels = c("FSC-A", "FSC-H"))
        not.selected.singlet <- notSubFrame(
          obj = currentGroup$flow.frames.beads,
          channels = c("FSC-A", "FSC-H"),
          filter = gate.singlet@boundaries
        )
        exprs(currentGroup$flow.frames.beads) <- currentGroup$flow.frames.beads@exprs[-not.selected.singlet@index, ]
        
        gate <- flowDensity(
          currentGroup$flow.frames.beads,
          c("FSC-A", "FSC-H"),
          position = c(FALSE, FALSE),
          ellip.gate = TRUE,
          use.percentile = c(TRUE, TRUE),
          percentile = c(0.9, 0.9),
          scale = 0.99
        )
        currentGroup$flow.frames.beads <- getflowFrame(gate)
        
        listObjects[[group_name]] <- currentGroup
      }
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  output$plot_histogram_peak2<- renderPlot({
    tryCatch({
      req(input$groupTransfo)
      currentGroup <- listObjects[[input$groupTransfo]]
      req(currentGroup)
      req(currentGroup$flow.frames.beads)
      beads<-currentGroup$flow.frames.beads
      hist(
        beads@exprs[, input$marker_x4],
        breaks = 100,
        col = "#BCEE68",
        border ="#BCEE68",
        xlab = input$marker_x4,
        main = paste("Beads before gating for :", input$groupTransfo)
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No available", cex = 1.2)
    })
  })
  
  # identify modes
  observeEvent(input$mclustMode, {
    tryCatch({
      for (group_name in names(listObjects)) {
        currentGroup <- listObjects[[group_name]]
        req(currentGroup$flow.frames.beads)
        
        if (!(input$marker_x4 %in% colnames(currentGroup$flow.frames.beads))){
          marker<-currentGroup$mapping[[input$marker_x4]]
          gmm_result <- Mclust(as.matrix(currentGroup$flow.frames.beads@exprs[, marker]), G = input$nbPeaks)
          
        } else{
          
          gmm_result <- Mclust(as.matrix(currentGroup$flow.frames.beads@exprs[, input$marker_x4]), G = input$nbPeaks)
        }
        
        if ("mclust" %in% colnames(exprs(currentGroup$flow.frames.beads))) {
          exprs(currentGroup$flow.frames.beads) <- exprs(currentGroup$flow.frames.beads)[, colnames(exprs(currentGroup$flow.frames.beads)) != "mclust"]
        }
        
        
        currentGroup$flow.frames.beads <- FlowCIPHE::enrich.FCS.CIPHE(currentGroup$flow.frames.beads, gmm_result$classification, "mclust")
        listObjects[[group_name]] <- currentGroup
      }
      
      output$peakChoice <- renderUI({
        
        
        radioButtons(
          inputId = "chosenPeak",
          label = "Select HIGH Peak",
          choices = sort(unique(gmm_result$classification))
        )
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  output$plot_histogram_peak3 <- renderPlot({
    tryCatch({
      req(input$groupTransfo, input$marker_x4)
      
      currentGroup <- listObjects[[input$groupTransfo]]
      req(currentGroup, currentGroup$flow.frames.beads)
      
      df <- as.data.frame(exprs(currentGroup$flow.frames.beads))
      req("mclust" %in% colnames(df), input$marker_x4 %in% colnames(df))
      
      ggplot(df, aes(x = .data[[input$marker_x4]], fill = as.factor(mclust))) +
        geom_histogram(bins = 100, position = "identity", alpha = 0.6) +
        labs(x = input$marker_x4, fill = "mclust") +
        theme_minimal()
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No available", cex = 1.2)
    })
  })
  
  # Choose peak
  observeEvent(input$chosenPeak, {
    
    req(input$chosenPeak)
    
    
    for (name in names(listObjects)){
      currentGroup<-listObjects[[name]]
      
      currentGroup$flow.frames.beads.gated <- currentGroup$flow.frames.beads[
        exprs(currentGroup$flow.frames.beads)[, "mclust"] == input$chosenPeak
      ]
      
      
      listObjects[[name]] <- currentGroup
    }
    req(input$groupTransfo)
    output$plot_peak <- renderPlot({
      tryCatch({
        req(input$groupTransfo)
        currentGroup <- listObjects[[input$groupTransfo]]
        
        req(input$chosenPeak)
        
        
        plotDens(
          currentGroup$flow.frames.beads.gated,
          channels = c(input$marker_x5, input$marker_y4),
          main = paste("Beads peaks n°", input$chosenPeak, "for group :", input$groupTransfo),
          cex = 2.5
        )
        
        
        if (!is.null(currentGroup$flow.frames.beads.gated.regate)) {
          gated_data <- currentGroup$flow.frames.beads.gated.regate@exprs
          points(
            gated_data[, input$marker_x5],
            gated_data[, input$marker_y4],
            col = "green",
            pch = 20
          )
        }
      }, error = function(e) {
        
        plot.new()
        text(0.5, 0.5, "No available", cex = 1.2)
      })
    })
  })
  
  # Remove gate
  observeEvent(input$reset_gate, {
    req(input$groupTransfo)
    tryCatch({
      currentGroup <- listObjects[[input$groupTransfo]]
      
      
      currentGroup$gated_population <- NULL
      currentGroup$flow.frames.beads.gated.regate<-NULL
      listObjects[[input$groupTransfo]] <- currentGroup
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No available", cex = 1.2)
    })
  })
  
  
  output$plot_histogram_peak<- renderPlot({
    tryCatch({
      req(input$groupTransfo)
      currentGroup<-listObjects[[input$groupTransfo]]
      
      req(input$chosenPeak)
      req(req(input$chosenPeak))
      hist(
        currentGroup$flow.frames.beads.gated@exprs[, input$marker_x5],
        breaks = 100,
        col = "#BCEE68",
        border ="#BCEE68",
        xlab = input$marker_x5,
        main =  paste("Beads peaks n°",input$chosenPeak, "for group :" , input$groupTransfo)
      )
      if (!is.null(currentGroup$flow.frames.beads.gated.regate)) {
        hist(
          currentGroup$flow.frames.beads.gated.regate@exprs[, input$marker_x5],
          breaks = 100,
          col = "#BCEE68",
          border ="#BCEE68",
          xlab = input$marker_x5,
          main =  paste("RE-GATE Beads peaks n°",input$chosenPeak, "for group :" , input$groupTransfo)
        )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No available", cex = 1.2)
    })
  })
  
  
  # Gating beads
  observeEvent(input$plot_brush, {
    
    req(input$groupTransfo)
    
    currentGroup <- listObjects[[input$groupTransfo]]
    
    
    brush <- input$plot_brush
    req(brush)
    tryCatch({
      
      x_min <- brush$xmin
      x_max <- brush$xmax
      y_min <- brush$ymin
      y_max <- brush$ymax
      
      
      channel_x <- currentGroup$flow.frames.beads.gated@exprs[, input$marker_x5]
      channel_y <- currentGroup$flow.frames.beads.gated@exprs[, input$marker_y4]
      
      selected_indices <- which(
        channel_x >= x_min & channel_x <= x_max &
          channel_y >= y_min & channel_y <= y_max
      )
      
      
      gated_data <- currentGroup$flow.frames.beads.gated[selected_indices, ]
      
      
      currentGroup$flow.frames.beads.gated.regate <- gated_data
      listObjects[[input$groupTransfo]] <- currentGroup
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  # untransform
  observeEvent(input$UnTransformVizu, {
    req(input$groupTransfo)
    tryCatch({
      
      for (group_name in names(listObjects)) {
        
        currentGroup <- listObjects[[group_name]]
        
        req(currentGroup)
        
        req(currentGroup$flow.frames.beads)
        
        currentGroup$flow.frames.beads <- invers.logicle.CIPHE(currentGroup$flow.frames.beads, 500,markers=colnames(currentGroup$flow.frames.beads))
        
        if (!is.null(currentGroup$flow.frames.beads.gated)) {
          
          currentGroup$flow.frames.beads.gated <- invers.logicle.CIPHE(currentGroup$flow.frames.beads.gated, 500,markers=colnames(currentGroup$flow.frames.beads))
        }
        if (!is.null(currentGroup$flow.frames.beads.gated.regate)) {
          
          currentGroup$flow.frames.beads.gated.regate <- invers.logicle.CIPHE(currentGroup$flow.frames.beads.gated.regate, 500,markers=colnames(currentGroup$flow.frames.beads))
        }
        
        listObjects[[group_name]] <- currentGroup
        
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No  available ", cex = 1.2)
    })
  })
  
  # valide bead for normalization
  observeEvent(input$valideBeads,{
    req(input$groupTransfo)
    
    
    for (group_name in names(listObjects)) {
      currentGroup <- listObjects[[group_name]]
      if (!is.null(currentGroup$flow.frames.beads.gated.regate)) {
        currentGroup$flow.frames.beads.gated <- currentGroup$flow.frames.beads.gated.regate
      }
      
      listObjects[[group_name]] <- currentGroup
    }
    
  })
  
  #### plates concatenation
 
  listObjectsBis <- reactiveValues()
  observe({
    req(listObjects)
    updateSelectInput(session, "plateForConcatenation", choices=names(listObjects))
  })
  
  observeEvent(input$RunPlateConcatenation, {
    req(input$plateForConcatenation)
    
    showModal(modalDialog(
      title = "Concat plates...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait during the concatenation.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      concatenated_list <- list()
      for (plate_name in input$plateForConcatenation) {
        concatenated_list[[plate_name]] <- listObjects[[plate_name]]$flow.frames.deconcatenate
      }
      
      num_elements <- length(concatenated_list[[input$plateForConcatenation[[1]]]])
      final_result <- vector("list", num_elements)
      
      for (i in seq_len(num_elements)) {
        combined_data <- lapply(input$plateForConcatenation, function(plate) {
          concatenated_list[[plate]][[i]]
        })
        final_result[[i]] <- concatenate.FCS.CIPHE(combined_data, "idAcquisition")
      }
      
      names(final_result) <- names(concatenated_list[[input$plateForConcatenation[[1]]]])
      listObjects[[input$plateForConcatenation[[1]]]]$flow.frames.deconcatenate <- final_result
      
  
      plates_to_keep <- input$plateForConcatenation[[1]]
      tmp_list <- reactiveValuesToList(listObjects)
      clean_list <- tmp_list[names(tmp_list) == plates_to_keep]
      
      listObjects <- restoreReactiveValues(clean_list)
      
      removeModal()
      showNotification("Plates successfully concatenated", type = "message")
      
      showModal(modalDialog(
        title = "Information ",
        HTML("
       <div style='margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 5px solid #ffc107;'>

       Once you've concatenated all the plates you want, you need to export the plates containing the concatenates by clicking on export FCS, and select the plates concerned. Then relaunch the app and re-import them.
      </div>
    "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error during concatenation",
        tags$p("An error occurred:", tags$br(), tags$code(e$message)),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  
  ################################################################################
  ##################### NORMALIZATION  ##########################################
  
  output$RDataFile1_ui<-renderUI({
    actionButton(inputId = "Save1", "save preprocessed files", style = " background-color: #FFE500;border-width: 2px; border-color: #FFE500;")
  })
  
  ## Commons markers between ref and others groups
  output$matchMarkerOutput <- renderUI({
    if (input$selector >= 2) {
      groups <- names(listObjects)[!names(listObjects) %in% "plate_1"]
      
      fluidRow(
        box(width = 8,
            HTML("<h4> <span style='background-color: #fdfd96;'>First : </span>  Select markers to normalize </h4>"),
            column(5,
                   selectInput("groupNorm", "Group", choices = c("", groups), multiple = FALSE)
            ),
            column(5,
                   selectInput("commonMarkers", "Common markers with Ref ", choices = c(""), multiple = TRUE)
            )
        )
      )
    }
  })
  
  
  observeEvent(input$Save1, {
    tryCatch({
      
      req(listObjects)
      
      
      showModal(modalDialog(
        
        title = "Save project ...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait."),
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      

      rv_list <- list(
        listObjects = reactiveValuesToList(listObjects),
        listObject = reactiveValuesToList(listObject)
      )
  
      base_filename <- paste0("preprocessed_", input$experimentName)
      save_path <- file.path(save.root, paste0(base_filename, ".rds"))
      

      get_available_filename <- function(path, ext = ".rds") {
        if (!file.exists(path)) return(path)
        i <- 1
        repeat {
          new_path <- sub(paste0(ext, "$"), paste0("_", i, ext), path)
          if (!file.exists(new_path)) return(new_path)
          i <- i + 1
        }
      }
      
  
      final_save_path <- get_available_filename(save_path)
      
  
      saveRDS(rv_list, final_save_path)
      
      
      removeModal()
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Normalization Run
  
  observeEvent(input$Normalize, {
    tryCatch({
      
      showModal(modalDialog(
        title = "Normalization...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait.")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      referenceGroup <- listObjects[["plate_1"]]
      
      withProgress(message = "Normalization in progress...", value = 0, {
        
        
        # Normalization in each group
        for (group_name in names(listObjects)) {
          if (group_name != "plate_1") {
            
            names<-c()
            
            currentGroup <- listObjects[[group_name]]
            
            channels<-currentGroup$commonMarkers
            
            
            
            # If there are unmatch columns between ref and group
            if (length(currentGroup$mapping) != 0){
              
              refMarker<-names(unlist(currentGroup$mapping))
              
              currentMarker<-unlist(currentGroup$mapping)
              
              for(i in length(refMarker)){
                
                if(!is.null(referenceGroup$flow.frames)){
                  
                  colnames(referenceGroup$flow.frames)<-gsub(refMarker[i], currentMarker[i], colnames(referenceGroup$flow.frames))
                }
                if(!is.null(referenceGroup$flow.frames.cells)){
                  
                  colnames(referenceGroup$flow.frames.cells)<-gsub(refMarker[i], currentMarker[i], colnames(referenceGroup$flow.frames.cells))
                }
                
              }
              
              channels<-c(channels,unlist(currentGroup$mapping))
              
            }
            
            
            
            if(!is.null(referenceGroup$flow.frames)){
              
              fcs<-referenceGroup$flow.frames
              
              if (nrow(referenceGroup$flow.frames@exprs) > 10000) {
                
                subset_indices <- sample(nrow(referenceGroup$flow.frames@exprs), 10000)
                
                exprs(fcs) <- fcs@exprs[subset_indices, ]
                
              }
              concat<-as.data.frame(fcs@exprs[,channels])
              
              concat$group<-rep(paste0("plate_1_ref"),nrow(concat))
              
              
            }else{
              
              concat<-data.frame()
            }
            
            fcs<-currentGroup$flow.frames
            
            if (nrow(currentGroup$flow.frames@exprs) > 10000) {
              
              subset_indices <- sample(nrow(currentGroup$flow.frames@exprs), 10000)
              
              exprs(fcs) <- fcs@exprs[subset_indices, ]
            }
            
            
            dfCurrentGroupBefNorm<-as.data.frame(fcs@exprs[,channels])
            
            dfCurrentGroupBefNorm$group<-rep(paste0(group_name,"_before_norm"), nrow(dfCurrentGroupBefNorm))
            
            concat<-rbind(concat, dfCurrentGroupBefNorm)
            
            
            
            harmonization_factors<-c()
            
            if(!is.null(currentGroup$flow.frames)){
              currentGroup$flow.frames.normalized<-currentGroup$flow.frames
              
            }
            
            
            for (x in channels) {
              print(paste("Processing channel:", x, "for group:", group_name))
              
              if (!(x %in% colnames(referenceGroup$flow.frames.beads.gated))){
                print(names(currentGroup$mapping)[which(currentGroup$mapping == x)])
                ref_median <- median(exprs(referenceGroup$flow.frames.beads.gated)[,names(currentGroup$mapping)[which(currentGroup$mapping == x)]])
                
                
              } else{
                ref_median <- median(exprs(referenceGroup$flow.frames.beads.gated)[, x])
                
              }
              group_median <- median(exprs(currentGroup$flow.frames.beads.gated)[, x])
              harmonization_factor <- ref_median / group_median
              harmonization_factors<-c(harmonization_factors,round(harmonization_factor,4))
              
              print(paste("Harmonization factor for channel", x, ":", harmonization_factor))
              
              
              exprs(currentGroup$flow.frames.normalized)[, x] <-
                exprs(currentGroup$flow.frames)[, x] * harmonization_factor
              
              currentGroup$flow.frames.deconcatenate.normalized<-lapply(currentGroup$flow.frames.deconcatenate, function(file){
                
                exprs(file)[, x] <-
                  exprs(file)[, x] * harmonization_factor
                
                return(file)
                
                
              })
              
            }
            
            harmonization_table<-data.frame(channel=channels, Factor=harmonization_factors)
            rownames(harmonization_table) <- harmonization_table$channel
            harmonization_table$channel <- NULL
            
            
            currentGroup$harmonization_table <- harmonization_table
            
            names<-c(names,group_name)
            fcs<-currentGroup$flow.frames.normalized
            if (nrow(currentGroup$flow.frames.normalized@exprs) > 10000) {
              subset_indices <- sample(nrow(currentGroup$flow.frames.normalized@exprs), 10000)
              
              exprs(fcs) <- fcs@exprs[subset_indices, ]
            }
            dfCurrentGroupAfterNorm<-as.data.frame(fcs@exprs[,channels])
            dfCurrentGroupAfterNorm$group<-rep(paste0(group_name,"_after_norm"), nrow(dfCurrentGroupAfterNorm))
            concat<-rbind(concat, dfCurrentGroupAfterNorm)
            
            currentGroup$concat<-concat
            
            
            if(!is.null(currentGroup$flow.frames.cells)){
              fcs<-currentGroup$flow.frames.cells.normalized
              if (nrow(currentGroup$flow.frames.cells.normalized@exprs) > 10000) {
                subset_indices <- sample(nrow(currentGroup$flow.frames.cells.normalized@exprs), 10000)
                
                exprs(fcs) <- fcs@exprs[subset_indices, ]
              }
              dfCurrentGroupAfterNormOnlyCells<-as.data.frame(fcs@exprs[,channels])
              
              dfCurrentGroupAfterNormOnlyCells$group<-rep(paste0(group_name,"_after_norm_onlyCells"), nrow(dfCurrentGroupAfterNormOnlyCells))
              concatOnlyCells<-rbind(concatOnlyCells, dfCurrentGroupAfterNormOnlyCells)
              currentGroup$concatOnlyCells<-concatOnlyCells
            }
            
            currentGroup$channels<-channels
            currentGroup$flow.frames<-currentGroup$flow.frames.normalized
            currentGroup$flow.frames.deconcatenate<-currentGroup$flow.frames.deconcatenate.normalized
            listObjects[[group_name]]<-currentGroup
          }
        }
        
      })
      # Visualization table
      output$groupVizTable<-renderUI({
        groups<-names(listObjects)[! names(listObjects) %in% as.vector("plate_1")]
        selectInput("groupVizTable", "Harmonization table for :", choices = groups, multiple=FALSE, width = 500)
      })
      removeModal()
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  # Display harmonization table
  output$harmTableVizu<-renderUI({
    
    req(input$groupVizTable)
    currentGroup<- listObjects[[input$groupVizTable]]
    req(currentGroup$harmonization_table)
    
    DT::datatable(
      currentGroup$harmonization_table
    ) %>%
      formatStyle(columns = "Factor",
                  background = styleInterval(c(0, 0.8, 2)-1e-6, c("white", "lightblue", "white", "#F08080")))
    
    
  })
  
  
  observe({
    req(input$groupVizTable)
    req(listObjects[[input$groupVizTable]]$channels)
    updateSelectInput(session, "marker_x6",  choices = listObjects[[input$groupVizTable]]$channels)
    
    
  })
  
  observe({
    req(input$groupVizTable)
    currentGroup <- listObjects[[input$groupVizTable]]
    req(currentGroup$concat)
    req(input$cellsOrcellsAndBeads)
    if(input$cellsOrcellsAndBeads == "rawFCS"){
      updateSelectInput(session, "groupVerif", selected=unique(currentGroup$concat$group), choices = unique(currentGroup$concat$group))
    }else{
      updateSelectInput(session, "groupVerif", selected=unique(currentGroup$concatOnlyCells$group), choices = unique(currentGroup$concatOnlyCells$group))
    }
    
  })
  
  
  observe({
    req(input$groupVizTable)
    currentGroup <- listObjects[[input$groupVizTable]]
    req(currentGroup$concat)
    updateSelectInput(session, "cellsOrcellsAndBeads", selected="rawFCS" , choices = c("rawFCS"))
    
    req(currentGroup$concatOnlyCells)
    updateSelectInput(session, "cellsOrcellsAndBeads", selected="onlyCells" , choices = c("rawFCS", "onlyCells"))
  })
  
  
  # Plot after normalization
  output$vizAfterNorm<-renderPlot({
    
    req(input$groupVizTable)
    currentGroup <- listObjects[[input$groupVizTable]]
    req(input$cellsOrcellsAndBeads)
    req(currentGroup$concat)
    if(input$cellsOrcellsAndBeads == "rawFCS"){
      subset<-currentGroup$concat[which(currentGroup$concat$group %in% input$groupVerif),]
    }else{
      subset<-currentGroup$concatOnlyCells[which(currentGroup$concatOnlyCells$group %in% input$groupVerif),]
    }
    ggplot(subset, aes(x=subset[[input$marker_x6]], fill=group)) +
      geom_density(alpha=.25)+
      scale_x_log10() +
      xlab(input$marker_x6)
    
    
  })
  
  
  # Export harmonization table
  
  output$downloadHarmTable<- downloadHandler(
    
    filename = function() {
      paste("harmonization_table-", input$groupVizTable,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(listObjects[[input$groupVizTable]]$harmonization_table, file, quote=FALSE)
    }
  )
  
  # Export transformation
  
  output$exportTransformation <- downloadHandler(
    
    filename = function() {
      paste("transf_table-", input$experimentName, ".csv", sep="")
    },
    content = function(file) {
      write.csv(listObject$data.table, file, quote=FALSE)
    }
  )
  

  ### FlowQC
  
  output$selectMaxPercCut <- renderUI({
    return(
      sliderInput("maxPercCut", "Perc. max of cells removed (maxPercCut)", min=1, max=100, step=5, value=50)
    )
  })
  output$selectFlowCutMarkers <- renderUI({
    req(listObjects)
    currentGroup<-listObjects[["plate_1"]]
    return(selectInput("selectFlowCutMarkers","Markers flowcut : ", choices =currentGroup$markerFCS,selected=currentGroup$markerFCS,multiple=TRUE, width = "60%"))
    
  })
  
  ## Run flowQC 
  observeEvent(input$runflowCut, {
    req(listObjects)
    
    showModal(modalDialog(
      title = "Processing...",
      tags$div(style = "text-align: center;", tags$p("Please wait while the data is being processed.")),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      plate_names <- names(listObjects)
      input_selectFlowCutMarkers <- input$selectFlowCutMarkers
      input_maxPercCut <- input$maxPercCut / 100
      
      result_list <- lapply(plate_names, function(name) {
        currentGroup <- listObjects[[name]]
        if (is.null(currentGroup)) return(NULL)
        
        # Cleaning doublets and margin events
        cleaned_basic <- lapply(currentGroup$flow.frames.deconcatenate, function(x) {
          debris_gate <- flowDensity(
            x, c("FSC-A", "SSC-A"), position = c(TRUE, TRUE),
            gates = c(quantile(exprs(x)[,"FSC-A"], 0.02),
                      quantile(exprs(x)[,"SSC-A"], 0.02))
          )
          x <- getflowFrame(debris_gate)
          
          gate.singlet <- openCyto:::.singletGate(x, channels = c("FSC-A", "FSC-H"))
          not.selected.singlet <- notSubFrame(x, c("FSC-A", "FSC-H"), filter = gate.singlet@boundaries)
          exprs(x) <- x@exprs[-not.selected.singlet@index, ]
          
          gate <- flowDensity(x, c("FSC-A", "FSC-H"),
                              position = c(FALSE, FALSE),
                              ellip.gate = FALSE, use.percentile = c(TRUE, TRUE),
                              percentile = c(0.9, 0.9), scale = 0.99)
          getflowFrame(gate)
        })
        
        currentGroup$flow.frames.cleaned.basic <- cleaned_basic
        
        # If flowcut activated
        if (isTRUE(input$enableFlowCut)) {
          currentGroup$flow.frames.clean <- lapply(cleaned_basic, function(x) {
      
            markers <- which(colnames(x) %in% input_selectFlowCutMarkers)
   
            
            flowCut(x, AllowFlaggedRerun = TRUE, Channels = markers,
                    Plot = "None", MaxPercCut = input_maxPercCut)
          })
        } else {
          currentGroup$flow.frames.clean <- cleaned_basic
        }
        
        list(name = name, group = currentGroup)
      })
      
      for (res in result_list) {
        if (!is.null(res)) {
          listObjects[[res$name]] <- res$group
        }
      }
      
      # MAJ QC plot
      for (plate_name in names(listObjects)) {
        current_plate <- listObjects[[plate_name]]
        ff_list <- current_plate$flow.frames.cleaned.basic
        
        sampled_list <- list()
        for (i in seq_along(ff_list)) {
          fcs <- ff_list[[i]]
          exprs_mat <- tryCatch(exprs(fcs), error = function(e) NULL)
          if (!is.null(exprs_mat) && is.matrix(exprs_mat) && is.numeric(exprs_mat)) {
            n <- min(1000, nrow(exprs_mat))
            sampled_exprs <- exprs_mat[sample(seq_len(nrow(exprs_mat)), n), , drop = FALSE]
            exprs(fcs) <- sampled_exprs
            sampled_list[[i]] <- fcs
          }
        }
        
        listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
      }
      
      concatenated_list_subset <- lapply(listObjects, function(grp) grp$flow.frames)
      listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
      
      removeModal()
      showNotification(
        if (isTRUE(input$enableFlowCut)) {
          "flowCut + cleaning applied to all files"
        } else {
          "Only margin/doublet cleanup applied (flowCut disabled)"
        },
        type = "message"
      )
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  
  observeEvent(input$X_scale2,{
    
    updateSelectInput(session, "X_scale2", choices=c(""), selected=c(""))
    
  })
  
  
  observeEvent(input$Y_scale2,{
    
    updateSelectInput(session, "Y_scale2", choices=c(""), selected=c(""))
    
  })
  
  output$plotQC <- renderPlot({
    tryCatch({
      
      req(input$groupTransfo)
      currentGroup <- listObjects[[input$groupTransfo]]
      plotDens(currentGroup$flow.frames, channels=c(input$marker_x10,input$marker_y10),xlim=input$X_scale2,
               ylim=input$Y_scale2, main="")
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No available", cex = 1.2)
    })
  })

  observe({
    req(input$groupTransfo)
    currentGroup<-listObjects[[input$groupTransfo]]
    updateSelectInput(session, "markerFlowClean", choices=currentGroup$markerFCS, selected=currentGroup$markerFCS)
  })
  
  observe({
    req(listObjects)
    
    lapply(names(listObjects), function(name) {
      currentGroup <- listObjects[[name]]
      
      if (!is.null(currentGroup$flow.frames.clean)) {
        
       
        table_data <- data.frame(
          Perc.Removed.byFlowcut = sapply(seq_along(currentGroup$flow.frames.clean), function(i) {
            f <- currentGroup$flow.frames.clean[[i]]
            if (is.list(f) &&
                !is.null(f$data) &&
                "% of events removed" %in% rownames(f$data)) {
              as.numeric(f$data["% of events removed", 1])
            } else {
              0
            }
          }),
          flowCut = sapply(currentGroup$flow.frames.clean, function(f) {
            is.list(f) &&
              !is.null(f$data) &&
              "% of events removed" %in% rownames(f$data)
          }) 
        )
        rownames(table_data) <- currentGroup$original_filenames
        
        output[[paste0("table_", name)]] <- DT::renderDataTable({
          DT::datatable(table_data, selection = 'single', options = list(pageLength = 5)) %>%
            formatStyle(
              "Perc.Removed.byFlowcut",
              backgroundColor = styleInterval(
                seq(0, 99, length.out = 99),
                colorRampPalette(c("#FFFFFF", "#FF0000"))(100)
              )
            )
        })
        
      
        observeEvent(input[[paste0("table_", name, "_rows_selected")]], {
          selected_row <- input[[paste0("table_", name, "_rows_selected")]]
          
          if (!is.null(selected_row)) {
            f_clean <- currentGroup$flow.frames.clean[[selected_row]]
            is_fc <- is.list(f_clean) &&
              !is.null(f_clean$data) &&
              "% of events removed" %in% rownames(f_clean$data)
            
            selected_data <- if (is_fc) f_clean$data else data.frame(Message = "flowCut not applied")
            output[[paste0("details_", name)]] <- renderPrint({ selected_data })
            
            fcs <- currentGroup$flow.frames.deconcatenate[[selected_row]]
            

            if (!inherits(fcs, "flowFrame")) {
              output[[paste0("plotAfterQC_", name)]] <- renderPlot({
                plot.new()
                text(0.5, 0.5, "", cex = 1.2)
              })
              return()
            }
            
            if (is_fc) {
              worst_channel <- f_clean$data["Worst channel", 1]
              badCells <- f_clean$ind
              
              if (!is.null(worst_channel) && !is.na(worst_channel)) {
                channel <- c(worst_channel)
                names(channel) <- NULL
                
                fcsTransfAfter <- tryCatch({
                  lgcl <- estimateLogicle(fcs, channel)
                  transform(fcs, lgcl)
                }, error = function(e) {
                  fcs
                })
                
                output[[paste0("plotAfterQC_", name)]] <- renderPlot({
                  plotDens(fcsTransfAfter, c("Time", worst_channel), main = "flowCut")
                  
                  if (input$visWithoutGreyPoints == "TRUE"){
                  points(fcsTransfAfter@exprs[badCells, "Time"],
                         fcsTransfAfter@exprs[badCells, worst_channel],
                         col = "darkgrey", pch = 20, cex = 0.5)
                  }
                })
              } else {
                output[[paste0("plotAfterQC_", name)]] <- renderPlot({
                  plot.new()
                  text(0.5, 0.5, "flowCut : channel not defined", cex = 1.2)
                })
              }
            } else {
              output[[paste0("plotAfterQC_", name)]] <- renderPlot({
                plot.new()
                text(0.5, 0.5, "flowCut not applied", cex = 1.2)
              })
            }
          } else {
            output[[paste0("details_", name)]] <- renderPrint({ NULL })
            output[[paste0("plotAfterQC_", name)]] <- renderPlot({ NULL })
          }
        })
        
      }
    })
  })
  
  
  output$flowQCResult_ui <- renderUI({
    req(listObjects)
    
    tabs <- lapply(names(listObjects), function(name) {
      currentGroup <- listObjects[[name]]
      
      if (!is.null(currentGroup$flow.frames.clean)) {
        tabPanel(name,
                 fluidRow(
                   column(4, tags$br(), DTOutput(paste0("table_", name))),
                   column(5, tags$br(), verbatimTextOutput(paste0("details_", name)),
                          actionButton("removeQCSelectedFile", label = "  RemoveQC (Selected file)", icon = icon("trash")),
                          actionButton("removeQCAllGroup", label = "  RemoveQC (Selected group)", icon = icon("trash")),
                          actionButton("validateQC", label = "ValidateQc (All Files)",
                                       icon = icon("check-circle"),
                                       style = "background-color: #BCEE68; border-width: 2px; border-color:  #BCEE68; margin-top: 10px;")
                   ),
                   column(3, checkboxInput("visWithoutGreyPoints", "visualize without grey points", value= FALSE),plotOutput(paste0("plotAfterQC_", name), width = "400px", height = "400px"))
                 )
        )
      } else {
        tabPanel(name, p("No cleaned files available."))
      }
    })
    
    do.call(tabsetPanel, c(id = "flowQCSelectedTab", tabs))
  })
  
  
  observeEvent(input$removeQCSelectedFile, {
 
     name <- input$flowQCSelectedTab
      currentGroup <- listObjects[[name]]
    
    selected_row <- input[[paste0("table_", name, "_rows_selected")]]
    
    if (!is.null(selected_row)) {
      f <- currentGroup$flow.frames.deconcatenate[[selected_row]]
      
   
      currentGroup$flow.frames.clean[[selected_row]] <- f
      listObjects[[name]] <- currentGroup
      showNotification("flowCut removed for selected file", type = "message")
    }
  })
  
  observeEvent(input$removeQCAllGroup, {
    req(input$flowQCSelectedTab)
    
    showModal(modalDialog(
      title = "Removing flowCut...",
      tags$div(style = "text-align: center;", tags$p("Please wait while removing flowCut from the selected group.")),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      name <- input$flowQCSelectedTab
      currentGroup <- listObjects[[name]]
      
      cleanedFrames <- lapply(seq_along(currentGroup$flow.frames.deconcatenate), function(i) {
        f <- currentGroup$flow.frames.deconcatenate[[i]]
      
        getflowFrame(gate)
      })
      
      currentGroup$flow.frames.clean <- cleanedFrames
      listObjects[[name]] <- currentGroup
      
      removeModal()
      showNotification(paste("flowCut removed for group:", name), type = "message")
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while removing flowCut:", e$message),
        easyClose = TRUE
      ))
    })
  })
  

  observeEvent(input$validateQC, {
    showModal(modalDialog(
      title = "Validation...",
      tags$div(style = "text-align: center;", tags$p("Validation des fichiers en cours...")),
      footer = NULL,
      easyClose = FALSE
    ))
    
    tryCatch({
      for (name in names(listObjects)) {
        currentGroup <- listObjects[[name]]
        req(currentGroup$flow.frames.clean)
        
        currentGroup$flow.frames.deconcatenate <- currentGroup$flow.frames.cleaned.basic
        
        
        listObjects[[name]] <- currentGroup
      }
      
      for (plate_name in names(listObjects)) {
        current_plate <- listObjects[[plate_name]]
      
        ff_list <- current_plate$flow.frames.deconcatenate
     
  
        sampled_list <- list()
        for (i in seq_along(ff_list)) {
          fcs <- ff_list[[i]]
          exprs_mat <- tryCatch(exprs(fcs), error = function(e) NULL)
          if (!is.null(exprs_mat) && is.matrix(exprs_mat) && is.numeric(exprs_mat)) {
            n <- min(1000, nrow(exprs_mat))
            sampled_exprs <- exprs_mat[sample(seq_len(nrow(exprs_mat)), n), , drop = FALSE]
            exprs(fcs) <- sampled_exprs
            sampled_list[[i]] <- fcs
          }
        }
        
        listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
      }
      
     
   
      concatenated_list_subset <- list()
      
      for (plate_name in names(listObjects)) {
        plate <- listObjects[[plate_name]]
        
        if (!is.null(plate$flow.frames)) {
          concatenated_list_subset[[plate_name]] <- plate$flow.frames
        }
      }
      
      if (length(concatenated_list_subset) > 0) {
        listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
      } else {
        showNotification("Aucun flow.frames valide trouvé pour la concaténation.", type = "error")
      }
    
      removeModal()
      showNotification("Validation successful: flowCut applied and data updated.", type = "message")
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Erreur lors de la validation",
        tags$p("Une erreur est survenue :", tags$br(), tags$code(e$message)),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  ##############################################################################
  ########################  VAEVICTIS  #########################################
  
  output$RDataFile2_ui<-renderUI({
    actionButton(inputId = "Save2", "save project", style = " background-color: #FFE500;border-width: 2px; border-color: #FFE500;")
  })
  
  observeEvent(input$Save2, {
    tryCatch({
      
      req(listObjects)
      
     
      showModal(modalDialog(
        
        title = "Save project ...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait."),
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      
      rv_list <- list(
        listObjects = reactiveValuesToList(listObjects),
        listObject = reactiveValuesToList(listObject),
        gate_coords2 = gate_coords2(),
        gate_applied2 = gate_applied2(),
        all_gates=all_gates(),
        gate_to_confirm=gate_to_confirm(),
        gate_coords=gate_coords()
      )
      
      base_filename <- paste0("debarcoding_", input$experimentName)
      save_path <- file.path(save.root, paste0(base_filename, ".rds"))
      
      
      get_available_filename <- function(path, ext = ".rds") {
        if (!file.exists(path)) return(path)
        i <- 1
        repeat {
          new_path <- sub(paste0(ext, "$"), paste0("_", i, ext), path)
          if (!file.exists(new_path)) return(new_path)
          i <- i + 1
        }
      }
      
      
      final_save_path <- get_available_filename(save_path)
      
      
      saveRDS(rv_list, final_save_path)
      
      
      removeModal()
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
   
  shinyFileChoose(input, "browseSaved", roots = vae.root, session = session, filetypes=c("json"))
  
  observe({
    if(is.atomic(input$browseSaved)){
      return(NULL)}else{
        if(is.null(input$browseSaved$files)) return(NULL)
      }
    listObject$vae.filename = input$browseSaved$files[["0"]][[length(input$browseSaved$files[["0"]])]]
    listObject$vae.config = paste(vae.root, listObject$vae.filename, sep="")
    listObject$vae.weights = paste(vae.root, paste(substr(listObject$vae.filename, 1, nchar(listObject$vae.filename)-5), ".h5", sep=""),sep = "")

    
    })
  
  output$savedModelName <- renderText({listObject$vae.filename})
  

  
  observeEvent(input$runVaevAll, {
    tryCatch({
    showModal(modalDialog(
      title = "Apply vaevictis model on all files ...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait."),
        
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    print("save RDS...")
    
    if(is.reactivevalues(listObjects)){
      saveRDS(reactiveValuesToList(listObjects), "listObjects.rds")

    }else{
      saveRDS(listObjects, "listObjects.rds")
    }
    
    if(is.reactivevalues(listObject)){
      saveRDS(reactiveValuesToList(listObject), "listObject.rds")
    }else{
      saveRDS(listObject, "listObject.rds")
    }
    
    if(is.reactivevalues(input)){
      saveRDS(reactiveValuesToList(input), "input_valuesAll.rds")
    }else{
      saveRDS(input, "input_valuesAll.rds")
    }

    system("Rscript run_vaevictisAll.R")
    
    print("RDS saved successfully...")
    
    
    loaded <- readRDS("listObject_processed.rds")
    
    
    current_names <- names(isolate(reactiveValuesToList(listObject)))
    for (name in current_names) {
      listObject[[name]] <- NULL
    }
    
    
    for (name in names(loaded)) {
      listObject[[name]] <- loaded[[name]]
    }
    loaded_list <- readRDS("listObject_processedAll.rds")
    

    existing_names <- names(isolate(shiny::reactiveValuesToList(listObjects)))
    for (n in existing_names) {
      listObjects[[n]] <- NULL
    }
    

    for (n in names(loaded_list)) {
      listObjects[[n]] <- loaded_list[[n]]
    }
    updateSelectInput(session, "marker_x7", choices = colnames(listObject$flow.frames.subset),selected="vaevictis1")
    updateSelectInput(session, "marker_y7", choices = colnames(listObject$flow.frames.subset), selected="vaevictis2")
    
    showNotification("vaevictis finished", type = "message")
   
    
    removeModal()
    
    
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })

  
  observeEvent(input$runVaev, {
    tryCatch({
    showModal(modalDialog(
      title = "Vaevictis training...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait."),
        
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    concatenated_list_subset<-list()
    for (plate_name in names(listObjects)) {
      concatenated_list_subset[[plate_name]]<-listObjects[[plate_name]]$flow.frames
    }
    
    listObject$flow.frames.subset<-concatenate.FCS.CIPHE(concatenated_list_subset,"idPlate")

    if(is.reactivevalues(listObject)){
      saveRDS(reactiveValuesToList(listObject), "listObject.rds")
    }else{
      saveRDS(listObject, "listObject.rds")
    }
    
    if(is.reactivevalues(input)){
      saveRDS(reactiveValuesToList(input), "input_values.rds")
    }else{
      saveRDS(input, "input_values.rds")
    }
  
    system("Rscript run_vaevictis.R")
    
    loaded <- readRDS("listObject_processed.rds")
    
  
    current_names <- names(isolate(reactiveValuesToList(listObject)))
    for (name in current_names) {
      listObject[[name]] <- NULL
    }
    

    for (name in names(loaded)) {
      listObject[[name]] <- loaded[[name]]
    }
   
    showNotification("vaevictis finished", type = "message")
    updateSelectInput(session, "marker_x7", choices = colnames(listObject$flow.frames.subset),selected="vaevictis1")
    updateSelectInput(session, "marker_y7", choices = colnames(listObject$flow.frames.subset), selected="vaevictis2")
    
   
    
    removeModal()
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
  })

  output$vaevPlot <- renderPlot({

    req(input$choosePlatevae)
    
    if (input$choosePlatevae == "Subset of all plates") {
      req(listObject$flow.frames.subset)
      
      if (methods::is(listObject$flow.frames.subset, "flowFrame")) {
        if (!is.null(colnames(listObject$flow.frames.subset)) && "vaevictis1" %in% colnames(listObject$flow.frames.subset)) {
          plotDens(listObject$flow.frames.subset, c("vaevictis1", "vaevictis2"), main = "Subset of all plates")
        }
      }
      
    } else {
      currentGroup <- listObjects[[input$choosePlatevae]]
      req(currentGroup)
      

      if (!is.null(currentGroup$flow.frames.deconcatenate)) {
        req(input$chooseFilevae)
        if (input$chooseFilevae %in% names(currentGroup$flow.frames.deconcatenate)) {
        
        flow_obj <- currentGroup$flow.frames.deconcatenate[[input$chooseFilevae]]
        
        if (methods::is(flow_obj, "flowFrame")) {
          if (!is.null(colnames(flow_obj)) && "vaevictis1" %in% colnames(flow_obj)) {
            
            plotDens(flow_obj, c("vaevictis1", "vaevictis2"), main = input$chooseFilevae)
          }
        }
      }
      }
    }
  })
  
  
  
  output$choosePlatevae_ui<-renderUI({
    
    selectInput("choosePlatevae", "Choose Plate", c("Subset of all plates", names(listObjects)))

  })
  
  
  
   output$chooseFilevae_ui<-renderUI({
     
     req(input$choosePlatevae)
     
     if (input$choosePlatevae != "Subset of all plates"){
       
       currentGroup<-listObjects[[input$choosePlatevae]]
       
       selectInput("chooseFilevae", "Choose file", c(names(currentGroup$flow.frames.deconcatenate)))
       
     }

   })
  
  
  ##############################################################################
  ######################## DEBARCODING #########################################

 
   gate_coords <- reactiveVal(data.frame(x = numeric(), y = numeric()))
   all_gates <- reactiveVal(list())
  
   gate_to_confirm <- reactiveVal(NULL)
   

   
   output$scatterPlot <- renderPlot({ 

     req(listObject$flow.frames.subset)
   
     
     if (methods::is(listObject$flow.frames.subset, "flowFrame")) {
       if (!is.null(colnames(listObject$flow.frames.subset)) && "vaevictis1" %in% colnames(listObject$flow.frames.subset)) {
         
         
         df <- as.data.frame(listObject$flow.frames.subset@exprs)
         
         set.seed(42) 

     req(df[["vaevictis1"]])
     if (nrow(df) > 10000) {
       df <- df[sample(nrow(df), 10000), ]
     }

     x <- df[["vaevictis1"]]
     y <- df[["vaevictis2"]]
 
     cols <- densCols(
       x, y,
       colramp = colorRampPalette(c("lightblue", "cornflowerblue", "mediumpurple", "deeppink", "red"))
     )
     

     plot(
       x, y,
       pch   = 16,
       col   = cols,
       cex   = 0.6,             
       xlab  = "vaevictis1",
       ylab  = "vaevictis2",
       main  = "Vaevictis output",
       panel.first =               
         rect(
           par("usr")[1], par("usr")[3],
           par("usr")[2], par("usr")[4],
           col = "grey98", border = NA
         )
     )
     
       }
       
     }
     
     gates <- all_gates()
     colors <- gate_color_map[as.character(seq_along(gates))]
     
     if(!is.null(listObject$flow.frames.subset.barcode)){
     df_barcode <- as.data.frame(listObject$flow.frames.subset.barcode@exprs)
     for (i in seq_along(gates)) {
       if (nrow(gates[[i]]) >= 3) {
         gate_cells <- sum(df_barcode$gate_id == i, na.rm = TRUE)
         if (gate_cells > 0) {
           gate_percentage <- round(gate_cells / nrow(df_barcode) * 100, 2)

           polygon(gates[[i]]$x, gates[[i]]$y,
                   col = adjustcolor(colors[i], alpha.f = 0.3),
                   border = colors[i])

           text(mean(gates[[i]]$x), mean(gates[[i]]$y),
                labels = paste0(gate_percentage, "%"),
                col = "black", cex = 1.2, pos = 3)
         }
       }
     }
     }
     coords <- gate_coords()
     if (nrow(coords) > 0) {
       points(coords$x, coords$y, col = "black", pch = 16, cex = 1.5)
     }
   
   })

   
   output$gate_legend <- renderUI({
     gates <- all_gates()
     colors <- gate_color_map[as.character(seq_along(gates))]
     n_gates<-length(gates)
     if (n_gates == 0) return(NULL)
     
   
     
     tags$div(
       style = "margin-top: 10px;",
       tags$h4("gates "),
       tags$ul(
         lapply(seq_len(n_gates), function(i) {
           tags$li(
             tags$span(style = paste0("display:inline-block; width:15px; height:15px; background-color:", colors[i], "; margin-right:8px; border:1px solid #000;")),
             paste("Gate", i)
           )
         })
       )
     )
   })
   
   
   
   observeEvent(input$plot_click, {
     click_data <- input$plot_click
     req(click_data)
     
     gates <- all_gates()
     clicked_gate <- NULL
     
     for (i in seq_along(gates)) {
       if (nrow(gates[[i]]) >= 3 &&
           point.in.polygon(click_data$x, click_data$y, gates[[i]]$x, gates[[i]]$y) > 0) {
         clicked_gate <- i
         break
       }
     }
     
     if (!is.null(clicked_gate)) {
       gate_to_confirm(clicked_gate)
       showModal(modalDialog(
         title = paste("Delete", clicked_gate, "?"),
         "Are U sure ?",
         footer = tagList(
           modalButton( "Cancel"),
           actionButton("confirm_delete_gate", "Yes", class = "btn-danger")
         )
       ))
     } else {
       coords <- rbind(gate_coords(), data.frame(x = click_data$x, y = click_data$y))
       if (nrow(coords) > 14) coords <- coords[1:14, ]
       gate_coords(coords)
     }
   })
   
   observeEvent(input$confirm_delete_gate, {
     gate_id <- gate_to_confirm()
     req(gate_id)
     
     gates <- all_gates()
     gates <- gates[-gate_id]
     all_gates(gates)
     
     if (!is.null(listObject$flow.frames.subset.barcode)) {
       df_barcode <- as.data.frame(exprs(listObject$flow.frames.subset.barcode))
       
  
       idx_to_clear <- which(!is.na(df_barcode$gate_id) & df_barcode$gate_id == gate_id)
       idx_to_shift <- which(!is.na(df_barcode$gate_id) & df_barcode$gate_id > gate_id)
       
       df_barcode$gate_id[idx_to_clear] <- NA
       df_barcode$gate_id[idx_to_shift] <- df_barcode$gate_id[idx_to_shift] - 1
       
       exprs(listObject$flow.frames.subset.barcode) <- as.matrix(df_barcode)
     }
     
     gate_to_confirm(NULL)
     removeModal()
   })
   

   observeEvent(input$reset_gates, {
     all_gates(list())
     gate_coords(data.frame(x = numeric(), y = numeric()))
     
     if (!is.null(listObject$flow.frames.subset.barcode)) {
       df_barcode <- as.data.frame(exprs(listObject$flow.frames.subset.barcode))
       df_barcode$gate_id <- NA
       exprs(listObject$flow.frames.subset.barcode) <- as.matrix(df_barcode)
     }
   })
   

   observeEvent(input$apply_gate, {
     coords <- gate_coords()
     req(nrow(coords) >= 3)
     df <- as.data.frame(listObject$flow.frames.subset@exprs)
     
     hull_coords <- coords[chull(coords$x, coords$y), ]
     gate_index <- length(all_gates()) + 1
     
     inside_gate <- point.in.polygon(df[["vaevictis1"]], df[["vaevictis2"]], hull_coords$x, hull_coords$y) > 0
     df$gate_id <- NA
     df$gate_id[inside_gate] <- gate_index
     
     if (is.null(listObject$flow.frames.subset.barcode)) {
       listObject$flow.frames.subset.barcode <- listObject$flow.frames.subset
       listObject$flow.frames.subset.barcode <- enrich.FCS.CIPHE(
         listObject$flow.frames.subset.barcode,
         as.matrix(df$gate_id),
         "gate_id"
       )
     } else {
       existing <- as.data.frame(exprs(listObject$flow.frames.subset.barcode))
       existing$gate_id[inside_gate] <- gate_index
       exprs(listObject$flow.frames.subset.barcode) <- as.matrix(existing)
     }
     
     new_gates <- all_gates()
     new_gates[[gate_index]] <- hull_coords
     all_gates(new_gates)
     gate_coords(data.frame(x = numeric(), y = numeric()))
   })
   
  
  
  
  

 ### 
   
   gate_coords2 <- reactiveVal(data.frame(x = numeric(), y = numeric()))
   gate_applied2 <- reactiveVal(NULL)


   output$gate_checkboxes_debarcoding <- renderUI({
     gates <- all_gates()
     if (length(gates) == 0) return(NULL)
     
     checkboxGroupInput(
       inputId = "selected_gates_debarcoding",
       label = "Select",
       choices = paste0("Gate ", seq_along(gates)),
       selected = paste0("Gate ", seq_along(gates)),
       inline = FALSE
     )
   })
   
   output$plot_debarcoding <- renderPlot({
     tryCatch({
       req(listObject$flow.frames.subset.barcode)
       df <- as.data.frame(listObject$flow.frames.subset.barcode@exprs)
       if (!"gate_id" %in% colnames(df)) stop("La colonne 'gate_id' n'existe pas dans le dataframe.")
       
     
       if (!is.null(input$selected_gates_debarcoding) && length(input$selected_gates_debarcoding) > 0) {
         selected_ids <- as.integer(gsub("Gate ", "", input$selected_gates_debarcoding))
         df <- df[df$gate_id %in% selected_ids, ]
       } else {
         return(NULL)
       }
       
    
       if (nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]
       
   
       x_vals <- df[[input$marker_x8]]
       y_vals <- df[[input$marker_y8]]
       
    
       unique_clusters <- sort(na.omit(unique(df$gate_id)))
       cluster_colors <- gate_color_map[as.character(unique_clusters)]
       

       df_full <- as.data.frame(listObject$flow.frames.subset.barcode@exprs)
       xlim_vals <- range(df_full[[input$marker_x8]], na.rm = TRUE)
       ylim_vals <- range(df_full[[input$marker_y8]], na.rm = TRUE)
   
       plot(x_vals, y_vals,
            col = cluster_colors[as.character(df$gate_id)],
            pch = 16,
            xlab = input$marker_x8,
            ylab = input$marker_y8,
            main = "Gate Visualization",
            xlim = xlim_vals,
            ylim = ylim_vals)
       
       
     
 
     
       
       coords <- gate_coords2()
       if (nrow(coords) > 0) {
         points(coords$x, coords$y, col = "black", pch = 19, cex = 1.5)
         if (nrow(coords) >= 3) {
           lines(c(coords$x, coords$x[1]), c(coords$y, coords$y[1]), col = "black", lwd = 2)
         } else if (nrow(coords) >= 2) {
           lines(coords$x, coords$y, col = "black", lwd = 2)
         }
       }
       
       applied_gate <- gate_applied2()
       if (!is.null(applied_gate) && nrow(applied_gate) >= 3) {
         polygon(applied_gate$x, applied_gate$y,
                 col = adjustcolor("lightgrey", alpha.f = 0.3),
                 border = "black")
       }

     }, error = function(e) {
       message("Not plot available : ", e$message)
     })
   })
   
   
  # 
   observeEvent(input$plot_click2, {
     click_data <- input$plot_click2
     req(click_data)
     
     coords <- rbind(gate_coords2(), data.frame(x = click_data$x, y = click_data$y))
     

     if (nrow(coords) > 14) {
       coords <- coords[1:14, ]
     }
     
     gate_coords2(coords)
   })
 
   
   observeEvent(input$apply_gate2, {
     req(listObject$flow.frames.subset.barcode)
     coords <- gate_coords2()
     req(nrow(coords) >= 3)
     
     df <- as.data.frame(listObject$flow.frames.subset.barcode@exprs)
     
     hull_coords <- coords[chull(coords$x, coords$y), ]
     
     inside_gate <- sp::point.in.polygon(
       df[[input$marker_x8]],
       df[[input$marker_y8]],
       hull_coords$x,
       hull_coords$y
     ) > 0 
     
   
     selected_cells <- df[inside_gate == 1 & !is.na(df$gate_id), ]
     
     if (nrow(selected_cells) == 0) {
       showNotification("Aucune cellule trouvée dans la gate", type = "warning")
       return(NULL)
     }
     

     listObject$flow.frames.subset.barcode.bis <- NULL
     listObject$flow.frames.subset.barcode.bis <- listObject$flow.frames.subset.barcode
     exprs(listObject$flow.frames.subset.barcode.bis) <- as.matrix(selected_cells)
     
     gate_applied2(hull_coords)
     gate_coords2(data.frame(x = numeric(), y = numeric()))
   })
   
   

   observeEvent(input$reset_gates2, {
     gate_coords2(data.frame(x = numeric(), y = numeric()))
     gate_applied2(NULL)  
     
     if (!is.null(listObject$flow.frames.subset.barcode.bis)) {
       df <- as.data.frame(exprs(listObject$flow.frames.subset.barcode.bis))
       df$gate_id <- NA
       exprs(listObject$flow.frames.subset.barcode.bis) <- as.matrix(df)
     }
   })
   
   

  
  
  
  
  
 available_markers <- reactiveVal(NULL)
 observe({
   req(listObject$flow.frames.subset.barcode)
   if (is.null(available_markers())) {
     available_markers(colnames(listObject$flow.frames.subset.barcode))
   }
 })
 
output$marker_x8_ui <- renderUI({
  req(available_markers())
  current <- isolate(input$marker_x8)
  column(2,
         selectInput("marker_x8", "X-axis Marker (gate)",
                     choices = available_markers(),
                     selected = if (!is.null(current) && current %in% available_markers()) current else NULL,
                     width = "100%"))
})

output$marker_y8_ui <- renderUI({
  req(available_markers())
  current <- isolate(input$marker_y8)
  column(2,
         selectInput("marker_y8", "Y-axis Marker (gate)",
                     choices = available_markers(),
                     selected = if (!is.null(current) && current %in% available_markers()) current else NULL,
                     width = "100%"))
})


available_markers2 <- reactiveVal(NULL)
observe({
  req(listObject$flow.frames.subset.barcode.bis)
  if (is.null(available_markers2())) {
    available_markers2(colnames(listObject$flow.frames.subset.barcode.bis))
  }
})


output$marker_x9_ui <- renderUI({
  req(available_markers2())
  current <- isolate(input$marker_x9)
  column(2,
         selectInput("marker_x9", "X-axis Marker (gate 2)",
                     choices = available_markers2(),
                     selected = if (!is.null(current) && current %in% available_markers2()) current else NULL,
                     width = "100%"))
})

output$marker_y9_ui <- renderUI({
  req(available_markers2())
  current <- isolate(input$marker_y9)
  column(2,
         selectInput("marker_y9", "Y-axis Marker (gate 2 )",
                     choices = available_markers2(),
                     selected = if (!is.null(current) && current %in% available_markers2()) current else NULL,
                     width = "100%"))
})


  output$plot_debarcodingBis <- renderPlot({
    req(listObject$flow.frames.subset.barcode.bis)
    tryCatch({
      
      req(input$marker_x9, input$marker_y9) 
      
      df <- as.data.frame(listObject$flow.frames.subset.barcode.bis@exprs)
      
   
      
      req(nrow(df) > 0)  
      
      if (!(input$marker_x9 %in% colnames(df)) || !(input$marker_y9 %in% colnames(df))) {
        return(NULL)  
      }
      
      
      df <- as.data.frame(listObject$flow.frames.subset.barcode.bis@exprs)
      
      if (!is.null(input$selected_gates_debarcoding) && length(input$selected_gates_debarcoding) > 0) {
        selected_ids <- as.integer(gsub("Gate ", "", input$selected_gates_debarcoding))
        df <- df[df$gate_id %in% selected_ids, ]
      } else {
        return(NULL)
      }
      
      req(nrow(df) > 0)  
      unique_clusters <- sort(na.omit(unique(df$gate_id)))
      cluster_colors <- gate_color_map[as.character(unique_clusters)]
      if (!"gate_id" %in% colnames(df)) stop("La colonne 'gate_id' n'existe pas dans le dataframe.")


      if (nrow(df) > 10000) df <- df[sample(nrow(df), 10000), ]
   
      
      plot(df[[input$marker_x9]], df[[input$marker_y9]],
           col = cluster_colors[as.character(df$gate_id)],
           pch = 16,
           xlab = input$marker_x9,
           ylab = input$marker_y9,
           main = "Gate 2 Visualization")

      legend("topright", legend = unique_clusters,
             col = cluster_colors[as.character(unique_clusters)],
             pch = 16, title = "Gate")
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "No available", cex = 1.2)
    })
  })


  ### Change X min scale 
  output$X_scale_min_ui<-renderUI({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    
    if (!is.null(currentGroup$flow.frames.barcode)) {
      
      numericInput("X_scale_barcode_min", "X min",
                   value = c(c(min(currentGroup$flow.frames.barcode@exprs[,input$marker_x8]),max(currentGroup$flow.frames.barcode@exprs[,input$marker_x8]))), step=100)
      
    }
  }) 
  

  
  ### Change X max scale
  output$X_scale_max_ui<-renderUI({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    
    if (!is.null(currentGroup$flow.frames.barcode)) {
      
      numericInput("X_scale_barcode_max", "X max",
                   value = c(c(max(currentGroup$flow.frames.barcode@exprs[,input$marker_x8]),max(currentGroup$flow.frames.barcode@exprs[,input$marker_x8]))), step=100)
      
    }
  })
  
  ### Change Y min scale
  output$Y_scale_min_ui<-renderUI({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    
    if (!is.null(currentGroup$flow.frames.barcode)) {
      
      numericInput("Y_scale_barcode_min", "Y min",
                   value = c(c(min(currentGroup$flow.frames.barcode@exprs[,input$marker_y8]),max(currentGroup$flow.frames.barcode@exprs[,input$marker_y8]))), step=100)
      
    }
  })
  
  ### Change Y max scale
  output$Y_scale_max_ui<-renderUI({
    req(input$groupTransfo)
    currentGroup <- listObjects[[input$groupTransfo]]
    
    if (!is.null(currentGroup$flow.frames.barcode)) {
      
      numericInput("Y_scale_barcode_max", "Y max",
                   value = c(c(min(currentGroup$flow.frames.barcode@exprs[,input$marker_y8]),max(currentGroup$flow.frames.barcode@exprs[,input$marker_y8]))), step=100)
      
    }
  })
  
  

  
  
  
  output$renameBarcode_ui<-renderUI({
    
    req(listObject$flow.frames.subset.barcode)
    fluidRow(column(6,HTML("<h4> <span style='background-color: #fdfd96;'>STEP 3 : </span> Reattribute id to each gate (ONLY NUMERIC id)</h4>" )))
    
  })

  

  output$exportMatchBarcode<-downloadHandler(
    
    filename = function() {
      paste("BarcodeIdMatch-", input$groupTransfo,"-",Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(listObjects[[input$groupTransfo]]$dfMatchBarcode, file, quote=FALSE)
    }
  )

  

  output$tableMatchBarcodeId <- renderRHandsontable({
    req(listObject$flow.frames.subset.barcode)
    
    df <- as.data.frame(listObject$flow.frames.subset.barcode@exprs)
    gates <- sort(na.omit(unique(df$gate_id)))
    
    if (length(gates) == 0) return(NULL)
    
    if (is.null(listObject$dfMatchBarcode) ||
        !setequal(gates, listObject$dfMatchBarcode$gate)) {
      
      listObject$dfMatchBarcode <- data.frame(
        gate = gates,
        barcode_id = rep(NA_integer_, length(gates)),
        barcode_label = rep("", length(gates)),  
        stringsAsFactors = FALSE
      )
    }
    
    rhandsontable(listObject$dfMatchBarcode, 
                  rowHeaders = FALSE, 
                  useTypes = TRUE, 
                  stretchH = "all") %>%
      hot_col("gate", readOnly = TRUE, renderer = "
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.background = '#f2f2f2';
        td.style.fontWeight = 'bold';
        td.style.color = '#333';
      }") %>%
      hot_col("barcode_id", type = "numeric") %>%
      hot_col("barcode_label", type = "text")  
  })
  

  output$tableMatchBarcodeId_ui<-renderUI({

    req(listObject$flow.frames.subset.barcode)


      rHandsontableOutput('tableMatchBarcodeId')
  
  })

  
  output$validateBarcode_ui <- renderUI({
    
    req(listObject$dfMatchBarcode)
    
   actionButton("validateBarcode","validate barcoding (All files)", style = " background-color: #BCEE68;border-width: 2px; border-color: #BCEE68;")
   
  })
  
  observe({
    
    req(input$tableMatchBarcodeId)

    updated_df <- hot_to_r(input$tableMatchBarcodeId)

    listObject$dfMatchBarcode <- updated_df
    
 
  })
 
  observeEvent(input$validateBarcode,{
    showModal(modalDialog(
      title = "Debarcoding all files...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait."),
        
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    for (name in names(listObjects)) {
      currentGroup <- listObjects[[name]]
      
      gates <- all_gates()
      df_match <- listObject$dfMatchBarcode  
   
      
      list <- lapply(currentGroup$flow.frames.deconcatenate, function(ff) {
  
        df <- as.data.frame(ff@exprs)
        df$barcodeID <- 0
        for (i in seq_along(gates)) {
          
          gate <- gates[[i]]
       
          if (!is.null(gate) && nrow(gate) >= 3) {
            inside <- sp::point.in.polygon(
              df[["vaevictis1"]],
              df[["vaevictis2"]],
              gate$x,
              gate$y
            ) > 0
            
           
            barcode_row <- df_match[df_match$gate == i, ]
            
            
            if (nrow(barcode_row) == 1) {
        
              df$barcodeID[inside] <- barcode_row$barcode_id
            }
          }
        }
    
        
        ff<-FlowCIPHE::enrich.FCS.CIPHE(ff, df$barcodeID, "barcodeID")
        df <- as.data.frame(ff@exprs)
        
        
        df <- df[df$barcodeID != 0, ]
        
        
        exprs(ff) <- as.matrix(df)
      
        return(ff)
      })
      
      currentGroup$flow.frames.deconcatenate<-list
      listObjects[[name]] <- currentGroup
  
      df_match <- listObject$dfMatchBarcode
      

      if (!is.null(df_match) && all(c("barcode_id", "barcode_label") %in% colnames(df_match))) {
     
        df_match <- df_match[!is.na(df_match$barcode_id) & df_match$barcode_label != "", ]
        

        listObject$barcode_dict <- setNames(as.character(df_match$barcode_label), df_match$barcode_id)
      
      }
      
      
      
    }
   
    for (plate_name in names(listObjects)) {
      current_plate <- listObjects[[plate_name]]
      ff_list <- current_plate$flow.frames.deconcatenate
      
      sampled_list <- list()
      
      for (i in seq_along(ff_list)) {
        fcs <- ff_list[[i]]
        total_cells <- nrow(fcs@exprs)
        n <- min(1000, total_cells) 
        
        sampled_indices <- sample(seq_len(total_cells), n)
        sampled_exprs <- fcs@exprs[sampled_indices, , drop = FALSE]
        
        sampled_fcs <- fcs
        sampled_fcs@exprs <- sampled_exprs
        
        sampled_list[[i]] <- sampled_fcs
      }
      
      
      listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
    }
    
    concatenated_list_subset <- list()
    
    for (plate_name in names(listObjects)) {
      plate <- listObjects[[plate_name]]
      
      if (!is.null(plate$flow.frames)) {
        concatenated_list_subset[[plate_name]] <- plate$flow.frames
      }
    }
    
    if (length(concatenated_list_subset) > 0) {
      listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
    } else {
      showNotification("Aucun flow.frames valide trouvé pour la concaténation.", type = "error")
    }
    
  
    removeModal()
    
  })
  
  output$exportBarcodeMatch_ui<-renderUI({
    req(listObject$dfMatchBarcode)

    downloadButton("exportBarcodeMatch", "Export table",style = "background-color: #BCEE68; color: black; 
                                            font-weight: bold; padding: 10px 15px; 
                                            border-radius: 5px; border-width: 2px; border-color: #BCEE68;")
  }) 
    
    
    
    
    
    output$exportBarcodeMatch <- downloadHandler(
    
    filename = function() {
      paste0("barcodeMatch_", listObject$experimentName, ".xlsx")
    },
    
    content = function(file) {
      
      write.xlsx(listObject$dfMatchBarcode[,c(2,3)], file)
    }
  )

    
    output$informationForDebarcoding <- renderUI({
      req(listObject$dfMatchBarcode)
      tags$div(
        style = "background-color: #e6f2ff; border-left: 5px solid #007acc; padding: 15px; margin-bottom: 20px; text-align: justify;",
        tags$strong("Information:"),
        tags$p(
          HTML(
            "The column <strong>barcode_id</strong> refers to the NUMERICAL values that will be added to the FCS files to represent barcodes. 
        FCS files can only store numeric values, which is why this column is required."
          )
        ),
        tags$p(
          HTML(
            "The column <strong>barcode_name</strong> will not be stored in the FCS files but serves as a human-readable label to help 
        match the <strong>barcode_id</strong> to a corresponding phenotype or condition. You can use any text here."
          )
        ),
        tags$p(
          "You can also export or import this table to preserve and reuse your barcode mappings in future sessions."
        )
      )
    })
    
 
  
  
  #################################################################################
  ################## ANNOTATION ###################################################
  
  
  
  ################################################################################
  ################# SCYAN ANNOTATION #############################################
    
    output$informationAnnotation <- renderUI({

      tags$div(
        style = "background-color: #e6f2ff; border-left: 5px solid #007acc; padding: 15px; margin-bottom: 20px; text-align: justify;",
        tags$strong("Information:"),
        tags$p(
          HTML(
            "This section allows you to <b>annotate a subset of your dataset</b> using reference files. You can choose between two methods: <b>Scyan</b> or <b>Scaffold</b>.<br><br>
        For both approaches, only <b>a subset of 1,000 cells per FCS file</b> is annotated by default. This step is designed to let you <b>quickly preview and validate</b> the annotations.<br><br>
        After annotation, go to the <b>'QC Annotation'</b> section below to visualize the results using <b>UMAP</b> and <b>scatter plots</b>.<br><br>
        If the results look correct, click on <b>'Run on all files'</b> to propagate the annotation across <b>all FCS files and cells</b> in your experiment."
          )
        )
      )
    })
    
  shinyFileChoose(input, "landmarkFCSScyan", roots = workingdir.root, session = session)
  
  output$RDataFile3_ui<-renderUI({
    actionButton(inputId = "Save3", "save annotated files", style = " background-color: #FFE500;border-width: 2px; border-color: #FFE500;")
  })
  
  
  
  observeEvent(input$Save3, {
    tryCatch({
      
      req(listObjects)
      
      
      showModal(modalDialog(
        
        title = "Save project ...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait."),
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      
      rv_list <- list(
        listObjects = shiny::reactiveValuesToList(listObjects),
        listObject = shiny::reactiveValuesToList(listObject)
      )
      
      base_filename <- paste0("annotated_", input$experimentName)
      save_path <- file.path(save.root, paste0(base_filename, ".rds"))
      
      
      get_available_filename <- function(path, ext = ".rds") {
        if (!file.exists(path)) return(path)
        i <- 1
        repeat {
          new_path <- sub(paste0(ext, "$"), paste0("_", i, ext), path)
          if (!file.exists(new_path)) return(new_path)
          i <- i + 1
        }
      }
      
      
      final_save_path <- get_available_filename(save_path)
      
      
      saveRDS(rv_list, final_save_path)
      
      
      removeModal()
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  readKnowledgeTable <- function(pathKnowledgeTable) {
    
    file_extension <- tools::file_ext(pathKnowledgeTable)
    
    
    if (file_extension == "csv") {
      
      table <- read.csv(pathKnowledgeTable)
    } else if (file_extension %in% c("xlsx", "xls")) {
      
      table <- read_excel(pathKnowledgeTable)
      
      
    } else {
      
      stop("Format de fichier non pris en charge")
    }
    
    return(table)
  }
  
  observeEvent(input$knowledgeTable, {

    # Read the knowledge table
    listObject$table <- readKnowledgeTable(input$knowledgeTable$datapath)
 
  })
  
 observe({
   
   req(listObject$table)
   
   # Get all column names
   cols <- colnames(listObject$table)
   
   # Exclude "popID" if present
   cols <- cols[cols != "popID"]
   
   # Update the select input with the filtered column names
   updateSelectInput(session, "markersPresentInKnowledgeTable", 
                     choices = cols, 
                     selected = cols)
   
   
 }
 )

  
  observe({
    
    req(listObjects)
    
    req(listObject$table)
    
    commonMarkers <- intersect(listObjects[["plate_1"]]$markerFCS, colnames(listObject$table))
    
    updateSelectInput(session, "markersUsedForPredictionsScyan", choices =  listObjects[["plate_1"]]$markerFCS, selected= commonMarkers)
  })
  
  
  
  # knowledge table 
  output$knowledgeTable <- DT::renderDataTable({
    
    req(listObject$table)
    
    DT::datatable(
      
      listObject$table,
      
      options = list(
        
        scrollX = TRUE,
        
        autoWidth = FALSE,
        
        columnDefs = list(
          
          list(width = '80px', targets = "_all")
        ),
        pageLength = 10,
        
        lengthMenu = c(10, 25, 50, 100)
      )
    )
  }, server = FALSE)
  
  
  ### Build knowledge table
  observe({
    req(input$landmarkFCSScyan)

    isolate({

      fileinfo <- parseFilePaths(roots = workingdir.root, input$landmarkFCSScyan)
      req(nrow(fileinfo) > 0)

   
      tryCatch({
        showModal(modalDialog(
          title = "Loading landmark FCS...",
          tags$div(
            style = "text-align: center;",
            tags$p("Please wait.")
          ),
          footer = NULL,
          easyClose = FALSE
        ))

        listLandmark <- fileinfo$datapath

        # Name of pop that you want to annotate
        namesLandmarks <- basename(listLandmark)

        # List of files you want to annotate
        listFCS <- as.character(listLandmark)

        # Set landmarks names
        temp.names <- namesLandmarks

        # Add names for each landmark
        filesname <-  as.vector(temp.names)

        # Loop for all files
        i <- 0

        new.flow.frames <- lapply(as.vector(listFCS), function(x) {
          i <<- i + 1

          fcs <- read.FCS(x, emptyValue = FALSE)

          if (dim(fcs)[1] == 1) {
            fcs@exprs <- fcs@exprs[c(1, 1, 1), ]
          }

          return(fcs)
        })


        names(new.flow.frames)<-as.vector(temp.names)

        listObject$landmarkFCS<-new.flow.frames
        
        fileNames <- names(listObject$landmarkFCS)


          metaList <- lapply(fileNames, function(filename) {
            
            file <- listObject$landmarkFCS[[filename]]

            split_name <- strsplit(filename, "-")[[1]]
            
            id <- as.numeric(split_name[1])
            
            pop <- split_name[2]
            
            pop<-gsub('.fcs',"",pop)

            id_vec <- matrix(rep(id, nrow(file@exprs)), ncol = 1)

            file <- enrich.FCS.CIPHE(file, id_vec, "popID")


            listObject$landmarkFCS[[filename]] <<- file


            return(data.frame( id = id, pop = pop, stringsAsFactors = FALSE))
          })


          landmarkMeta <- do.call(rbind, metaList)
          
          listObject$landmarkMeta<-landmarkMeta

        listObject$landmarkFCSConcat<-FlowCIPHE::concatenate.FCS.CIPHE(listObject$landmarkFCS, "idpop")

    
        exprs(listObject$landmarkFCSConcat)<-exprs(listObject$landmarkFCSConcat)[, -c(length(colnames(listObject$landmarkFCSConcat)))]
        
        updateSelectInput(session, "markersPresentInKnowledgeTableBuilding", choices = colnames(listObject$landmarkFCSConcat), selected = colnames(listObject$landmarkFCSConcat))
      
        

        removeModal()
        
      }, error = function(e) {
        
        removeModal()
        
        showNotification(paste("Erreur de lecture :", e$message), type = "error")
        NULL
      })

    })
  })
  output$knowledgeTableBuilder_ui <- renderUI({
    
    req(input$landmarkFCSScyan)
    
    column(
      2,
      
      selectizeInput(
        "markersPresentInKnowledgeTableBuilding",
        "Choose core pannel markers",
        choices = c(""), 
        multiple = TRUE
      ),
      
      actionButton(
        "buildLandmarkFCSScyan",
        "Build knowledge table",
        style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68; margin-top: 10px;"
      ),
      tags$br(),
      uiOutput("exportTable_ui")
    )
  })
  

  output$matchPopId <- DT::renderDataTable({
    req(listObject$landmarkMeta)
    datatable(
      listObject$landmarkMeta,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  
  

  observeEvent(input$buildLandmarkFCSScyan, {
    req(listObject$landmarkFCSConcat)
    
    tryCatch({
      showModal(modalDialog(
        title = "Build scyan knowledge table...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait.")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Convert flowFrame to data.frame and keep original column names
      data <- as.data.frame(exprs(listObject$landmarkFCSConcat), check.names = FALSE)
      
      # Subset to selected markers (preserving original names)
      df <- data[, input$markersPresentInKnowledgeTableBuilding, drop = FALSE]
      
      # Robust normalization function (centered on median, scaled by MAD, clipped to [-1, 1])
      robust_normalize <- function(col) {
        med <- median(col, na.rm = TRUE)
        mad_val <- mad(col, na.rm = TRUE)
        
        if (mad_val == 0) {
          return(rep(0, length(col)))
        }
        
        scaled <- (col - med) / mad_val
        scaled <- pmax(pmin(scaled, 1), -1)
        return(scaled)
      }
      
      # Apply robust normalization to each marker
      scaled_df <- as.data.frame(lapply(df, robust_normalize), check.names = FALSE)
      
      # Add population annotation
      scaled_df$popID <- data$popID
      
      # Compute median normalized expression per population
      scaled_medians <- aggregate(. ~ popID, data = scaled_df, FUN = median, na.rm = TRUE)
      
      # Store result
      listObject$table <- scaled_medians
      
     
      removeModal()
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error :", e$message), type = "error")
      NULL
    })
  })
  output$exportTable_ui<-renderUI({
    req(input$landmarkFCSScyan)
    req(listObject$table)
    downloadButton("exportTable", "Export knowledge Table",style = "background-color: #BCEE68; color: black; 
                                            font-weight: bold; padding: 10px 15px; 
                                            border-radius: 5px; border-width: 2px; border-color: #BCEE68;")
  })
  
  output$exportTable <- downloadHandler(
    
    filename = function() {
      paste0("knowledgeTable_", listObject$experimentName, ".xlsx")
    },
    
    content = function(file) {
      
      write.xlsx(listObject$table, file)
    }
  )
  
  ##### RUN SCYAN
  
  
  preparMatrix<-function(flowFrame,markersInFCSFile, markersInModel, algo){
    
    # Extract expression matrix
    matrix<- as.data.frame(flowFrame@exprs)

    markersInFCSFile<-c(markersInFCSFile)
    
    markersInModel<-c(markersInModel)
    
    # Keep only markers you want
    df <- matrix[, markersInFCSFile]
    
    # Give features names of model to our expression matrix
    colnames(df)<-markersInModel
    

    return(df)
  }
  
  observe({
    req(listObject$table)
    updateSelectInput(session, "popToAnnotate", choices=colnames(listObject$table))
  })
 
  observeEvent(input$annotate_data_with_Scyan, {
    showModal(modalDialog(
      title = "Run Annotation...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait. (Patiently :) )"),
        
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    tryCatch({
      
      for (plate_name in names(listObjects)) {
        current_plate <- listObjects[[plate_name]]
        ff_list <- current_plate$flow.frames.deconcatenate
        
        sampled_list <- list()
        
        for (i in seq_along(ff_list)) {
          fcs <- ff_list[[i]]
          total_cells <- nrow(fcs@exprs)
          n <- min(1000, total_cells) 
          
          sampled_indices <- sample(seq_len(total_cells), n)
          sampled_exprs <- fcs@exprs[sampled_indices, , drop = FALSE]
          
          sampled_fcs <- fcs
          sampled_fcs@exprs <- sampled_exprs
          
          sampled_list[[i]] <- sampled_fcs
        }
        
        
        listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
   
      }
      
     
        concatenated_list_subset<-list()
        for (plate_name in names(listObjects)) {
          concatenated_list_subset[[plate_name]]<-listObjects[[plate_name]]$flow.frames
        }
        
        listObject$flow.frames.subset<-concatenate.FCS.CIPHE(concatenated_list_subset,"idPlate")
        
  
     
      data <- preparMatrix(listObject$flow.frames.subset, input$markersUsedForPredictionsScyan, input$markersPresentInKnowledgeTable, "scyan")
      
      table<-listObject$table[,input$markersPresentInKnowledgeTable]
      
      table<-as.data.frame(table)
      
      rownames(table)<-listObject$table[[input$popToAnnotate]]
      
      # Build the AnnData object
      anndataScyan <- builAnnDataObject(data)
      
      # If "idAcquisition" exists, use it as batch information
      if ("idAcquisition" %in% colnames(anndataScyan$obs)) {
        batch_vector <- anndataScyan$obs$idAcquisition
        anndataScyan$obs$batch <- batch_vector
        batch_col <- "batch"
      } else {
        batch_col <- NULL
      }
      
      # # Preprocessing (e.g., scaling)
      # s$preprocess$scale(anndataScyan)
      # 
      # Train the model with optional batch correction
      model <- runModel(anndataScyan, table, input$std, input$lr, batch_col)
      
      # Save the trained model inside listObject
      listObject$scyan_model <- model
      
      # Predict cell populations using the trained model
      test(model)
      
      # Extract prediction results from AnnData obs
      resultsScyan <- as.data.frame(anndataScyan$obs)
      
      # Handle predicted population labels
      pop_names <- as.character(resultsScyan$scyan_pop)
      pop_names[is.na(pop_names)] <- "0_NI"  # Assign default label for unclassified cells
      
      # Extract population numbers (assumes labels like "1_Tcell", "2_Bcell", etc.)
      pop_numbers <- as.integer(gsub("_.*", "", pop_names))
      
      # Create a dictionary mapping pop numbers to labels
      dico_pop <- setNames(pop_names, pop_numbers)
      
      # Set dynamic column name if needed (to avoid overwriting)
      column_name <- "popIDScyan"
      existing_columns <- colnames(listObject$flow.frames.subset)
      count <- sum(grepl("^popIDScyan", existing_columns))
      if (count > 0) {
        column_name <- paste0("popIDScyan", count + 1)
      }
      
      # Enrich FCS subset with predicted population numbers
      listObject$flow.frames.subset <- FlowCIPHE::enrich.FCS.CIPHE(
        listObject$flow.frames.subset, pop_numbers, column_name
      )
      
      # Save the dictionary of populations
      listObject$dico_pop <- dico_pop
      
      
      output$colorBy_ui <- renderUI({
        req(listObject$flow.frames.subset)
        req(listObject$dico_pop)
        
        selectInput("colorBy", "Color by : ", choices=c("None", colnames(listObject$flow.frames.subset)), selected="None", width="30%")
      })
      
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  
  
  ################################################################################
  ################# SCAFFOLD ANNOTATION ##########################################
  
  # je trouve pas ca pratique si y a plusieurs fichiers à prendre en meme temps faut cliquer 
  shinyFileChoose(input, "loadlandmark", roots = workingdir.root, session = session)
  
  shinyFileChoose(input, "oldScaffoldMap", roots =scaffold.root, session = session)
  
  output$scaffoldMap_ui<-renderUI({
    actionButton("scaffoldMap", "Build Scaffold Map",  style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;")     # Action button to build scaffod map
  })
  
 
  observeEvent(input$clustering, {
    tryCatch({
      showModal(modalDialog(
        title = "CLARA clustering...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait."),
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      if(is.null(listObject$flow.frames.subset)){
        concatenated_list_subset<-list()
        for (plate_name in names(listObjects)) {
          concatenated_list_subset[[plate_name]]<-listObjects[[plate_name]]$flow.frames
        } 
        
        listObject$flow.frames.subset<-concatenate.FCS.CIPHE(concatenated_list_subset,"idPlate")
      }
      
      list<-list()
      
      list<-list(listObject$flow.frames.subset)

     
      
      listResult <- claraClustering(list, input$marker_clustering,input$clustering_parameter)
      
      listObject$flow.frames.subset<-listResult[[1]]
  
     listObject$clusteringColumn<-"CLARA"
      
      removeModal()
      
      output$messageClustering <- renderUI({
        tags$div(
          style = "background-color: #e6ffe6; border-left: 5px solid #33cc33; padding: 12px; margin-bottom: 15px;",
          tags$strong("Clustering completed successfully!"),
          tags$p("Your dataset has been clustered using the CLARA algorithm.")
        )
      })
    }, error = function(e) {
      
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
      
    })
    
  })
  
  ## Build scaffold Map
  
  observeEvent(input$scaffoldMap, {
    tryCatch({
      showModal(modalDialog(
        title = "Build Scaffold Map...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait."),
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      # Use shinyFiles to get selected landmark files
      fileinfo <- parseFilePaths(workingdir.root,input$loadlandmark)
    
      if (nrow(fileinfo) == 0) {
        showNotification("Please, add landmarks (reference pop)", type = "message")
      } else {
        
        # If clustering is already done, set clustering parameters
        if (input$alreadyClustered == TRUE) {
          listObject$clusteringColumn <- input$clusteringColumn
          listObject$marker_clustering <- input$marker_scaffoldmap        
        }
        
       
        # Apply clustering table transformation on all subset files
        listObject$flow.frames.subset.tab <- builddCSVTab(listObject$flow.frames.subset, listObject$clusteringColumn)
        list<-list()
        list<-list(listObject$flow.frames.subset.tab)
        listObject$flow.frames.subset.tab <- list
        # Initialize object for gated flow frames
        gated.flow.frames <- NULL
        
        # Get landmark file paths
        listLandmark <- fileinfo$datapath
        
   
        
        # Get landmark file names
        namesLandmarks <- basename(listLandmark)
        
        # Convert to character vector
        listFCS <- as.character(listLandmark)
        
        # Temporary names
        temp.names <- namesLandmarks
        
        # Combine old names with new landmark names
        filesname <- c(names(gated.flow.frames), as.vector(temp.names))
        
        # Read each landmark FCS file
        i <- 0
        new.flow.frames <- lapply(as.vector(listFCS), function(x) {
          i <<- i + 1
          
          fcs <- read.FCS(x, emptyValue = FALSE)
          
          # Duplicate row if the FCS file has only one row
          if (dim(fcs)[1] == 1) {
            fcs@exprs <- fcs@exprs[c(1, 1, 1), ]
          }
          
          return(fcs)
        })
        
        # Add new flow frames to gated list
        gated.flow.frames <- c(gated.flow.frames, new.flow.frames)
        
        names(gated.flow.frames) <- filesname
        
        # Markers used for scaffold annotation
        col.names <- input$marker_clustering
        # Initialize and assign clustered files
        clusteredFiles <- NULL
        clusteredFiles <- listObject$flow.frames.subset.tab
        names(clusteredFiles) <- c(input$experimentName)
        
        map.clusteredFiles.names <- c(input$experimentName)
        
        clustedFiles <- clusteredFiles
        
        # Load C++ code to increase C stack limit
        sourceCpp("forceatlas2.cpp")
       
        

        fcs_names <- basename(filesname)
        
 
        listObject$pop_dict <- setNames(
 
          gsub("\\.fcs", "", trimws(sub(".*?-", "", fcs_names))),
       
          sub("-.*", "", fcs_names)
        )
        
      
          
        # Run scaffold analysis
        result <- runn_analysis_gated(
          gated.flow.frames,             # Landmarks
          clustedFiles,                  # Clustered files to annotate
          outputDir = scaffold.root,           # Output directory
          map.clusteredFiles.names,      # Names of clustered files
          FALSE,                         # Boolean flag
          col.names,                     # Markers used for annotation
          col.names,                     # Markers used again for annotation
          col.names.inter_cluster = NULL,
          ew_influence = NULL,
          inter_cluster.weight_factor = 0.7,
          inter.cluster.connections = TRUE,
          overlap_method = "repel"
        )
        
        # Display success message
        output$messageBuildScaffoldMap <- renderText("Scaffold map created successfully !")
        
        # Save result to listObject
        listObject$Map <- result
      }
      removeModal()
      
      
      output$messageBuildScaffoldMap <- renderUI({
        tags$div(
          style = "background-color: #e6f7ff; border-left: 5px solid #3399ff; padding: 12px; margin-bottom: 15px;",
          tags$strong("Scaffold map created successfully!"),
          tags$p("The landmark-based Scaffold map has been generated and stored in memory.")
        )
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  # Run scaffold 
  observeEvent(input$RunScaffoldAnnotation, {
    tryCatch({
      showModal(modalDialog(
        title = "Run Scaffold Annotation...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait."),
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      # Use shinyFileChoose to access selected scaffold map file
      fileinfo <- parseFilePaths(scaffold.root, input$oldScaffoldMap)
      
      # Load scaffold map if a file has been selected
      if (nrow(fileinfo) > 0) {
        listObject$Map <- my_load(fileinfo$datapath)
      }
      
      # Proceed if the scaffold map is successfully loaded
      if (!is.null(listObject$Map)) {
        scaffold <- listObject$Map
        
        # File names to use in the export
        list2 <-  c(1)
        list1 <-  c(1)
        
        
        list<-list()
        list<-list(listObject$flow.frames.subset)

     

        # Perform scaffold annotation
        res <- scaffold_events_export(
          list1,
          list2,
          list,
          scaffold,
          listObject$clusteringColumn
        )
   
        
        #
        # Update transformed flow frames with annotation results
        listObject$flow.frames.subset<- res[[1]]
        
       
      }
      
      removeModal()
      output$messageRunScaffoldAnnotation <- renderUI({
        tags$div(
          style = "background-color: #fff5e6; border-left: 5px solid #ff9900; padding: 12px; margin-bottom: 15px;",
          tags$strong("Scaffold annotation complete!"),
          tags$p("Cluster identities were successfully projected onto your experimental data.")
        )
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })

  



  
  
  ###############################################################################
  ################ QC Annotation ################################################
  observe({
    req(listObjects)
    updateSelectInput(session,"markersUMAP", choices=listObjects[["plate_1"]]$markerFCS, selected=listObjects[["plate_1"]]$markerFCS)
  })
  
  observe({
    req(listObjects[["plate_1"]]$flow.frames)
    updateSelectInput(session,"marker_clustering", choices=listObjects[["plate_1"]]$markerFCS, selected=listObjects[["plate_1"]]$markerFCS)
  })
  
  observe({
    req(listObjects[["plate_1"]]$flow.frames)
    updateSelectInput(session,"clusteringColumn", choices=listObjects[["plate_1"]]$markerFCS)
  })
  
  
  # 
  observeEvent(input$runUMAP,{
    tryCatch({
    showModal(modalDialog(
      title = "Run UMAP...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait."),
        
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    req(listObject$flow.frames.subset)

 
    umap_result <- uwot::umap(
      listObject$flow.frames.subset@exprs[, input$markersUMAP],
      n_neighbors = 15,
      min_dist = 0.1,
      n_threads = parallel::detectCores() - 1,
      verbose = TRUE
    )
  
    listObject$flow.frames.subset <- FlowCIPHE::enrich.FCS.CIPHE(
      listObject$flow.frames.subset,
      umap_result[,1],
      "umap1"
    )
    listObject$flow.frames.subset <- FlowCIPHE::enrich.FCS.CIPHE(
      listObject$flow.frames.subset,
      umap_result[,2],
      "umap2"
    ) 
    output$colorBy_ui <- renderUI({
      req(listObject$flow.frames.subset)
     
      selectInput("colorBy", "Color by : ", choices=c("None", colnames(listObject$flow.frames.subset)), selected="None", width="30%")
    })
    removeModal()
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
  })
  
  output$QCAnnotation_ui<-renderUI({
    plotOutput("QCAnnotation", width="500px", height="500px")
   
  })
  color_mapping <- reactive({
    req(input$colorBy)
    df <- as.data.frame(listObject$flow.frames.subset@exprs)
    if (!(input$colorBy %in% colnames(df))) return(NULL)
    
    groups <- sort(unique(df[[input$colorBy]]))
    pal <- rainbow(length(groups))
    names(pal) <- groups
    pal
  })
  

  output$QCAnnotation <- renderPlot({
    req(listObject$flow.frames.subset)
    df <- as.data.frame(listObject$flow.frames.subset@exprs)
    req(input$colorBy)
    
    if (input$colorBy == "None") {
      plot(
        df[["umap1"]],
        df[["umap2"]],
        pch = 16,
        col = densCols(df$umap1, df$umap2),
        xlab = "umap1",
        ylab = "umap2",
        main = ""
      )
      return()
    }
    
    colors <- color_mapping()
    plot(
      df[["umap1"]],
      df[["umap2"]],
      pch = 16,
      col = colors[as.character(df[[input$colorBy]])],
      xlab = "umap1",
      ylab = "umap2",
      main = "",
      cex = 0.5
    )
  })

  output$gate_legend2 <- renderUI({
    pal <- color_mapping()
    if (is.null(pal)) return(NULL)
    
    tags$div(
      style = "margin-top: 10px;",
      tags$h4(paste("Legend:", input$colorBy)),
      tags$ul(
        style = "list-style-type: none; padding-left: 0;",
        lapply(names(pal), function(g) {
          tags$li(
            tags$span(style = paste0(
              "display:inline-block; width:15px; height:15px; background-color:", pal[[g]],
              "; margin-right:8px; border:1px solid #000; vertical-align:middle;"
            )),
            tags$span(style = "vertical-align:middle;", g)
          )
        })
      )
    )
  })

  output$selected_annotation_column_ui <- renderUI({
    req(listObject$flow.frames.subset)
    
    df <- as.data.frame(listObject$flow.frames.subset@exprs)
    

    annotation_cols <- colnames(df)
    

    selectInput(
      inputId = "selected_annotation_column",
      label = "Select annotation column:",
      choices = c("None",annotation_cols),
      selected = "None",
      width='30%'
    )
  })


  output$annotation_table <- DT::renderDataTable({
    req(input$selected_annotation_column)
    
    if (input$selected_annotation_column != "None") {
      df <- as.data.frame(listObject$flow.frames.subset@exprs)
      
      ann_col <- input$selected_annotation_column
      if (!ann_col %in% colnames(df)) return(NULL)
      
      total_cells <- nrow(df)
      ann_counts <- table(df[[ann_col]])
      ann_names <- names(ann_counts)
      ann_percentages <- round((ann_counts / total_cells) * 100, 3)
      
      ann_df <- data.frame(
        Annotation = ann_names,
        Percentage = as.numeric(ann_percentages),
        stringsAsFactors = FALSE
      )
      
      if (ann_col %in% c("popIDScyan", "popIDScaffold") && !is.null(listObject$pop_dict)) {
        pop_label <- sapply(ann_names, function(popId) {

          listObject$pop_dict[[as.numeric(popId)]]
        })
        ann_df$pop_label <- pop_label
      }
      
 
      
      rownames(ann_df) <- ann_df$Annotation
      ann_df$Annotation <- NULL
      
      listObject$annotation_table <- ann_df
      
      DT::datatable(
        ann_df,
        selection = "multiple",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = list(list(width = 'auto', targets = "_all"))
        ),
        class = 'cell-border stripe compact'
      ) %>%
        DT::formatStyle(
          "Percentage",
          backgroundColor = styleInterval(
            seq(0, 99, length.out = 99),
            colorRampPalette(c("#FFFFFF", "#FF0000"))(100)
          )
        )
    }
  })
 

  output$marker_x11_ui<-renderUI({
    req(listObject$flow.frames.subset)
    selectInput("marker_x11", "X-Marker", choices=colnames(listObject$flow.frames.subset))

  })


  output$marker_y11_ui<-renderUI({
    req(listObject$flow.frames.subset)
    selectInput("marker_y11", "Y-Marker", choices=colnames(listObject$flow.frames.subset))

  })
  

  observeEvent(input$annotation_table_rows_selected,{
    tryCatch({
      selected <- input$annotation_table_rows_selected  
      
      if (length(selected) > 0) {
        
        selected_rownames <- rownames(listObject$annotation_table)[selected]  
        
        numeric_part <- selected_rownames
  
        data<-as.data.frame(listObject$flow.frames.subset@exprs)
        
        data<-data[which(data[[input$selected_annotation_column]] %in% numeric_part),]
 
        data<-as.matrix(data)
        
        listObject$flow.frames.subset.pop<-listObject$flow.frames.subset
        
        exprs(listObject$flow.frames.subset.pop)<-data
        
        
      } else {
        return("")
      }
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  output$verifAnnotationPlot <- renderPlot({
    req(listObject$flow.frames.subset.pop, input$marker_x11, input$marker_y11)
    df <- as.data.frame(listObject$flow.frames.subset.pop@exprs)
    ann_col <- input$selected_annotation_column
    
    if (!ann_col %in% colnames(df)) return()
    
    groups <- unique(df[[ann_col]])
    colors <- rainbow(length(groups))
    names(colors) <- groups
    
    plot(df[[input$marker_x11]],
         df[[input$marker_y11]],
         pch = 16,
         col = colors[as.character(df[[ann_col]])],
         xlab = input$marker_x11,
         ylab = input$marker_y11,
         main = ""
    )
  })
  color_mapping2 <- reactive({
    req(input$selected_annotation_column)
    req(listObject$flow.frames.subset.pop)
    df <- as.data.frame(listObject$flow.frames.subset.pop@exprs)
    if (!(input$selected_annotation_column %in% colnames(df))) return(NULL)
    
    groups <- sort(unique(df[[input$selected_annotation_column]]))
    pal <- rainbow(length(groups))
    names(pal) <- groups
    pal
  })
  
  output$gate_legend3 <- renderUI({
    pal <- color_mapping2()
    if (is.null(pal)) return(NULL)
    
    ann_col <- input$selected_annotation_column
    use_pop_labels <- ann_col %in% c("popIDScyan", "popIDScaffold") && !is.null(listObject$pop_dict)
    use_barcode_labels <- ann_col == "barcodeID" && !is.null(listObject$barcode_dict)
    
    tags$div(
      style = "margin-top: 10px;",
      tags$h4(paste("Legend:", ann_col)),
      tags$ul(
        style = "list-style-type: none; padding-left: 0;",
        lapply(names(pal), function(g) {
          label <- g
          if (use_pop_labels) {
            label_value <- listObject$pop_dict[[as.numeric(g)]]
            if (!is.null(label_value)) {
              label <- paste0(g, " - ", label_value)
            }
          } else if (use_barcode_labels) {
            label_value <- listObject$barcode_dict[[as.character(g)]]
            if (!is.null(label_value)) {
              label <- paste0(g, " - ", label_value)
            }
          }
          
          tags$li(
            tags$span(style = paste0(
              "display:inline-block; width:15px; height:15px; background-color:", pal[[g]],
              "; margin-right:8px; border:1px solid #000; vertical-align:middle;"
            )),
            tags$span(style = "vertical-align:middle;", label)
          )
        })
      )
    )
  })
  
  
  
  
  
  
  observeEvent(input$runScyanAll,{
    
    tryCatch({
      showModal(modalDialog(
        title = "Run Annotation for all files...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait. (Patiently :) )"),
          
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      concatenated_list_subset<-list()
      for (plate_name in names(listObjects)) {
        currentGroup<-listObjects[[plate_name]]
        
        currentGroup$flow.frames.deconcatenate<-lapply(currentGroup$flow.frames.deconcatenate, function (x){
          
          data <- preparMatrix(x, input$markersUsedForPredictionsScyan, input$markersPresentInKnowledgeTable, "scyan")
          table<-listObject$table[,input$markersPresentInKnowledgeTable]
          table<-as.data.frame(table)
          rownames(table)<-listObject$table[[input$popToAnnotate]]
          anndataScyan <- builAnnDataObject(data)

          if ("idAcquisition" %in% colnames(anndataScyan$obs)) {
            batch_vector <- anndataScyan$obs$idAcquisition
            anndataScyan$obs$batch <- batch_vector
            batch_col <- "batch"
          } else {
            batch_col <- NULL
          }
          model <- runModel(anndataScyan, table,input$std, input$lr,batch_key=batch_col)
          test(model)
          resultsScyan <- as.data.frame(anndataScyan$obs)
          
          pop_names <- resultsScyan$scyan_pop
          
          pop_names <- as.character(pop_names)
          
          pop_names[is.na(pop_names)] <- "0_NI"
          
          pop_numbers <- as.integer(gsub("_.*", "", pop_names))
          
          dico_pop <- setNames(pop_names,pop_numbers)
          
          column_name <- "popIDScyan"
          
          existing_columns <- colnames(x)
          
          count <- sum(grepl("^popIDScyan", existing_columns))
          
          if (count > 0) {
            column_name <- paste0("popIDScyan", count + 1)
          }
          
          
          file <- FlowCIPHE::enrich.FCS.CIPHE(x, pop_numbers, column_name)
          
          
          
          
          return(file)
          
        })
        
        
      }
      
      
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
    
    
  })
  
  
  
  ## RunScaffoldAll
  observeEvent(input$runScaffoldAll,{
    
    tryCatch({
    # showModal(modalDialog(
    #   title = "Run Scaffold annotation for all files...",
    #   tags$div(
    #     style = "text-align: center;",
    #     tags$p("STEP 1 : CLARA clustering... Please wait. (Patiently :) )")
    #     
    #   ),
    #   footer = NULL,
    #   easyClose = FALSE
    # ))
    # 
    # ## clustering on all files 
    # for (plate_name in names(listObjects)) {
    #   currentGroup<-listObjects[[plate_name]]
    # 
    #   currentGroup$flow.frames.deconcatenate <- claraClustering(currentGroup$flow.frames.deconcatenate, input$marker_clustering,input$clustering_parameter)
    #   listObjects[[plate_name]]<-currentGroup
    # }
    # removeModal()
    # 
    showModal(modalDialog(
      title = "Run Scaffold annotation for all files...",
      tags$div(
        style = "text-align: center;",
        tags$p("STEP 2 : Scaffold mapping... Please wait. (Patiently :) )")
        
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    
    for (plate_name in names(listObjects)) {
      
      currentGroup<-listObjects[[plate_name]]

      currentGroup$flow.frames.deconcatenate.tab <- lapply(currentGroup$flow.frames.deconcatenate, function (x){
        builddCSVTab(listObject$flow.frames.subset, listObject$clusteringColumn)

      })
      listObjects[[plate_name]]<- currentGroup
 
      

    refClusteredFiles <- currentGroup$flow.frames.deconcatenate

    refClusteredFiles <- names(currentGroup$flow.frames.deconcatenate)

    clusteredTables<- currentGroup$flow.frames.deconcatenate.tab
    names(clusteredTables) <-names(currentGroup$flow.frames.deconcatenate)
    scaffold<-listObject$Map
    outputDir <- paste0(scaffold.root, input$experimentName)
    col.names.map <- scaffold$scaffold.col.names
    col.names.matrix <- col.names <- input$marker_clustering

    inter.cluster.connections <- TRUE
    overlap_method <- "repel"
    col.names.inter_cluster <- NULL
    inter_cluster.weight_factor <- NULL
    ew_influence <- NULL
    mode <- ""

    sourceCpp("forceatlas2.cpp")

    listObject$Map<-run_analysis_existing(scaffold = scaffold,
                          refClusteredFiles = refClusteredFiles,
                          clusteredTables = clusteredTables,
                          outputDir = outputDir,
                          col.names.matrix = col.names.matrix,
                          col.names.map = col.names.map,
                          inter.cluster.connections = inter.cluster.connections,
                          mode = mode,col.names.inter_cluster = col.names.inter_cluster,
                          inter_cluster.weight_factor = inter_cluster.weight_factor,
                          overlap_method = overlap_method,
                          ew_influence = ew_influence
    )
    
 

      
      scaffold <-listObject$Map

      res <- scaffold_events_export(names(currentGroup$flow.frames.deconcatenate), names(currentGroup$flow.frames.deconcatenate), currentGroup$flow.frames.deconcatenate, scaffold, input$clusteringColumn)
      
      currentGroup$flow.frames.deconcatenate<-res
      
      listObjects[[plate_name]]<- currentGroup
    }
    
    
    for (plate_name in names(listObjects)) {
      current_plate <- listObjects[[plate_name]]
      ff_list <- current_plate$flow.frames.deconcatenate
      
      sampled_list <- list()
      
      for (i in seq_along(ff_list)) {
        fcs <- ff_list[[i]]
        total_cells <- nrow(fcs@exprs)
        n <- min(1000, total_cells) 
        
        sampled_indices <- sample(seq_len(total_cells), n)
        sampled_exprs <- fcs@exprs[sampled_indices, , drop = FALSE]
        
        sampled_fcs <- fcs
        sampled_fcs@exprs <- sampled_exprs
        
        sampled_list[[i]] <- sampled_fcs
      }
      
      
      listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
    }
    
    # concat all plates
    concatenated_list_subset <- list()
    
    for (plate_name in names(listObjects)) {
      plate <- listObjects[[plate_name]]
      
      if (!is.null(plate$flow.frames)) {
        concatenated_list_subset[[plate_name]] <- plate$flow.frames
      }
    }
    
    if (length(concatenated_list_subset) > 0) {
      listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
    } else {
      showNotification("Aucun flow.frames valide trouvé pour la concaténation.", type = "error")
    }
    
    removeModal()
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
  })
  
  
  
  
  
  ###############################################################################  
  #################### pyInfinity Flow  #########################################
  
  plateVisibility <- reactiveValues()
  
  output$uploadPlate_ui <- renderUI({
    req(listObjects)
    
    lapply(seq_along(names(listObjects)), function(i) {
      tagList(
        fileInput(
          inputId = paste0("uploadPlate_", i),
          label = paste("Upload plate", i, "(TXT format)"),
          multiple = FALSE
        ),
        actionButton(
          inputId = paste0("showPlate_", i),
          label = paste("Show/hide plate", i),
          icon = icon("table")
        ),
        tags$br(),
        uiOutput(paste0("plate_table_ui_", i)),
        tags$hr()
      )
    })
  })
  
  observe({
    req(listObjects)
    
    lapply(seq_along(names(listObjects)), function(i) {
      observeEvent(input[[paste0("uploadPlate_", i)]], {
        inFile <- input[[paste0("uploadPlate_", i)]]
        req(inFile)
        
        file <- inFile$datapath
        plate <- read.csv(file, sep = "\t")
        
        currentGroup <- listObjects[[paste0("plate_", i)]]
        currentGroup$plate <- plate
        
        listObjects[[paste0("plate_", i)]] <- currentGroup
      })
    })
  })
  
  
  observe({
    req(listObjects)
    
    lapply(seq_along(names(listObjects)), function(i) {
      local({
        idx <- i
        observeEvent(input[[paste0("showPlate_", idx)]], {
          toggle_id <- paste0("plate_", idx)
          
 
          if (is.null(plateVisibility[[toggle_id]])) {
            plateVisibility[[toggle_id]] <- FALSE
          }
          
          # Toggle
          plateVisibility[[toggle_id]] <- !plateVisibility[[toggle_id]]
          
          output[[paste0("plate_table_ui_", idx)]] <- renderUI({
            if (isTRUE(plateVisibility[[toggle_id]])) {
              currentGroup <- listObjects[[toggle_id]]
              if (!is.null(currentGroup$plate)) {
                DT::dataTableOutput(paste0("plate_table_", idx))
              } else {
                tags$p("No plate uploaded yet.")
              }
            } else {
              NULL  
            }
          })
          
          output[[paste0("plate_table_", idx)]] <- DT::renderDataTable({
            req(listObjects[[toggle_id]]$plate)
            DT::datatable(listObjects[[toggle_id]]$plate, options = list(pageLength = 5))
          })
        })
        
      })
    })
  })
  

 
  
  
  ## Run pyInfinityFlow
  observeEvent(input$runPyInfinityFlow, {
    tryCatch({
      cores <- as.numeric(input$nCores)
      
      
      ## Construction of backbone_annotation file
      backbones <- listObject$backbone_specification[listObject$backbone_specification$type == "backbone", ]
      backbones$type <- backbones$name
      backbones$desc <- backbones$name
      colnames(backbones) <- c("Reference_Backbone", "Query_Backbone", "Final_Name")
      rownames(backbones) <- seq_len(nrow(backbones))
      backbone_annotation <- backbones
      
      input_dir <- file.path(pyInf.root, listObject$experimentName)
      
      try(dir.create(input_dir, recursive = TRUE))
      
      showModal(modalDialog(
        title = "PyInfinity Flow is running ...",
        tags$div(style = "text-align: center;", tags$p(paste0("Please wait."))),
        footer = NULL,
        easyClose = FALSE
      ))
      infinity_annotations <- list()
      
      
      try(dir.create(file.path(input_dir, "fcs")))
      try(dir.create(file.path(input_dir, "csv")))
      try(dir.create(file.path(input_dir, "output")))
      
      for (name in names(listObjects)) {
        currentGroup <- listObjects[[name]]
        
        infinity_marker_annotation <- currentGroup$plate
        
        exploratory <- listObject$backbone_specification[listObject$backbone_specification$type == "exploratory", ]$name
        
        infinity_marker_annotation$Channel <- exploratory
        
        infinity_marker_annotation$Name <- infinity_marker_annotation$Infinity_target
        
        infinity_marker_annotation$Infinity_target <- NULL
        
        if("X" %in% colnames(infinity_marker_annotation)){
          
          infinity_marker_annotation$X<-NULL
        }
        infinity_marker_annotation$Isotype <- infinity_marker_annotation$Infinity_isotype
        
        infinity_marker_annotation$Infinity_isotype <- NULL
        
        infinity_marker_annotation$File <- as.vector(names(currentGroup$flow.frames.deconcatenate))
        
        infinity_marker_annotation <- infinity_marker_annotation %>% relocate(File, Channel, Name, Isotype)
        
        infinity_annotations[[name]] <- infinity_marker_annotation
        
        lapply(names(currentGroup$flow.frames.deconcatenate), function(file_name) {
          
          write.FCS(currentGroup$flow.frames.deconcatenate[[file_name]], file.path(input_dir, "fcs", file_name))
          
        })
      }
      
      combined_annotation <- do.call(rbind, infinity_annotations)
      
      combined_csv <- file.path(input_dir, "csv","infinity_marker_annotation.csv")
      
      write.csv(combined_annotation, combined_csv, row.names = FALSE)
      
      write.csv(backbone_annotation, file.path(input_dir, "csv", "backbone_annotation.csv"), row.names = FALSE)
      
      out_dir <- file.path(input_dir, "output")
      
      
      transform <- ifelse(input$TransformLogiclePy, 'True', 'False')
      
      cmd <- paste0(
        "pyInfinityFlow --data_dir ", file.path(input_dir, "fcs"),
        " --out_dir ", out_dir,
        " --backbone_annotation ", file.path(input_dir, "csv","backbone_annotation.csv"),
        " --infinity_marker_annotation ", combined_csv,
        " --use_logicle_scaling ", transform,
        " --n_events_combine 0 --save_regression_models False ",
        "--save_file_handler False --add_umap False --find_clusters False ",
        "--find_markers False --make_feature_plots False --use_pca False ",
        "--save_h5ad False --save_feather False --save_file_handler False ",
        "--n_cores ", cores, " --random_state 7"
      )
      
      print(cmd)
      system(cmd)
      removeModal()
      
      output$min_cell_ui <- renderUI({

        numericInput("minCellExport","min cells by barcode for export", min=1,max=1000000, value=1000)

      })
      
      # unlink(file.path(pyInf.root, listObject$experimentName, "fcs"))
      outputPy <- file.path(pyInf.root,listObject$experimentName, "output/regression_results/infinity_flow_results.fcs")
      
      fcs <- f$parse(outputPy)[[2]]
      listObject$pyInfinityFlowFCS<-fcs
      
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  
  
  observeEvent(input$selection_table, {
    if (!is.null(input$selection_table)) {
      listObject$backbone_specification <-
        as.data.frame(hot_to_r(input$selection_table))
      listObject$selection_table <- renderRHandsontable({
        rhandsontable(listObject$backbone_specification)
      })
      if (any(is.na(listObject$backbone_specification[, 'type']))) {
        shinyjs::disable('confirm')
      }
      
      if (!any(is.na(listObject$backbone_specification[, 'type']))) {
        shinyjs::enable('confirm')
      }
      
      
      
    }
  })
  
  
  
  select_modify <- function(file) {
    requireNamespace("flowCore")
    
    data_channels <- pData(flowCore::parameters(file)[, c("name", "desc")])
    choices <- c("backbone", "exploratory", "discard")
    result <-
      data.frame(data_channels, factor = factor(
        'discard',
        levels = c('backbone', 'exploratory', 'discard'),
        ordered = TRUE
      ))
    colnames(result) <- (c('name', 'desc', 'type'))
    
    return(result)
    
  }
  
  observe({
    req(listObjects[["plate_1"]]$flow.frames)
    
    #Calls the infinityFlow package for background specification
    listObject$backbone_specification <- select_modify(listObjects[["plate_1"]]$flow.frames)
    
    #Render the table
    output$selection_table <- renderRHandsontable({
      
      rhandsontable(listObject$backbone_specification, width = 380)
    })
    
  })
  

  
  output$export_pyInfinity_ui <- renderUI ({
    
    req(input$minCellExport)
    
    downloadButton("export_pyInfinity", "Export pyInifinity FCS",style = "background-color: #BCEE68; color: black; 
                                            font-weight: bold; padding: 10px 15px; 
                                            border-radius: 5px; border-width: 2px; border-color: #BCEE68;")
    
    
    
    
  })
  observe({
    req(listObjects[["plate_1"]]$plate)
    
    if ("popIDScyan" %in%  colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]])) {
      updateSelectizeInput(session, "annotationColumn", choices = colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]]), selected="popIDScyan")
 
      
    }else{
      updateSelectizeInput(session, "annotationColumn", choices = colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]]))
     
      
    }
    if ("popIDScaffold" %in%  colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]])) {
      updateSelectizeInput(session, "annotationColumn", choices = colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]]), selected="popIDScaffold")
      
      
    }else{
      updateSelectizeInput(session, "annotationColumn", choices = colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]]))
      
      
    }
    
  })
  observe({
    req(listObjects[["plate_1"]]$plate)
    
    if ("barcodeID" %in%  colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]])) {
      updateSelectizeInput(session, "barcodeColumn", choices = colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]]), selected="barcodeID")

    }else{
      updateSelectizeInput(session, "barcodeColumn", choices = colnames(listObjects[["plate_1"]]$flow.frames.deconcatenate[[1]]))
    }

  })

  output$informationForSummaryCounts <- renderUI({
    req(input$minCellExport)
    tags$div(
      style = "background-color: #e6f2ff; border-left: 5px solid #007acc; padding: 15px; margin-bottom: 20px; text-align: justify;",
      tags$strong("Information:"),
      tags$p(
        "This section allows you to export an FCS file containing the PE estimations generated by pyInfinityFlow. ",
        "By default, the system will select 1,000 cells for each barcode and each identified cell population. ",
        "You can adjust this number using the input below. ",
        "If there are fewer than 1,000 cells for a specific barcode/population combination, the export will include the maximum available number of cells for that group."
      )
    )
  })
  
  

  observe({
    req(input$minCellExport)
    
    outputPy <- file.path(pyInf.root, listObject$experimentName, "output/regression_results/infinity_flow_results.fcs")
    
    fcs <- f$parse(outputPy)
    fcs <- fcs[[2]]

    
    markerXGBoost <- colnames(fcs)[which(grepl("InfinityMarker_", colnames(fcs)))]
    
    data <- as.data.frame(fcs)

    selected_data <- data.frame()
    summary_counts <- data.frame()
    

    if (input$barcodeColumn %in% names(data) && input$annotationColumn %in% names(data)) {
   
      for (bc in unique(na.omit(data[[input$barcodeColumn]]))) {
        for (pop in unique(na.omit(data[[input$annotationColumn]]))) {
          subset_rows <- data[data[[input$barcodeColumn]] == bc & data[[input$annotationColumn]] == pop, ]
          n_available <- nrow(subset_rows)
          n_to_sample <- min(input$minCellExport, n_available)
          
          if (n_available > 0) {
            sampled <- subset_rows[sample(n_available, n_to_sample), ]
            selected_data <- rbind(selected_data, sampled)
     
            # Determine label from barcode_dict if available
            label_value <- if (!is.null(listObject$barcode_dict)) {
              as.character(listObject$barcode_dict[as.character(bc)])
            } else {
              as.character(input$barcodeColumn)
            }
            label_value2 <- if (!is.null(listObject$pop_dict)) {
              as.character(listObject$pop_dict[as.character(bc)])
            } else {
              as.character(input$annotationColumn)
            }
            
            summary_counts <- rbind(summary_counts, data.frame(
              barcodeId = bc,
              barcode_label = label_value,
              popID = pop,
              
              cellsSelected = n_to_sample
            ))
            
          }
        }
      }
    }
    
 
    
    # Convert selected data to flowFrame
    fcs_matrix <- as.matrix(selected_data)
    ff <- flowFrame(fcs_matrix)
    listObject$FCSXGboostExport <- ff
    
    # Render summary table
    output$summaryPopExport <- DT::renderDT({
      DT::datatable(
        summary_counts,
        filter = "top",
        options = list(
          scrollY = "300px",
          pageLength = 10,
          dom = 'tip'
        ),
        rownames = FALSE
      )
    })
  })
  
  output$export_pyInfinity <- downloadHandler(
    
    filename = function() {
      paste0("pyInfEstim_", listObject$experimentName, ".fcs")
    },
    
    content = function(file) {
      
      write.FCS(listObject$FCSXGboostExport, file)
    }
  )
  
  ########################################################################################################################################################################
  #############################################              STATS PYINFINITYFLOW         ################################################################################
  

  # untransform data before calculate stats 
  observeEvent(input$unTransformData, {
    for (group_name in names(listObjects)) {
      currentGroup <- listObjects[[group_name]]
      
      if (!is.null(currentGroup)) {
        
        if (input$transfo == "arcsinh") {
          showModal(modalDialog(
            title = "Apply deTransformation on all files...",
            tags$div(
              style = "text-align: center;",
              tags$p("Please wait.")
            ),
            footer = NULL,
            easyClose = FALSE
          ))
          
          arg <- c(listObject$data.table[, 2])
          numeric_rows <- grepl("^[0-9]+(\\.[0-9]+)?$", arg)
          filtered_table <- listObject$data.table[
            suppressWarnings(!is.na(as.numeric(as.character(listObject$data.table$Arg)))), 
            , 
            drop = FALSE
          ]
          marker <- filtered_table$Fluo
          arg <- as.numeric(filtered_table[, 2])
          
          if (!is.null(currentGroup$flow.frames.deconcatenate)) {
            for (sub_name in names(currentGroup$flow.frames.deconcatenate)) {
              currentGroup$flow.frames.deconcatenate[[sub_name]] <- inverseArcsinhCIPHE(
                currentGroup$flow.frames.deconcatenate[[sub_name]], marker = marker, arg
              )
            }
          }
        
        
          removeModal()
          
        } else if (input$transfo == "None") {
          showModal(modalDialog(
            title = "⚠️ Warning" ,
            HTML("
            <div style='margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 5px solid #ffc107;'>
              The transformation table and the transformation selection are empty, so there are no information to process to detransformation of your data. 
              <strong>Please, provide these informations and then re-click on the button.</strong>
            </div>
          "),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
        
        listObjects[[group_name]] <- currentGroup
      }
      
      for (plate_name in names(listObjects)) {
        
        current_plate <- listObjects[[plate_name]]
        
        ff_list <- current_plate$flow.frames.deconcatenate
        
        sampled_list <- list()
        
        for (i in seq_along(ff_list)) {
          
          fcs <- ff_list[[i]]
          total_cells <- nrow(fcs@exprs)
          n <- min(1000, total_cells) 
          
          sampled_indices <- sample(seq_len(total_cells), n)
          sampled_exprs <- fcs@exprs[sampled_indices, , drop = FALSE]
          
          sampled_fcs <- fcs
          sampled_fcs@exprs <- sampled_exprs
          
          sampled_list[[i]] <- sampled_fcs
        }
        
        
        listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
      }
      
      
      concatenated_list_subset <- list()
      
      for (plate_name in names(listObjects)) {
        plate <- listObjects[[plate_name]]
        
        if (!is.null(plate$flow.frames)) {
          concatenated_list_subset[[plate_name]] <- plate$flow.frames
        }
      }
      
      if (length(concatenated_list_subset) > 0) {
        listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
      } else {
        showNotification("No valid flowframes for the concatenation", type = "error")
      }
      
    }
    
    
    ## apply transfo on pyInfinityFIle
    req(listObject$pyInfinityFlowFCS)
    
    if (input$transfo == "arcsinh") {
      showModal(modalDialog(
        title = "Apply deTransformation on file predicted by pyInfinityFlow...",
        tags$div(
          style = "text-align: center;",
          tags$p("Please wait.")
        ),
        footer = NULL,
        easyClose = FALSE
      ))
      
      ff <- flowFrame(as.matrix(listObject$pyInfinityFlowFCS))
 
      arg <- c(listObject$data.table[, 2])
      numeric_rows <- grepl("^[0-9]+(\\.[0-9]+)?$", arg)
      filtered_table <- listObject$data.table[
        suppressWarnings(!is.na(as.numeric(as.character(listObject$data.table$Arg)))), 
        , 
        drop = FALSE
      ]
      
      marker <- filtered_table$Fluo
      
      arg <- as.numeric(filtered_table[, 2])
      
      markerXGBoost <- colnames(listObject$pyInfinityFlowFCS)[grepl("InfinityMarker_", colnames(fcs))]
      
      exploratory <- listObject$backbone_specification[listObject$backbone_specification$type == "exploratory", ]$name
      
      argExploratory<-as.numeric(listObject$data.table[listObject$data.table[,1]==exploratory,]$Arg)

      argExploratory<-rep(argExploratory, length(markerXGBoost))
      marker<-c(marker,markerXGBoost)
      arg<-c(arg, argExploratory)
      
      ff<- inverseArcsinhCIPHE(ff, marker = marker, arg)
    
      ff<-as.data.frame(ff@exprs)
      
      listObject$pyInfinityFlowFCS <-ff
      
      removeModal()
      
    } else if (input$transfo == "None") {
      showModal(modalDialog(
        title = "⚠️ Warning" ,
        HTML("
            <div style='margin-top: 15px; padding: 10px; background-color: #fff3cd; border-left: 5px solid #ffc107;'>
              The transformation table and the transformation selection are empty, so there are no information to process to detransformation of your data. 
              <strong>Please, provide these informations and then re-click on the button.</strong>
            </div>
          "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
    
    
    
  })

  ## Decompensate Data
  observeEvent(input$decompData, {
    showModal(modalDialog(
      title = "Apply decompensation on all files...",
      tags$div(
        style = "text-align: center;",
        tags$p("Please wait.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
   
    for (group_name in names(listObjects)) {
      currentGroup <- listObjects[[group_name]]

      for (sub_name in names(currentGroup$flow.frames.deconcatenate)) {
        fcs <- currentGroup$flow.frames.deconcatenate[[sub_name]]
        
        spill_key <- found.spill.CIPHE(fcs)[[1]]
        if (!is.null(spill_key) && spill_key %in% names(fcs@description)) {
          spill_matrix <- fcs@description[[spill_key]]
          fcs <- flowCore::decompensate(fcs, spillover = spill_matrix)
        } else {
          warning(paste("No valid spillover found for", sub_name))
        }
        
        currentGroup$flow.frames.deconcatenate[[sub_name]] <- fcs
      }
      
      
      listObjects[[group_name]]<-currentGroup
    }
    
  
    for (plate_name in names(listObjects)) {
      current_plate <- listObjects[[plate_name]]
      ff_list <- current_plate$flow.frames.deconcatenate
      
      sampled_list <- list()
      
      for (i in seq_along(ff_list)) {
        fcs <- ff_list[[i]]
        total_cells <- nrow(fcs@exprs)
        n <- min(1000, total_cells) 
        
        sampled_indices <- sample(seq_len(total_cells), n)
        sampled_exprs <- fcs@exprs[sampled_indices, , drop = FALSE]
        
        sampled_fcs <- fcs
        sampled_fcs@exprs <- sampled_exprs
        
        sampled_list[[i]] <- sampled_fcs
      }
      
      
      listObjects[[plate_name]]$flow.frames <- concatenate.FCS.CIPHE(sampled_list, "idAcquisition")
    }
    
    
    concatenated_list_subset <- list()
    
    for (plate_name in names(listObjects)) {
      plate <- listObjects[[plate_name]]
      
      if (!is.null(plate$flow.frames)) {
        concatenated_list_subset[[plate_name]] <- plate$flow.frames
      }
    }
    
    if (length(concatenated_list_subset) > 0) {
      listObject$flow.frames.subset <- concatenate.FCS.CIPHE(concatenated_list_subset, "idPlate")
    } else {
      showNotification("No valid flowframes for the concatenation", type = "error")
    }
    
  
    
    removeModal()
    
  })

  
  ## set ISO threshold 
  

  threshold_values <- reactiveValues()
  
  observe({
    req(listObjects)
    
    for(name in names(listObjects)){
      currentGroup <- listObjects[[name]]
      req(currentGroup$plate)
      
      plate <- as.data.frame(currentGroup$plate)
      isos <- unique(plate$Infinity_isotype)
      
      index <- c()
      for (iso in isos){
        idx <- which(plate$Infinity_target == iso)
        index <- c(index, idx)
      }
      
      currentGroup$index_iso <- index
      listObjects[[name]] <- currentGroup
      
    }
    
  })
  
  output$isoPyInfinityFlowPlate_ui <- renderUI({
    req(listObjects)
    
    selectInput("isoPyInfinityFlowPlate", "Select plate", 
                choices = names(listObjects), multiple = FALSE, width="30%")
    
  })
  
  
  output$isoPyInfinityFlow_ui <- renderUI({
    req(input$isoPyInfinityFlowPlate)
    currentGroup <- listObjects[[input$isoPyInfinityFlowPlate]]
    
    selectInput("isoPyInfinityFlow", "Select ISO file:", 
                choices = names(currentGroup$flow.frames.deconcatenate)[currentGroup$index_iso])
  })
  
  
  output$plotIso <- renderPlot({
    req(input$isoPyInfinityFlowPlate, input$isoPyInfinityFlow, input$threshold)

    currentGroup <- listObjects[[input$isoPyInfinityFlowPlate]]
    fcs_data <- currentGroup$flow.frames.deconcatenate[[input$isoPyInfinityFlow]]
    req(fcs_data)

    df <- as.data.frame(exprs(fcs_data))
    threshold <- input$threshold

    if (!"G-PE-A" %in% colnames(df)) return(NULL)

    ggplot(df, aes(x = `G-PE-A`)) +
      geom_density(fill = "steelblue", color = "black", alpha = 0.4) +
      xlab("G-PE-A") +
      ylab("Density") +
      ggtitle("Isotype") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      ) +
      geom_vline(xintercept = threshold, color = "green", linetype = "dashed", linewidth = 1.2)
  })


  output$threshold_ui <- renderUI({
    req(input$isoPyInfinityFlowPlate, input$isoPyInfinityFlow)
    
    currentGroup <- listObjects[[input$isoPyInfinityFlowPlate]]
    fcs_data <- currentGroup$flow.frames.deconcatenate[[input$isoPyInfinityFlow]]
    
    req(fcs_data)
    
    min_val <- ceiling(min(fcs_data@exprs[, "G-PE-A"], na.rm = TRUE))
    max_val <- ceiling(max(fcs_data@exprs[, "G-PE-A"], na.rm = TRUE)) + 1000
    
    key <- paste0(input$isoPyInfinityFlowPlate, "_", input$isoPyInfinityFlow)
    threshold <- threshold_values[[key]]
    if (is.null(threshold)) threshold <- min_val
    
    numericInput("threshold", "ISO threshold:",
                 value = threshold, min = min_val, max = max_val, step = 1)
  })
  
  

  
  observe({
    req(input$isoPyInfinityFlowPlate, input$isoPyInfinityFlow)
    key <- paste0(input$isoPyInfinityFlowPlate, "_", input$isoPyInfinityFlow)
    if (is.null(threshold_values[[key]])) {
      threshold_values[[key]] <- 100
    }
  })
  
  observeEvent(input$threshold, {
    req(input$isoPyInfinityFlowPlate, input$isoPyInfinityFlow)
    
    key <- paste0(input$isoPyInfinityFlowPlate, "_", input$isoPyInfinityFlow)
    threshold_values[[key]] <- input$threshold
  })
  
  

  # calcStats
  
  observeEvent(input$calcStats, {
    tryCatch({
      
      showModal(modalDialog(
        title = "Calculating stats for PE measured ...",
        tags$div(style = "text-align: center;", tags$p("Please wait.")),
        footer = NULL,
        easyClose = FALSE
      ))
      stats_df <- if (!is.null(listObject$barcode_dict)) {
        data.frame(
          Marker = 0, threshold.pos = 0, barcodeId = 0, barcodeLabel=NA, popID = 0,
          count.pos = 0, mean.pos = 0, median.pos = 0, Perc.PE.pos = 0, standard.dev.pos = 0,
          p5.pos = 0, p25.pos = 0, p75.pos = 0, p95.pos = 0
        )
      } else {
        data.frame(
          Marker = 0, threshold.pos = 0, barcodeId = 0, popID = 0,
          count.pos = 0, mean.pos = 0, median.pos = 0, Perc.PE.pos = 0, standard.dev.pos = 0,
          p5.pos = 0, p25.pos = 0, p75.pos = 0, p95.pos = 0
        )
      }
     
      exploratory <- listObject$backbone_specification[listObject$backbone_specification$type == "exploratory", ]$name
      for (name in names(listObjects)) {
        currentGroup <- listObjects[[name]]
        
        infinity_marker_annotation <- as.data.frame(read.csv(
          file.path(pyInf.root, listObject$experimentName, "csv/infinity_marker_annotation.csv")
        ))
        isotype_controls <- infinity_marker_annotation %>%
          filter(Name %in% Isotype) %>%
          dplyr::select(File, Isotype)
        
        annotated_data <- infinity_marker_annotation %>%
          left_join(isotype_controls, by = "Isotype", suffix = c("", ".iso")) %>%
          rename(Isotype_File = File.iso, Original_File = File) %>%
          dplyr::select(Original_File, Channel, Name, Isotype, Isotype_File)

  
      
        for (file in names(currentGroup$flow.frames.deconcatenate)) {
          fcs <- currentGroup$flow.frames.deconcatenate[[file]]
          isotype <- annotated_data[annotated_data$Original_File == file, ]$Isotype_File
          marker <- annotated_data[annotated_data$Original_File == file, ]$Name
          key <- paste0(name, "_", isotype)
          threshold <- threshold_values[[key]]
        
          
          data <- as.data.frame(fcs@exprs)
          
          if (input$barcodeColumn %in% colnames(fcs)) {
            barcodeId <- unique(data[[input$barcodeColumn]])
            barcodeId <- unique(data[[input$barcodeColumn]])
            for (barcode in barcodeId) {
              if (!is.null(listObject$barcode_dict)) {
                barcodeLabel <- as.character(listObject$barcode_dict[as.character(barcode)])
              } else {
                barcodeLabel <- NA
              }
              
              df <- data[data[[input$barcodeColumn]] == barcode, ]
              
              if (input$annotationColumn %in% colnames(fcs)) {
                pops <- unique(df[[input$annotationColumn]])
                
                for (pop in pops) {
                  
                  if (!is.null(listObject$barcode_dict)) {
                    vector <- c(marker, threshold, barcode, barcodeLabel, pop)
                    
                  }else{
                    vector <- c(marker, threshold, barcode, pop)

                  }
          
                  df2 <- df[df[[input$annotationColumn]] == pop, ]
                  df2_filtered <- df2[df2[[exploratory]] > threshold, ]
                  
                  if (nrow(df2_filtered) > 0) {
                    count <- nrow(df2_filtered)
                    mean <- round(mean(df2_filtered[, exploratory]), 2)
                    median <- round(median(df2_filtered[, exploratory]), 2)
                    PE <- round(count / nrow(df2) * 100, 2)
                    sd <- round(sd(df2_filtered[, exploratory]), 2)
                    p5 <- round(quantile(df2_filtered[, exploratory], 0.05), 2)
                    p25 <- round(quantile(df2_filtered[, exploratory], 0.25), 2)
                    p75 <- round(quantile(df2_filtered[, exploratory], 0.75), 2)
                    p95 <- round(quantile(df2_filtered[, exploratory], 0.95), 2)
                  } else {
                    count <- mean <- median <- PE <- sd <- p5 <- p25 <- p75 <- p95 <- NA
                  }
          
                  vector <- c(vector, count, mean, median, PE, sd, p5, p25, p75, p95)
                  stats_df[nrow(stats_df) + 1, ] <- vector
                }
              }
            }
          }
        }
      } 
      
      stats_df <- stats_df[-1, ]
      
      if (!is.null(listObject$barcode_dict)) {
        stats_df$barcode_label <- as.character(listObject$barcode_dict[as.character(stats_df$barcodeId)])
      } 
      
      
      
      listObject$stats_df <- stats_df
      
      removeModal()

    
      
      req(listObject$pyInfinityFlowFCS)
      
   
        showModal(modalDialog(
          title = "Calculating stats for PE estimated by XGBoost ...",
          tags$div(style = "text-align: center;", tags$p("Please wait.")),
          footer = NULL,
          easyClose = FALSE
        ))
        
        
        fcs<-listObject$pyInfinityFlowFCS
        
        stats_df_estimated <- if (!is.null(listObject$barcode_dict)) {
          data.frame(
            Marker = 0, threshold.pos = 0, barcodeId = 0, barcodeLabel=NA, popID = 0,
            count.pos = 0, mean.pos = 0, median.pos = 0, Perc.PE.pos = 0, standard.dev.pos = 0,
            p5.pos = 0, p25.pos = 0, p75.pos = 0, p95.pos = 0
          )
        } else {
          data.frame(
            Marker = 0, threshold.pos = 0, barcodeId = 0, popID = 0,
            count.pos = 0, mean.pos = 0, median.pos = 0, Perc.PE.pos = 0, standard.dev.pos = 0,
            p5.pos = 0, p25.pos = 0, p75.pos = 0, p95.pos = 0
          )
        }
        
       
        data <- as.data.frame(fcs) 
        
        data$fileId <- ceiling(data$fileId)
        data[[input$annotationColumn]]<- ceiling( data[[input$annotationColumn]])
   
        markerXGBoost <- colnames(fcs)[grepl("InfinityMarker_", colnames(fcs))]
        
        for (marker in markerXGBoost) {
          markerRename <- gsub("InfinityMarker_", "", marker)
          isotype <- annotated_data[annotated_data$Name == markerRename, ]$Isotype_File
          
          for (name in names(listObjects)) {
            currentGroup <- listObjects[[name]]
            if (isotype %in% names(currentGroup$thresholdISO)) {
              threshold <- currentGroup$thresholdISO[[isotype]]
            }
          }
          
          if (input$barcodeColumn %in% colnames(data)) {
            barcodeId <- unique(data[[input$barcodeColumn]])
          
            for (barcode in barcodeId) {
              if (!is.null(listObject$barcode_dict)) {
                barcodeLabel <- as.character(listObject$barcode_dict[as.character(barcode)])
              } else {
                barcodeLabel <- NA
              }
              
              df <- data[data[[input$barcodeColumn]]== barcode, ]
              
              if (input$annotationColumn %in% colnames(fcs)) {
                pops <- unique(df[[input$annotationColumn]])
                
                for (pop in pops) {
                  
                  if (!is.null(listObject$barcode_dict)) {
                    vector <- c(marker, threshold, barcode, barcodeLabel, pop)
                    
                  }else{
                    vector <- c(marker, threshold, barcode, pop)
                    
                  }
              
                  df2 <- df[df[[input$annotationColumn]] == pop, ]
                  df2_filtered <- df2[df2[, marker] > threshold, ]
                  
                  if (nrow(df2_filtered) > 0) {
                    count <- nrow(df2_filtered)
                    mean <- round(mean(df2_filtered[, marker]), 2)
                    median <- round(median(df2_filtered[, marker]), 2)
                    PE <- round(count / nrow(df2) * 100, 2)
                    sd <- round(sd(df2_filtered[, marker]), 2)
                    p5 <- round(quantile(df2_filtered[, marker], 0.05), 2)
                    p25 <- round(quantile(df2_filtered[, marker], 0.25), 2)
                    p75 <- round(quantile(df2_filtered[, marker], 0.75), 2)
                    p95 <- round(quantile(df2_filtered[, marker], 0.95), 2)
                  } else {
                    count <- mean <- median <- PE <- sd <- p5 <- p25 <- p75 <- p95 <- NA
                  }
                  
                  vector <- c(vector, count, mean, median, PE, sd, p5, p25, p75, p95)
                  stats_df_estimated[nrow(stats_df_estimated) + 1, ] <- vector
                }
              }
            }
          }
        }
        
        stats_df_estimated <- stats_df_estimated[-1, ]
        
        if (!is.null(listObject$barcode_dict)) {
          stats_df_estimated$barcode_label <- as.character(listObject$barcode_dict[as.character(stats_df_estimated$barcodeId)])
        } 
        
        listObject$stats_df_estimated <- stats_df_estimated
        
        removeModal()
      
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  }) 
  

  
  output$statsResult <- DT::renderDataTable({
    req(listObject$stats_df)
    req(input$mesured_estimated_choices)
    
    if (input$mesured_estimated_choices == "PE mesured"){
      
      data<-listObject$stats_df
      
      
    }else{
      
      data<-listObject$stats_df_estimated
      
    }
    
    req(data)
    
    DT::datatable(
      data,
      rownames = FALSE,
      selection = "none",
      extensions = 'AutoFill',
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '80px', targets = "_all")
        ),
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100)
      )
    )
    
  }, server = FALSE)
  
  
  
  output$exportStats_ui<-renderUI({
    
    req(listObject$stats_df)
    
    downloadButton("exportStats", "Export stats", style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;", width='40%')
    
  })
  
  output$exportStats <- downloadHandler(
    
    filename = function() {
      paste0("Stats_", listObject$experimentName, ".xlsx")
    },
    
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb, "PE mesured")
      writeData(wb, sheet = "PE mesured", listObject$stats_df)
      
      addWorksheet(wb, "PE estimated (XGBoost)")
      writeData(wb, sheet = "PE estimated (XGBoost)", listObject$stats_df_estimated)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  

  # check if values are missing or not
  output$statsHtml_ui<-renderUI({
    req(listObject$stats_df)
    
    HTML("<h6> Missing values is when there are 0 positive cells (above ISO threshold)  :</h6>" )
    
  })
  
  
  output$statsHtml1_ui<-renderUI({
    req(listObject$stats_df)
    HTML("<h4><span style='background-color: #fdfd96;'> Stats :</span></h4>" )
    
  })
  
  
  output$mesured_estimated_choices_ui<-renderUI({
    req(listObject$stats_df)
    selectInput("mesured_estimated_choices","Select : ", choices=c("PE mesured", "PE estimated"))
    
  })
  

  
  
  output$exportStats_ui<-renderUI({
    
    req(listObject$stats_df)
    
    downloadButton("exportStats", "Export stats", style = "background-color:#BCEE68; border-width: 2px; border-color: #BCEE68;margin-top: 10px;", width='40%')
    
  })
  
  output$exportStats <- downloadHandler(
    
    filename = function() {
      paste0("Stats_", listObject$experimentName, ".xlsx")
    },
    
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb, "PE mesured")
      writeData(wb, sheet = "PE mesured", listObject$stats_df)
      
      addWorksheet(wb, "PE estimated (XGBoost)")
      writeData(wb, sheet = "PE estimated (XGBoost)", listObject$stats_df_estimated)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  

  # check if values are missing or not
  output$statsHtml_ui<-renderUI({
    req(listObject$stats_df)
    
    HTML("<h6> Missing values is when there are 0 positive cells (above ISO threshold)  :</h6>" )
    
  })
  
  
  output$statsHtml1_ui<-renderUI({
    req(listObject$stats_df)
    HTML("<h4><span style='background-color: #fdfd96;'> Stats :</span></h4>" )
    
  })
  
  
  output$mesured_estimated_choices_ui<-renderUI({
    req(listObject$stats_df)
    selectInput("mesured_estimated_choices","Select : ", choices=c("PE mesured", "PE estimated"))
    
  })
  

  
})
