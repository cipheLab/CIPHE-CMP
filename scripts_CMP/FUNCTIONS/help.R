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
          <li><strong>1. Marker selection:</strong> Choose the markers you want to use for clustering. These should be relevant for distinguishing cell populations.</li>
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
