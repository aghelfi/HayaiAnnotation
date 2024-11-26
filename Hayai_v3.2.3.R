suppressMessages(require(data.table))
suppressMessages(require(DT))
suppressMessages(require(shiny))
suppressMessages(require(shinydashboard))
suppressMessages(require(dplyr))
suppressMessages(require(tidyr))
suppressMessages(require(ggplot2))
suppressMessages(require(igraph))
suppressMessages(require(visNetwork))
suppressMessages(require(shinyjs))
suppressMessages(require(shinyBS))
suppressMessages(require(ggraph))
suppressMessages(require(reshape2))
suppressMessages(require(zip))

load(file = "./data/plants.rda"); load(file = "./data/goid2term.rda"); load(file = "./data/interpro.rda") 

options(shiny.maxRequestSize = 150*1024^2)

# JavaScript to enforce input restrictions
jsCode <- "
$(document).ready(function(){
  $('#file1_label').attr('maxlength', '20').on('input', function() {
    console.log('File 1 label input: ' + this.value);  // Debugging output
    this.value = this.value.replace(/[^a-zA-Z0-9]/g, '');
  });
  
});
"
# CSS styling
cssCode <- "
/* Improve navbar and logo colors */
.skin-blue .main-header .navbar, .skin-blue .main-header .logo {
  background-color: #002e3b;
}
/* Adjust sidebar background color */
.skin-blue .main-sidebar {
  background-color: #003b2b;
}
/* Increase spacing within the sidebar to prevent overlapping */
.sidebar-form, .form-group {
  margin-bottom: 15px;
  padding: 10px;
}
/* Style for the sidebar links */
.sidebar a {
  color: #FFFFFF;
  padding: 5px;
  display: block;
  margin: 5px 0;
}"

ui <- dashboardPage(
  
  dashboardHeader(title = "Hayai-Annotation Plants"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      # Primary Analysis
      menuItem("Functional Annotation", tabName = "primary", icon = icon("chart-bar")
      ),
      # Secondary Analysis
      menuItem("Network Analysis", tabName = "secondary", icon = icon("chart-line")
      )
    ) 
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$script(HTML(jsCode)), 
    tags$style(HTML(cssCode)),
    tabItems(
      # Primary Analysis 
      tabItem(tabName = "primary",
              
              fluidRow(
                # Box for Upload Data and Run Analysis
                box(title = "Functional Protein Annotation", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, radioButtons("align","Sequence Type", choices= list("Protein sequences" = "blastp", "DNA sequences" = "blastx"), selected = "blastp")),
                      
                      column(4, selectizeInput("sensitivity","Diamond Sensitivity", choices= list("faster mode (less accurate)" = "faster", "fast mode" = "fast", "mid sensitive" = "mid-sensitive",
                                                                                                  "sensitive mode" = "sensitive", "more sensitive mode" = "more-sensitive", "very sensitive mode" = "very-sensitive",
                                                                                                  "ultra sensitive (more accurate)" = "ultra-sensitive"), selected = "more-sensitive")),
                      column(4, sliderInput("threads","Number of Threads", min = 1, max = 24, value = 10, step = 1))
                    ),
                    fluidRow(
                      column(4, fileInput("userInput", "Upload FASTA File", multiple = FALSE, accept = c(".fasta", ".fa", ".faa", ".fna"))),
                      column(4, textInput("file1_label", "Project Name", value = "MyProject1")),
                      column(4, checkboxInput("orthologer", "Run OrthoLoger (only for protein sequences)", value = FALSE))
                    ),
                    fluidRow(
                      column(4, actionButton("submit", "Submit", icon = icon("play"), class = "btn-success")),
                      column(4, shinyjs::hidden(p(id = "text1", "Starting process, please wait..."))),
                      column(4, downloadButton("downloadData", "Download Results", icon = icon("download"), disabled = TRUE))
                    )
                )
              ),
              fluidRow(
                # Box for displaying results 
                box(title = "Annotation Output", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("hayaiv3")  
                )
              )
      ),
      
      # Secondary Analysis 
      tabItem(tabName = "secondary",
              fluidRow(
                # Box for uploading files and setting parameters
                box(title = "Compare Network between two samples", status = "success", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4,fileInput("file1_upload", "Choose File 1 (ProjectName_Hayai_annotation_v3.2.tsv)", accept = c(".tsv"))),
                      column(4, sliderInput("identity", "Sequence Identity (%)", min = 0, max = 100, value = 50, step = 5)),
                      column(4, radioButtons("go_domain", "Select GO Domain", choices= list("Molecular Function" = "GO_MF", "Biological Process" = "GO_BP"), selected = "GO_MF"))
                    ),
                    fluidRow(
                      column(4, fileInput("file2_upload", "Choose File 2 (ProjectName_Hayai_annotation_v3.2.tsv)", accept = c(".tsv"))),
                      column(4, sliderInput("threshold", "Network threshold (log):", min = 0, max = 2, value = 0, step = 0.20)),
                      column(4, radioButtons("odb_priority", "Select Priority for OrthoDB Inferences", choices= list("Zen (UniProt aligned with OrthoDB)" = "Zen_OrthoDB", "OrthoLoger (OrthoDB tool)" = "ODB_OG"), selected = "ODB_OG"))
                      
                    ),
                    fluidRow(
                      column(4, actionButton("secondary_run_button", "Run Analysis", icon = icon("play"), class = "btn-success")),
                      column(4, downloadButton("downloadData2", "Download Results", icon = icon("download"), disabled = TRUE)),
                      column(4, shinyjs::hidden(p(id = "network_ui", "Starting process, please wait..."))),
                    )
                )
              ),
              fluidRow(
                # Box for displaying secondary analysis results
                box(title = "Network Dataset (select row GO or OrthoDB, use search engine to find your target)", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("network_table")),  
                box(title = "Display Selected Network (build using OrthoDB and GO Domain as nodes)", status = "success", solidHeader = TRUE, width = 12,
                    visNetworkOutput("network", height = "800px")),
                box(title = "Main Annotation File 1", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("full_file1")),
                box(title = "Main Annotation File 2", status = "success", solidHeader = TRUE, width = 12,
                    DTOutput("full_file2"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    shinyjs::toggleState("submit", !is.null(input$userInput))
    shinyjs::toggleState("secondary_run_button", !is.null(input$file1_upload) & !is.null(input$file2_upload))
  })
  
  observeEvent(input$userInput, {
    req(input$userInput)
    inFile <- input$userInput
    if (is.null(inFile))
      return(NULL)
    
    # Define directory paths
    temp_dir <- "./temp"
    workspace_dir <- "./workspace"
    
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE)
    } else {
      unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE) # !!! uncomment this line !!!
    }
    
    if (!dir.exists(workspace_dir)) {
      dir.create(workspace_dir, recursive = TRUE)
    } else {
      unlink(list.files(workspace_dir, full.names = TRUE), recursive = TRUE) # !!! uncomment this line !!!
    }
    
  })
  
  formData <- eventReactive(input$submit, {
    
    hayai_version <- "Hayai-Annotation Plants v3.2 \n"
    log_message(hayai_version)
    
    # Validate text inputs
    validate(
      need(nchar(input$file1_label) <= 20, "Label for File 1 must be 20 characters or fewer."),
      need(grepl("^[a-zA-Z0-9]*$", input$file1_label), "Label for File 1 must be alphanumeric.")
    )
    
    userInputFile_path <- input$userInput$datapath
    file1_label <- input$file1_label
    output_file_path <- paste0("./temp/", file1_label, ".fasta")
    
    check_file_lines <- function(path) {
      if (file.exists(path)) {
        lines_count <- length(readLines(path))
        return(lines_count)
      } else {
        return(0) 
      }
    }
    
    process_fasta_headers <- function(input_path, output_path) {
      
      lines <- readLines(input_path)
      processed_lines <- sapply(lines, function(line) {
        if (startsWith(line, ">")) {
          line <- gsub("\\|", "_", line)
          line <- sub("\\s.*$", "", line)
        }
        return(line)
      })
      writeLines(processed_lines, output_path)
    }
    
    process_fasta_headers(userInputFile_path, output_file_path)
    
    create_timing_message <- function(step_name, start_time, end_time) {
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
      message <- paste(
        step_name, "\n",
        "Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n",
        "Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n",
        "Duration:", round(duration, 2), "seconds", "\n"
      )
      return(message)
    }
    
    align <- input$align
    threads <- input$threads
    sensitivity <- input$sensitivity
    file1_label <- input$file1_label
    
    command_type <- ifelse(align == "blastp", "blastp", "blastx")
    
    run_diamond <- sprintf("diamond %s -q %s -d ./db/zen.dmnd -o ./temp/output_diamond.m10 --threads %s --%s --top 1 --evalue 1e-6 --quiet --un ./workspace/unaligned.fasta -f 6 qseqid pident length qstart qend sstart send evalue bitscore stitle",
                           command_type, output_file_path, threads, sensitivity)
    
    shinyjs::html("text1", "Step 1: Running Diamond, please wait this step can take a while...")
    start_diamond <- Sys.time()
    
    result <- tryCatch({
      system(run_diamond) # Run diamond 
      TRUE 
    }, error = function(e) {
      error_message <- paste("An error occurred:", e$message)
      log_message(error_message)
      FALSE 
    })
    
    end_diamond <- Sys.time()
    shinyjs::html("text1", "Step 1: Diamond Finished...")
    
    parameters_message <- paste(
      "Project Name: ", file1_label, "\n",
      "Diamond sensitivity: ", sensitivity, "\n",
      "Number of threads: ", threads, "\n"
    )
    log_message(parameters_message)
    timing_message1 <- create_timing_message("Sequence alignment: Diamond v2.1.9.163, Database UniProtKB - Viridiplantae", start_diamond, end_diamond)
    log_message(timing_message1)
    log_message(paste("diamond command line: ", run_diamond, "\n")) 
    
    size_test_file <- check_file_lines("./temp/output_diamond.m10")


    if (input$align == "blastp" & input$orthologer){
      
        start_orthologer <- Sys.time() 
        shinyjs::html("text1", "Step 2: Running OrthoLoger, please wait this step can take a while...")

        run_orthologer <- "ODB-mapper"
        args <- paste("MAP", file1_label, output_file_path, "33090", collapse = "")
        log_file_path <- "./workspace/orthologer.log"
        system2(run_orthologer, args = args, stdout = log_file_path, stderr = log_file_path, wait = TRUE) # run OrthoLoger

        end_orthologer <- Sys.time() 
        shinyjs::html("text1", "Step 2: Finished OrthoLoger...")
        timing_message2 <- create_timing_message("Ortholog Identification: OrthoLoger conda v3.5", start_orthologer, end_orthologer)
        log_message(timing_message2)
    }
    
    start_annotation <- Sys.time()
    shinyjs::html("text1", "Step 3: Hayai is processing data...")
    
    if(size_test_file == 0){ 
      outtable <- data.frame("Query" = 0, "Target" = 0)
    } else {
      diamond_output <- fread("./temp/output_diamond.m10", header = FALSE, sep = "\t")
      fwrite(diamond_output, "./temp/formatted_diamond.m6", sep = "|", quote = FALSE)
      uniprot <- fread("./temp/formatted_diamond.m6", header = FALSE, sep = "|", quote = "", stringsAsFactors = FALSE)
      colnames(uniprot) <- c("Query", "Identity", "Length", "qstart", "qend", "sstart", "send", "Evalue", "Score", 
                             "Accession","Product_Name", "Zen_OrthoDB", "UKB_OrthoDB", "Evidence_existence",
                             "InterPro", "Pfam", "UKB_GO_BP", "UKB_GO_MF", "GO_CC")
      uniprot$Evidence_existence <- sub("PE: ", "", uniprot$Evidence_existence)
      uniprot$InterPro <- sub("InterPro: ", "", uniprot$InterPro)
      uniprot$Pfam <- sub("Pfam: ", "", uniprot$Pfam)
      uniprot$UKB_GO_BP <- sub("GO_BP: ", "", uniprot$UKB_GO_BP)
      uniprot$UKB_GO_MF <- sub("GO_MF: ", "", uniprot$UKB_GO_MF)
      uniprot$GO_CC <- sub("GO_CC: ", "", uniprot$GO_CC)
      uniprot$Product_Name <- sub("Product_Name: ", "", uniprot$Product_Name)
      uniprot$UKB_OrthoDB <- sub("OrthoDB: ", "", uniprot$UKB_OrthoDB) # Source: original data from UniProtKB based on OrthoDB at2759
      uniprot$Zen_OrthoDB <- sub("Zen_OrthoDB: ", "", uniprot$Zen_OrthoDB) # Source: inferred from alignment between UniProt-Plants and OrthoDB-Plants (Zen mapping)
      
      colnames(ortho) <- c("Zen_OrthoDB", "Zen_OrthoDB_Desc")  
      uniprot <- uniprot %>% left_join(ortho,  by = c("Zen_OrthoDB"))
      uniprot <- uniprot[!duplicated(uniprot[, c("Query")]), ]
      uniprot <- uniprot[ , c("Query", "Accession", "Product_Name", "Zen_OrthoDB", "Zen_OrthoDB_Desc", "UKB_OrthoDB",
                              "Evidence_existence", "InterPro", "Pfam", "UKB_GO_BP", "UKB_GO_MF", "GO_CC", 
                              "Identity", "Length", "Evalue", "Score")]
      
      ogfilename <- "./odbmapper/v12/pipeline/Results/PROJECTNAME.og.annotations"
      ogfilename <- sub("PROJECTNAME", file1_label, ogfilename)
      size_test_file2 <- check_file_lines(ogfilename)
      
      if(size_test_file2 > 0){
        orthologer <- fread(ogfilename, header = TRUE, stringsAsFactors = FALSE)
        colnames(orthologer) <- c("Query", "ODB_OG", "ODB_evalue", "ODB_score", "ODB_COG_category", "ODB_Description", "ODB_GO_MF", "ODB_GO_BP", "ODB_EC", "ODB_KEGG_ko", "ODB_Interpro")
        data <- uniprot %>% full_join(orthologer, by = "Query")
        
        # Create ODB_OG: source orthologer; OrthoDB: source UniProtKB; v3.2
        data <- data %>% mutate(OrthoDB = if_else(!is.na(ODB_OG) & ODB_OG != "", ODB_OG, Zen_OrthoDB))
        
        # Add information from orthologer if uniprot has no data (UKB_GO_BP, UKB_GO_MF); v3.2
        data <- data %>% mutate(GO_BP = if_else(!is.na(UKB_GO_BP) & UKB_GO_BP != "", UKB_GO_BP, ODB_GO_BP))
        data <- data %>% mutate(GO_MF = if_else(!is.na(UKB_GO_MF) & UKB_GO_MF != "", UKB_GO_MF, ODB_GO_MF))
        
        colnames(ortho) <- c("OrthoDB", "OrthoDB_Desc")
        data <- data %>% left_join(ortho, by = "OrthoDB")
        data <- data[!duplicated(data[, c("Query")]), ]
        outtable <- data[ , c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "Zen_OrthoDB", "Zen_OrthoDB_Desc", "UKB_OrthoDB", 
                              "GO_BP", "GO_MF", "GO_CC", "Evidence_existence", "InterPro", "Pfam", "UKB_GO_BP", "UKB_GO_MF", "Identity", "Length", "Evalue", "Score", 
                              "ODB_OG", "ODB_Description","ODB_GO_MF", "ODB_GO_BP", "ODB_EC", "ODB_COG_category", "ODB_KEGG_ko", "ODB_Interpro", "ODB_evalue", "ODB_score")]
        
        output_filename <- paste0("./workspace/", file1_label, "_Hayai_annotation_v3.2.tsv")
        fwrite(outtable, output_filename, sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
        
      } else {
        data <- uniprot %>%
          rename(
            OrthoDB = Zen_OrthoDB,
            OrthoDB_Desc = Zen_OrthoDB_Desc,
            GO_BP = UKB_GO_BP,
            GO_MF = UKB_GO_MF
          )

        outtable <- data[ , c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "UKB_OrthoDB", 
                             "GO_BP", "GO_MF", "GO_CC", "Evidence_existence", "InterPro", "Pfam",  "Identity", "Length", "Evalue", "Score")]
        
        output_filename <- paste0("./workspace/", file1_label, "_Hayai_annotation_v3.2.tsv")
        fwrite(outtable, output_filename, sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
        
      }
      outtable_len <- nrow(outtable)
      
      if(outtable_len > 0){
        outtable$Accession <- as.character(outtable$Accession)
        outtable$Accession <- paste0("<a href='","https://www.uniprot.org/uniprot/",outtable$Accession,"' target='_blank'>", outtable$Accession,"</a>")
        outtable$OrthoDB <- ifelse(outtable$OrthoDB != "NA",
                                      paste0("<a href='https://www.orthodb.org/?query=", outtable$OrthoDB, "' target='_blank'>", outtable$OrthoDB, "</a>"), "NA")
        outtable <- outtable[ , c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "Identity", "GO_BP", "GO_MF", "GO_CC", "InterPro", "Pfam")]
        
        count_terms <- function(column) {
          terms_list <- strsplit(column, ",") 
          valid_terms <- terms_list[!is.na(column)]
          all_terms <- unlist(valid_terms)
          term_counts <- table(all_terms)
          return(term_counts)
        }
        
        # GO_MF
        go_counts <- count_terms(data$GO_MF)
        go_counts <- go_counts[names(go_counts) != "NA"]
        go_counts_df <- data.frame(GO = names(go_counts), Count = as.integer(go_counts))
        # go_counts_df <- merge(go_counts_df, go, by="GO")
        go_counts_df <- go_counts_df %>% inner_join(go,  by = c("GO"))
        go_counts_df <- go_counts_df[, c( "GO", "GO_Desc", "Count")]
        go_counts_df <- go_counts_df[order(go_counts_df$Count, decreasing = TRUE),]
        fwrite(go_counts_df, "./workspace/Hayai_annotation_GO_MF.tsv", sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
        if(dim(go_counts_df)[1] > 50){
          top_go <- go_counts_df[1:50, ]
          top_go$GO_Desc <- strtrim(top_go$GO_Desc, 55)
          pdf("./workspace/Graph_Hayai_GO_MF.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggplot(top_go, aes(x = reorder(GO_Desc, Count), y = Count)) +
                  geom_bar(stat = "identity", fill = "darkblue") +
                  coord_flip() + 
                  theme_minimal() +
                  labs(title = "Top 50 GO Molecular Function Distribution", x = "GO Molecular Function", y = "Number of Genes") +
                  theme(panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 1)))
          dev.off()
        }
        # GO_BP
        go_counts <- count_terms(data$GO_BP)
        go_counts <- go_counts[names(go_counts) != "NA"]
        go_counts_df <- data.frame(GO = names(go_counts), Count = as.integer(go_counts))
        # go_counts_df <- merge(go_counts_df, go, by = "GO")
        go_counts_df <- go_counts_df %>% inner_join(go, by = c("GO"))
        go_counts_df <- go_counts_df[, c( "GO", "GO_Desc", "Count")]
        go_counts_df <- go_counts_df[order(go_counts_df$Count, decreasing = TRUE),]
        fwrite(go_counts_df, "./workspace/Hayai_annotation_GO_BP.tsv", sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
        if(dim(go_counts_df)[1] > 50){
          top_go <- go_counts_df[1:50, ]
          top_go$GO_Desc <- strtrim(top_go$GO_Desc, 55)
          pdf("./workspace/Graph_Hayai_GO_BP.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggplot(top_go, aes(x = reorder(GO_Desc, Count), y = Count)) +
                  geom_bar(stat = "identity", fill = "darkblue") +
                  coord_flip() + 
                  theme_minimal() +
                  labs(title = "Top 50 GO Biological Process Distribution", x = "GO Biological Process", y = "Number of Genes") +
                  theme(panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 1)))
          dev.off()
        }
        # GO_CC
        go_counts <- count_terms(data$GO_CC)
        go_counts <- go_counts[names(go_counts) != "NA"]
        go_counts_df <- data.frame(GO = names(go_counts), Count = as.integer(go_counts))
        # go_counts_df <- merge(go_counts_df, go, by = "GO")
        go_counts_df <- go_counts_df %>% inner_join(go, by = c("GO"))
        go_counts_df <- go_counts_df[, c( "GO", "GO_Desc", "Count")]
        go_counts_df <- go_counts_df[order(go_counts_df$Count, decreasing = TRUE),]
        fwrite(go_counts_df, "./workspace/Hayai_annotation_GO_CC.tsv", sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
        if(dim(go_counts_df)[1] > 25){
          top_go <- go_counts_df[1:25, ]
          top_go$GO_Desc <- strtrim(top_go$GO_Desc, 55)
          pdf("./workspace/Graph_Hayai_GO_CC.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggplot(top_go, aes(x = reorder(GO_Desc, Count), y = Count)) +
                  geom_bar(stat = "identity", fill = "darkblue") +
                  coord_flip() + 
                  theme_minimal() +
                  labs(title = "Top 50 GO Cellular Component Distribution", x = "GO Cellular Component", y = "Number of Genes") +
                  theme(panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 1)))
          dev.off()
        }
        # Interpro
        go_counts <- count_terms(data$InterPro)
        go_counts <- go_counts[names(go_counts) != "NA"]
        go_counts_df <- data.frame(InterPro = names(go_counts), Count = as.integer(go_counts))
        # go_counts_df <- merge(go_counts_df, interpro, by = "InterPro")
        go_counts_df <- go_counts_df %>% inner_join(interpro, by = c("InterPro"))
        go_counts_df <- go_counts_df[, c( "InterPro", "InterPro_Desc", "Count")]
        go_counts_df <- go_counts_df[order(go_counts_df$Count, decreasing = TRUE),]
        fwrite(go_counts_df, "./workspace/Hayai_annotation_Interpro.tsv", sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
        if(dim(go_counts_df)[1] > 50){
          top_go <- go_counts_df[1:50, ]
          top_go$InterPro_Desc <- strtrim(top_go$InterPro_Desc, 55)
          pdf("./workspace/Graph_Hayai_InterPro.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggplot(top_go, aes(x = reorder(InterPro_Desc, Count), y = Count)) +
                  geom_bar(stat = "identity", fill = "darkblue") +
                  coord_flip() + 
                  theme_minimal() +
                  labs(title = "Top 50 InterPro Distribution", x = "InterPro", y = "Number of Genes") +
                  theme(panel.grid.minor = element_blank(), panel.background = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust = 1)))
          dev.off()
        }
        
        end_annotation <- Sys.time()
        
        timing_message3 <- create_timing_message("Functional Annotation: Hayai v3.2", start_annotation, end_annotation)
        log_message(timing_message3)
        
      }
      outtable
    }
    
  })
  
  # this starts block bottom action
  plotReady <- reactiveValues(primary = FALSE, secondary = FALSE)
  
  observeEvent(input$submit, {
    # block submit bottom until annotation if finished
    shinyjs::disable("submit")
    shinyjs::show("text1")
    plotReady$primary <- FALSE

    withProgress(message = 'Processing...', value = 0, {
      outtable <- formData()
      incProgress(1)
    })
    
    # unblock submit bottom
    shinyjs::enable("downloadData")
    shinyjs::hide("text1")
    plotReady$primary <- TRUE
  })
  
  output$hayaiv3 <- DT::renderDT({
    if (plotReady$primary) {
      outtable <- formData()
      DT::datatable(outtable, escape = FALSE, rownames = FALSE, 
                    class = 'cell-border stripe', extensions = 'Buttons', 
                    options = list(
                      columnDefs = list(list(targets = c(5:10), visible = FALSE)),
                      dom = 'Bfrtip',
                      buttons = list(list(extend = 'colvis', columns = c(5:10))), 
                      pageLength = 10
                    ))
    }
  })
  
  log_file <- "./workspace/hayai_v3.2.log"
  
  log_message <- function(message) {
    cat(message, "\n", file = log_file, append = TRUE)
  }
  
  get_zip_path <- function() {
    zip_path <- Sys.which("zip")
    
    # Check if the path is non-empty
    if (zip_path == "") {
      stop("Error: 'zip' utility not found. Please ensure 'zip' is installed in your environment.")
    }
    
    return(zip_path[1])
  }
  
  # Set the zip path dynamically
  zip_path <- get_zip_path()
  
  # Create the download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      file1_label <- input$file1_label
      paste(file1_label, "HayaiAnnotation_v3.2.zip", sep = "_") 
    },
    content = function(file) {
      log_message(paste("Starting to create zip file:", file))
      
      # Define the directory to zip
      workspace_dir <- "./workspace"
      
      # Check if the directory exists
      if (!dir.exists(workspace_dir)) {
        log_message("Error: Workspace directory does not exist.")
        stop("Workspace directory does not exist.")
      }
      
      # Create the zip file
      result <- try(
        zip::zipr(zipfile = file, files = workspace_dir),
        silent = TRUE
      )
      
      if (inherits(result, "try-error")) {
        log_message(paste("Error occurred during zipping:", result))
        stop("Error occurred during zipping.")
      } else {
        log_message("Zip file creation completed successfully.")
      }
    },
    contentType = "application/zip"
  )

  # tab2
  file1_data <- reactive({
    req(input$file1_upload)
    pattern <- "_Hayai_annotation_v3\\.2\\.tsv$"
    file1_upload_name <- input$file1_upload$name
    file_name_match <- grepl(pattern, file1_upload_name)
    if(file_name_match) {
      fread(input$file1_upload$datapath, header = TRUE)    
    } else {
      showNotification("Error: Input ProjectName_Hayai_annotation_v3.2.tsv file", type = "error")
      return(NULL)
    }
  })
  
  file2_data <- reactive({
    req(input$file2_upload)
    pattern <- "_Hayai_annotation_v3\\.2\\.tsv$"
    file2_upload_name <- input$file2_upload$name
    file_name_match <- grepl(pattern, file2_upload_name)
    if(file_name_match) {
      fread(input$file2_upload$datapath, header = TRUE)
    } else {
      showNotification("Error: Input ProjectName_Hayai_annotation_v3.2.tsv file", type = "error")
      return(NULL)
    }
  })
  
  colnamesHayai <- c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "UKB_OrthoDB", "GO_BP", "GO_MF")
  colnamesODB <- c( "Zen_OrthoDB", "ODB_OG")
  
  inputFiles <- reactive({

    tb1 <- file1_data()
    tb2 <- file2_data()
    
    if (is.null(tb1) || is.null(tb2)) {
      showNotification("Error: One or both input files are missing. ", type = "error")
      return(NULL)
    } else {
      
      columns_tb1 <- names(tb1)
      columns_tb2 <- names(tb2)
      
      if (all(colnamesHayai %in% columns_tb1)) {
        tb1_checkpoint1 <- 1
        if (all(colnamesODB %in% columns_tb1)) {
          tb1_checkpoint2 <- 1
        } else {
          tb1_checkpoint2 <- 0
        } 
      } else {
        tb1_checkpoint1 <- 0
        log_message("Error: File 1 does not match expected column names for Hayai v3.2.")
        showNotification("Error: File 1 does not match expected column names for Hayai v3.2.", type = "error")
      }
      
      # Determine the type of tb2
      if (all(colnamesHayai %in% columns_tb2)) {
        tb2_checkpoint1 <- 1
        if (all(colnamesODB %in% columns_tb2)) {
          tb2_checkpoint2 <- 1
        } else {
          tb2_checkpoint2 <- 0
        }  
      } else {
        tb2_checkpoint1 <- 0
        log_message("Error: File 2 does not match expected column names for Hayai v3.2.")
        showNotification("Error: File 2 does not match expected column names for Hayai v3.2.", type = "error")
      }
      
      if (tb1_checkpoint1 == 1 & tb2_checkpoint1 == 1) {
        go_domain <- input$go_domain
        threshold <- input$threshold
        # Filter files
        identity <- input$identity
        apply_filter <- function(tb, identity) {
          condition2 <- tb$Identity >= identity & !is.na(tb$Identity)
          selected_rows <- tb[condition2, ]
          return(selected_rows)
        }
        filt_tb1 <- apply_filter(tb1, identity)
        filt_tb2 <- apply_filter(tb2, identity)
        
        # Interactive give priority based on users option: orthologer (ODB_OG) or zen (Uniprot aligned with orthoDB); v3.2
        priority_col <- input$odb_priority
        secondary_col <- setdiff(c("ODB_OG", "Zen_OrthoDB"), priority_col)
        
        handle_priority <- function(filt_tb, tb_checkpoint2) {
          shinyjs::show("network_ui")
          
          if (tb_checkpoint2 == 1) {
            
            shinyjs::html("network_ui", "Please click on 'Run Analysis' to apply the changes.")
            
            filt_tb <- filt_tb %>%
              mutate(across(all_of(c(priority_col, secondary_col)), ~ na_if(., ""))) %>%
              mutate(OrthoDB = coalesce(.data[[priority_col]], .data[[secondary_col]]))
            
            filt_tb <- filt_tb[, c("Query",	"Accession", "Product_Name", "Identity", "OrthoDB",	
                                   "Zen_OrthoDB",	"Zen_OrthoDB_Desc",	"ODB_OG", "ODB_Description", "GO_MF",
                                   "GO_BP", "GO_CC", "Pfam", "InterPro")] 
            
            
          } else if (tb_checkpoint2 == 0) {
            shinyjs::html("network_ui", "ODB_OG column does not exist. Using Zen_OrthoDB instead.")
            
            filt_tb <- filt_tb[, c("Query",	"Accession",	"Product_Name", "Identity",	"OrthoDB",
                                   "OrthoDB_Desc",	"UKB_OrthoDB", "InterPro", "GO_MF",	"GO_BP",
                                   "GO_CC", "Pfam")] 
            
          }
          
        }
        
        filt_tb1 <- handle_priority(filt_tb1, tb1_checkpoint2)
        filt_tb2 <- handle_priority(filt_tb2, tb2_checkpoint2)
        
      }
      
      return(list(filt_tb1 = filt_tb1, filt_tb2 = filt_tb2))
      
    }
    
  })

  formData2 <- eventReactive(input$secondary_run_button, {
    
    filtered_data <- inputFiles()
    
    if (is.null(filtered_data)) {
      return(NULL)
    } else {
      go_domain <- input$go_domain
      threshold <- input$threshold 
      filt_tb1 <- filtered_data$filt_tb1
      filt_tb2 <- filtered_data$filt_tb2
      
      create_contingency_matrix <- function(tb, col1, col2){
        tb <- tb[, .(get(col1), get(col2))]
        setnames(tb, c(col1, col2))
        
        tb <- tb[!is.na(tb[[col1]]) & !is.na(tb[[col2]]), ]
        long_tb <- as.data.frame(tb) %>%
          tidyr::separate_rows(all_of(col1), sep = ",") %>%
          dplyr::filter(!is.na(.data[[col1]])) 
        contingency_matrix <- table(long_tb[[col2]], long_tb[[col1]])
        melted_data <- reshape2::melt(contingency_matrix, varnames = c(col2, col1))
        melted_data <- melted_data[melted_data$value > 0, ]
        melted_data <- melted_data[order(melted_data$value, decreasing = T),]
        return(melted_data)
      }
      
      melted_tb1 <- create_contingency_matrix(filt_tb1, go_domain, "OrthoDB")
      melted_tb2 <- create_contingency_matrix(filt_tb2, go_domain, "OrthoDB")
      melted_tb1 <- melted_tb1 %>% rename(counts_f1 = value) 
      melted_tb2 <- melted_tb2 %>% rename(counts_f2 = value)
      
      # Function to safely perform a full join
      safe_full_join <- function(df1, df2, by_columns) {
        if (nrow(df1) == 0 & nrow(df2) == 0) {
          # Both data frames are empty: return an empty data frame with the correct structure
          return(data.frame(matrix(ncol = length(by_columns), nrow = 0, 
                                   dimnames = list(NULL, by_columns))))
        } else if (nrow(df1) == 0) {
          # Only df1 is empty: return df2
          return(df2)
        } else if (nrow(df2) == 0) {
          # Only df2 is empty: return df1
          return(df1)
        } else {
          # Neither data frame is empty: perform the full join
          return(df1 %>% full_join(df2, by = c(by_columns)))
        }
      }
      
      data <- safe_full_join(melted_tb1, melted_tb2, by = c("OrthoDB", go_domain))
      
      if (nrow(data) > 0) {
        data <- data %>%
          mutate(across(where(is.numeric), ~ replace_na(., 0)))
        
        data <- data %>%
          mutate(across(where(is.numeric), log1p, .names = "log_{.col}")) # log1p(x)=ln(1+x)
        
        # Log transform the values
        data$diff <- abs(data$log_counts_f1 - data$log_counts_f2)
        sig <- data[data$diff >= threshold,]
        
        if (nrow(sig) > 0) {
          colnames(ortho) <- c("OrthoDB", "OrthoDB_Desc")  
          
          sig_anota <- sig %>% 
            inner_join(ortho, by = c("OrthoDB"))
          
          colnames(go) <- c(go_domain, "GO_Desc")
          
          sig_anota <- sig_anota %>% 
            inner_join(go, by = c(go_domain))
          
          sig_anota <- sig_anota[order(sig_anota$diff, decreasing = TRUE), ]
          sig_anota <- sig_anota[ , c("OrthoDB", "OrthoDB_Desc", go_domain, "GO_Desc", "counts_f1", "counts_f2")]
          
          filename <- paste0("workspace/Network_Dataset_", go_domain, ".tsv")
          fwrite(sig_anota, filename, sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
          sig_anota
          
        }
      }
    }
    
  })
  
  observeEvent(input$secondary_run_button, {

    shinyjs::disable("secondary_run_button")
    shinyjs::disable("downloadData2")
    shinyjs::show("network_ui")
    plotReady$secondary <- FALSE
    
    withProgress(message = 'Processing...', value = 0, {
      sig_anota <- formData2()
      incProgress(1)
    })
    
    sig_anota$go_trunc <- substr(sig_anota$GO_Desc, 1, 30)
    sig_anota$ortho_trunc <- substr(sig_anota$OrthoDB_Desc, 1, 20)
    sig_anota$ortho_trunc <- paste("odb", sig_anota$ortho_trunc, sep = ":")

    output$network <- renderVisNetwork({
      req(selected_value())  # Ensure network is rendered a row is selected
      shinyjs::hide("network_ui")
      
      go_domain <- input$go_domain
      if(go_domain == "GO_MF"){
        selection <- sig_anota[sig_anota$GO_MF == selected_value(), "OrthoDB"]  
      } else if(go_domain == "GO_BP"){
        selection <- sig_anota[sig_anota$GO_BP == selected_value(), "OrthoDB"]  
      }
      
      filtered_sig_anota <- sig_anota[sig_anota$OrthoDB %in% selection, ]  
      edges <- filtered_sig_anota[ , c("ortho_trunc", "go_trunc", "counts_f1", "counts_f2")]
      len_edges <- dim(edges)[1]
      
      if(len_edges <= 100){
        shinyjs::hide("network_ui")
        
        rownames(edges) <- 1:len_edges
        
        edges_df <- data.frame(
          id = 1:nrow(edges),
          from = edges$ortho_trunc,
          to = edges$go_trunc,
          label = paste0("f1: ", edges$counts_f1, ", f2: ", edges$counts_f2)
        )
        
        nodes <- unique(c(edges$ortho_trunc, edges$go_trunc))
        node_df <- data.frame(id = nodes, 
                              group = ifelse(nodes %in% edges$ortho_trunc, "ortho", "go"))
        
        # Render the network using the filtered data
        visNetwork(node_df, edges_df) %>%
          visNodes(size = 6, shadow = TRUE, font = list(size = 16), fixed = FALSE) %>%
          visEdges(scaling = list(min = 1, max = 5), arrows = "to", 
                   font = list(size = 12), color = list(color = "gray")) %>%
          visGroups(groupname = "ortho", color = "orange") %>%
          visGroups(groupname = "go", color = "blue") %>%
          visLayout(randomSeed = 124, improvedLayout = TRUE) %>%
          visPhysics(stabilization = TRUE, solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -30), enabled = TRUE) %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE) %>%
          visInteraction(dragNodes = TRUE, dragView = TRUE, navigationButtons = FALSE)
      } else {
        shinyjs::show("network_ui")
        shinyjs::html("network_ui", "The network is too big and can't be rendered. Please increase network threshold value and Run Analysis again.")
      }
      
    })
    
    output$network_table <- DT::renderDT({
      validate(
        need(!is.null(inputFiles()), "Please upload a valid file to see the data.")
      )
      sig_anota2 <- formData2()
      DT::datatable(sig_anota2, escape = FALSE, rownames = FALSE, selection = "single") 
    })
    
    selected_value <- reactive({
      req(input$network_table_rows_selected)  # Wait until a row is selected
      selected_row <- input$network_table_rows_selected
      sig_anota2 <- formData2()
      go_domain <- input$go_domain
      if(go_domain == "GO_MF"){
        sig_anota2 <- sig_anota2[selected_row, "GO_MF"] 
      } else if(go_domain == "GO_BP"){
        sig_anota2 <- sig_anota2[selected_row, "GO_BP"] 
      }
    })
    
    output$downloadData2 <- downloadHandler(
      
      filename = function() {
        get_file_path(input$go_domain)
      },
      content = function(fname) {
        fs <- get_file_path(input$go_domain)
        
        if (!file.exists(fs)) {
          log_message("Error: Network file is missing.")
          stop("No file to download.")
        }
        
        # Copy the file to the download location
        file.copy(fs, fname)
      },
      
      contentType = "application/tsv"
    )
    
    
    get_file_path <- function(go_domain) {
      if (go_domain == "GO_MF") {
        return("workspace/Network_Dataset_GO_MF.tsv")
      } else if (go_domain == "GO_BP") {
        return("workspace/Network_Dataset_GO_BP.tsv")
      } else {
        stop("Invalid GO domain selected.")
      }
    }
    
    output$full_file1 <- DT::renderDT({
      
      validate(
        need(!is.null(inputFiles()), "Please upload a valid file to see the data.")
      )
      
      filtered_data <- inputFiles()
      outtable <- filtered_data$filt_tb1
      outtable$Accession <- as.character(outtable$Accession)
      outtable$Accession <- paste0("<a href='","https://www.uniprot.org/uniprot/",outtable$Accession,"' target='_blank'>", outtable$Accession,"</a>")
      outtable$OrthoDB <- ifelse(outtable$OrthoDB != "NA",
                                    paste0("<a href='https://www.orthodb.org/?query=", outtable$OrthoDB, "' target='_blank'>", outtable$OrthoDB, "</a>"), "NA")
      t1len <- dim(outtable)[2] - 1  
      
      DT::datatable(outtable, escape = FALSE, rownames = FALSE, 
                    class = 'cell-border stripe', extensions = 'Buttons', 
                    options = list(
                      columnDefs = list(list(targets = c(5:t1len), visible = FALSE)),
                      dom = 'Bfrtip',
                      buttons = list(list(extend = 'colvis', columns = c(5:t1len))), 
                      pageLength = 6
                    ))
      
    })
    
    output$full_file2 <- DT::renderDT({
      
      validate(
        need(!is.null(inputFiles()), "Please upload a valid file to see the data.")
      )
      
      filtered_data <- inputFiles()
      outtable <- filtered_data$filt_tb2
      outtable$Accession <- as.character(outtable$Accession)
      outtable$Accession <- paste0("<a href='","https://www.uniprot.org/uniprot/",outtable$Accession,"' target='_blank'>", outtable$Accession,"</a>")
      outtable$OrthoDB <- ifelse(outtable$OrthoDB != "NA",
                                    paste0("<a href='https://www.orthodb.org/?query=", outtable$OrthoDB, "' target='_blank'>", outtable$OrthoDB, "</a>"), "NA")
      t2len <- dim(outtable)[2] - 1 
      DT::datatable(outtable, escape = FALSE, rownames = FALSE, 
                    class = 'cell-border stripe', extensions = 'Buttons', 
                    options = list(
                      columnDefs = list(list(targets = c(5:t2len), visible = FALSE)),
                      dom = 'Bfrtip',
                      buttons = list(list(extend = 'colvis', columns = c(5:t2len))), 
                      pageLength = 6
                    ))
    })
    
    shinyjs::hide("network_ui")
    shinyjs::enable("downloadData2") 
    shinyjs::enable("secondary_run_button") 
    plotReady$secondary <- TRUE  
    
  })
  
}

shinyApp(ui, server)
