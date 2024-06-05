list.of.packages <- c("shiny","shinydashboard", "shinyjs", "shinyBS", "data.table", "dplyr", "reshape2", "ggplot2", "tidyr", "igraph", "ggraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE, repos="https://cran.ism.ac.jp//")

suppressMessages(require(shiny))
suppressMessages(require(shinydashboard))
suppressMessages(require(shinyjs))
suppressMessages(require(shinyBS))
suppressMessages(require(data.table))
suppressMessages(require(dplyr))
suppressMessages(require(reshape2))
suppressMessages(require(ggplot2))
suppressMessages(require(tidyr))
suppressMessages(require(igraph))
suppressMessages(require(ggraph))


options(shiny.maxRequestSize = 150*1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "Hayai-Annotation"),

  dashboardSidebar(
    tags$head(
      tags$style(
        HTML("
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
          }
        ")
      )
    ),
    div(style = "padding: 10px;", 
        radioButtons("align", "Query Sequence Type", 
                     choices = list("Protein" = "blastp", "DNA" = "blastx"), 
                     selected = "blastp"),
        br(),
        shinyBS::bsPopover("align", "<b>Query sequences</b>", 
                           "Protein: only amino acid sequences <br> DNA: only nucleotide sequences", 
                           "bottom", trigger="hover", options = list(container = "body")),
        tags$a(href = "ftp://ftp.kazusa.or.jp/pub/hayai/hayai-annotation/plants/sample.fasta", target = "_blank", "Download Sample"),
        fileInput("userInput", "Upload FASTA File", multiple = FALSE, 
                  accept = c(".fasta", ".fa", ".faa", ".fna")),
        actionButton("submit", "Submit"),
        actionButton("question_mark", "?"),
        shinyBS::bsPopover("question_mark", "<b>Attention</b>", 
                           "Please check if query is DNA or Protein", "right", 
                           trigger="click", options = list(container = "body")), 
        shinyjs::hidden(p(id = "text1", "Processing, please wait.")),
        br(),
        downloadLink("downloadData", "Download Results")
    )
  ),
   dashboardBody(     
    shinyjs::useShinyjs(),
    h3(id="big-heading", "Hayai-Annotation Plants v3.0"),
    h4(id="big-heading", "A functional gene prediction tool that integrates orthologs and gene ontology for network analysis"),
    h4(id="big-heading", "Specialized in Plant Species"),
    tags$hr(style="border-color: DarkBlue;"),    
    hr(),
    fluidRow(
    DT::dataTableOutput("hayaiv3")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    shinyjs::toggleState("submit", !is.null(input$userInput))
  })
  observeEvent(input$userInput , {
    inFile <- input$userInput
    if (is.null(inFile))
      return(NULL)
    if(!dir.exists("./temp")){
      system("mkdir ./temp")
    }
    if(!dir.exists("./workspace")){
      system("mkdir ./workspace")
    }
    write.csv(inFile, "./temp/df.csv", row.names = FALSE) 
  })
  formData <- eventReactive(input$submit, {
    userInputFile <- read.csv("./temp/df.csv")
    userInputFile_path <- as.character(userInputFile$datapath)

    generate_random_name <- function() {
      chars <- c(letters, LETTERS, 0:9)
      paste(sample(chars, 8, replace = TRUE), collapse = "")
    }
    random_name <- generate_random_name()
    output_file_path <- paste0("./temp/", random_name, ".fasta")
    print(random_name)

    count_terms <- function(column) {
      terms_list <- strsplit(column, ",")
      valid_terms <- terms_list[!is.na(column)]
      all_terms <- unlist(valid_terms)
      term_counts <- table(all_terms)
      return(term_counts)
    }
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

    align <- input$align
    
    print(output_file_path)

    if (align == "blastp"){
      run_diamond <- paste("./src/diamond blastp -q", output_file_path, "-d ./db/zen.dmnd -o ./workspace/output_diamond.m10 --threads 10 --more-sensitive --top 1 --evalue 1e-6 --quiet --un ./workspace/unaligned.fasta -f 6 qseqid pident length qstart qend sstart send evalue bitscore stitle", collapse = "")
    }
    if (align == "blastx"){
      run_diamond <- paste("./src/diamond blastx -q", output_file_path, "-d ./db/zen.dmnd -o ./workspace/output_diamond.m10 --threads 10 --more-sensitive --top 1 --evalue 1e-6 --quiet --un ./workspace/unaligned.fasta -f 6 qseqid pident length qstart qend sstart send evalue bitscore stitle", collapse = "")
    }
    system(run_diamond) 
    writeLines(run_diamond, "./temp/diamond.log")

    if (align == "blastp"){ 
      run_orthologer <- "docker"
      output_file_path2 <- sub("./temp", "/tmp", output_file_path)
      args <- paste("run -u $(id -u) -v ./temp:/tmp -v ./src/orthologer:/odbwork ezlabgva/orthologer:v3.0.5 ./orthomapper.sh MAP", random_name, output_file_path2, "33090", collapse = "")
      log_file_path <- "./temp/orthologer.log"
      system2(run_orthologer, args = args, stdout = log_file_path, stderr = log_file_path, wait = TRUE)
    }
    
    size_test_file <- check_file_lines("./workspace/output_diamond.m10")
    print(size_test_file)
    
    if(size_test_file == 0){ 
      outtable <- data.frame("Query" = 0, "Target" = 0)
    } else {

      load(file = "./data/33090.rda"); load(file = "./data/goid2term.rda"); load(file = "./data/interpro.rda")
      diamond_output <- fread("./workspace/output_diamond.m10", header = FALSE, sep = "\t")
      fwrite(diamond_output, "./temp/formatted_diamond.m6", sep = "|", quote = FALSE)
      uniprot <- fread("./temp/formatted_diamond.m6", header = FALSE, sep = "|", quote = "", stringsAsFactors = FALSE)
      colnames(uniprot) <- c("Query", "Identity", "Length", "qstart", "qend", "sstart", "send", "Evalue", "Score", "Accession", "Evidence_existence", "InterPro", "Pfam", "GO_BP", "GO_MF", "GO_CC", "Product_Name", "OrthoDB")
      uniprot$Evidence_existence <- sub("PE: ", "", uniprot$Evidence_existence)
      uniprot$InterPro <- sub("InterPro: ", "", uniprot$InterPro)
      uniprot$Pfam <- sub("Pfam: ", "", uniprot$Pfam)
      uniprot$GO_BP <- sub("GO_BP: ", "", uniprot$GO_BP)
      uniprot$GO_MF <- sub("GO_MF: ", "", uniprot$GO_MF)
      uniprot$GO_CC <- sub("GO_CC: ", "", uniprot$GO_CC)
      uniprot$Product_Name <- sub("Product_Name: ", "", uniprot$Product_Name)
      uniprot$OrthoDB <- sub("OrthoDB: ", "", uniprot$OrthoDB)
      ogfilename <- "./src/orthologer/Results/PROJECTNAME.og.annotations"
      ogfilename <- sub("PROJECTNAME", random_name, ogfilename)
      size_test_file2 <- check_file_lines(ogfilename)
      print(ogfilename)
      print(size_test_file2)

      if(size_test_file2 > 0){
        orthologer <- fread(ogfilename, header = TRUE, stringsAsFactors = FALSE)
        colnames(orthologer) <- c("Query", "ODB_OG", "ODB_evalue", "ODB_score", "ODB_COG_category", "ODB_Description", "ODB_GOs_mf", "ODB_GOs_bp", "ODB_EC", "ODB_KEGG_ko", "ODB_Interpro")
        data <- uniprot %>% full_join(orthologer, by = "Query")
        data <- data %>% mutate(OrthoDB = if_else(!is.na(ODB_OG) & ODB_OG != "", ODB_OG, OrthoDB))
        data <- data %>% left_join(ortho, by = "OrthoDB")
        data <- data[!duplicated(data[, c("Query")]), ]
        outtable <- data[ , c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "Evidence_existence", "InterPro", "Pfam", "GO_BP", "GO_MF", "GO_CC","Identity", "Length", "Evalue", "Score", "ODB_OG", "ODB_COG_category", "ODB_Description", "ODB_GOs_mf", "ODB_GOs_bp", "ODB_EC", "ODB_KEGG_ko", "ODB_Interpro", "ODB_evalue", "ODB_score")]
        ### Print out main data with replaced OrthoDB if ODB_OG from orthologer is available
        fwrite(outtable, "./workspace/Hayai_annotation_v3.0.tsv", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
      } else {
        data <- uniprot %>% left_join(ortho,  by = "OrthoDB")
        data <- data[!duplicated(data[, c("Query")]), ]
        outtable <- data[ , c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "Evidence_existence", "InterPro", "Pfam", "GO_BP", "GO_MF", "GO_CC","Identity", "Length", "Evalue", "Score")]
        fwrite(outtable, "./workspace/Hayai_annotation_v3.0.tsv", sep = "\t", col.names = TRUE, row.names = FALSE, quote = FALSE)
      }
      outtable_len <- length(outtable$Accession)
      if(outtable_len > 0){
        outtable$Accession <- as.character(outtable$Accession)
        outtable$Accession <- paste0("<a href='","https://www.uniprot.org/uniprot/",outtable$Accession,"' target='_blank'>", outtable$Accession,"</a>")
        outtable$OrthoDB <- paste0("<a href='","https://www.orthodb.org/?query=",outtable$OrthoDB, "' target='_blank'>" ,outtable$OrthoDB,"</a>")
        outtable <- outtable[ , c("Query", "Accession", "Product_Name", "OrthoDB", "OrthoDB_Desc", "Evidence_existence")]

        # GO_MF
        go_counts <- count_terms(data$GO_MF)
        go_counts <- go_counts[names(go_counts) != "NA"]
        go_counts_df <- data.frame(GO = names(go_counts), Count = as.integer(go_counts))
        go_counts_df <- go_counts_df %>% inner_join(go,  by = "GO")
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
        go_counts_df <- go_counts_df %>% inner_join(go, by = "GO")
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
        go_counts_df <- go_counts_df %>% inner_join(go, by = "GO")
        go_counts_df <- go_counts_df[, c( "GO", "GO_Desc", "Count")]
        go_counts_df <- go_counts_df[order(go_counts_df$Count, decreasing = TRUE),]
        write.table(go_counts_df, "./workspace/Hayai_annotation_GO_CC.tsv", sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
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
        go_counts_df <- go_counts_df %>% inner_join(interpro, by = "InterPro")
        go_counts_df <- go_counts_df[, c( "InterPro", "InterPro_Desc", "Count")]
        go_counts_df <- go_counts_df[order(go_counts_df$Count, decreasing = TRUE),]
        write.table(go_counts_df, "./workspace/Hayai_annotation_Interpro.tsv", sep="\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
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
        # Heatmap and Network analysis
        ### OrthoDB vs GO_MF
        tb <- data[,c("GO_MF", "OrthoDB")]
        tb <- tb[tb$GO_MF != "NA" & tb$OrthoDB != "NA", ]
        # Transforming the data from line separated into different line
        long_tb <- tb %>%
          tidyr::separate_rows(GO_MF, sep = ",") %>%
          dplyr::filter(!is.na(GO_MF)) 
        # Contingency table (co-occurrence matrix)
        co_occurrence_matrix <- table(long_tb$OrthoDB, long_tb$GO_MF)
        melted_data <- reshape2::melt(co_occurrence_matrix, varnames = c("OrthoDB", "GO_MF"))
        melted_data <- melted_data[melted_data$value > 0, ]
        melted_data <- melted_data[order(melted_data$value, decreasing = T),]
        melted_desc <- melted_data %>% inner_join(ortho, by = "OrthoDB")
        colnames(go)[1] <- "GO_MF"
        melted_desc <- melted_desc %>% inner_join(go, by = "GO_MF")
        melted_desc <- melted_desc[order(melted_desc$value, decreasing = T),]
        melted_desc <- melted_desc[, c("OrthoDB", "OrthoDB_Desc", "GO_MF", "GO_Desc", "value" )]
        fwrite(melted_desc, "./workspace/Correlations_OrthoDB_GO_MF.tsv", sep = "\t", col.names = TRUE, row.names = FALSE)
        if(dim(melted_data)[1] > 100) {
          top_melted <- melted_data[1:100, ]
          pdf("./workspace/Graph_Heatmap_OrthoDB_GO_MF.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggplot(top_melted, aes(x = GO_MF, y = OrthoDB, fill = value)) +
            geom_tile() +  
            scale_fill_gradient(low = "blue", high = "red") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Co-occurrence Heatmap - OrthoDB OGs & Molecular Function GOs", x = "GO", y = "OrthoDB"))
          dev.off()
          # Create Network
          edges <- top_melted[ , 1:2]
          rownames(edges) <- 1:dim(edges)[1]
          nodes <- unique(edges$OrthoDB)
          temp <- unique(edges$GO_MF)
          nodes <- c(nodes, temp)
          graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
          # Plot Network
          pdf("./workspace/Graph_Network_OrthoDB_GO_MF.pdf", width = 11, height = 11 )
          print(ggraph(graph, layout = "fr") + 
          geom_edge_link(edge_alpha = 0.5, edge_width = 0.5, edge_color = "gray40") +
          geom_node_point(color = "orange", size = 5) +
          geom_node_text(aes(label = name), repel = TRUE, color = "darkred", fontface = "plain") +
          theme_void() +
          ggtitle("Network - OrthoDB OGs & Molecular Function GOs") +
          theme(plot.title = element_text(hjust = 0.5)))
          dev.off()
        }
        ### OrthoDB vs GO_BP
        tb <- data[,c("GO_BP", "OrthoDB")]
        tb <- tb[tb$GO_BP != "NA" & tb$OrthoDB != "NA", ]
        # Transforming the data from line separated into different line
        long_tb <- tb %>%
          tidyr::separate_rows(GO_BP, sep = ",") %>%
          dplyr::filter(!is.na(GO_BP)) 
        # Contingency table (co-occurrence matrix)
        co_occurrence_matrix <- table(long_tb$OrthoDB, long_tb$GO_BP)
        melted_data <- reshape2::melt(co_occurrence_matrix, varnames = c("OrthoDB", "GO_BP"))
        melted_data <- melted_data[melted_data$value > 0, ]
        melted_data <- melted_data[order(melted_data$value, decreasing = T),]
        melted_desc <- melted_data %>% inner_join(ortho, by = "OrthoDB")
        colnames(go)[1] <- "GO_BP"
        melted_desc <- melted_desc %>% inner_join(go, by = "GO_BP")
        melted_desc <- melted_desc[order(melted_desc$value, decreasing = T),]
        melted_desc <- melted_desc[, c("OrthoDB", "OrthoDB_Desc", "GO_BP", "GO_Desc", "value" )]
        fwrite(melted_desc, "./workspace/Correlations_OrthoDB_GO_BP.tsv", sep = "\t", col.names = TRUE, row.names = FALSE)
        if(dim(melted_data)[1] > 100) {
          top_melted <- melted_data[1:100, ]
          pdf("./workspace/Graph_Heatmap_OrthoDB_GO_BP.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggplot(top_melted, aes(x = GO_BP, y = OrthoDB, fill = value)) +
            geom_tile() + 
            scale_fill_gradient(low = "blue", high = "red") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Co-occurrence Heatmap - OrthoDB OGs & Biological Process GOs", x = "GO", y = "OrthoDB"))
          dev.off()
          # Create Network
          edges <- top_melted[ , 1:2]
          rownames(edges) <- 1:dim(edges)[1]
          nodes <- unique(edges$OrthoDB)
          temp <- unique(edges$GO_BP)
          nodes <- c(nodes, temp)
          graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
          # Plot Network
          pdf("./workspace/Graph_Network_OrthoDB_GO_BP.pdf", width = 11, height = 11 )
          par(mar=c(2,2,6,2), oma=c(0.5,16,5,0.5))
          print(ggraph(graph, layout = "fr") + 
            geom_edge_link(edge_alpha = 0.5, edge_width = 0.5, edge_color = "gray40") +
            geom_node_point(color = "orange", size = 5) +
            geom_node_text(aes(label = name), repel = TRUE, color = "darkred", fontface = "plain") +
            theme_void() +
            ggtitle("Network - OrthoDB OGs & Biological Process GOs") +
            theme(plot.title = element_text(hjust = 0.5)))
          dev.off()
        } 
      }
      outtable
    }
    
  })
  
  # this starts block bottom action
  plotReady <- reactiveValues(ok = FALSE)
  
  observeEvent(input$submit, {
    # block submit bottom until annotation if finished
    shinyjs::disable("submit")
    shinyjs::show("text1")
    plotReady$ok <- FALSE
    datasetHayai <- formData()
    # unblock submit bottom
    Sys.sleep(1)
    plotReady$ok <- TRUE
  })
  
  output$hayaiv3 <- DT::renderDataTable({
    if (plotReady$ok) {
      shinyjs::hide("text1")
      outtable <- formData()
      DT::datatable(outtable, escape=FALSE, rownames=F)
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output_HayaiAnnotation_v3", "zip", sep=".")
    },
    content = function(fname) {
      fs <- c("./workspace/Hayai_annotation_v3.0.tsv", "./workspace/Hayai_annotation_Interpro.tsv", "./workspace/Hayai_annotation_GO_MF.tsv", "./workspace/Hayai_annotation_GO_BP.tsv", "./workspace/Hayai_annotation_GO_CC.tsv", "./workspace/Graph_Hayai_InterPro.pdf", "./workspace/Graph_Hayai_GO_MF.pdf", "./workspace/Graph_Hayai_GO_BP.pdf", "./workspace/Graph_Hayai_GO_CC.pdf", "./workspace/Graph_Heatmap_OrthoDB_GO_BP.pdf", "./workspace/Graph_Heatmap_OrthoDB_GO_MF.pdf", "./workspace/Graph_Network_OrthoDB_GO_BP.pdf", "./workspace/Graph_Network_OrthoDB_GO_MF.pdf", "./workspace/Correlations_OrthoDB_GO_BP.tsv", "./workspace/Correlations_OrthoDB_GO_MF.tsv", "./temp/diamond.log", "./temp/orthologer.log", "./workspace/output_diamond.m10", "./workspace/unaligned.fasta")
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  ) 
}
shinyApp(ui,server)
