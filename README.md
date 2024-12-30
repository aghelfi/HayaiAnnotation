# Hayai Annotation v3.2

A functional gene prediction tool that integrates orthologs and gene ontology for network analysis in plant species.

Hayai Annotation v3.2 is an R-Shiny application that employs two approaches to infer functional annotation:

- **DIAMOND**: For sequence alignment using the UniProtKB Plants database.
- **OrthoLoger's orthomapper**: Utilizing the *Viridiplantae* node to detect orthologs.

Research Article: [Hayai Annotation v3.2](https://doi.org/10.1016/j.csbj.2024.12.011) 

---

## Table of Contents

- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
  - [Install Conda](#install-conda)
  - [Clone the Repository](#clone-the-repository)
  - [Set Up the Environment](#set-up-the-environment)
  - [Download the Database](#download-the-database)
  - [Verify the Download](#verify-the-download)
- [Usage](#usage)
  - [Running Locally with RStudio](#running-locally-with-rstudio)
  - [Running on a Remote Server](#running-on-a-remote-server)
- [Output](#output)
- [Note](#note)
- [Citation](#citation)

---

## Features

### The Functional Annotation Module

  ![Figure1](https://github.com/user-attachments/assets/f2c8c95c-0533-4370-bff7-fe96de82aebd)

  - Implements DIAMOND to align sequences against UniProtKB-Plants (Viridiplantae) database, now including algae.
  - Incorporates ortholog information by integrating OrthoLoger.
  - Supports both protein and DNA FASTA sequences.
  - Provides a user-friendly GUI via R-Shiny.
  - Output results and graphics, downloadable as a zip file.


### The Network Analysis Module

![Figure2](https://github.com/user-attachments/assets/72bdcadd-1d13-4b64-90f4-14bda4d3d2e0)


  - Provides an intuitive interface for comparative annotation analysis between two species. Leveraging ortholog relationships to map genes across species and analyzing their functional similarity through gene ontology (GO) co-occurrences, specifically within Molecular Function (MF) and Biological Process (BP) GO domains.

![leaf_development_network](https://github.com/user-attachments/assets/0bfa8efe-ae09-4b90-9253-20c439f1470f)

---

## Prerequisites

1. **Conda**: Required for managing dependencies and setting up the environment.
2. **Operating System**: Linux or macOS (Intel-based).  
   **Note**: Ensure your system supports Conda.
3. **Jupiter Lab**: This setup is compatible with remote servers and local installations: Follow the instructions in the [Running on a Remote Server](#running-on-a-remote-server) section.
4. Optional **RStudio**: For local installations, download [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
   
   

---

## Installation

### Install [Conda](https://docs.anaconda.com/miniconda/install/)

1. Download the latest Miniconda installer for Linux:

```
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
```
2. Run the installer:
```
bash Miniconda3-latest-Linux-x86_64.sh
```
3. Follow the on-screen instructions to complete the installation. Once installed, restart your terminal or run:

```
source ~/.bashrc
```
### Clone the Repository
Clone the repository:
```
git clone https://github.com/aghelfi/HayaiAnnotation.git
```
### Set Up the Environment
Use the provided env.yml file to set up the Conda environment.

1. Navigate to the cloned repository:
```
cd HayaiAnnotation
```
2. Create and activate the Conda environment:
```
conda env create -f env.yml
conda activate hayai_v3.2
```
This will install all required dependencies, including R and R packages, DIAMOND, and OrthoLoger.

3. Verify installation of DIAMOND:
```
diamond help
```
You should see output similar to:
```
diamond v2.1.9.163 (C) Max Planck Society for the Advancement of Science, Benjamin Buchfink, University of Tuebingen
Documentation, support and updates available at http://www.diamondsearch.org
Please cite: http://dx.doi.org/10.1038/s41592-021-01101-x Nature Methods (2021)
```
### Download the Database
Navigate to the **db** directory:
```
cd HayaiAnnotation/db
```
Step 1: Download the Database

  - Option 1: Using wget
```
wget -c -t 10 --retry-connrefused --waitretry=10 "https://drive.google.com/drive/folders/1C33bsP8HvsRlhsOJB4YWHWkttpDwMqNw?usp=sharing"
```
  - Option 2: Using curl
```
curl -C - --retry 10 --retry-delay 10 -O "https://drive.google.com/drive/folders/1C33bsP8HvsRlhsOJB4YWHWkttpDwMqNw?usp=sharing"
```

---
### Verify the Download
After downloading, verify the integrity of the file:
```
md5sum -c zen.dmnd.md5
```
If the file is intact, you will see a success message.

---

## Usage

### Running Locally with RStudio

1. Install RStudio Desktop.
2. Open RStudio and set the working directory to the cloned repository:
```
setwd("/path/to/HayaiAnnotation")
```
3. Run the application:
```
shiny::runApp('Hayai_v3.2.3.R', host = '0.0.0.0', port = 8787)
```
4. Use the application:

  - A GUI will appear.
  - Select Protein or DNA based on your FASTA file.
  - Upload your sequences in FASTA format.
  - Click on Submit.

5. Retrieve Results:
   
  - Results will be displayed in the GUI.
  - Download complete results and graphics as a zip file using the Download Results button.
  - Output files are also saved in the ./hayai/workspace directory.

---

### Running on a Remote Server

1. SSH into the server with port forwarding:
   
```
ssh -L 8888:localhost:8888 -L 8787:localhost:8787 username@your.server
```
2. Activate the Conda environment and start Jupyter Lab:
  
```
cd /path/to/HayaiAnnotation
conda activate hayai_v3.2
jupyter lab
```
3. Access Jupyter Lab locally:
   
   - Open your browser and navigate to http://localhost:8888.

5. Start the Shiny app in Jupyter Lab's R console:
   
```
shiny::runApp('Hayai_v3.2.3.R', host = '0.0.0.0', port = 8787)
```

5. Access the Shiny app locally:

   - Open your browser and navigate to http://localhost:8787.

---

## Output

  - Results are displayed within the GUI.
  - Downloadable as a zip file containing:
    - Functional analysis reports.
    - Graphics and network tables.
  - All output files are saved in the ./hayai/workspace directory.

---
## Note

  - Jupyter Lab can be used locally as an alternative to RStudio.
  - To minimize unnecessary downloads and installations, it is recommended to run Hayai-Annotation always in the same directory.
  - Before beginning a new analysis, ensure that all necessary files are downloaded.
  - To run OrthoLoger, the FASTA file must contain protein sequences, not DNA sequences.

---
## Citation 

Ghelfi, Andrea and Isobe, Sachiko. Hayai-Annotation: A functional gene prediction tool that integrates orthologs and gene ontology for network analysis in plant species. Computational and Structural Biotechnology Journal, Volume 27, 117 - 126. DOI: 10.1016/j.csbj.2024.12.011.
