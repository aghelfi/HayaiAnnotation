# Hayai Annotation v3.0

A functional gene prediction tool that integrates orthologs and gene ontology for network analysis

Hayai-Annotation v3 (HAv3) is an R-Shiny application that uses two methods to infer functional annotation: DIAMOND for sequence alignment using UniProtKB Plants as a database and the orthomapper function from OrthoLoger using the node Viridiplantae to detect orthologs.

## Operation System
- Run on Linux and MacOS (Intel)
- Configured to work with 10 threads

## Dependencies

### Summary 
Clone this repository and follow the instructions in the README.txt files in the directories 'db', 'src' and 'src/orthologer'.


### Full instructions

### Install [Orthologer Docker v3.0.5](https://orthologer.ezlab.org/)

```
docker pull ezlabgva/orthologer:v3.0.5

# create a workdir for HAv3 called hayai

mkdir -p ./hayai/src/orthologer
cd ./hayai/src/orthologer
docker run -u $(id -u) -v .:/odbwork ezlabgva/orthologer:v3.0.5 setup_odb.sh

# change 'odb10' to 'odb11': export BUSCO_ODB_VERSION="odb10" -> export BUSCO_ODB_VERSION="odb11" 

awk '{gsub("odb10", "odb11"); print $0}' orthomapper_conf.sh > temp_orthomapper_conf.sh
mv temp_orthomapper_conf.sh orthomapper_conf.sh
egrep "odb11" orthomapper_conf.sh
```
<pre>
egrep result should be as below:

  # BUSCO orthodb version string (eg odb11)
  export BUSCO_ODB_VERSION="odb11"
</pre>

### Install [Diamond v2.1.9](https://github.com/bbuchfink/diamond/wiki/2.-Installation)
Example for Linux: Download diamond and place on src directory
```
cd hayai/src
wget https://github.com/bbuchfink/diamond/releases/download/v2.1.9/diamond-linux64.tar.gz
tar xzf diamond-linux64.tar.gz
```

#### Test if diamond is working 
```
./diamond help
```
<pre>
diamond v2.1.9.163 (C) Max Planck Society for the Advancement of Science, Benjamin Buchfink, University of Tuebingen
Documentation, support and updates available at http://www.diamondsearch.org
Please cite: http://dx.doi.org/10.1038/s41592-021-01101-x Nature Methods (2021)
</pre>

### Download Database (DockerHub)
```
cd hayai/db
wget https://plantgarden.jp/download/zen.dmnd
wget https://plantgarden.jp/download/zen.md5
md5sum -c zen.md5
```
<pre>
  # the result should be:
  zen.dmnd: OK
</pre>

### Install [R v4.3 or latest](https://cran.rstudio.com/) and [Rstudio-destop](https://posit.co/download/rstudio-desktop/) for local access. Install [Rstudio-server](https://posit.co/download/rstudio-server/) if running from a remote server
Open Rstudio 
Go to 'hayai' directory

Load and Run the file Hayai_annotation_v3.R
It will open a graphical interface, you may also open in the browser if you prefer.

Select Protein or DNA, based on you FASTA file.
Upload the FASTA sequences and click on 'Submit'.


![HAv3_RStudio](https://github.com/aghelfi/HayaiAnnotation/assets/5419143/4e7b647e-39d7-4486-9f88-f2de1629f7df)

Upon completion, the results will be displayed in the GUI, and the complete results and graphics can be downloaded as a zip file using the 'Download Results' button.
The output files are also available in the directory ./hayai/workspace 

![Figure_HA_v3_interface](https://github.com/aghelfi/HayaiAnnotation/assets/5419143/74d9ed33-1f00-45a1-bcbc-18aee4d3054b)

