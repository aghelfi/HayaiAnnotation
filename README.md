# Hayai Annotation v3.0
A functional gene prediction tool that integrates orthologs and gene ontology for network analysis

Hayai-Annotation v3 (HAv3) is an R-Shiny application that uses two methods to infer functional annotation: DIAMOND for sequence alignment using UniProtKB Plants as a database and the orthomapper function from OrthoLoger (v3.0.5, “https://orthologer.ezlab.org/”) using the node Viridiplantae to detect orthologs.

## Operation System
- Run on Linux and MacOS (Intel)

## Dependencies

### Orthologer (Docker version; https://orthologer.ezlab.org/)

```
docker pull ezlabgva/orthologer:v3.0.5

# create a workdir for HAv3 called hayai

mkdir -p .hayai/src/orthologer
cd .hayai/src/orthologer
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

### Diamond (v2.1.9)
Download Diamond and place on src directory
```
cd hayai/src
wget https://github.com/bbuchfink/diamond/releases/download/v2.1.9/diamond-linux64.tar.gz
tar xzf diamond-linux64.tar.gz
```

# test if diamond is working 
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

  



