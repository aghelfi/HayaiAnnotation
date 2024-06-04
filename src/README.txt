# Place here DIAMOND

wget https://github.com/bbuchfink/diamond/releases/download/v2.1.9/diamond-linux64.tar.gz
tar xzf diamond-linux64.tar.gz

# Get orthologer using docker

docker pull ezlabgva/orthologer:v3.0.5

# In the current directory
cd orthologer
docker run -u $(id -u) -v .:/odbwork ezlabgva/orthologer:v3.0.5 setup_odb.sh

# change 'odb10' to 'odb11': export BUSCO_ODB_VERSION="odb10" -> export BUSCO_ODB_VERSION="odb11" 

awk '{gsub("odb10", "odb11"); print $0}' orthomapper_conf.sh > temp_orthomapper_conf.sh
mv temp_orthomapper_conf.sh orthomapper_conf.sh
egrep "odb11" orthomapper_conf.sh
