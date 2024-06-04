# In the current directory get orthologer using docker

docker pull ezlabgva/orthologer:v3.0.5

docker run -u $(id -u) -v .:/odbwork ezlabgva/orthologer:v3.0.5 setup_odb.sh

# change 'odb10' to 'odb11': export BUSCO_ODB_VERSION="odb10" -> export BUSCO_ODB_VERSION="odb11" 

awk '{gsub("odb10", "odb11"); print $0}' orthomapper_conf.sh > temp_orthomapper_conf.sh
mv temp_orthomapper_conf.sh orthomapper_conf.sh
egrep "odb11" orthomapper_conf.sh
