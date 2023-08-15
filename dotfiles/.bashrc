# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias hk='hadoop_kinit --no-prompt amp_recommendations'
alias hls='hadoop fs -ls $1'
alias hlst='hadoop fs -ls -t $1'
alias hfs='hadoop fs $1'

# Common Hadoop File System Aliases
alias hf="hadoop fs"                                         # Base Hadoop fs command
alias hfcat="hf -cat"                                        # Output a file to standard out
alias hfchgrp="hf -chgrp"                                    # Change group association of files
alias hfchmod="hf -chmod"                                    # Change permissions
alias hfchown="hf -chown"                                    # Change ownership
alias hfcfl="hf -copyFromLocal"                              # Copy a local file reference to HDFS
alias hfctl="hf -copyToLocal"                                # Copy a HDFS file reference to local
alias hfcp="hf -cp"                                          # Copy files from source to destination
alias hfdu="hf -du"                                          # Display aggregate length of files
alias hfdush="hf -du -s -h"                                        # Display a summary of file lengths
alias hfduh="hf -du -h"                                        # Display a summary of file lengths
alias hfdus="hf -dus"                                        # Display a summary of file lengths
alias hfget="hf -get"                                        # Get a file from hadoop to local
alias hfgetm="hf -getmerge"                                  # Get files from hadoop to a local file
alias hfls="hf -ls"                                          # List files
alias hfll="hf -lsr"                                         # List files recursivly
alias hfmkdir="hf -mkdir"                                    # Make a directory
alias hfmv="hf -mv"                                          # Move a file
alias hfput="hf -put"                                        # Put a file from local to hadoop
alias hfrm="hf -rm"                                          # Remove a file
alias hfrmr="hf -rmr"                                        # Remove a file recursivly
alias hfsr="hf -setrep"                                      # Set the replication factor of a file
alias hfstat="hf -stat"                                      # Returns the stat information on the path
alias hftail="hf -tail"                                      # Tail a file
alias hftest="hf -test"                                      # Run a series of file tests. See options
alias hftouch="hf -touchz"                                   # Create a file of zero length
alias hfcount="hf -count -h -v"                              # file count
alias ykill="yarn application -kill "                        # yarn kill

alias table="column -s, -t "                                 # csv to table format
alias h5="head -n5 "
alias t5="tail -n5 "

#### reset
reset

# echo and eval cmd
eeval(){
    cmd="$@"
    echo "****************************"
    echo $cmd
    echo "****************************" 
    eval $cmd
}

# distcp
distcpBowieToMeteor() {
    cmd="distcp --sync hdfs://pv39-bowie-sla$1 hdfs://usmsc23-meteor-adhoc$1"
    eeval $cmd
}

# distcp MESON to AMPDEV
distcpMesonToDev() {
    cmd="distcp --sync hdfs://usmsc21-meson-sla$1 hdfs://usprz17-amp-dev$1"
    eeval $cmd
}

# distcp METEOR to AMPDEV
distcpMeteorToDev() {
    cmd="distcp --sync hdfs://usmsc23-meteor-adhoc$1 hdfs://usprz17-amp-dev$1"
    eeval $cmd
}

# distcp METEOR to AMPQA
distcpMeteorToQA() {
    cmd="distcp --sync hdfs://usmsc23-meteor-adhoc$1 hdfs://usprz17-amp-qa$1"
    eeval $cmd
}

# distcp AMPDEV to METEOR
distcpDevToMeteor() {
    cmd="distcp --sync hdfs://usprz17-amp-dev$1 hdfs://usmsc23-meteor-adhoc$1"
    eeval $cmd
}

# distcp AMPDEV to AMPQA
distcpDevToQa() {
    cmd="distcp --sync hdfs://usprz17-amp-dev$1 hdfs://usprz17-amp-qa$1"
    eeval $cmd
}

# distcp MESON to METEOR
distcpMesonToMeteor() {
    cmd="distcp --sync hdfs://usmsc21-meson-sla$1 hdfs://usmsc23-meteor-adhoc$1"
    eeval $cmd
}

# distcp METEOR to MESON 
distcpMeteorToMeson() {
    cmd="distcp --sync hdfs://usmsc23-meteor-adhoc$1 hdfs://usmsc21-meson-sla$1" 
    eeval $cmd
}



####### SPARK COMMANDS ########
## SPARK PARAMS
export SPARK_BASH_PARAMS=' --master yarn --deploy-mode cluster --conf spark.dynamicAllocation.enabled=false --conf spark.dynamicAllocation.maxExecutors=50 --executor-memory 64g --executor-cores 5 --driver-memory 32g --queue root.adhoc.amp_recommendations --conf spark.yarn.maxAppAttempts=1 --conf yarn.resourcemanager.am.max-attempts=1 --conf spark.network.timeout=1000s --conf spark.executor.heartbeatInterval=30s --conf spark.ui.showConsoleProgress=false --conf spark.yarn.executor.memoryOverhead=8g --conf spark.kryoserializer.buffer.max=2047m'


# ### Usage: my-spark-submit CLASS REMAINING_PARAMS
# example: my-spark-submit com.apple.amp.disco.reco.science.mlr.books.UploadMangaTrainingDataToS3 --num-executors 5 jars/book-reco-spark-pipeline-1.1.0-SNAPSHOT-shadow.jar --flow mlr-adhoc --correlation-key 20200304-000000 
my-spark-submit() {
    CLASS=$1
    shift
    cmd="spark-submit --class $CLASS $SPARK_BASH_PARAMS $@  --properties-leaf /config/meteor/cluster.properties --num-partitions 100 --extra-properties '{}' --yarn-master"

    eeval $cmd
}

# get latest file timestamp
hlts() {
    LOC=${1:-/group/amp_recommendations/prod/books/item-features}
    PATTERN=${2:-_SUCCESS}
    
    cmd="hadoop fs -ls $LOC/*/$PATTERN | grep $LOC | awk '{print \$8}'| tac | head -1 |grep -oP '[0-9]+-[0-9]+'"

    # eeval $cmd
    eval $cmd
}

sparkshell2() { 
  spark-shell --queue root.scheduled.amp_recommendations --master yarn --deploy-mode client --conf spark.dynamicAllocation.maxExecutors=100
}
sparkshell() {
  spark-shell --queue root.adhoc.amp_recommendations --num-executors 10 --driver-memory 12G --executor-memory 4G --executor-cores 2 --deploy-mode client
}

# hadoop_kinit --no-prompt amp_recommendations

