#!/bin/bash


ps -aux|grep nginx|awk '{print $2}'|xargs kill -9
ps -aux|grep "python serv.py"|awk '{print $2}'|xargs kill -9

# Start the first process
nginx
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start nginx: $status"
  exit $status
fi

export DB_NAME=/var/lib/drop/db/node_collector.db
export FLOWS_DIR=/var/lib/drop/flows

HOSTNAME=`env hostname -f`


sed -i "/urlDb:/c\ urlDb:'http://"$HOSTNAME":8080'," js/json2db.js
sed -i "/urlMongo:/c\ urlMongo:'http://"$HOSTNAME":8080'" js/json2db.js


# Start the second process
cd ./tools && (exec python serv.py > /dev/null 2>&1 &)
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start webbone: $status"
  exit $status
fi

# Naive check runs checks once a minute to see if either of the processes exited.
# This illustrates part of the heavy lifting you need to do if you want to run
# more than one service in a container. The container will exit with an error
# if it detects that either of the processes has exited.
# Otherwise it will loop forever, waking up every 60 seconds

while /bin/true; do
  ps aux |grep nginx |grep -q -v grep
  PROCESS_1_STATUS=$?
  ps aux |grep "python serv.py" |grep -q -v grep
  PROCESS_2_STATUS=$?

  # echo $PROCESS_1_STATUS
  # echo $PROCESS_2_STATUS
 
  # If the greps above find anything, they will exit with 0 status
  # If they are not both 0, then something is wrong
  if [ $PROCESS_1_STATUS -ne 0 -o $PROCESS_2_STATUS -ne 0 ]; then
    echo "One of the processes has already exited."
    exit -1
  fi
  sleep 10
done
