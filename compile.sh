# Author: Pontus Laestadius
# Version: 1.3

# Intro message.
echo "-----------FUML SERVER COMPILNATION SCRIPT VERSION 1.3-----------"

# Iterates over all erlang source files in the current directory.
cd Server/src
for f in *.erl; do
	
	# Echo the terminal that it is compiling
	echo "> $f"
	
	# Compiles the erlang file.
	erlc -v $f
done

cd ../../

# What erlang module function to run.
EXECUTE="loadbalancer init"


echo "--- DOCKER ---"

# Stops all docker instances.
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)

#removes the previous image
docker rmi imagename

# Docker containers in new processes.
docker build -t imagename .
docker run -p 8043:8043 imagename escript nodebalancer.erl 8043  &
docker run -p 8042:8042 imagename escript nodebalancer.erl 8042  &
docker run -p 8041:8041 imagename escript nodebalancer.erl 8041  &

echo "--- DOCKER LOADBALANCER STARTED ---"

# Runs the erlang EXECUTE with no shell and stops once finished.
docker run -p 8040:8040 imagename escript loadbalancer.erl

