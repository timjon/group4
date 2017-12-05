# Author: Pontus Laestadius
# Version: 1.2

# Intro message.
echo "-----------ERLANG COMPILATION SCRIPT VERSION 1.2-----------"
FINISHED="-------------------------FINISHED!-------------------------"

# Iterates over all erlang source files in the current directory.
for f in *.erl; do
	
	# Echo the terminal that it is compiling
	echo "> $f"
	
	# Compiles the erlang file.
	erlc -v $f
done

# Dump file
DUMP="compile.dump"

# What erlang module function to run.
EXECUTE="loadbalancer init"

# Echo the file it's executing.
echo "running $EXECUTE"

# Stores the execution result when it finishes.
RESU=
	# Runs the erlang EXECUTE with no shell and stops once finished.
	$(erl -noshell -run $EXECUTE -s init stop)


# Store sthe data in the dump file.
date +"%T" >> $DUMP

# Echo the returned code.
echo $RESU

# Store the result in the DUMP file.
$RES >> $DUMP

# Outro message.
echo $FINISHED

