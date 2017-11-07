# Author: Pontus Laestadius
# Version: 1.0

# Iterates over all erlang source files in the current directory.
for f in *.erl; do
	
	# Compiles the erlang file.
	erlc -v $f
	
	# Exho the terminal that it compiled.
	echo "compiled -> $f"
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
