# Author: Pontus Laestadius
for f in *.erl; do
	erlc -v $f
	  echo "compiled -> $f"
done

DUMP="compile.dump"
EXECUTE="loadbalancer init"

echo "running $EXECUTE"

RESU=$(erl -noshell -run $EXECUTE -s init stop)
date +"%T" >> $DUMP
echo $RESU
$RES >> $DUMP
