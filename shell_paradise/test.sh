num=$(wc -l test.txt | cut -d" " -f 1)
fhalf=$((num / 2))
lhalf=$((num - fhalf))
echo $fhalf
echo $lhalf

tail -n $lhalf test.txt > lhalf.txt
head -n $fhalf test.txt >> lhalf.txt
cat lhalf.txt

