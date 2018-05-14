# bash tools/format_inputs.sh

for f in tests/io/andy_input_*.json
do
  echo "$f"
  python3 -m json.tool $f > temp.json
  mv temp.json $f
done