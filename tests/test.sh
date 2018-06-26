#
#   Test sample requests
#

REQUEST=$1

curl -H "Content-Type: application/json" -X POST \
  -d @tests/io/andy_input_$1.json \
  http://0.0.0.0:5000/forecast \
  > tests/io/andy_output_$1.json
