#
#   Test sample requests
#

REQUEST=$1

curl -H "Content-Type: application/json" -X POST \
  -d @tests/io/andy_input_$1.json \
  http://0.0.0.0:5000/forecast \
  > tests/io/andy_output_$1.json

# # prettify the JSON response
# python3 -m json.tool tests/io/andy_output_$1.json > temp.json
#   mv temp.json tests/io/andy_output_$1.json