#
#   Test sample requests
#

test_request () {
  # arg1: the ifp test request file
  # arg2: expected response
  REQUEST=$1
  EXPECT=$2
  RESPONSE=`curl -H "Content-Type: application/json" -X POST -d @test/requests/$1 http://0.0.0.0:5000/forecast`
  echo $RESPONSE | grep -qi "Request does not contain" && OUT="error"
  echo $RESPONSE | grep -qi "metadata" && OUT="ok"
  if [ -z "$OUT" ]; then
    OUT="not sure"
  fi
  echo $OUT
  if [ "$OUT" == "$EXPECT" ]; then
    echo True
  else
    echo False
  fi
}

request () {
  REQUEST=$1
  RESPONSE=`curl -H "Content-Type: application/json" -X POST -d @test/requests/$1 http://0.0.0.0:5000/forecast`
  echo "$RESPONSE"
}

# empty JSON (file does not exist)
test_request foo.json error

# Example from ISI
test_request example1.json ok

request example1.json 
# Multivariate time series, should fail
request example2.json
# no correct date
request example3.json

# 5: ACLED, daily data aggregated to monthly for question
request ifp5a.json
# data ends month before question, should be two forecasts
request ifp5b.json
# partial data for month before question (less than half/more than half)
request ifp5c.json
request ifp5d.json
# partial data for question period (update forecast compared to 5a)
request ifp5e.json


#### not updated to new API format yet

# 65: oil prices
test_request ifp65a.json ok
# forecasts should be for two time periods
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp65b.json http://0.0.0.0:5000/forecast

# 12: interest rates, monthly data, monthly question
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast
# forecasts should be for two time periods
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12b.json http://0.0.0.0:5000/forecast


# 68
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp68a.json http://0.0.0.0:5000/forecast
