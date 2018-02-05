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

# empty JSON (file does not exist)
test_request foo.json error

# Example from ISI
test_request example1.json ok

curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast

# Multivariate time series, should fail
curl -H "Content-Type: application/json" -X POST -d @test/requests/example2.json http://0.0.0.0:5000/forecast

# 65: oil prices
test_request ifp65a.json ok
# forecasts should be for two time periods
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp65b.json http://0.0.0.0:5000/forecast

# 12: interest rates, monthly data, monthly question
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast
# forecasts should be for two time periods
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12b.json http://0.0.0.0:5000/forecast

# 5: ACLED, daily data aggregated to monthly for question
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp5a.json http://0.0.0.0:5000/forecast
# data ends month before question, should be two forecasts
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp5b.json http://0.0.0.0:5000/forecast
# partial data for month before question
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp5c.json http://0.0.0.0:5000/forecast
# partial data for question period (update forecast compared to 5a)
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp5d.json http://0.0.0.0:5000/forecast

# 68
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp68a.json http://0.0.0.0:5000/forecast
