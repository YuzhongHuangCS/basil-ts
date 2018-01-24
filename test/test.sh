curl -H "Content-Type: application/json" -vX POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast | tail

# 65: oil prices
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp65a.json http://0.0.0.0:5000/forecast
# forecasts should be for two time periods
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp65b.json http://0.0.0.0:5000/forecast

# 12: interest rates, monthly data, monthly question
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast
# forecasts should be for two time periods
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp12b.json http://0.0.0.0:5000/forecast

# 5: ACLED, daily data aggregated to monthly for question
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp5a.json http://0.0.0.0:5000/forecast
# data ends month before question, should be two forecasts
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp5b.json http://0.0.0.0:5000/forecast
# partial data for month before question
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp5c.json http://0.0.0.0:5000/forecast
# partial data for question period (update forecast compared to 5a)

# 68
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp68a.json http://0.0.0.0:5000/forecast
