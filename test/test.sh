curl -H "Content-Type: application/json" -vX POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast | tail

# 65
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp65a.json http://0.0.0.0:5000/forecast

# 12
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast

# 5
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp5a.json http://0.0.0.0:5000/forecast

# 68
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp68a.json http://0.0.0.0:5000/forecast
