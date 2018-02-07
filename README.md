# basil-ts: time-series forecaster for SAGE

A collection of R scripts (`basil-ts/basil-ts`) in a Python Flask microservice (`basil-ts`). 

## Setup/running

### Docker

To start a container running the forecaster in a Docker container:

```bash
cd ~/Work/SAGE-ward-share/basil-ts

# build/run app
docker build -t basil-ts ./ 
docker run -dp 5000:5000 -it --name basil-ts basil-ts

# hello world
curl http://0.0.0.0:5000; echo
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast
```

### macOS

This will skip the Docker part and just run the app in the local terminal:

```bash
cd ~/Work/SAGE-wardassoc/basil-ts

if [ ! -d "env" ]; then
  echo "Setting up Python environment"
  python3 -m venv env
fi
source env/bin/activate
pip3 install -r requirements.txt
python3 app.py

# later
deactivate
```

From another terminal:

```bash
# hello world
curl http://0.0.0.0:5000; echo
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast
```

### More example requests

Some more example request, including ones that fail with known causes:

```bash
# assumes wd is basil-ts already
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast

# this should return error (ifp12.json does not exist)
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12.json http://0.0.0.0:5000/forecast 
# more intentional errors
curl -H "Content-Type: application/json" -X POST -d @test/requests/example2.json http://0.0.0.0:5000/forecast 
curl -H "Content-Type: application/json" -X POST -d @test/requests/example3.json http://0.0.0.0:5000/forecast 
curl -H "Content-Type: application/json" -X POST -d @test/requests/example4.json http://0.0.0.0:5000/forecast 
```

## Response format

```
{
  "ts_colnames": ["date", "actual_forecast", "lower_bound_95_percent", "upper_bound_95_percent"],
  "ts": [
    ["2017-10-31", "1272", "1250.343", "1293.657"]
  ],
  "option_probabilities": [0.0001, 0.3729, 0.6234, 0.0037, 3.3857e-10],
  "forecast_is_usable": [1],
  "forecast_created_at": ["2018-01-31T22:43:26"],
  "model_info": {
    "data_period": {
      "period": ["day"],
      "days": [null]
    },
    "question_period": {
      "period": ["day"],
      "days": [null]
    },
    "question_date": ["2017-10-31"],
    "series_type": ["continuous"],
    "h": [1],
    "skew": [1.1744],
    "lambda": {},
    "rmse": [11.0467],
    "rmse_mean": [170.6564],
    "rmse_rwf": [11.0494]
  }
} 
```



