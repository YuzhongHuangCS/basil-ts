# basil-ts: time-series forecaster for SAGE

A collection of R scripts (`basil-ts/basil-ts`) in a Python Flask microservice (`basil-ts`). 

## Setup

### Docker

```bash
cd ~/Work/SAGE-ward-share/basil-ts
docker build -t basil-ts ./ 
docker images

docker run -dp 5000:5000 -it --name basil-ts basil-ts
docker exec -it basil-ts bin/bash
cd /home/basil-ts
export FLASK_APP=/home/basil-ts/app.py
flask run --host=0.0.0.0
```

http://0.0.0.0:5000 should show a hello world message.

Here's how to try out the sample request you sent. This should return a ridiculously long JSON answer, because the data end almost a year before the forecast date:

```
# assumes wd is basil-ts already
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast

# this should return error (ifp12.json does not exist)
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12.json http://0.0.0.0:5000/forecast 
# more intentional errors
curl -H "Content-Type: application/json" -X POST -d @test/requests/example2.json http://0.0.0.0:5000/forecast 
curl -H "Content-Type: application/json" -X POST -d @test/requests/example3.json http://0.0.0.0:5000/forecast 
```

### Response format

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

*****

## Misc notes

### macOS

```bash
cd ~/Work/SAGE-wardassoc/basil-ts

source env/bin/activate
export FLASK_APP=app.py
flask run --host=0.0.0.0

# from another terminal
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast

deactivate
```

First time around setup

```bash
cd ~/Work/SAGE-wardassoc/basil-ts

python3 -m venv env
source env/bin/activate
pip3 install -r requirements.txt
```

### Setup notes

```bash
cd ~/Work/SAGE-wardassoc/basil-ts

python3 -m venv env
source env/bin/activate

pip3 install Flask
pip3 freeze > requirements.txt

export FLASK_APP=basil-ts.py
flask run

# in browser, now try http://127.0.0.1:5000/

# when done, leave python environment
deactivate
```


