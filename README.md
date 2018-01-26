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
# the sample request from ISI, returns really long answer so only check last bit
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast | tail
curl -H "Content-Type: application/json" -X POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast > test/responses/example1.json

curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast

# this should return error (ifp12.json does not exist)
curl -H "Content-Type: application/json" -X POST -d @test/requests/ifp12.json http://0.0.0.0:5000/forecast 
# more intentional errors
curl -H "Content-Type: application/json" -X POST -d @test/requests/example2.json http://0.0.0.0:5000/forecast 
curl -H "Content-Type: application/json" -X POST -d @test/requests/example3.json http://0.0.0.0:5000/forecast 
```

### Response format

```
{
  # METADATA when the forecast was created, and if the request had 'hfcId' in the metadata, it will be copied here
  "metadata": {
    "forecastCreatedAt": [
      "2018-01-24T10:00:30"
    ], 
    "hfcId": {}
  }, 
  # MODEL INFO this is mostly stuff for checking what the R portion of the app is doing
  # One relevnat piece of info for down the line are model fit stats, "rmse" etc. See below.
  "model_info": {
    "data_period": [
      "month"
    ], 
    "h": [
      1
    ], 
    "lambda": {}, 
    "question_period": [
      "month"
    ], 
    # MODEL FIT STATS
    # rmse: root mean squared error for the forecast model
    # rmse_mean: root mean squared error for 'prediction always = mean of the series'
    # rmse_rwf: rmse for a naive random walk forecast
    # Pedro at some point asked for model fit stats, so that we know whether a model is good enough to
    # present at all. For that, as a first cut, i'd suggest the heuristic that if rmse is lower than 
    # both rmse_mean and rmse_rwf, it's good enough to use it. 
    "rmse": [
      0.0407
    ], 
    "rmse_mean": [
      0.564
    ], 
    "rmse_rwf": [
      0.0617
    ], 
    "series_type": [
      "continuous"
    ], 
    "skew": [
      1.0368
    ]
  }, 
  # FORECASTS FOR THE QUESTION OPTIONS
  # These are probabilities for each of the answer options in the request
  # This is the part that will be in the histogram portion of the plots shown to users (??)
  "options": {
    "name": [
      "Less than -1.76", 
      "Between -1.76 and -0.76, inclusive", 
      "More than -0.76 but less than 0.10", 
      "Between 0.10 and 1.10, inclusive", 
      "More than 1.10"
    ], 
    "phat": [
      7.9425e-264, 
      1.044e-25, 
      1, 
      0, 
      0
    ]
  }, 
  # RAW FORECASTS
  # Raw forecasts that are on the same scale as the time series data passed in the request
  # This is the line + interval portion of the plot that will be shown to users (??)
  # A time series with columns for date, lower/upper 80% and 95% prediction intervals, and mean prediction
  # Depending on how many time periods ahead the forecast has to be, this can contain multiple rows, but always
  # at least 1 row.
  "raw_forecasts": {
    "_row": [
      "95%"
    ], 
    "date": [
      "2017-08-01"
    ], 
    "l80": [
      -0.3835
    ], 
    "l95": [
      -0.4115
    ], 
    "mean": [
      -0.3307
    ], 
    "u80": [
      -0.2779
    ], 
    "u95": [
      -0.2499
    ]
  }
}

```

*****

## Misc notes

### macOS

```bash
cd ~/Work/SAGE-wardassoc/basil-ts

python3 -m venv env
source env/bin/activate
pip3 install -r requirements.txt
```

```bash
cd ~/Work/SAGE-wardassoc/basil-ts

source env/bin/activate
export FLASK_APP=app.py
flask run --host=0.0.0.0

deactivate
```

```bash
curl -H "Content-Type: application/json" -vX POST -d @test/example1.json http://0.0.0.0:5000/forecast
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

For API:

http://flask-restplus.readthedocs.io/en/stable/quickstart.html

https://flask-restful.readthedocs.io/en/latest/

http://michal.karzynski.pl/blog/2016/06/19/building-beautiful-restful-apis-using-flask-swagger-ui-flask-restplus/

