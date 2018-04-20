# basil-ts: time-series forecaster for SAGE

A collection of R scripts (`basil-ts/basil-ts`) in a Python Flask microservice (`basil-ts`). 

[Gallery of forecast charts](tests/test-output.md)

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
curl -H "Content-Type: application/json" -X POST -d @tests/io/example1.json http://0.0.0.0:5000/forecast
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
curl -H "Content-Type: application/json" -X POST -d @tests/io/example1.json http://0.0.0.0:5000/forecast
```

## API

For normal use, use 

```
http://0.0.0.0:5000/forecast
```

For backcasting, add a `backcast=True` option:

```
http://0.0.0.0:5000/forecast?backast=True
```

This will by default drop all data from the beginning of the question period on. Another argument controls how much data is dropped, `drop-after=YYYY-mm-dd`: 

```
http://0.0.0.0:5000/forecast?backast=True&drop-after=2017-10-29
```

This should be a date in ISO format, i.e. 'YYYY-mm-dd'. If it exceeds the question period end date it will be reset to the question period end date - 1. 

### Request conventions

Data aggregation

- For questions with weird fixed time periods like a 40-day period, the data in the request will be daily and aggregation will take place in basil-ts. 
- For questions with regular time periods (day, week, month), data in the request will already be aggregated and the request should include additional information: 
    - a `aggregated-data` field identifying the level of aggregation, e.g. "month".
    - a `last-event-date` field listing the date through which the source data reach, or the last observed date in the source data before aggregation. This is used to handle partial data for a time period. 



### Response format

- "to_date": the date through which the forecast goes; this should be the end date of the question time period, aka the resolution date in the charts

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



## Running tests

Right now the tests are spread over several files/commands. Anyways.

```bash
# from basil-ts dir

# API tests
python3 tests/test_api.py

# Unit tests (R)
Rscript 'basil-ts/tests/testthat.R'

# Run all RCT-A requests
# needs app running in another terminal
python3 tests/run_all.py
Rscript -e 'library(rmarkdown); rmarkdown::render("tests/test-output.Rmd", "html_document")'

# To run a specific IFP
bash tests/test.sh 1055
```



## TODO

- convert old sample requests to new API format
- expand tests; CI?
- truncated normal for catfcast? this might already be covered by existing func
- adjust as stuff breaks...

