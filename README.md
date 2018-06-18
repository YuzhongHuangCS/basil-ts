# basil-ts: time-series forecaster for SAGE

A collection of R scripts (`basil-ts/basil-ts`) in a Python Flask microservice (`basil-ts`). 

[Gallery of forecast charts](tests/README.md)

## Setup/running

### Docker

To start a container running the forecaster in a Docker container:

```bash
cd ~/Work/2017-HFC/SAGE-ward-share/basil-ts

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

```url
http://0.0.0.0:5000/forecast
```

For backcasting, add a `backcast=True` option:

```url
http://0.0.0.0:5000/forecast?backcast=True
```

This will by default drop all data from the beginning of the question period on. Another argument controls how much data is dropped, `drop-after=YYYY-mm-dd`: 

```url
'http://0.0.0.0:5000/forecast?backcast=True&drop-after=2017-10-29'
```

This should be a date in ISO format, i.e. 'YYYY-mm-dd'. If it exceeds the question period end date it will be reset to the question period end date - 1. 

List of options:

<table>
<thead>
<tr class="header">
<th>Option</th>
<th>Value</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>backcast</code></td>
<td>True</td>
<td>Use with backcasting option, which will drop data after question start (default) or `drop-after` if specified.</td>
</tr>
<tr class="even">
<td><code>drop-after</code></td>
<td>YYYY-MM-DD</td>
<td>If `backcast=True`, drop data after this date.</td>
</tr>
<tr class="odd">
<td>`quick`</td>
<td>True</td>
<td>Do not estimate secondary models in `forecasts`, only estimate main ARIMA model forecast.</td>
</tr>
</tbody>
</table>

### Request conventions / API input file expectations

Data aggregation

- For questions with weird fixed time periods like a 40-day period, the data in the request will be daily and aggregation will take place in basil-ts. 
- For questions with regular time periods (day, week, month), data in the request will already be aggregated and the request should include additional information: 
    - a `aggregated-data` field identifying whether the data were aggregated in the platform, "true/false".
    - a `last-event-date` field listing the date through which the source data reach. This is used to handle partial data for a time period. 
- For monthly data, the reference date should be the first of the month, e.g. the time series historical data should be indexed by the first day of each month. 


### Response format

The response is a JSON object. It contains the ARIMA forecast and two other JSON objects, one containing forecasts for a larger set of models, e.g. ETS, random walk, etc., and the other one ("parsed_request") has debugging info on how the request was internally processed. 

```
{
  FORECAST
  "forecasts": {
    # a named set of FORECAST objects containing forecasts from different models,
    # including the one copied at the top level above
    "ARIMA": { FORECAST },
    "ETS": { FORECAST },
    ...
  },
  "parsed_request": { information on how the request was process internally }
}
```

Where FORECAST has the following structure:

```
"model": [ "ARIMA" ],

# Raw time series forecast
"ts": [
  ...,
  ["2018-05-01", "1272", "1250.343", "1293.657"]
],
"ts_colnames": ["date", "Poin Forecast", "Lo 95", "Hi 95"],

# Answer category probabilities
# for multinomial IFPs, this will contain a probability for all answer options
# for binary IFPs, this will be a single probability for the "Yes" option
"option_probabilities": [0.0001, 0.3729, 0.6234, 0.0037, 3.3857e-10],
"option_labels": [ <copied from request separations> ],

"forecast_is_usable": [ 1 ], 
"trainN": [ 60 ],             # how many data points were used for training 
                              # the model
"to_date": [ "2018-05-31" ],  # forecast covers through this date, compare 
                              # to "ts" date above
"internal": { <internal model-related info> }
```

- "to_date": the date through which the forecast goes; this should be the end date of the question time period, aka the resolution date in the charts


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
Rscript -e 'library(rmarkdown); rmarkdown::render("tests/README.Rmd")'

# To run a specific IFP
bash tests/test.sh 1055

# To pretty format io files
bash tools/format_inputs.sh
```

