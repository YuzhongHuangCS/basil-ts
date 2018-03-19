#
#   Basil: Predictive Heuristics forecaster for SAGE
#

from flask import Flask, jsonify, request
from datetime import datetime
import os
import subprocess
import pandas as pd
import json
import uuid
import warnings

app = Flask(__name__)

@app.route("/")
@app.route('/index')
def index():
    return "I'm alive!"

# Error handling
# from http://flask.pocoo.org/docs/0.12/patterns/apierrors/
class InvalidUsage(Exception):
    status_code = 400

    def __init__(self, message, status_code=None, payload=None):
        Exception.__init__(self)
        self.message = message
        if status_code is not None:
            self.status_code = status_code
        self.payload = payload

    def to_dict(self):
        rv = dict(self.payload or ())
        rv['message'] = self.message
        return rv

@app.errorhandler(InvalidUsage)
def handle_invalid_usage(error):
    response = jsonify(error.to_dict())
    response.status_code = error.status_code
    return response


@app.route('/forecast', methods = ['GET', 'POST'])
def get_forecast():
  
    # parse request arguments
    content = request.get_json(silent = True)
    backcast = request.args.get('backcast')
    if backcast is None:
        backcast = False
    if backcast == 'True':
        warnings.warn("?backcast=True, do not use this for live forecasts during the RCT")
    
    if content is None:
        raise InvalidUsage('Request does not contain JSON data', status_code=400)
    
    # create request UUID (for filenames)
    request_id = str(uuid.uuid4())
    request_fh = "basil-ts/request-" + request_id + ".json"
    
    # Check request is valid
    
    ts = pd.DataFrame.from_records(content['payload']['historical_data']['ts'])
    # Check first column is convertible to datetime
    # TODO right now this will parse integers as dates, which is not OK. But R down the line throws an error for it.
    try: 
        pd.to_datetime(ts[ts.columns[0]])
    except ValueError: 
        raise InvalidUsage("Request contains invalid time series", status_code=400, 
                           payload = {'error_message': 'first column could not be parsed to datetime'})
    # Reject multi-variate time-series
    if ts.shape[1] > 2:
        raise InvalidUsage("Request contains invalid time series", status_code=400,
                           payload = {'error_message': 'time series has more than one value column; support for multi-variate time-series is not implemented'})
    
    # Pass request to R script
    with open(request_fh, "w") as outfile:
        json.dump(content, outfile)
    try:
        subprocess.check_output(["Rscript", "--vanilla", "basil-ts/ts-forecast.R", request_id, str(backcast)], shell = False,
                                stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        raise InvalidUsage("Internal R error", status_code=500, payload = {'r_error_message': e.output.decode("utf-8")})

    resp_fh = 'basil-ts/forecast-' + request_id + '.json'
    with open(resp_fh, "r") as resp:
        fcasts = json.load(resp)
    os.remove(resp_fh)
    return(jsonify(fcasts))

if __name__ == '__main__':
    app.run(debug=True, host = '0.0.0.0')

