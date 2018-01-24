#
#   Basil: Predictive Heuristics forecaster for SAGE
#

from flask import Flask, jsonify, request
from datetime import datetime
import os
import subprocess
import pandas as pd
import json

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

    if content is None:
        raise InvalidUsage('Request does not contain JSON data', status_code=400)
    
    with open("basil-ts/request.json", "w") as outfile:
        json.dump(content, outfile)
    
    # TODO...? if multiple requests come in at the same time, could it happen that files are mixed up?
    # maybe name the request and forecast.json files with unique ID (UUID)
    # https://stackoverflow.com/questions/2961509/python-how-to-create-a-unique-file-name
    try:
        subprocess.check_output(["Rscript", "--vanilla", "basil-ts/ts-forecast.R"], shell = False,
                                stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        raise InvalidUsage("Internal R error", status_code=500, payload = {'r_error_message': e.output.decode("utf-8")})
    
    # old code when intermediary was CSV file
    #fcasts = pd.read_csv('basil-ts/forecast.csv')
    #fcasts = fcasts.set_index('date')
    #answer = fcasts.reset_index().to_json(orient = "records", lines = True)
    #response = make_response(answer)
    #response.mimetype = "application/json"
    fcasts = json.load(open('basil-ts/forecast.json'))
    os.remove("basil-ts/forecast.json")
    return(jsonify(fcasts))

if __name__ == '__main__':
    app.run(debug=True, host = '0.0.0.0')

