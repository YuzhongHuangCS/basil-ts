#
#   Basil: Predictive Heuristics forecaster for SAGE
#

from flask import Flask, jsonify, request
from datetime import datetime
import subprocess
import pandas as pd
import json

app = Flask(__name__)

@app.route("/")
@app.route('/index')
def index():
    return "I'm alive!"

@app.route('/forecast', methods = ['GET', 'POST'])
def get_forecast():
  
    # parse request arguments
    content = request.get_json(silent = True)
    with open("basil-ts/request.json", "w") as outfile:
        json.dump(content, outfile)
        
    subprocess.call("Rscript --vanilla basil-ts/ts-forecast.R -e 'main()'", shell = True)
    
    # old code when intermediary was CSV file
    #fcasts = pd.read_csv('basil-ts/forecast.csv')
    #fcasts = fcasts.set_index('date')
    #answer = fcasts.reset_index().to_json(orient = "records", lines = True)
    #response = make_response(answer)
    #response.mimetype = "application/json"
    fcasts = json.load(open('basil-ts/forecast.json'))
    return(jsonify(fcasts))

if __name__ == '__main__':
    app.run(debug=True, host = '0.0.0.0')

