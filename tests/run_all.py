import requests
import json
import os

helloworld = requests.get("http://0.0.0.0:5000")

# figure out which dir we are in
basilts = 'app.py' in os.listdir()

inputs = os.listdir('tests/io')
inputs = [x for x in inputs if 'input' in x]
inputs.sort()
for req in inputs:
    print(req)
    outfile = 'tests/io/' + req.replace('input', 'output')
    with open('tests/io/' + req) as infile:
        resp = requests.post(url='http://0.0.0.0:5000/forecast',
                             json=json.load(infile))
    with open(outfile, 'w') as out:
        json.dump(resp.json(), out, indent = 2)
