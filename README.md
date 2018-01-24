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
curl -H "Content-Type: application/json" -vX POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast | tail
curl -H "Content-Type: application/json" -vX POST -d @test/requests/example1.json http://0.0.0.0:5000/forecast > test/responses/example1.json

curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp12a.json http://0.0.0.0:5000/forecast > test/responses/ifp12a.json

# this should return error (ifp12.json does not exist)
curl -H "Content-Type: application/json" -vX POST -d @test/requests/ifp12.json http://0.0.0.0:5000/forecast 
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

