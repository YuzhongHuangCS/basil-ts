import unittest
import json
import sys
sys.path.append('.')
from app import app

#os.chdir("/Users/andybega/Work/2017-HFC/SAGE-ward-share/basil-ts")

class TestSampleRequests(unittest.TestCase):
    def setUp(self):
        app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
        self.app = app.test_client()

    def test_hello_world(self):
        response = self.app.get('/')
        assert response.status_code == 200
        assert response.data == b"I'm alive!"

    def test_good_request(self):
        with open("tests/io/example1.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
        with open("tests/io/example1_output.json", 'w') as outfile:
            json.dump(payload, outfile, indent=2)

    def test_ts_too_many_columns(self):
        with open("tests/io/example2.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 400
        payload = json.loads(response.data)
        with open("tests/io/example2_ouput.json", 'w') as outfile:
            json.dump(payload, outfile, indent=2)

    def test_ts_invalid_dates(self):
        with open("tests/io/example3.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 400
        payload = json.loads(response.data)
        with open("tests/io/example3_ouput.json", 'w') as outfile:
            json.dump(payload, outfile, indent=2)

    def test_r_side_error(self):
        with open("tests/io/example4.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 500
        payload = json.loads(response.data)
        with open("tests/io/example4_ouput.json", 'w') as outfile:
            json.dump(payload, outfile, indent=2)

    # Check that option drop-after is correctly parsed
    def test_option_drop_after(self):
        # basic backcast call
        with open("tests/io/example1.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['backcast'][0] == True
        assert payload['parsed_request']['h'][0] == 1

        # call with option-after, forecast horizon should change
        with open("tests/io/example1.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2017-10-29',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['h'][0] == 2

        # call with wrong option-after format
        with open("tests/io/example1.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2017-10-99',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 400
        payload = json.loads(response.data)
        assert payload['message'] == "Invalid 'drop_after' argument"

    def test_quick_option(self):
        with open("tests/io/example1.json", "rb") as req:
            response = self.app.post('/forecast?quick=True',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
        assert len(payload['forecasts']) == 1

# class TestRctRequests(unittest.TestCase):
#     def setUp(self):
#         self.app = app.test_client()
#
#     def run_all_rct_ifps(self):

class TestBackcastFeature(unittest.TestCase):
    # for debugging: self = unittest.TestCase()

    def setUp(self):
        app.config['JSONIFY_PRETTYPRINT_REGULAR'] = False
        self.app = app.test_client()

    def test_monthly_series_no_aggregation(self):
        # check: drop_after works, not aggregated data, no partial train
        with open("tests/io/ex_1037_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-03-31',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0] == '2018-03-31'
        assert payload['parsed_request']['aggregated_data'][0] == False
        assert payload['parsed_request']['partial_train'][0] == ''

        # check: errors out, nothing to forecast
        with open("tests/io/ex_1037_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-04-01',
                                     data = req,
                                     content_type = 'application/json')
        payload = json.loads(response.data)
        assert response.status_code == 500
        assert 'exceed question end date' in payload['r_error_message']

        # check: still keeping data for March based on index date
        with open("tests/io/ex_1037_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-03-01',
                                     data = req,
                                     content_type = 'application/json')
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0] == '2018-03-31'

        # check: now March is dropped too
        with open("tests/io/ex_1037_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-02-28',
                                     data = req,
                                     content_type = 'application/json')
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0] == '2018-02-28'

    def test_monthly_series_aggregation_on_platform(self):
        # 866 earthquakes; note the partial updating does not work
        # correctly without regenerating the request files

        # check: basic call
        with open("tests/io/ex_866_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-02-28',
                                     data = req,
                                     content_type = 'application/json')

        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-02-28'
        assert payload['parsed_request']['aggregated_data'][0] == True
        assert payload['parsed_request']['partial_train'][0]   == 'no'
        assert payload['parsed_request']['partial_outcome'][0] == False
        # no updating of last train data value
        assert payload['parsed_request']['target']['value'][-1] == 176

        # check: partial train
        with open("tests/io/ex_866_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-02-15',
                                             data = req,
                                             content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-02-15'
        assert payload['parsed_request']['partial_train'][0]   == 'used'
        assert payload['parsed_request']['partial_outcome'][0] == False

    def test_daily_series(self):
        # 1028 gold prices

        with open("tests/io/ex_1028_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-04-25',
                                             data = req,
                                             content_type = 'application/json')

        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-04-25'
        assert payload['parsed_request']['aggregated_data'][0] == False
        assert payload['parsed_request']['partial_train'][0]   == ''
        assert payload['parsed_request']['partial_outcome'][0] == False

        with open("tests/io/ex_1028_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-04-24',
                                             data = req,
                                             content_type = 'application/json')

        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-04-24'
        assert payload['parsed_request']['aggregated_data'][0] == False
        assert payload['parsed_request']['partial_train'][0]   == ''
        assert payload['parsed_request']['partial_outcome'][0] == False

    def test_monthly_series_aggregation_in_basilts(self):
            # 1208 sea ice extent

        # basic request
        with open("tests/io/ex_1208_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-03-20',
                                             data = req,
                                             content_type = 'application/json')

        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-03-20'
        assert payload['parsed_request']['aggregated_data'][0] == True
        assert payload['parsed_request']['partial_train'][0]   == 'no'
        assert payload['parsed_request']['partial_outcome'][0] == False
        assert payload['parsed_request']['data_period']['period']['period'][0] == 'fixed'

        # check partial train
        with open("tests/io/ex_1208_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-03-19',
                                             data = req,
                                             content_type = 'application/json')

        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-03-19'
        assert payload['parsed_request']['aggregated_data'][0] == True
        assert payload['parsed_request']['partial_train'][0]   == 'used'
        assert payload['parsed_request']['partial_outcome'][0] == False

        # check partial outcome
        with open("tests/io/ex_1208_input.json", "rb") as req:
            response = self.app.post('/forecast?backcast=True&drop-after=2018-03-25',
                                             data = req,
                                             content_type = 'application/json')

        assert response.status_code == 200
        payload = json.loads(response.data)
        assert payload['parsed_request']['data_updated_to'][0]       == '2018-03-25'
        assert payload['parsed_request']['aggregated_data'][0] == True
        assert payload['parsed_request']['partial_train'][0]   == ''
        assert payload['parsed_request']['partial_outcome'][0] == True



if __name__ == "__main__":
    unittest.main()
