import unittest
import json
import sys
sys.path.append('.')
from app import app

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


if __name__ == "__main__":
    unittest.main()
