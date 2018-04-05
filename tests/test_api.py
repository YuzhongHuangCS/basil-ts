import unittest
import json
import sys
sys.path.append('.')
from app import app

class TestSampleRequests(unittest.TestCase):
    def setUp(self):
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
        with open("tests/io/example1_ouput.json", 'w') as outfile:
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

class TestRctRequests(unittest.TestCase):
    def setUp(self):
        self.app = app.test_client()
        
    def run_all_rct_ifps(self):
        


if __name__ == "__main__":
    unittest.main()
