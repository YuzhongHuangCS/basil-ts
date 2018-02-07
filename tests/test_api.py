import unittest
import json
import sys
sys.path.append('.')
from app import app


#foo = app.test_client()
#response = foo.get('/')
#response = foo.post('/forecast',
#                    data = open('tests/requests/example1.json'),
#                    content_type='application/json')
#payload = json.loads(response.data)

class TestSampleRequests(unittest.TestCase):
    def setUp(self):
        self.app = app.test_client()

    def test_hello_world(self):
        response = self.app.get('/')
        assert response.status_code == 200
        assert response.data == b"I'm alive!"
    
    def test_good_request(self):
        with open("tests/requests/example1.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 200
        payload = json.loads(response.data)
    
    def test_ts_too_many_columns(self):
        with open("tests/requests/example2.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 400
      
    def test_ts_invalid_dates(self):
        with open("tests/requests/example3.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        assert response.status_code == 400

    def test_r_side_error(self):
        with open("tests/requests/example4.json", "rb") as req:
            response = self.app.post('/forecast',
                                     data = req,
                                     content_type = 'application/json')
        print(response.status_code)
        assert response.status_code == 500

if __name__ == "__main__":
    unittest.main()
