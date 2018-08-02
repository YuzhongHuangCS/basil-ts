import os
import hashlib
import pdb
from filelock import FileLock
import shutil
import subprocess
import time
import random

# uncomment to force CPU training
os.environ['CUDA_VISIBLE_DEVICES'] = '-1'
os.environ['KMP_BLOCKTIME'] = '0'
os.environ['KMP_SETTINGS'] = 'true'
os.environ['OMP_NUM_THREADS'] = '1'

import sys
import json
from config import Config
import numpy as np

if __name__ == "__main__":
	config = Config()

	filename = sys.argv[1]
	output_filename = filename.replace('request', 'forecast')

	text_byte = open(filename, 'rb').read()
	md5sum = hashlib.md5(text_byte).hexdigest()
	lock_filename = 'cache/{}.lock'.format(md5sum)
	json_filename = 'cache/{}.json'.format(md5sum)

	with FileLock(lock_filename, timeout=86400):
		if os.path.exists(json_filename):
			print('Already exists')
			shutil.copyfile(json_filename, output_filename)
		else:
			print('Run new')
			if os.name == 'posix':
				time.sleep(random.uniform(0, 10))
				#set cpu affinity
				command = "mpstat -P ALL 1 1 | tail -n 48 | awk '{print ($12)}'"
				ret = subprocess.check_output(command, shell=True)
				idles = [float(x) for x in ret.split()]
				max_idle_index = np.argmax(idles)
				max_idle = idles[max_idle_index]
				print('idle', max_idle, max_idle_index)
				os.environ['KMP_AFFINITY'] = 'granularity=thread,explicit,proclist=[{}],verbose'.format(max_idle_index)

			from rnn_predictor import RNNPredictor
			predictor = RNNPredictor(config)
			text = open(filename).read()
			content = json.loads(text)
			predictor.basename = os.path.basename(filename)
			predictor.predict(content, filename)
			shutil.copyfile(output_filename, json_filename)
