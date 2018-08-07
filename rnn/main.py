import hashlib
import json
import logging
import numpy as np
import os
import platform
import psutil
import shutil
import subprocess
import sys
from config import Config
from filelock import FileLock

# uncomment to force CPU training
os.environ['CUDA_VISIBLE_DEVICES'] = '-1'
os.environ['OMP_NUM_THREADS'] = '1'

if __name__ == "__main__":
    config = Config()

    filename = sys.argv[1]
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s.%(msecs)03d %(levelname)s %(module)s - %(funcName)s: %(message)s',
                        datefmt="%Y-%m-%d %H:%M:%S")
    output_filename = filename.replace('request', 'forecast')

    text_byte = open(filename, 'rb').read()
    md5sum = hashlib.md5(text_byte).hexdigest()
    lock_filename = 'rnn/cache/{}.lock'.format(md5sum)
    json_filename = 'rnn/cache/{}.json'.format(md5sum)

    with FileLock(lock_filename, timeout=86400):
        if os.path.exists(json_filename):
            logging.info('Already exists')
            shutil.copyfile(json_filename, output_filename)
        else:
            logging.info('Run new')
            core_lock = None
            if platform.system() == 'Linux':
                # set cpu affinity
                while True:
                    command = "mpstat -P ALL 1 1 | tail -n 48 | awk '{print ($12)}'"
                    ret = subprocess.check_output(command, shell=True)
                    idles = [float(x) for x in ret.split()]
                    max_idle_index = np.argmax(idles)
                    max_idle = idles[max_idle_index]

                    logging.info('Try to lock core {}'.format(max_idle_index))
                    core_filename = 'rnn/core/{}.lock'.format(max_idle_index)
                    core_lock = FileLock(core_filename, timeout=1)
                    try:
                        core_lock.acquire()
                        logging.debug('Lock acquired on core {}'.format(max_idle_index))
                        break
                    except Exception as e:
                        logging.debug('Unable to get lock on core {}'.format(max_idle_index))

                logging.info('idle', max_idle, max_idle_index)
                p = psutil.Process()
                p.cpu_affinity([max_idle_index])
                logging.debug('{}, {}'.format(p, p.cpu_affinity()))

            try:
                from rnn_predictor import RNNPredictor

                predictor = RNNPredictor(config)
                predictor.predict(filename)
                shutil.copyfile(output_filename, json_filename)
            finally:
                if core_lock is not None:
                    core_lock.release()
