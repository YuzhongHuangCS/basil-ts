import json
import logging
import numpy as np
import sklearn
import sklearn.preprocessing
import tensorflow as tf


# Each instance is created for each incoming request
class RNNPredictor(object):
    """docstring for RNNPredictor"""

    def __init__(self, config):
        super(RNNPredictor, self).__init__()
        self.config = config

    def predict(self, filename):
        content = json.loads(open(filename).read())
        n_predict_step = content['h'][0]

        data = content['ts']
        scaler = sklearn.preprocessing.StandardScaler()
        data_scaled = scaler.fit_transform(np.asarray(data).reshape(-1, 1))

        x = data_scaled.reshape(-1, 1)
        n_valid = min(n_predict_step, self.config.max_valid)
        n_train = len(x) - n_valid
        n_total = len(x)

        logging.info('Predict step: {}'.format(n_predict_step))
        logging.info('n_total: {}, n_train: {}, n_valid: {}'.format(n_total, n_train, n_valid))

        # build network
        batchX_placeholder = tf.placeholder(tf.float32, [n_total, self.config.n_input_dim])

        W1 = tf.get_variable('W1', shape=(self.config.n_neurons, self.config.n_dense_dim), initializer=tf.glorot_uniform_initializer())
        b1 = tf.get_variable('b1', shape=(1, self.config.n_dense_dim), initializer=tf.zeros_initializer())
        W2 = tf.get_variable('W2', shape=(self.config.n_dense_dim, self.config.n_output_dim), initializer=tf.glorot_uniform_initializer())
        b2 = tf.get_variable('b2', shape=(1, self.config.n_output_dim), initializer=tf.zeros_initializer())

        # Unpack columns
        inputs_series = tf.reshape(batchX_placeholder, (1, -1, 1))

        # Forward passes
        cell = tf.nn.rnn_cell.GRUCell(self.config.n_neurons, kernel_initializer=tf.orthogonal_initializer(), bias_initializer=tf.zeros_initializer())
        cell_state = cell.zero_state(self.config.n_batch, dtype=tf.float32)

        states_series, current_state = tf.nn.dynamic_rnn(cell, inputs_series, initial_state=cell_state,
                                                         parallel_iterations=1)

        prediction = tf.matmul(tf.tanh(tf.matmul(tf.squeeze(states_series), W1) + b1), W2) + b2

        pred_point_train = tf.slice(prediction, (0, 0), (n_train - n_predict_step, 1))
        pred_lower_train = tf.slice(prediction, (0, 1), (n_train - n_predict_step, 1))
        pred_upper_train = tf.slice(prediction, (0, 2), (n_train - n_predict_step, 1))

        pred_point_valid = tf.slice(prediction, (n_train - n_predict_step, 0), (n_valid, 1))
        pred_lower_valid = tf.slice(prediction, (n_train - n_predict_step, 1), (n_valid, 1))
        pred_upper_valid = tf.slice(prediction, (n_train - n_predict_step, 2), (n_valid, 1))

        pred_point_test = tf.slice(prediction, (n_total - n_predict_step, 0), (n_predict_step, 1))
        pred_lower_test = tf.slice(prediction, (n_total - n_predict_step, 1), (n_predict_step, 1))
        pred_upper_test = tf.slice(prediction, (n_total - n_predict_step, 2), (n_predict_step, 1))

        pred_point_total = tf.slice(prediction, (0, 0), (n_total - n_predict_step, 1))
        pred_lower_total = tf.slice(prediction, (0, 1), (n_total - n_predict_step, 1))
        pred_upper_total = tf.slice(prediction, (0, 2), (n_total - n_predict_step, 1))

        labels_series_train = batchX_placeholder[n_predict_step:n_train, :]
        labels_series_valid = batchX_placeholder[n_train:, :]
        labels_series_total = batchX_placeholder[n_predict_step:, :]

        # the total loss take all predictions into account
        def get_total_loss(pred_point, pred_lower, pred_upper, label):
            point_loss = tf.reduce_mean(tf.squared_difference(pred_point, label))

            diff_lower = (pred_lower - label)
            diff_p_l = tf.reduce_mean(tf.square(tf.clip_by_value(diff_lower, 0, 1e10)))
            diff_n_l = tf.reduce_mean(tf.square(tf.clip_by_value(diff_lower, -1e10, 0)))
            lower_loss = diff_p_l * 0.99 + diff_n_l * 0.01

            diff_upper = (pred_upper - label)
            diff_p_u = tf.reduce_mean(tf.square(tf.clip_by_value(diff_upper, 0, 1e10)))
            diff_n_u = tf.reduce_mean(tf.square(tf.clip_by_value(diff_upper, -1e10, 0)))
            upper_loss = diff_p_u * 0.01 + diff_n_u * 0.99

            total_loss = point_loss + 0.5 * (lower_loss + upper_loss)
            return total_loss

        total_loss_train = get_total_loss(pred_point_train, pred_lower_train, pred_upper_train, labels_series_train)
        total_loss_valid = get_total_loss(pred_point_valid, pred_lower_valid, pred_upper_valid, labels_series_valid)
        total_loss_total = get_total_loss(pred_point_total, pred_lower_total, pred_upper_total, labels_series_total)

        learning_rate = tf.Variable(self.config.lr, trainable=False)
        learning_rate_decay_op = learning_rate.assign(learning_rate * self.config.lr_decay)
        optimizer = tf.train.AdamOptimizer(learning_rate)

        gradients, variables = zip(*optimizer.compute_gradients(total_loss_train))
        gradients, _ = tf.clip_by_global_norm(gradients, 5.0)
        train_step = optimizer.apply_gradients(zip(gradients, variables))

        gradients_total, variables_total = zip(*optimizer.compute_gradients(total_loss_total))
        gradients_total, _ = tf.clip_by_global_norm(gradients_total, 5.0)
        train_step_total = optimizer.apply_gradients(zip(gradients_total, variables_total))

        tf_config = tf.ConfigProto()
        tf_config.intra_op_parallelism_threads = 1
        tf_config.inter_op_parallelism_threads = 1
        with tf.Session(config=tf_config) as sess:
            sess.run(tf.global_variables_initializer())

            smallest_loss = float('inf')
            smallest_train_loss = float('inf')
            wait = 0

            def _save_weight():
                tf_vars = tf.get_collection(tf.GraphKeys.TRAINABLE_VARIABLES)
                self.smallest_weight = sess.run(tf_vars)

            def _load_weights():
                tf_vars = tf.get_collection(tf.GraphKeys.TRAINABLE_VARIABLES)
                ops = []
                for i_tf in range(len(tf_vars)):
                    ops.append(tf.assign(tf_vars[i_tf], self.smallest_weight[i_tf]))
                sess.run(ops)

            _current_cell_state = np.zeros((self.config.n_batch, self.config.n_neurons), dtype=np.float32)
            for i in range(self.config.n_max_epoch):
                logging.info('Epoch: {}/{}'.format(i, self.config.n_max_epoch))
                # train
                train_loss, valid_loss, _train_step = sess.run(
                    [total_loss_train, total_loss_valid, train_step],
                    feed_dict={
                        batchX_placeholder: x,
                        cell_state: _current_cell_state,
                    }
                )

                sum_loss = train_loss * (1 - self.config.valid_loss_weight) + valid_loss * self.config.valid_loss_weight
                logging.info('Epoch {}, Train loss {}, Valid loss {}, Sum loss {}'.format(i, train_loss, valid_loss, sum_loss))
                if wait <= self.config.n_patience:
                    if sum_loss < smallest_loss:
                        smallest_loss = sum_loss
                        smallest_train_loss = train_loss
                        _save_weight()
                        wait = 0
                        logging.info('New smallest')
                    else:
                        wait += 1
                        logging.info('Wait {}'.format(wait))
                        if wait % self.config.n_lr_decay == 0:
                            sess.run(learning_rate_decay_op)
                            logging.info('Apply lr decay, new lr: %f' % learning_rate.eval())
                else:
                    break

            _load_weights()
            logging.info('Training loss {}'.format(smallest_train_loss))

            _total_loss = sess.run(
                [total_loss_total],
                feed_dict={
                    batchX_placeholder: x,
                    cell_state: _current_cell_state,
                }
            )

            for i in range(self.config.n_max_epoch_total):
                if _total_loss < smallest_train_loss:
                    break

                logging.info('Epoch_total: {}/{}'.format(i, self.config.n_max_epoch_total))
                _total_loss, _train_step = sess.run(
                    [total_loss_total, train_step_total],
                    feed_dict={
                        batchX_placeholder: x,
                        cell_state: _current_cell_state,
                    }
                )
                logging.info("Epoch_total {}, Total Loss {}".format(i, _total_loss))

            # test
            logging.info('In test')
            logging.info('Min loss {}'.format(_total_loss))

            pred_test, pred_test_lower, pred_test_upper = sess.run(
                [pred_point_test, pred_lower_test, pred_upper_test],
                feed_dict={
                    batchX_placeholder: x,
                    cell_state: _current_cell_state,
                }
            )

            pred_test = scaler.inverse_transform(pred_test)
            pred_test_lower = scaler.inverse_transform(pred_test_lower)
            pred_test_upper = scaler.inverse_transform(pred_test_upper)
            pred_test_lower = np.minimum(pred_test, np.minimum(pred_test_lower, pred_test_upper))
            pred_test_upper = np.maximum(pred_test, np.maximum(pred_test_upper, pred_test_lower))

            json_ret = {
                'mean': pred_test.tolist(),
                'lower': pred_test_lower.tolist(),
                'upper': pred_test_upper.tolist()
            }
            with open(filename.replace('request', 'forecast'), 'w') as fout:
                json.dump(json_ret, fout)
