class Config(object):
    def __init__(self):
        super(Config, self).__init__()

        self.n_batch = 1
        self.n_max_epoch = 1000
        self.n_max_epoch_total = 100
        self.n_neurons = 256
        self.n_input_dim = 1
        self.n_dense_dim = 128
        self.n_output_dim = 3
        self.n_patience = 100
        self.n_lr_decay = 5
        self.lr = 1e-2
        self.lr_decay = 0.95
        self.max_valid = 10
        self.valid_loss_weight = 0.5
