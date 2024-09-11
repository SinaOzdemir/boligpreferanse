# -*- coding: utf-8 -*-
"""
Created on Wed Jul 24 14:43:54 2024

@author: sioz
"""

#All the packages and libraries to do the analysis
import os
import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
import tensorflow as tf
from tensorflow.keras import layers, models
from tensorflow.keras import backend as K
from tensorflow.keras.utils import plot_model
from tensorflow.keras.models import Model


#set working environment to data folder

if not "survey results" in os.getcwd():
    print("wrong working director! Changing it to correct one")
    os.chdir(os.path.normpath(r"C:\Users\sioz\Desktop\arbeid\prosjektene\boligpreferanse\data\survey\survey results"))
else:
    print("correct working directory")

#DO NOT RUN, used for prototyping functions:    

# Deep Autoencoder 

class Dautoencoder(Model):
    def __init__(self,latent_dim,shape):
        super(Dautoencoder,self).__init__()
        self.latent_dim = latent_dim
        self.shape = shape
        
        #encoder with 3 hidden layers
        self.encoder = tf.keras.Sequential([
            #layers.Flatten(input_shape = shape),#this isnt necessary because I am supplying tabular data
            layers.Dense(9, activation = "relu"),
            layers.Dense(latent_dim, activation = "relu"),
            #layers.Dense(latent_dim, activation = "relu"), latent_dim layer is the dimension of bottle neck layer,
            #in my case the previous one already serves as the bottle neck
            ])
        
        #Decoder
        
        self.decoder= tf.keras.Sequential([
            #layers.Dense(3, activation = "relu"), 3 node layer is the bottle neck and it should not be included in 
            #the decoder as far as I understand
            layers.Dense(9, activation = "relu"),
            layers.Dense(tf.math.reduce_prod(shape).numpy(),activation = "sigmoid"),
            #layers.Reshape(shape)
            ])
        
    def call(self,x):
        encoded = self.encoder(x)
        decoded = self.decoder(encoded)
        
        return decoded
    
    def build_graph(self):
        x = tf.keras.Input(shape = (self.shape,))
        return Model(inputs = [x], outputs=self.call(x))

# Variational autoencoder (works)
        

def dvautoencoder(input_dim, mode="mini"):
    
    # dims:
    layer_dims = {"Input_layer": input_dim, "Encoder_layer1": input_dim // 2, "Latent_dim": input_dim // 4}
    
    print("initializing %s" % mode)
    
    class Sampling(layers.Layer):
        def call(self, inputs):
            z_mean, z_log_var = inputs
            batch = K.shape(z_mean)[0]
            dim = K.int_shape(z_mean)[1]
            epsilon = K.random_normal(shape=(batch, dim))
            return z_mean + K.exp(0.5 * z_log_var) * epsilon
    
    class VAELoss(layers.Layer):
        def call(self, inputs):
            x, decoded_mean, z_mean, z_log_var = inputs
            xent_loss = K.sum(tf.keras.losses.binary_crossentropy(x, decoded_mean), axis=-1)
            kl_loss = -0.5 * K.sum(1 + z_log_var - K.square(z_mean) - K.exp(z_log_var), axis=-1)
            return K.mean(xent_loss + kl_loss)
    
    if mode == "mini":
        input_layer = layers.Input(shape=(layer_dims["Input_layer"],))
        x = layers.Dense(units=layer_dims["Encoder_layer1"])(input_layer)
        z_mean_layer = layers.Dense(units=layer_dims["Encoder_layer1"])(x)
        z_log_var_layer = layers.Dense(units=layer_dims["Encoder_layer1"])(x)
        
        z_layer = Sampling()([z_mean_layer, z_log_var_layer])
        
        decoded_mean = layers.Dense(units=layer_dims["Input_layer"], activation="sigmoid")(z_layer)
        
        mini_vae = models.Model(input_layer, [decoded_mean, z_layer])
        
        vae_loss = VAELoss()([input_layer, decoded_mean, z_mean_layer, z_log_var_layer])
        
        mini_vae.add_loss(vae_loss)
        
        mini_vae.compile(optimizer="rmsprop")
        
        return mini_vae
    
    else:
        input_layer = layers.Input(shape=(layer_dims["Input_layer"],))
        x = layers.Dense(units=layer_dims["Encoder_layer1"], activation="relu")(input_layer)
        encoder_layer2 = layers.Dense(units=layer_dims["Latent_dim"], activation="relu")(x)
        z_mean_layer = layers.Dense(units=layer_dims["Latent_dim"])(encoder_layer2)
        z_log_var_layer = layers.Dense(units=layer_dims["Latent_dim"])(encoder_layer2)
        
        z_layer = Sampling()([z_mean_layer, z_log_var_layer])
       
        decoder_layer1 = layers.Dense(units=layer_dims["Encoder_layer1"], activation="relu")
        decoder_output_layer = layers.Dense(layer_dims["Input_layer"], activation="sigmoid")
        h_decoded = decoder_layer1(z_layer)
        decoded_mean = decoder_output_layer(h_decoded)
        
        deep_vae = models.Model(input_layer, [decoded_mean, z_mean_layer])
        
        vae_loss = VAELoss()([input_layer, decoded_mean, z_mean_layer, z_log_var_layer])
        
        deep_vae.add_loss(vae_loss)
        
        deep_vae.compile(optimizer="rmsprop")

        return deep_vae
       
    
    
    
#Function that applies autoencoder to the data

def data_encoder(data,encoding_mode = "mini_encoder",latent_dim = None,colnames = None):
    #convert the pandas dataframe to numpy array for encoding
    if encoding_mode not in ["deep_encoder","mini_encoder","deep_vencoder","mini_vencoder"]:
        raise ValueError("encoding mode should be one of mini_encoder,deep_encoder,deep_vencoder,mini_vencoder")
        
    input_shape = data.shape[1]
    data = data.to_numpy()
    
    #stacked/deep autoencoder option
    if encoding_mode == "deep_encoder":
        print("Compiling and training a deep autoencoder")
        
        if latent_dim is None:
            latent_dim = data.shape[1]//4
            
        dautoencoder = Dautoencoder(latent_dim, input_shape)
        dautoencoder.compile(optimizer = "adam",loss = "mean_squared_error")
        dautoencoder.fit(x = data,
                         y = data,
                         batch_size = (data.shape[0]//10),
                         epochs = 10,
                         validation_split = .2,
                         shuffle = True)
        dautoencoder_results = pd.DataFrame(dautoencoder.predict(data),columns = colnames)
        return dautoencoder_results
    
    #single layer autoencoder option
    if encoding_mode == "mini_encoder":
       
       print("Compiling and training a single layer autoencoder") 
       #calculate the node size for the bottle neck layer
       #since mini encoder has only one hidden layer, bottle neck dimensions are set to the
       #half the number of features
       if latent_dim is None:
           latent_dim=data.shape[1]//2
       
       #generate an input layer
       input_layer = layers.Input(shape = (input_shape,))
       #define the encoding(bottle neck) layer
       encoder = layers.Dense(latent_dim,activation = "relu")(input_layer)
       #define the output/decoder layer
       decoder = layers.Dense(input_shape, activation = "sigmoid")(encoder)
       mini_encoder = models.Model(inputs = input_layer,outputs = decoder)

       #compile the autoencoder and train
       mini_encoder.compile(optimizer = "adam", loss = "mean_squared_error")
       # ##train the model
       mini_encoder.fit(data,data,epochs = 10, batch_size = data.shape[0]//10, shuffle = True, validation_split = 0.2)

       #extract embeddings
       mini_encoder_results = pd.DataFrame(mini_encoder.predict(data),columns=colnames)
       return mini_encoder_results
   
    #deep variational encoder option
    ## need to define a function for this first
    
    if encoding_mode == "deep_vencoder":
        print("compiling and training a deep variational encoder")
        deep_vencoder = dvautoencoder(input_shape, mode = "deep")
        deep_vencoder.fit(data,[data,data],
                                          epochs = 10, batch_size = data.shape[0]//10,
                                          shuffle = True, validation_split = 0.2)
        #reconstructed = values from outputlayer, embeddings = values from bottleneck layer
        reconstructed, embeddings = deep_vencoder.predict(data)
        #so df should be made with reconstructed
        vencoded_data = pd.DataFrame(reconstructed, columns = colnames)
        return vencoded_data
        
    if encoding_mode == "mini_vencoder":
        print("compiling and training a mini variational encoder")
        mini_vencoder = dvautoencoder(input_shape,mode = "mini")
        mini_vencoder.fit(data,[data,data],epochs = 10,
                                          batch_size = data.shape[0]//10,shuffle = True,
                                          validation_split = 0.2)
        reconstructed_mini, embeddings_mini = mini_vencoder.predict(data)
        mini_embeddings_df = pd.DataFrame(reconstructed_mini,columns = colnames)
        return mini_embeddings_df


#function test
## test exception handling for encoding mode
#test = data_encoder(data = data_frame, encoding_mode= "hebelehubele",colnames= col_names)
###encoding_mode exception handling works

##test mini autoencoder
#test_mini = data_encoder(data = data_frame, encoding_mode= "mini_encoder",colnames= col_names)
###mini encoder works (maybe with an overfit?)

##test deep autoencoder
#test_deep = data_encoder(data = data_frame, encoding_mode="deep_encoder",colnames = col_names)
### deep encoder works

## test deep variational auto encoder
#test_deep_variational = data_encoder(data = data_frame,encoding_mode="deep_vencoder",colnames = col_names)
###1.  Dense.__init__() missing 1 required positional argument: 'units'(done)
###2.  KeyError: 'Input_dim'()(done)
###3.  TypeError: unsupported operand type(s) for *: 'int' and 'BinaryCrossentropy'
###4.  TypeError: You are passing KerasTensor(type_spec=TensorSpec(shape=(), dtype=tf.float32, name=None),
#### name='Placeholder:0', description="created by layer 'tf.cast_3'"),
#### an intermediate Keras symbolic input/output,
#### to a TF API that does not allow registering custom dispatchers,
#### such as `tf.cond`, `tf.function`, gradient tapes, or `tf.map_fn`.
#### Keras Functional model construction only supports TF API calls that *do* support dispatching,
#### such as `tf.math.add` or `tf.reshape`. Other APIs cannot be called directly on symbolic Kerasinputs/outputs.
#### You can work around this limitation by putting the operation in a custom Keras layer `call` and calling
#### that layer on this symbolic input/output. (attempt:2)(done)
###5. 'History' object has no attribute 'predict' (done)
####(changed = mini_vencoded = mini_vencoder.fit(...)\n mini_vencoded.predict(...) to mini_vencoder.fit()\n mini_vencoder.predict)
###6. Shape of passed values is (401, 4), indices imply (401, 18)
#### pandas error: reconstructed, embeddings = model.predict(df) where reconstructed is from output layer and embeddings is from the bottleneck layer
#### so pandas.DataFrame should be made with reconstructed rather then embeddings
### Worked

## test mini variational autoencoder
#test_mini_variational = data_encoder(data = data_frame,encoding_mode="mini_vencoder",colnames = col_names)
### Worked!

# Apply functions

data_files = [file for file in os.listdir(os.getcwd()) if ".csv" in file]

encoding_methods = ["deep_encoder","mini_encoder","deep_vencoder","mini_vencoder"]

for file in data_files:
    data_origin = file.split("_")[-1].replace(".csv", "")
    df = pd.read_csv(file,sep = ",")
    col_names = df.columns.tolist()
    for method in encoding_methods:
        save_name_csv = "clustering_"+data_origin+"_"+method+".csv"
        save_name_excel = "clustering_"+data_origin+"_"+method+".xlsx"
        encoded_data = data_encoder(data = df,
                                    encoding_mode= method,
                                    colnames = col_names)
        encoded_data.to_csv(save_name_csv,sep = ",",header = True,index = False)
        encoded_data.to_excel(save_name_excel, header = True,index=False)


