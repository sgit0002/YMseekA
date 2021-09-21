from flask import Flask, render_template, request, render_template_string

import tensorflow as tf
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
import numpy as np

max_words = 112544
max_len = 500

model = tf.keras.models.load_model('model-bi-new.h5')


app = Flask(__name__, template_folder="templates")

@app.route('/index')
def index():
    return render_template('index.html')

@app.route('/', methods=['GET', 'POST'])
def predict():
    feeling = request.args.get('feelings')
    # feeling = str(str(feeling).split('+'))

    sentiment = ["Not Depressed", "Depressed"]
    tokenizer = Tokenizer(num_words=max_words)
    sequence = tokenizer.texts_to_sequences([feeling])
    print(sequence)
    test = pad_sequences(sequence, maxlen=max_len)
    
    pred = sentiment[np.around(model.predict(test), decimals=0).argmax(axis=1)[0]]
    print(pred)
    return render_template('output.html', prediction = feeling)
    


if __name__ == '__main__':
    app.run(port=3000, debug=True)