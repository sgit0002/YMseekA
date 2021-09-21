from flask import Flask, render_template, request, render_template_string

from keras.preprocessing.text import tokenizer_from_json
from keras.preprocessing.sequence import pad_sequences
import numpy as np, json
from keras.models import load_model

max_words = 5000
max_len = 500

with open('tokenizer.json') as f:
    data = json.load(f)
    tokenizer = tokenizer_from_json(data)

model = load_model('model-bi-new.h5')

def preprocess():
    return 

app = Flask(__name__, template_folder="templates")

@app.route('/index')
def index():
    return render_template('index.html')

@app.route('/', methods=['GET', 'POST'])
def predict():
    feeling = request.args.get('feelings')
    feeling = str(feeling)
    
    sentiment = ["Not Depressed", "Depressed"]
    
    tokenizer.fit_on_texts([feeling])
    sequence = tokenizer.texts_to_sequences([feeling])
    
    test = pad_sequences(sequence, maxlen=max_len)
    
    pred = sentiment[np.around(model.predict(test), decimals=0).argmax(axis=1)[0]]
    index = model.predict(test).argmax(axis=1)[0]
    perc = model.predict(test)[0][index] * 100
    perc = np.round(perc, decimals=2)

    return render_template('output.html', prediction = pred, perc = str(perc)+'%')
    
if __name__ == '__main__':
    app.run(port=3000, debug=True)