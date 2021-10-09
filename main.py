from flask import Flask, render_template, request, render_template_string
import tensorflow as tf
from tensorflow.keras.preprocessing.text import tokenizer_from_json
from tensorflow.keras.preprocessing.sequence import pad_sequences
import numpy as np, json, re


max_len = 500
loaded_model = tf.keras.models.load_model('model-bi-new.h5')

with open('tokenizer.json') as f:
    data = json.load(f)
    tokenizer = tokenizer_from_json(data)


def preProcess_data(text): #cleaning the data
    
    text = text.lower()
    new_text = re.sub('[^a-zA-z0-9\s]','',text)
    new_text = re.sub('rt', '', new_text)
    return new_text

def my_pipeline(text): #pipeline
  text_new = preProcess_data(text)
  X = tokenizer.texts_to_sequences([text_new])
  X = pad_sequences(X, maxlen=max_len)
  return X

def preprocess():
    return 

app = Flask(__name__, template_folder="templates")

@app.route('/index')
def index():
    return render_template('index.html')

@app.route('/', methods=['POST' ,'GET'])
def predict():
    feeling = request.args.get('feeling')
    feeling = str(feeling)
    
    clean_text = my_pipeline(feeling) #cleaning and preprocessing of the texts
     #loading the saved model

    predictions = loaded_model.predict(clean_text) #making predictions
    
    sentiment = int(np.argmax(predictions)) #index of maximum prediction
    probability = max(predictions.tolist()[0]) #probability of maximum prediction
    if sentiment==0: #assigning appropriate name to prediction
        t_sentiment = 'may choose not to seek out professional help'
    elif sentiment==1:
        t_sentiment = 'must seek out professional help'

    if feeling in ['', ' ', '                   ']:
        return render_template('diff_output.html', prediction = 'Please tell me how are you feeling today...')
    else:
        return render_template('output.html', prediction = t_sentiment, perc = str(round(probability*100,2))+'%')
    


if __name__ == '__main__':
    app.run(port=3000, debug=True)