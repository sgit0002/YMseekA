from flask import Flask, render_template, request, render_template_string

app = Flask(__name__, template_folder="templates")

@app.route('/index')
def index():
    return render_template('index.html')

@app.route('/', methods=['GET'])
def predict():
    feeling = request.args.get('feelings')
    
    return render_template('output.html', prediction = feeling)
    


if __name__ == '__main__':
    app.run(port=3000, debug=True)