from flask import Flask, render_template
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from io import BytesIO
import base64
from bs4 import BeautifulSoup 
import requests

#don't change this
matplotlib.use('Agg')
app = Flask(__name__) #do not change this

#insert the scrapping here
url_get = requests.get('https://www.exchange-rates.org/history/IDR/USD/T')
soup = BeautifulSoup(url_get.content,"html.parser")

#find your right key here
table = soup.find("table", attrs={"class":"table table-striped table-hover table-hover-solid-row table-simple history-data"})
row = table.find_all('td')

row_length = len(row)

temp = [] #initiating a list 

#insert the scrapping process here
# sortir tanggal dan harga
for i in range(0, row_length):
    if i % 2 == 0:   # Operasi membuktikan bilangan genap (data yang dibutuhkan memiliki index genap ex. 0,2,4 dst)
         data = table.find_all('td')[i].text  # mengambil nilai datanya
         temp.append(data) # menambahkan ke list temp

# mengurutkan data tanggal dan Harga Harian menyatu di suatu tuple.
data_right = []
for i in range(0, len(temp)):
    if i % 2 == 0:  # index posisi genap merupakan data tanggal
        tanggal = temp[i] 
    else:
        harga = temp[i] # index posisi ganjil merupakan data harga
        data_right.append((tanggal,harga))
    

data_right = data_right[::-1]

#change into dataframe
df = pd.DataFrame(data_right, columns = ('Tanggal','Harga_Harian'))

#insert data wrangling here
# 1. Ubah tipe data Tanggal menjadi datetime
df['Tanggal'] = df['Tanggal'].astype('datetime64')

# 2. Hapus koma dan IDR
df['Harga_Harian'] = df['Harga_Harian'].str.replace(',','')
df['Harga_Harian'] = df['Harga_Harian'].str.replace('IDR','')

# 3. Ubah tipe data Harga_Harian menjadi float64
df['Harga_Harian'] = df['Harga_Harian'].astype('float64')

#end of data wranggling 

# Tambahan

# Membuat dataframe
dictionary = {"Bulan" : ["Januari", "Februari", "Maret", "April", "Mei", "Juni"],
              "Tingkat Inflasi (YoY)": [7.48, 7.87, 8.54, 8.26, 8.58, None],
              "Tingkat Suku Bunga": [0.25, 0.25, 0.50, 0.50, 1, 1.75]}

rate = pd.DataFrame(dictionary)

# Mengganti style plot
plt.style.use('seaborn')


@app.route("/")
def index(): 
	
	card_data = f'{df["Harga_Harian"].mean().round(2)}' #be careful with the " and '  

# plot
	ax = df.set_index('Tanggal').plot(xlabel="Periode", 
                                      ylabel="Harga")

	# Rendering plot
	# Do not change this
	figfile = BytesIO()
	plt.savefig(figfile, format='png', transparent=True)
	figfile.seek(0)
	figdata_png = base64.b64encode(figfile.getvalue())
	plot_result = str(figdata_png)[2:-1]
 
# plot 1
	ay = rate.set_index("Bulan").plot(xlabel="Periode", 
                                      ylabel="Rate (%)")
 
	# Rendering plot
	# Do not change this
	figfile = BytesIO()
	plt.savefig(figfile, format='png', transparent=True)
	figfile.seek(0)
	figdata_png = base64.b64encode(figfile.getvalue())
	plot_result_1 = str(figdata_png)[2:-1]


	# render to html
	return render_template('index.html',
		card_data = card_data, 
		plot_result=plot_result,
		plot_result_1=plot_result_1,
		)


if __name__ == "__main__": 
    app.run(debug=True)