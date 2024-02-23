# Capstone Webscrapping using BeautifulSoup

## Description 
Project ini berisi bagaimana kita mengambil data dari suatu website (*webscrapping*). Teknik *Web scaripping* ini menggunakan bantuan *library* [**BeautifulSoup**](https://beautiful-soup-4.readthedocs.io/en/latest/). Website yang digunakan untuk scarping berasal dari [EXCHANGE-RATES.ORG](https://www.exchange-rates.org/history/IDR/USD/T). Kemudian saya juga menambahkan data inflasi dan tingkat suku bunga US agar dapat menjelaskan pergerakan nilai tukar rupiah. Selain itu, data yang saya gunakan sudah dilengkapi visualisasi dari [matplotlib](https://matplotlib.org/) dan visualisasi interaktif dari [plotly](https://plotly.com/python/).

## Background
Halo semua perkenalkan saya Toni Andreas Susanto, biasa dipanggil Toni. Saya merupakan mahasiswa program studi Ekonomi Pembangunan, di Universitas Mulawarman. Saya mempelajari *data analytic* bertujuan menambah skil saya dalam mengolah data. Skil ini sangat berkaitan erat dengan jurusan kuliah saya karena dalam membuat suatu keputusan ekonomi atau kebijakan ekonomi mesti didasarkan oleh data agar dapat menyusun keputusan yang efektif.

## Dependencies

- beautifulSoup4
- pandas
- flask
- matplotlib dll.

Atau Bapak Ibu cukup menginstall requirements.txt dengan cara berikut

```python
pip install -r requirements.txt
```

## Rubics

- Environment preparation
- Finding the right key to scrap the data  & Extracting the right information
- Creating data frame & Data wrangling 
- Creating a tidy python notebook as a report.
- Implement it on flask dashboard


## File
   1. File Pertama :
      - `README.md` -> Berisi penjelasan pada file ini.
      - `requirements.txt` -> packages-packages yang dibutuhkan dalam pembuatan environment.
      - `Notebook Skeleton Guide Capstone Beautiful Soup.ipynb` -> file notebook yang digunakan dalam membuat project ini. Terdapat tambahan visualiasi dengn `plotly`.
      - `Notebook Skeleton Guide Capstone Beautiful Soup.html` -> file html agar memudahkan melihat notebook langsung dalam website (chrome etc).
      - `asset` -> foto yang digunakan dalam menampilkan gambar di notebook.

    2. File Aplikasi
      - `static` (style.css) -> file CSS yang digunakan dalam memberi warna dan design aplikasi.
      - `template` (index.html) -> file html yang digunakan dalam membuat kerangka aplikasi.
      - `app.py` -> program yang menjalankan aplikasinya.
      - `result` -> file hasil akhir tampilannya.
         - `127.0.0.1.html` -> hasil akhir kerangaka yang dibuat.
         - `127.0.0.1_files` -> berisi file pendukung agar mempercantik tampilan ketika membuka file `127.0.0.1.html`.


## Conclusion
  Pada uraian materi dan code di atas kita telah melihat bagaimana melakukan *web scrapping* dengan bantuan **BeautifulSoup**. Dengan bantuan **BeautifulSoup** dan terkhusus pemrograman cenderung kita bisa lebih mudah mendapatkan informasi dari internet tanpa melakukannya secara manual. Kemudian setelah mendapatkan informasi tersebut kita dapat menyimpannya dalam dataframe. Selanjutnya kita susun dan bersihkan (*data wrangling and data cleansing*) sehingga nantinya dapat diolah dengan melakukan visualisasi melalui `matplotlib` dan `plotly`. Selain itu, kita juga telah menganalisis dari salah satu faktor yang mempengaruhi pergerakan nilai tukar rupiah terhadap dolar AS.


Happy learning! 
