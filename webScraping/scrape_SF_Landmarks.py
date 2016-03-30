from urllib.request import urlopen
from bs4 import BeautifulSoup
html = urlopen("https://en.wikipedia.org/wiki/List_of_San_Francisco_Designated_Landmarks")
bsObj = BeautifulSoup(html.read(), 'lxml');
print(bsObj.h1)