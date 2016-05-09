# https://www.crummy.com/software/BeautifulSoup/bs3/documentation.html

from bs4 import BeautifulSoup
import urllib

# r = urllib.urlopen('http://www.aflcio.org/Legislation-and-Politics/Legislative-Alerts').read()
r = urllib.urlopen('https://en.wikipedia.org/wiki/List_ofstate_and_union_territory_capitals_in_India').read()
soup = BeautifulSoup(r, "html.parser")

# print type(soup)
# print soup.prettify().encode('utf-8')
# print soup.title
# print soup.title.string

# print soup.a
# all_links = soup.find_all("a")
#for link in all_links:
#    print link.get("href")

# all_tables = soup.find_all("table")
#for table in all_tables:
#     print table

all_tables = soup.find('table', class_='wikitable sortable plainrowheaders')
print all_tables
for table in all_tables:
    print table    
