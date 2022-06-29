#! /usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages(ps: with ps; [ beautifulsoup4 requests isbnlib setuptools ])"


""" Given a library.lol page, download the book and add to database. """

from bs4 import BeautifulSoup
import requests
from urllib.request import urlretrieve
from sys import argv
import subprocess
import isbnlib

destFile = "/home/jon/Dokumentujo/Papers/library.bib"

def findISBN(soup):
    allParas = soup.find_all("p")
    for para in allParas:
        text = para.get_text()
        if text.startswith('ISBN:'):
            isbn = text.split(',')[0][5:].strip()
    return isbn

def findBookLink(soup):
    allAs = soup.find_all("a")
    for a in allAs:
        if 'href' in a.attrs:
            if a.attrs['href'].startswith('https://cloudflare-ipfs.com'):
                return a.attrs['href']

def isbnToBibtex(isbn):
    meta = isbnlib.meta(isbn)
    tobibtex = isbnlib.registry.bibformatters['bibtex']
    return tobibtex(meta)

def appendBibtex(bibtex, destFile=destFile):
    bibtex = f"\n{bibtex}\n"
    with open(destFile, 'a') as f:
        f.write(bibtex)

def downloadBook(url, dest):
    urlretrieve(url, dest)
    print(f"Wrote {dest}")

def getKey(bibtex):
    return bibtex.split('{')[1].split(',')[0]

def openNotes(key):
   elisp = f'(bibtex-completion-edit-notes (list "{key}"))'
   subprocess.call(["emacsclient", "--eval", elisp])

def main():
    print(f"Getting book from {argv[1]}")
    url = argv[1]
    resp = requests.get(url)
    webpage = resp.text
    soup = BeautifulSoup(webpage, features='html.parser')
    isbn = findISBN(soup)
    print(f"ISBN: {isbn}")
    bibtex = isbnToBibtex(isbn)
    key = getKey(bibtex)
    print(f"Key: {key}")
    appendBibtex(bibtex)
    bookLink = findBookLink(soup)
    pdfDest = f"/home/jon/Dokumentujo/Papers/{key}.pdf"
    if bookLink.strip().endswith('pdf'):
        dest = f"/home/jon/Dokumentujo/Papers/{key}.pdf"
        downloadBook(bookLink, dest)
    elif bookLink.strip().endswith('epub'):
        dest = f"/home/jon/Dokumentujo/Papers/{key}.epub"
        downloadBook(bookLink, dest)
    else:
        exit(f"Can't download book with this extension.")
    openNotes(key)

if __name__ == "__main__":
    main()
    destFile = "/home/jon/Dokumentujo/Papers/library.bib"
