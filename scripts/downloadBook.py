#! /usr/bin/env nix-shell
#!nix-shell -i python3 -p "python3.withPackages(ps: with ps; [ beautifulsoup4 requests isbnlib setuptools ])"


""" Given a library.lol page, download the book and add to database. """

from bs4 import BeautifulSoup
import requests
from urllib.request import urlretrieve
from sys import argv
import subprocess
import isbnlib

papersDir = "/home/jon/Dokumentoj/Papers"
destFile = f"{papersDir}/library2.bib"


def findISBN(soup):
    allParas = soup.find_all("p")
    for para in allParas:
        text = para.get_text()
        if text.startswith('ISBN:'):
            isbn = text.split(',')[0][5:].strip()
    return isbn


def findBookLinks(soup):
    allAs = soup.find_all("a")
    booklinks = []
    for a in allAs:
        if 'href' in a.attrs:
            link = a.attrs['href']
            if isBookLink(link):
                booklinks.append(link)
    return booklinks

def isBookLink(link):
    pats = ["https://cloudflare-ipfs.com", "https://gateway.ipfs.io", "https://gateway.pinata.cloud"]
    for pat in pats:
        if link.startswith(pat):
            return True

def isbnToBibtex(isbn):
    meta = isbnlib.meta(isbn)
    tobibtex = isbnlib.registry.bibformatters['bibtex']
    return tobibtex(meta)


def appendBibtex(bibtex, destFile=destFile):
    bibtex = f"\n{bibtex}\n"
    with open(destFile, 'a') as f:
        f.write(bibtex)


def downloadBook(bookLink, papersDir, key):
    if bookLink.strip().endswith('pdf'):
        dest = f"{papersDir}/{key}.pdf"
    elif bookLink.strip().endswith('epub'):
        dest = f"{papersDir}/{key}.epub"
    else:
        exit(f"Can't download book with this extension.")
    urlretrieve(bookLink, dest)
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
    bookLinks = findBookLinks(soup)
    for bookLink in bookLinks:
        print("Trying download: ", bookLink)
        try:
            downloadBook(bookLink, papersDir, key)
            break
        except:
            print("Download failed.")

    openNotes(key)


if __name__ == "__main__":
    main()
    # destFile = "/home/jon/Dokumentoj/Papers/library.bib"
