#! /usr/bin/env nix-shell
#! nix-shell -i python -p "python310.withPackages(ps: with ps; [ requests bibtexparser click ])""
#
import requests
import json
from bibtexparser import loads, dumps
from bibtexparser.bibdatabase import BibDatabase
import datetime
import click


@click.command()
@click.argument('query')
def queryGoogleBooks(query):
    """Query for Google Books, e.g., the title of the book."""
    resp = requests.get(f"https://www.googleapis.com/books/v1/volumes?q={query}")
    # click.echo(resp.status_code)
    if not resp.ok:
        raise click.ClickException(f"Got status code {resp.status_code}: {resp.error}")
        exit()
    book = resp.json()['items'][0]['volumeInfo']
    click.echo(formatBook(book))

def formatBook(book):
    # Parse date
    publishedDate = book["publishedDate"]
    publishedDateParsed = datetime.date.fromisoformat(publishedDate)
    year = str(publishedDateParsed.year)
    # Get ISBN
    for identifier in book["industryIdentifiers"]:
        if identifier["type"] == "ISBN_13":
            isbn = identifier["identifier"]
            break
        else:
            isbn = ""
    # Generate bibtex ID
    firstAuthorLast = book['authors'][0].split(' ')[1]
    entryID = firstAuthorLast + year

    # Create a new entry
    entry = {
        'ID': entryID,
        'ENTRYTYPE': 'book',
        'author': ", ".join(book['authors']),
        'title': book['title'],
        'subtitle': book['subtitle'],
        'publisher': book['publisher'],
        'year': year,
        'isbn': isbn,
        'url': book['canonicalVolumeLink']
    }

    # Initialize bibliographic database
    bib_database = BibDatabase()
    # Add entry
    bib_database.entries.append(entry)
    return dumps(bib_database)

if __name__ == '__main__':
    queryGoogleBooks()
