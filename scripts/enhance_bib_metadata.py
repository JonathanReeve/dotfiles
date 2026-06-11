import os
import re
import requests
import time
from pathlib import Path

DIR = Path.expanduser(Path('~/Dokumentoj/Org/Roam/'))

def get_crossref_metadata(doi):
    url = f"https://api.crossref.org/works/{doi}"
    try:
        r = requests.get(url, timeout=10)
        if r.status_code == 200:
            data = r.json()['message']
            title = data.get('title', [''])[0]
            authors = [f"{a.get('family', '')}, {a.get('given', '')}" for a in data.get('author', [])]
            author_str = "; ".join(authors)
            year = data.get('published-print', data.get('published-online', {})).get('date-parts', [[None]])[0][0]
            return {"TITLE": title, "AUTHOR": author_str, "YEAR": str(year) if year else None}
    except Exception as e:
        print(f"Error fetching CrossRef for {doi}: {e}")
    return None

def get_google_books_metadata(query):
    url = "https://www.googleapis.com/books/v1/volumes"
    params = {"q": query, "maxResults": 1}
    try:
        r = requests.get(url, params=params, timeout=10)
        if r.status_code == 200:
            data = r.json()
            if data.get('totalItems', 0) > 0:
                item = data['items'][0]['volumeInfo']
                return {
                    "TITLE": item.get('title'),
                    "AUTHOR": "; ".join(item.get('authors', [])),
                    "YEAR": item.get('publishedDate', '')[:4],
                    "URL": item.get('infoLink'),
                    "PUBLISHER": item.get('publisher')
                }
    except Exception as e:
        print(f"Error fetching Google Books for {query}: {e}")
    return None

def enhance_notes():
    updated = 0
    total = 0
    for file in DIR.glob('*.org'):
        with open(file, 'r', encoding='utf-8') as f:
            content = f.read()
            
        if ':REFERENCES: cite:' not in content and ':TITLE:' not in content:
            continue
            
        total += 1
        props = dict(re.findall(r'^:(\w+):\s*(.*)$', content, re.MULTILINE))
        
        # Missing critical metadata?
        needs_update = not props.get('AUTHOR') or not props.get('YEAR') or not props.get('URL')
        if not needs_update:
            continue
            
        metadata = None
        
        # 1. Try DOI
        doi = props.get('DOI')
        if doi:
            metadata = get_crossref_metadata(doi)
            
        # 2. Try ISBN
        if not metadata:
            isbn = props.get('ISBN')
            if isbn:
                metadata = get_google_books_metadata(f"isbn:{isbn}")
                
        # 3. Try Title/Author
        if not metadata:
            title = props.get('TITLE')
            author = props.get('AUTHOR')
            if title:
                metadata = get_google_books_metadata(f"intitle:{title} inauthor:{author or ''}")

        if metadata:
            new_content = content
            modified = False
            for k, v in metadata.items():
                if v and (not props.get(k) or not props[k].strip()):
                    # Add/Update property in top drawer
                    if re.search(f'^:{k}:', new_content, re.MULTILINE):
                        new_content = re.sub(f'^:{k}:.*$', f':{k}: {v}', new_content, flags=re.MULTILINE)
                    else:
                        # Insert before :END: of first drawer
                        new_content = re.sub(r'^:END:', f':{k}: {v}\n:END:', new_content, count=1, flags=re.MULTILINE)
                    modified = True
            
            if modified:
                with open(file, 'w', encoding='utf-8') as f:
                    f.write(new_content)
                updated += 1
                print(f"Enhanced {file.name}")
                time.sleep(1) # Be nice to APIs

    print(f"Processed {total} notes, updated {updated}")

if __name__ == "__main__":
    enhance_notes()
