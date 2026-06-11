#!/usr/bin/env python3
import sys
import requests
import subprocess
from datetime import datetime

def emacs_eval(elisp):
    result = subprocess.run(['emacsclient', '--eval', elisp], capture_output=True, text=True)
    return result.stdout.strip()

def search_movie(title):
    url = "https://www.wikidata.org/w/api.php"
    params = {
        "action": "wbsearchentities",
        "search": title,
        "language": "en",
        "format": "json",
        "type": "item"
    }
    r = requests.get(url, params=params)
    data = r.json()
    return data.get('search', [])

def get_label(entity_id):
    url = "https://www.wikidata.org/w/api.php"
    params = {
        "action": "wbgetentities",
        "ids": entity_id,
        "props": "labels",
        "languages": "en",
        "format": "json"
    }
    r = requests.get(url, params=params)
    data = r.json()
    return data['entities'][entity_id]['labels']['en']['value']

def get_entity_details(entity_id):
    url = f"https://www.wikidata.org/wiki/Special:EntityData/{entity_id}.json"
    r = requests.get(url)
    data = r.json()
    entity = data['entities'][entity_id]
    claims = entity.get('claims', {})
    
    label = entity.get('labels', {}).get('en', {}).get('value', 'Unknown')
    
    # Director (P57)
    director = "Unknown"
    director_claims = claims.get('P57', [])
    if director_claims:
        d_id = director_claims[0]['mainsnak']['datavalue']['value']['id']
        director = get_label(d_id)
        
    # Publication date (P577)
    year = ""
    date_claims = claims.get('P577', [])
    if date_claims:
        date_str = date_claims[0]['mainsnak']['datavalue']['value']['time']
        if date_str.startswith('+'):
            year = date_str[1:5]
            
    return {"id": entity_id, "label": label, "director": director, "year": year}

def main():
    if len(sys.argv) < 2:
        print("Usage: wikidata_movie.py <movie title>")
        sys.exit(1)
        
    query = " ".join(sys.argv[1:])
    results = search_movie(query)
    
    if not results:
        print("No results found.")
        sys.exit(1)
        
    # Pick first result for now
    entity_id = results[0]['id']
    details = get_entity_details(entity_id)
    
    director = details['director']
    safe_director = director.replace('"', '\\"')
    
    # Find director node
    director_node_id = emacs_eval(f'(let ((nodes (vulpea-db-query))) (when-let ((node (seq-find (lambda (n) (string-match-p "{safe_director}" (vulpea-note-title n))) nodes))) (vulpea-note-id node)))')
    
    director_link = f"[[id:{director_node_id}][{director}]]" if (director_node_id and director_node_id != "nil") else director
    title = f"{details['label']} ({details['year']})" if details['year'] else details['label']
    today = datetime.now().strftime("[%Y-%m-%d %a]")
    
    # Find movies.org 'watched' heading ID
    movies_id = emacs_eval('(let ((node (seq-find (lambda (n) (and (string-match-p "movies.org" (vulpea-note-path n)) (string= (vulpea-note-title n) "watched"))) (vulpea-db-query)))) (when node (vulpea-note-id node)))')
    if movies_id == "nil":
        movies_id = emacs_eval('(let ((node (seq-find (lambda (n) (and (string-match-p "movies.org" (vulpea-note-path n)) (eq (vulpea-note-level n) 0))) (vulpea-db-query)))) (when node (vulpea-note-id node)))')

    final_elisp = f"""(let ((parent (vulpea-db-get-by-id "{movies_id}")))
      (vulpea-create "{title}" nil 
        :parent parent
        :properties '(("WIKIDATA" . "wd:{entity_id}") ("RATING" . ""))
        :body "{today}\\n\\nDirected by {director_link}.\\n"))"""
        
    emacs_eval(final_elisp)
    print(f"Created note for {title} in movies.org")

if __name__ == "__main__":
    main()
