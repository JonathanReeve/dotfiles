#! /usr/bin/env nix-shell
#! nix-shell -i python -p "python38.withPackages(ps: with ps; [ requests ])""

import json
import requests
import sys
from subprocess import run, PIPE

query = sys.argv[1]
baseURL = "https://www.wikidata.org/w/api.php"
params = {"action": "wbsearchentities",
          "search": query,
          "language": "en",
          "format": "json"} 
response = requests.get(baseURL, params=params)

if response.ok:
    decoded = json.loads(response.text)
    # print(decoded['search'][0])
    itemsWithDescriptions = '\n'.join(['\t'.join(["http:"+item.get('url'), item.get('label', ''), item.get('descriptions', '')])
                             for item in decoded['search']])
    out = run(['rofi', '-dmenu'], input=itemsWithDescriptions, encoding='ascii', capture_output=True)
    print('#+wikidata: '+out.stdout.split('\t')[0])
else:
    print("Something went wrong.")
    print(response)
