#!/home/jon/.nix-profile/bin/fish
set file "~/Dropbox/Org/Projects/notes.org"
set id "c8c13cd9-1ab1-4f48-afb6-9f48f0b38002"
emacsclient -e "(progn (find-file \"$file\" ) \
                       (org-id-goto \"$id\") \
                       (org-html-export-to-html nil t))"
