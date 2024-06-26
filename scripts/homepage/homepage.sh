#!/etc/profiles/per-user/jon/bin/fish
set file "$HOME/Dokumentoj/Org/notes.org"
set out "$HOME/Dokumentoj/Org/notes.html"
set id "c8c13cd9-1ab1-4f48-afb6-9f48f0b38002"
emacs --batch --eval "(progn (find-file \"$file\" )(org-id-goto \"$id\")(org-html-export-to-html nil t nil t))"
set bg (head ~/.cache/wal/colors -n 1 | tail -n 1)
set fg (head ~/.cache/wal/colors -n 2 | tail -n 1)
pandoc -f html -t html --template=template-pandoc.html --metadata pagetitle="Homepage" -V fg=$fg -V bg=$bg -o $PWD/homepage.html $out
