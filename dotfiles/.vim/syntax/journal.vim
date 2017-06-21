"Custom syntax highlighting for my personal journal

"highlight dividers
syntax match journalDivider /^-\{10,\}$/
highlight link journalDivider Special

" highlight dates
syntax match journalDate /.*T 20.*/
highlight link journalDate Comment

" highlight entry titles that start with #
syntax match notesAtxHeading /^#\+.*/ contains=notesAtxMarker,@notesInline
highlight def link notesAtxHeading Title
syntax match notesAtxMarker /^#\+/ contained
highlight def link notesAtxMarker Comment

" Highlight @tags as hyperlinks. {{{2
syntax match notesTagName /\(^\|\s\)\@<=@\w\+/
highlight def link notesTagName Underlined

