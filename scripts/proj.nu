def proj [project] {
  open projects.yaml | where name == $project | select websites | each { qutebrowser $it.websites };
  open projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
}
