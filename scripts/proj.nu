def proj [project] {
  open projects.yaml | where name == $project | select name | each { |it| swaymsg workspace $it };
  open projects.yaml | where name == $project | select websites | each { |it| qutebrowser $it.websites };
  open projects.yaml | where name == $project | select textFiles | each { |it| emacsclient -c $it.textFiles & };
}
