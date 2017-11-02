set -x PATH ~/.local/bin ~/.node/bin (ruby -rubygems -e "puts Gem.user_dir")/bin $PATH

set -e EDITOR
set -e BROWSER
set -Ux EDITOR vim
set -Ux BROWSER qutebrowser

# Emacs ansi-term support
if test -n "$EMACS"
  set -x TERM eterm-color
  function fish_right_prompt
    true
  end
end

set acceptable_terms eterm-color xterm-256color screen-256color

if contains "$TERM" acceptable_terms
  # Don't use vi keybindings in unknown terminals,
  # since weird things can happen.
  fish_vi_key_bindings
end 

# This function may be required for Emacs support. 
function fish_title
  true
end
