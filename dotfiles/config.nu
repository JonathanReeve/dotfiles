
# spawn task to run in the background
use ~/Agordoj/scripts/nu_scripts/modules/background_task/task.nu

# Wallpaper management
def wal-fav [] {
  open ~/.cache/wal/colors.json | get wallpaper |
  each { |it| echo $it (char newline)} |
  str join | save --append ~/.cache/wal/favs
}

def wal-fav-set [] {
  task spawn -i -l swaybg {
    ^pkill swaybg
    task kill
    let w = (open ~/.cache/wal/favs | lines | uniq | shuffle | first)
    echo $"Using ($w)"
    swaybg -i $w -m fill
  }
}

def wal-recent [] {
  wal -i (ls /run/media/jon/systemrestore/.systemrestore/Bildoj
         | sort-by modified -r
         | first 50
         | shuffle
         | first
         | get name)
}

def wal-backup [] {
  sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore
}

# Project managemnt
def proj [project] {
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { |it| qutebrowser $it.websites };
    open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
}

def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Handy aliases
def em [f] { spawn { emacsclient -c $f } }

module vterm {
  # Escape a command for outputting by vterm send
  def escape-for-send [
    to_escape: string # The data to escape
  ] {
    $to_escape | str replace --all '\\' '\\' | str replace --all '"' '\"'
  }

  # Send a command to vterm
  export def send [
    command: string # Command to pass to vterm
    ...args: string # Arguments to pass to vterm
  ] {
    print --no-newline "\e]51;E"
    print --no-newline $"\"(escape-for-send $command)\" "
    for arg in $args {
      print --no-newline $"\"(escape-for-send $arg)\" "
    }
    print --no-newline "\e\\"
  }

  # Clear the terminal window
  export def clear [] {
    send vterm-clear-scrollback
    tput clear
  }

  # Open a file in Emacs
  export def open [
    filepath: path # File to open
  ] {
    send "find-file" $filepath
  }
}

module vprompt {
  # Complete escape sequence based on environment
  def complete-escape-by-env [
    arg: string # argument to send
  ] {
    let tmux: string = (if ($env.TMUX? | is-empty) { '' } else { $env.TMUX })
    let term: string = (if ($env.TERM? | is-empty) { '' } else { $env.TERM })
    if $tmux =~ "screen|tmux" {
      # tell tmux to pass the escape sequences through
      $"\ePtmux;\e\e]($arg)\a\e\\"
    } else if $term =~ "screen.*" {
      # GNU screen (screen, screen-256color, screen-256color-bce)
      $"\eP\e]($arg)\a\e\\"
    } else {
      $"\e]($arg)\e\\"
    }
  }

  # Output text prompt that vterm can use to track current directory
  export def left-prompt-track-cwd [] {
    $"(create_left_prompt)(complete-escape-by-env $'51;A(whoami)@(hostname):(pwd)')"
  }
}

use vterm
use vprompt
#$env.PROMPT_COMMAND = {|| vprompt left-prompt-track-cwd }
