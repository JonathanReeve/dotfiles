
# Wallpaper management
def wal-fav [] {
  dms ipc wallpaper get | save --append ~/.cache/wal/favs
}

def wal-fav-set [] {
  dms ipc wallpaper set (open ~/.cache/wal/favs | lines | shuffle | first)
}

def wal-recent [] {
  let wall = (ls /run/media/jon/systemrestore/.systemrestore/Bildoj
         | sort-by modified -r | first 50 | shuffle | first | get name)
  dms ipc wallpaper set $wall 
}

def wal-backup [] {
  sudo rsync -a /home/systemrestore/Bildoj /run/media/jon/systemrestore/.systemrestore
}

# Project managemnt
# def proj [project] {
#     open ~/Dotfiles/scripts/projects.yaml | where name == $project | select websites | each { |it| qutebrowser $it.websites };
#     open ~/Dotfiles/scripts/projects.yaml | where name == $project | select textFiles | each { emacsclient -c $it.textFiles & };
# }

def create_left_prompt [] {
    starship prompt --cmd-duration $env.CMD_DURATION_MS $'--status=($env.LAST_EXIT_CODE)'
}

# Handy aliases
def em [f] { job spawn { emacsclient -c $f } }

# Vault management
const vault_mount = "/home/jon/.private-mount"
const vault_enc = "/home/jon/Dokumentoj/Personal/.Vault_gocryptfs"

def vault [] {
    gocryptfs $vault_enc $vault_mount
}

def unvault [] {
    fusermount -u $vault_mount
}

def jnl [] {
    do {
        vault
        emacsclient -c ($vault_mount | path join Journal jnl.org)
        unvault
    }
}

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

# Startup message
def show-proverbo [] {
    let proverbo_file = "/home/jon/Agordoj/scripts/proverboj.txt"
    if ($proverbo_file | path exists) {
        let proverbo = (open $proverbo_file | lines | shuffle | first)
        if (which cowsay | is-empty) {
            print $proverbo
        } else {
            print ($proverbo | cowsay)
        }
    }
}

show-proverbo
