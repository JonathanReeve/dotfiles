#!/run/current-system/sw/bin/bash

#
# Include BitBar metadata like this at the top of the file
# (commented out, of course):
#
# <xbar.title>Org-mode clock display</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jonathan Reeve</xbar.author>
# <xbar.author.github>jonathanreeve</xbar.author.github>
# <xbar.desc>Show the currently clocked-in task from the org-mode clock.</xbar.desc>
# <xbar.image></xbar.image>
# <xbar.abouturl></xbar.abouturl>

export CLOCKSTRING=$(/opt/homebrew/bin/emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)' 2>&1)

off=" Emacs off!"
#noclock='<span color="#f00"> Off-clock!</span>'
noclock=' Off-clock!'

case "$CLOCKSTRING" in
    *"can\'t find socket"*)
        echo $off ;;
    "-1")
        echo $off ;;
    *"server-start"*)
        echo $off ;;
    *"ERROR"*)
        echo $noclock ;;
    *"function definition"*)
        echo $noclock ;;
    *)
        echo $CLOCKSTRING | cut -d '"' -f 2 | xargs;
esac

echo "---"
echo "Clock in last | bash='/etc/profiles/per-user/jon/bin/emacsclient --eval (org-clock-in-last)'"
echo "Stop clock | bash='/etc/profiles/per-user/jon/bin/emacsclient --eval (org-clock-out)'"
