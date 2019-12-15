#!/run/current-system/sw/bin/bash

export CLOCKSTRING=$(emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)' 2>&1 )

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
        echo ${CLOCKSTRING} > /tmp/errors ;
esac

# echo "---"
# echo "Clock in last | bash='emacsclient --eval (org-clock-in-last)'"
# echo "Stop clock | bash='emacsclient --eval (org-clock-out)'"
