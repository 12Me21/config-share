# NOTE: A lot of this stuff is copied from the default DEBIAN .bashrc file,

#start=`date +%s%N`

# If not running interactively, don't do anything
case $- in
	*i*) ;;
	*) return;;
esac

HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s checkwinsize
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
#if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
#	debian_chroot=$(cat /etc/debian_chroot)
#fi
# ${debian_chroot:+($debian_chroot)}

case $TERM in
	xterm*)
		function esc {
			echo "\[\e$1\]$2"
		}
		PS1=""
		PS1+="`esc ']0;\u: \w\a'       `" # set window title
		PS1+="`esc '[0;4;38;5;22m' '\u'`" # reset attributes, underline, dark blue.  Username
		PS1+="`esc '[39m'          ':' `" # reset color.  ":"
		PS1+="`esc '[1;34m'        '\w'`" # bold, blue.  Current Path
		PS1+="`esc '[39;22m'       '\$'`" # reset color, reset bold.  "$" (or "#" if root)
		PS1+="`esc '[m'            ' ' `" # reset attribytes.  Space
		unset -f esc

		# magic function which prints a newline before printing prompt
		# if the cursor is not at the start of the row
		PROMPT_COMMAND=_my_prompt_command
		function _my_prompt_command {
			local curpos
			IFS=';' read -p$'\e[6n' -d'R' -s -t5 _ curpos
			((curpos!=1)) && echo
			echo -n $'\e[m\e[K' #clear row
		}

		# programs that use stupid hardcoded sequences
		if [ -x /usr/bin/dircolors ]; then
			test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
			for c in ls dir vdir grep fgrep egrep; do
				alias $c=$c' --color=auto'
			done
		fi

		export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
		export LS_COLORS='rs=0' #reset
		# File types
		LS_COLORS+=':di=38;5;20;1;48;5;189' #directories  97;42;1
		LS_COLORS+=':ln=0;106' #symlink
		LS_COLORS+=':mh=0'  #I don't remember most of these and they aren't used often
		LS_COLORS+=':pi=40;33:so=1;35:do=1;35:bd=40;33;1:cd=40;33;1'
		LS_COLORS+=':or=91;106' #orphaned symlink
		LS_COLORS+=':mi=0'
		LS_COLORS+=':su=:sg=:ca=:tw=:ow=:st=' #permissions I think
		LS_COLORS+=':ex=4;3' #executable
		# File extensions:
		LS_COLORS+=':*.tar=102:*.tar.gz=102:*.tar.xz=102:*.7z=102:*.zip=102:*.rar=102' # archives
		LS_COLORS+=':*.py=36'
		LS_COLORS+=':*.lua=94'
		LS_COLORS+=':*.js=33;38;5;166'
		LS_COLORS+=':*.html=31:*.htm=31'
		LS_COLORS+=':*.pdf=31'
		LS_COLORS+=':*.css=35'
		LS_COLORS+=':*makefile=92:*.mk=92:*.sh=92'
		LS_COLORS+=':*.c=90'
		LS_COLORS+=':*.png=35:*.gif=35:*.bmp=35:*.jpeg=35:*.jpg=35:*.webp=35:*.pdn=35:*.svg=35:*.JPG=35:*.PNG=35' # image
		LS_COLORS+=':*.ogg=36:*.wav=36:*.midi=36:*.mp3=36:*.flac=36:*.m4a=36:*.aac=36:*.aiff=36:*.mid=36:*.psg=36:*.vtx=36:*.pt3=36:*.mp4=36:*.mov=36:*.MOV=36:*.avi=36:*.AVI=36:*.webm=36:*.mkv=36:*.wmv=36:*.flv=36' # audio/video
		LS_COLORS+=':*.ini=90:*.json=90:*.JSON=90'

esac # xterm

alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# enable programmable completion features (you don't need to enable this, if it's already enabled in /etc/bash.bashrc and /etc/profile sources /etc/bash.bashrc).
if ! shopt -oq posix; then
	if [ -f /usr/share/bash-completion/bash_completion ]; then
		. /usr/share/bash-completion/bash_completion
	elif [ -f /etc/bash_completion ]; then
		. /etc/bash_completion
	fi
fi

#PATH="$PATH:~/scripts:." # evil

#end=`date +%s%N`
#echo Execution time was `expr '(' $end - $start ')' '/' 1000000`ms.
