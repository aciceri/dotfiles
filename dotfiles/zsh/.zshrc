HISTFILE=~/.history
HISTSIZE=1000
SAVEHIST=1000
bindkey -v


#ZSH_THEME="agnoster"
source $ANTIGEN/share/antigen/antigen.zsh
antigen theme denysdovhan/spaceship-prompt
antigen theme spwhitt/nix-zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle supercrabtree/k
antigen bundle zsh-users/zsh-completions

#source $ZSH/oh-my-zsh.sh
# source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
#powerline-daemon -q
#. /usr/lib/python3.8/site-packages/powerline/bindings/zsh/powerline.zsh

# Aliases
alias ls="exa -l"
alias em="emacsclient -ct"
alias nh="st -f \"Dejavu Sans Mono:pixelsize=20\" -e zsh -c 'echo \"Make this window full screen and press ENTER\"; read; stty columns 80; stty rows 24; asciinema rec -c \"ssh nethack@alt.org\" --title \"Nethack gameplay - $(date)\"'"
alias weather="curl wttr.in/Milan"
alias rr="curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash"
alias pip-upgrade="pip list --user --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"
alias clean-downloads='sh ~/bin/clean-downloads.sh'

export visual="vim"


SPACESHIP_TIME_SHOW=true
SPACESHIP_USER_SHOW=always
SPACESHIP_HOST_SHOW=always

#eval $(thefuck --alias)
antigen apply
