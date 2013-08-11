# Prompt
autoload colors
colors

PROMPT="%1d%# "


# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups
setopt share_history

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end


# Key
bindkey -e
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end


# Completion
autoload -U compinit
compinit


# Other Zsh Settings
setopt auto_pushd


# Aliases
case "${OSTYPE}" in
freebsd*|darwin*)
	alias ls="ls -FG"
	;;
linux*)
	alias ls="ls -F --color"
	;;
esac

alias ll="ls -la"
alias la="ls -a"


# Local Settings
if [ -f ~/.zshrc_local ]; then
	source ~/.zshrc_local
fi

