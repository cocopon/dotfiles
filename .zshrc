autoload colors
autoload -Uz vcs_info

precmd() {
	vcs_info
}

# Prompt
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats '[%b%m%u%c]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{green}*%f"
zstyle ':vcs_info:git:*' unstagedstr "%F{yellow}*%f"
setopt PROMPT_SUBST
PROMPT='%1d${vcs_info_msg_0_}%# '


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
bindkey "^n" history-beginning-search-forward-end
bindkey "^p" history-beginning-search-backward-end


# Completion
autoload -U compinit
compinit


# Word Selection
autoload -U select-word-style
select-word-style bash


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


# peco
if (( $+commands[peco] )); then
	function peco-select-history() {
		local tac
		if (( $+commands[tac] )); then
			tac="tac"
		else
			tac="tail -r"
		fi
		BUFFER=$(history -n 1 | \
			eval $tac | \
			peco --query "$LBUFFER")
		CURSOR=$#BUFFER
		zle clear-screen
	}
	zle -N peco-select-history
	bindkey '^r' peco-select-history
fi


# Local Settings
if [ -f ~/.zshrc_local ]; then
	source ~/.zshrc_local
fi
