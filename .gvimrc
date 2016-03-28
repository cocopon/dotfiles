let s:env = VimrcEnvironment()


if has('gui_running')
	" Reload langmenu to fix garbled characters
	if s:env.is_win
		source $VIMRUNTIME/delmenu.vim
		set langmenu=ja_JP.utf-8
		source $VIMRUNTIME/menu.vim
	endif

	" Font
	if s:env.is_win
		set guifont=Ricty_Diminished:h12
	else
		set guifont=SourceCodePro-Regular:h12
	endif

	" Misc
	set guioptions-=m
	set guioptions-=T
	set showtabline=2
	set visualbell t_vb=
end
