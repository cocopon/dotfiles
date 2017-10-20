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

	" Input method
	if s:env.is_mac
		" Disable IM to avoid problem with autocompletion plugin
		" https://github.com/splhack/macvim-kaoriya/wiki/Readme#%E6%97%A5%E6%9C%AC%E8%AA%9E%E5%85%A5%E5%8A%9Bim%E8%87%AA%E5%8B%95%E3%82%AA%E3%83%B3%E3%82%AA%E3%83%95
		set imdisable

		" And also should set up Karabiner-Elements to turn off IM
		" after backing to normal mode
		" https://gist.github.com/cocopon/5238b296bc58f8a9bab0017ebecf62e6
	endif

	" Misc
	set guioptions-=m
	set guioptions-=T
	set showtabline=2
	set visualbell t_vb=
end
