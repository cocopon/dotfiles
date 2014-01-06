let s:env = VimrcEnvironment()


if has('gui')
	" Reload langmenu to fix garbled characters
	if s:env.is_win
		source $VIMRUNTIME/delmenu.vim
		set langmenu=ja_JP.utf-8
		source $VIMRUNTIME/menu.vim
	endif

	" Color Scheme
	try
		colorscheme hybrid
	catch /:E185:/
		" Cannot find color scheme
	endtry

	" Font
	if s:env.is_win
		set guifont=BDF_UM+:h9
	else
		set guifont=SourceCodePro-Regular:h12
	endif

	" Misc
	set guioptions-=m
	set guioptions-=T
	set showtabline=2
	set visualbell t_vb=
end
