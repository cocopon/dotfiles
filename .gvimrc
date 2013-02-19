let s:env = VimrcEnvironment()

" Appearance
if has('gui_macvim')
	colorscheme hybrid

	set cursorline
	set guioptions-=T
	set showtabline=2
	set transparency=3

	if s:env.is_win
		set guifont=BDF_UM+:h9
	else
		set guifont=SourceCodePro-Regular:h12
	endif
end
