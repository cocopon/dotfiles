let s:env = VimrcEnvironment()

" Appearance
if has('gui')
	try
		colorscheme hybrid
	catch /:E185:/
		" Cannot find color scheme
	endtry

	set cursorline
	set guioptions-=m
	set guioptions-=T
	set showtabline=2

	if s:env.is_win
		set guifont=BDF_UM+:h9
	else
		set guifont=SourceCodePro-Regular:h12
	endif
end
