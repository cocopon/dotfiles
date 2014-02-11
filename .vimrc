"   __  __ _ _____ _________
"   \ \ | |_|     V  __/  __|
"    \ \| | | | | | |  | [__
" [_] \___|_|_|_|_|_|  \____|


" Execute the following command to install plugins:
"
" 	:call VimrcInstallPluginManager()


" Encoding {{{
set encoding=utf-8
scriptencoding utf-8
" }}}


" Environment {{{
function! VimrcEnvironment()
	let env = {}
	let env.is_win = has('win32') || has('win64')

	let user_dir = env.is_win
				\ ? expand('$VIM/vimfiles')
				\ : expand('~/.vim')
	let env.path = {
				\ 	'user':          user_dir,
				\ 	'neobundle':     user_dir . '/neobundle.vim',
				\ 	'bundle':        user_dir . '/bundle',
				\ 	'data':          user_dir . '/data',
				\ 	'local_vimrc':   user_dir . '/.vimrc_local',
				\ 	'bundle_preset': user_dir . '/bundle-preset.vim',
				\ }

	return env
endfunction

function! VimrcSupports()
	let supports = {}

	let supports.neocomplete = has('lua')
				\ && (v:version > 703 || (v:version == 703 && has('patch885')))

	return supports
endfunction

let s:env = VimrcEnvironment()
let s:supports = VimrcSupports()
" }}}


" Required Plugins {{{
let s:plugins = [
			\ 	'Shougo/unite.vim',
			\ 	'Shougo/vesting',
			\ 	'Shougo/vimfiler',
			\ 	'Shougo/vimproc',
			\ 	'Shougo/vimshell',
			\ 	'Shougo/vinarise',
			\ 	'cocopon/colorswatch.vim',
			\ 	'cocopon/googkit.vim',
			\ 	'cocopon/iceberg.vim',
			\ 	'cocopon/shadeline.vim',
			\ 	'cocopon/snapbuffer.vim',
			\ 	'cocopon/svss.vim',
			\ 	'cocopon/todo.vim',
			\ 	'davidhalter/jedi-vim',
			\ 	'groenewege/vim-less',
			\ 	'h1mesuke/unite-outline',
			\ 	'hynek/vim-python-pep8-indent',
			\ 	'itchyny/thumbnail.vim',
			\ 	'kana/vim-textobj-indent',
			\ 	'kana/vim-textobj-user',
			\ 	'mattn/emmet-vim',
			\ 	'nanotech/jellybeans.vim',
			\ 	'pangloss/vim-javascript',
			\ 	'scrooloose/syntastic',
			\ 	'thinca/vim-prettyprint',
			\ 	'thinca/vim-qfreplace',
			\ 	'thinca/vim-quickrun',
			\ 	'thinca/vim-ref',
			\ 	'tomtom/tcomment_vim',
			\ 	'tpope/vim-fugitive',
			\ 	'tpope/vim-markdown',
			\ 	'tpope/vim-surround',
			\ 	'tyru/open-browser.vim',
			\ 	'ujihisa/camelcasemotion',
			\ 	'ujihisa/unite-colorscheme',
			\ 	'vim-jp/vital.vim',
			\ 	'vim-scripts/matchit.zip',
			\ 	'vim-scripts/rest.vim',
			\ 	'w0ng/vim-hybrid',
			\ 	'yuratomo/w3m.vim',
			\ 	s:supports.neocomplete
			\ 		? 'Shougo/neocomplete.vim'
			\ 		: 'Shougo/neocomplcache',
			\ ]
let s:colorscheme = 'hybrid'
" }}}


" Installing {{{
function! s:mkdir_silently(dir)
	if isdirectory(a:dir)
		return 0
	endif

	call mkdir(a:dir, 'p')
	return 1
endfunction

function! s:install_plugins()
	call s:mkdir_silently(s:env.path.bundle)

	if exists(':Unite')
		Unite neobundle/install:!
	else
		NeoBundleInstall!
	endif
endfunction

function! s:clone_repository(url, local_path)
	if isdirectory(a:local_path)
		return
	endif

	execute printf('!git clone %s %s', a:url, a:local_path)
endfunction

function! VimrcInstallPluginManager()
	call s:mkdir_silently(s:env.path.user)
	call s:mkdir_silently(s:env.path.data)

	call s:clone_repository(
				\ 'https://github.com/Shougo/neobundle.vim',
				\ s:env.path.neobundle)
	call s:clone_repository(
				\ 'https://github.com/cocopon/bundle-preset.vim',
				\ s:env.path.bundle_preset)

	call s:activate_plugin_manager()

	call s:install_plugins()

	echo 'Restart vim to finish the installation.'
endfunction
" }}}


" Activating {{{
function! s:activate_plugin(path, func, defined_command, ...)
	if exists(a:defined_command)
		" Already activated
		return 1
	endif

	try
		execute 'set runtimepath+=' . a:path
		call call(a:func, a:000[3:])

		return 1
	catch /:E117:/
		" E117: Unknown function
		return 0
	endtry
endfunction

function! s:activate_plugins()
	if !exists(':NeoBundle')
		" Plugin manager not installed yet
		return 0
	endif

	let command = exists(':PresetBundle')
				\ ? 'PresetBundle'
				\ : 'NeoBundle'

	for plugin in s:plugins
		execute printf("%s 'https://github.com/%s'", command, plugin)
	endfor

	filetype indent on
	filetype plugin on

	return 1
endfunction

function! s:activate_plugin_manager()
	if !s:activate_plugin(
				\ s:env.path.neobundle,
				\ 'neobundle#rc',
				\ ':NeoBundle',
				\ s:env.path.bundle)
		return 0
	endif

	call s:activate_plugin(
				\ s:env.path.bundle_preset,
				\ 'bundle_preset#rc',
				\ ':PresetBundle')

	return s:activate_plugins()
endfunction

let s:bundle_activated = s:activate_plugin_manager()
" }}}


" Mapping {{{
" Turn off the IME when escaping from Insert mode
inoremap <silent> <ESC> <ESC>:<C-u>set iminsert=0<CR>

" Intuitive cursor movement in wrapped line
noremap j gj
noremap k gk

" Switch active window
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Emacs-like keybind
cnoremap <C-a> <Home>
cnoremap <C-b> <Left>
cnoremap <C-d> <Del>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>
cnoremap <C-h> <Backspace>
cnoremap <C-k> <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>
inoremap <C-a> <Home>
inoremap <C-b> <Left>
inoremap <C-d> <Del>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-h> <Backspace>
inoremap <C-k> <C-o>D
inoremap <C-n> <Down>
inoremap <C-p> <Up>

" Refer to history in Command-line mode
cnoremap <C-p> <Up>
cnoremap <Up> <C-p>
cnoremap <C-n> <Down>
cnoremap <Down> <C-n>

" Open parent directory, or current directory if not saved yet
nnoremap <silent> <C-u> :execute 'e ' . ((strlen(bufname('')) == 0) ? '.' : '%:h')<CR>

" For JIS keyboard
inoremap <C-@> <Nop>

" For US keyboard
noremap ; :

" Misc
nnoremap Y y$
" }}}


" File Types {{{
augroup vimrc_filetype
	autocmd!
	autocmd BufRead,BufNewFile *.as     setlocal ft=javascript
	autocmd BufRead,BufNewFile *.gradle setlocal ft=groovy
	autocmd BufRead,BufNewFile *.pde    setlocal ft=java sw=2 ts=2 expandtab
	autocmd FileType css        setlocal sw=4 ts=4
	autocmd FileType eruby      setlocal sw=2 ts=2
	autocmd FileType html       setlocal sw=2 ts=2 indentexpr&
	autocmd FileType javascript setlocal sw=4 ts=4
	autocmd FileType php        setlocal sw=4 ts=4
	autocmd FileType python     setlocal sw=4 ts=4 expandtab
	autocmd FileType rst        setlocal indentexpr&
	autocmd FileType ruby       setlocal sw=2 ts=2
	autocmd FileType scss       setlocal sw=4 ts=4
	autocmd FileType text       setlocal tw=0
	autocmd FileType vim        setlocal sw=2 ts=2 fdm=marker
	autocmd FileType xhtml      setlocal sw=2 ts=2 indentexpr&
	autocmd FileType xml        setlocal sw=2 ts=2
	autocmd FileType yaml       setlocal sw=2 ts=2
augroup END
" }}}


" Misc {{{
set completeopt=menu,menuone
set cursorline
set display=lastline
set grepprg=grep\ -nH
set laststatus=2
set nrformats-=octal
set number
set shortmess=aTI
set virtualedit=block

" Backup
set nobackup
set noswapfile

" IME
set iminsert=0
set imsearch=-1
set noimcmdline
set noimdisable

" Indent
set autoindent
set list
set listchars=eol:¬,tab:▸\ 
set noexpandtab
set nosmartindent
set shiftround
set shiftwidth=4
set tabstop=4

" Scroll
set scrolloff=8
set sidescroll=1
set sidescrolloff=16

" Search
set hlsearch
set ignorecase
set incsearch
set smartcase
" }}}


" Plugins {{{
" netrw {{{
let g:netrw_altv = 1
let g:netrw_preview = 1
" }}}

if s:bundle_activated
	" camelcasemotion {{{
	map <silent> b <Plug>CamelCaseMotion_b
	map <silent> e <Plug>CamelCaseMotion_e
	map <silent> w <Plug>CamelCaseMotion_w
	" }}}

	" jedi {{{
	let g:jedi#auto_vim_configuration = 0
	let g:jedi#popup_on_dot = 0
	let g:jedi#popup_select_first = 0
	let g:jedi#rename_command = '<leader>R'
	augroup vimrc_jedi
		autocmd!
		autocmd FileType python let b:did_ftplugin = 1
	augroup END
	" }}}

	" neocomplcache/neocomplete {{{
	if s:supports.neocomplete
		let g:neocomplete#enable_at_startup = 1
		let g:neocomplete#data_directory = s:env.path.data . '/neocomplete'

		" neocomplete + jedi
		let g:neocomplete#force_omni_input_patterns = {
					\ 	'python': '\h\w*\|[^. \t]\.\w*'
					\ }
		let g:neocomplete#sources#omni#functions = {
					\ 	'python': 'jedi#completions'
					\ }
	else
		let g:neocomplcache_enable_at_startup = 1
		let g:neocomplcache_temporary_dir = s:env.path.data . '/neocomplcache'

		" neocomplcache + jedi
		let g:neocomplcache_force_omni_patterns = {
					\ 	'python': '\h\w*\|[^. \t]\.\w*'
					\ }
		let g:neocomplcache_omni_functions = {
					\ 	'python': 'jedi#completions'
					\ }
	endif
	" }}}

	" open-browser {{{
	nmap gW <Plug>(openbrowser-open)
	" }}}

	" quickrun {{{
	let g:quickrun_config = {}
	let g:quickrun_config['_'] = {
				\ 	'runner': 'vimproc',
				\ 	'runner/vimproc/updatetime': 40
				\ }
	let g:quickrun_config['rst'] = {
				\ 	'command': 'rst2html.py',
				\ 	'outputter': 'browser',
				\ 	'runner': 'system'
				\ }
	" }}}

	" ref {{{
	let g:ref_cache_dir = s:env.path.data . '/ref'
	" }}}

	" shadeline {{{
	let g:shadeline = {}
	let g:shadeline.active = {
				\ 	'left': [
				\ 		'fname',
				\ 		'flags',
				\ 	],
				\ 	'right': [
				\ 		'<',
				\ 		['ff', 'fenc', 'ft'],
				\ 		'ruler',
				\ 	],
				\ }
	let g:shadeline.inactive = {
				\ 	'left': [
				\ 		'fname',
				\ 		'flags',
				\ 	],
				\ }
	" }}}

	" svss {{{
	let g:svss_auto_source = 1
	" }}}

	" syntastic {{{
	let g:syntastic_mode_map = {
				\ 	'mode': 'passive',
				\ 	'active_filetypes': ['javascript', 'python'],
				\ 	'passive_filetypes': [],
				\ }
	let g:syntastic_auto_loc_list = 0

	" javascript
	let g:syntastic_javascript_checkers = ['jshint']

	" python
	" E501 ... line too long
	let g:syntastic_python_checkers = ['flake8']
	let g:syntastic_python_flake8_args = join([
				\ 	'--ignore=E501'
				\ ])
	" }}}

	" unite {{{
	let g:unite_data_directory = s:env.path.data . '/unite'
	let g:unite_enable_start_insert = 0
	let g:unite_split_rule = 'botright'
	nnoremap ,ub :Unite buffer<CR>
	nnoremap ,ul :Unite line<CR>
	nnoremap ,um :Unite file_mru:all<CR>
	nnoremap ,uo :Unite outline<CR>
	nnoremap ,ut :Unite todo/all<CR>
	nnoremap <C-g> :Unite file_mru:all<CR>
	" }}}

	" vimfiler {{{
	let g:vimfiler_as_default_explorer = 1
	let g:vimfiler_enable_auto_cd = 1
	let g:vimfiler_safe_mode_by_default = 0
	let g:vimfiler_data_directory = s:env.path.data . '/vimfiler'

	function! s:change_vimfiler_mapping()
		unmap <buffer> <C-j>
		unmap <buffer> <C-l>
		map <buffer> D <Plug>(vimfiler_switch_to_drive)
		map <buffer> L <Plug>(vimfiler_redraw_screen)
	endfunction
	augroup vimrc_vimfiler
		autocmd!
		autocmd FileType vimfiler call s:change_vimfiler_mapping()
	augroup END
	" }}}

	" vimshell {{{
	let g:vimshell_temporary_directory = s:env.path.data . '/vimshell'
	" }}}
endif

" Disable unused plugins
let g:loaded_gzip = 1
let g:loaded_tar = 1
let g:loaded_tarPlugin = 1
let g:loaded_zip = 1
let g:loaded_zipPlugin = 1

" Disable unused kaoriya plugins
let g:plugin_dicwin_disable = 1
" }}}


" Local Settings {{{
if filereadable(s:env.path.local_vimrc)
	execute 'source ' . s:env.path.local_vimrc
endif
" }}}


" Color Scheme {{{
if s:bundle_activated
	if !has('gui_running')
		syntax enable
		execute printf('colorscheme %s', s:colorscheme)
	else
		augroup vimrc_colorscheme
			autocmd!
			execute printf('autocmd GUIEnter * colorscheme %s', s:colorscheme)
		augroup END
	endif
endif
" }}}
