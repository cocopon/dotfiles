"   __  __ _ _____ _________
"   \ \ | |_|     V  __/  __|
"    \ \| | | | | | |  | [__
" [_] \___|_|_|_|_|_|  \____|


" Execute the following command to bring the full power:
"
" 	:call VimrcSetUp()


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
				\ 	'user':        user_dir,
				\ 	'plugins':     user_dir . '/plugins',
				\ 	'plug_preset': user_dir . '/plug-preset.vim',
				\ 	'data':        user_dir . '/data',
				\ 	'local_vimrc': user_dir . '/.vimrc_local',
				\ 	'tmp':         user_dir . '/tmp',
				\ 	'undo':        user_dir . '/data/undo',
				\ 	'vim_plug':    user_dir . '/vim-plug',
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
			\ 	'AndrewRadev/linediff.vim',
			\ 	'ctrlpvim/ctrlp.vim',
			\ 	'Shougo/vimproc',
			\ 	'Shougo/vinarise',
			\ 	'cespare/vim-toml',
			\ 	'cocopon/colorswatch.vim',
			\ 	'cocopon/iceberg.vim',
			\ 	'cocopon/shadeline.vim',
			\ 	'cocopon/svss.vim',
			\ 	'digitaltoad/vim-jade',
			\ 	'editorconfig/editorconfig-vim',
			\ 	'groenewege/vim-less',
			\ 	'hynek/vim-python-pep8-indent',
			\ 	'itchyny/thumbnail.vim',
			\ 	'justinmk/vim-dirvish',
			\ 	'kana/vim-textobj-indent',
			\ 	'kana/vim-textobj-user',
			\ 	'kannokanno/previm',
			\ 	'kchmck/vim-coffee-script',
			\ 	'leafgarland/typescript-vim',
			\ 	'mattn/emmet-vim',
			\ 	'mxw/vim-jsx',
			\ 	'nanotech/jellybeans.vim',
			\ 	'pangloss/vim-javascript',
			\ 	'rking/ag.vim',
			\ 	'scrooloose/syntastic',
			\ 	'sophacles/vim-processing',
			\ 	'stephpy/vim-yaml',
			\ 	'thinca/vim-qfreplace',
			\ 	'thinca/vim-quickrun',
			\ 	'thinca/vim-ref',
			\ 	'thinca/vim-themis',
			\ 	'thinca/vim-zenspace',
			\ 	'tikhomirov/vim-glsl',
			\ 	'tomtom/tcomment_vim',
			\ 	'toyamarinyon/vim-swift',
			\ 	'tpope/vim-fugitive',
			\ 	'tpope/vim-markdown',
			\ 	'tpope/vim-surround',
			\ 	'tyru/open-browser.vim',
			\ 	'ujihisa/camelcasemotion',
			\ 	'vim-jp/vital.vim',
			\ 	'vim-scripts/matchit.zip',
			\ 	'w0ng/vim-hybrid',
			\ 	s:supports.neocomplete
			\ 		? 'Shougo/neocomplete.vim'
			\ 		: 'Shougo/neocomplcache.vim',
			\ ]
let s:colorscheme = 'iceberg'
" }}}


" Setup {{{
function! VimrcSetUp()
	call s:install_plugin_manager()
endfunction
" }}}


" Installing {{{
function! s:mkdir_if_needed(dir)
	if isdirectory(a:dir)
		return 0
	endif

	call mkdir(a:dir, 'p')
	return 1
endfunction

function! s:install_plugins()
	call s:mkdir_if_needed(s:env.path.plugins)

	if exists(':PlugInstall')
		PlugInstall
		return 1
	endif

	return 0
endfunction

function! s:clone_repository(url, local_path)
	if isdirectory(a:local_path)
		return
	endif

	execute printf('!git clone %s %s', a:url, a:local_path)
endfunction

function! s:install_plugin_manager()
	call s:mkdir_if_needed(s:env.path.user)
	call s:mkdir_if_needed(s:env.path.data)

	call s:clone_repository(
				\ 'https://github.com/junegunn/vim-plug',
				\ s:env.path.vim_plug . '/autoload')
	call s:clone_repository(
				\ 'https://github.com/cocopon/plug-preset.vim',
				\ s:env.path.plug_preset)

	if !s:activate_plugin_manager()
		return 0
	endif

	if !s:install_plugins()
		return 0
	endif

	echo 'Restart vim to finish the installation.'
	return 1
endfunction
" }}}


" Activating {{{
function! s:load_plugin(path)
	try
		execute 'set runtimepath+=' . a:path

		return 1
	catch /:E117:/
		" E117: Unknown function
		return 0
	endtry
endfunction

function! s:activate_plugins()
	if !exists(':Plug')
		" Plugin manager not installed yet
		return 0
	endif

	let command = exists(':PresetPlug')
				\ ? 'PresetPlug'
				\ : 'Plug'

	for plugin in s:plugins
		execute printf("%s 'https://github.com/%s'", command, plugin)
	endfor

	return 1
endfunction

function! s:activate_plugin_manager_internal()
	" Activate plugin manager
	if !exists(':Plug')
		execute 'set runtimepath+=' . s:env.path.vim_plug
	endif
	call plug#begin(s:env.path.plugins)

	try
		" Activate PresetPlug
		if !exists(':PresetPlug')
			execute 'set runtimepath+=' . s:env.path.plug_preset
		endif
		call plug_preset#init()

		" Activate plugins
		return s:activate_plugins()
	finally
		call plug#end()
		filetype indent on
		filetype plugin on
	endtry
endfunction

function! s:activate_plugin_manager()
	try
		return s:activate_plugin_manager_internal()
	catch /:E117:/
		" E117: Unknown function
		" Plugin manager not installed yet
		return 0
	endtry
endfunction
" }}}


" Initializing {{{
call s:mkdir_if_needed(s:env.path.tmp)
call s:mkdir_if_needed(s:env.path.undo)
let s:plugins_activated = s:activate_plugin_manager()
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
cnoremap <C-k> <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos() - 2]<CR>
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

" Open the parent directory, or the current directory if not saved yet
nnoremap <silent> <C-u> :execute 'e ' . ((strlen(bufname('')) == 0) ? '.' : '%:h')<CR>

" Insert escaped '/' while inputting a search pattern
cnoremap <expr> / getcmdtype() == '/' ? '\/' : '/'

" For JIS keyboard
inoremap <C-@> <Nop>

" For US keyboard
noremap ; :

" Misc
nnoremap Y y$
nnoremap K <Nop>
" }}}


" File Types {{{
augroup vimrc_filetype
	autocmd!
	autocmd BufNewFile,BufRead *.as      setlocal filetype=javascript
	autocmd BufNewFile,BufRead *.gradle  setlocal filetype=groovy
	autocmd BufNewFile,BufRead *.pde     setlocal filetype=processing
	autocmd BufNewFile,BufRead *.podspec setlocal filetype=ruby
	autocmd BufNewFile,BufRead Podfile   setlocal filetype=ruby
	autocmd FileType apache     setlocal shiftwidth=2 tabstop=2
	autocmd FileType coffee     setlocal shiftwidth=2 tabstop=2 expandtab
	autocmd FileType css        setlocal shiftwidth=4 tabstop=4
	autocmd FileType eruby      setlocal shiftwidth=2 tabstop=2
	autocmd FileType gitcommit  setlocal nocursorline spell
	autocmd FileType html       setlocal shiftwidth=2 tabstop=2 indentexpr&
	autocmd FileType jade       setlocal shiftwidth=2 tabstop=2
	autocmd FileType javascript setlocal shiftwidth=2 tabstop=2
	autocmd FileType php        setlocal shiftwidth=2 tabstop=2
	autocmd FileType python     setlocal shiftwidth=4 tabstop=4 expandtab
	autocmd FileType rst        setlocal indentexpr&
	autocmd FileType ruby       setlocal shiftwidth=2 tabstop=2 expandtab
	autocmd FileType scss       setlocal shiftwidth=4 tabstop=4
	autocmd FileType text       setlocal textwidth=0
	autocmd FileType vim        setlocal shiftwidth=2 tabstop=2 fdm=marker
	autocmd FileType xhtml      setlocal shiftwidth=2 tabstop=2 indentexpr&
	autocmd FileType xml        setlocal shiftwidth=2 tabstop=2
	autocmd FileType yaml       setlocal shiftwidth=2 tabstop=2
augroup END
" }}}


" Misc {{{
set completeopt=menu,menuone
set display=lastline
set grepprg=grep\ -nH
set nrformats-=octal
set shortmess=aTI
set virtualedit=block
set wildmenu

" Appearance
set cmdheight=2
set cursorline
set laststatus=2
set list
set listchars=eol:¬,tab:▸\ 
set number
set numberwidth=5
set showcmd
set showmatch
set wrap

" Backup
set nobackup
set noswapfile
execute 'set undodir=' . s:env.path.undo
set undofile

" IME
set iminsert=0
set imsearch=-1
set noimcmdline
set noimdisable

" Indent
set autoindent
if exists('&breakindent')
	set breakindent
endif
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
set wrapscan
" }}}


" Plugins {{{
" netrw {{{
let g:netrw_altv = 1
let g:netrw_preview = 1
" }}}

if s:plugins_activated
	" camelcasemotion {{{
	map <silent> b <Plug>CamelCaseMotion_b
	map <silent> e <Plug>CamelCaseMotion_e
	map <silent> w <Plug>CamelCaseMotion_w
	" }}}

	" ctrlp {{{
	let g:ctrlp_cache_dir = s:env.path.data . '/ctrlp'
	let g:ctrlp_map = ''
	let g:ctrlp_prompt_mappings = {
				\ 	'PrtBS()':            ['<C-h>', '<BS>'],
				\ 	'PrtClear()':         ['<C-k>'],
				\ 	'PrtCurLeft()':       ['<C-b>', '<Left>'],
				\ 	'PrtCurRight()':      ['<C-f>', '<Right>'],
				\ 	'PrtDelete()':        ['<C-d>', '<Del>'],
				\ 	'PrtHistory(-1)':     [],
				\ 	'PrtHistory(1)':      [],
				\ 	'PrtSelectMove("j")': ['<C-n>', '<Down>'],
				\ 	'PrtSelectMove("k")': ['<C-p>', '<Up>'],
				\ 	'ToggleByFname()':    [],
				\ 	'ToggleType(-1)':     ['<C-Down>'],
				\ 	'ToggleType(1)':      ['<C-Up>'],
				\ }

	nnoremap <C-g> :CtrlPMRU<CR>
	" }}}

	" dirvish {{{
	function! s:dirvish_init()
		nmap <buffer> K :!mkdir %
		nmap <buffer> <silent> h <Plug>(dirvish_up)
		nmap <buffer> <silent> l <CR>

		" Simulate autochdir
		execute printf('cd %s', b:dirvish._dir)
	endfunction

	augroup vimrc_dirvish
		autocmd!
		autocmd FileType dirvish call s:dirvish_init()
	augroup END
	" }}}

	" neocomplcache/neocomplete {{{
	if s:supports.neocomplete
		let g:neocomplete#enable_at_startup = 1
		let g:neocomplete#data_directory = s:env.path.data . '/neocomplete'

		let g:neocomplete#force_omni_input_patterns = {
					\ 	'python': '\h\w*\|[^. \t]\.\w*',
					\ }
	else
		let g:neocomplcache_enable_at_startup = 1
		let g:neocomplcache_temporary_dir = s:env.path.data . '/neocomplcache'

		let g:neocomplcache_force_omni_patterns = {
					\ 	'python': '\h\w*\|[^. \t]\.\w*',
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
				\ 	'runner/vimproc/updatetime': 40,
				\ }
	let g:quickrun_config['rst'] = {
				\ 	'command': 'rst2html.py',
				\ 	'outputter': 'browser',
				\ 	'runner': 'system',
				\ }
	let g:quickrun_config['processing'] = {
				\ 	'command': 'processing-java',
				\ 	'exec': '%c --sketch=%s:p:h/ --output=' . s:env.path.tmp . '/processing --force --run',
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
				\ 		'ShadelineItemGitBranch',
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

	function! ShadelineItemGitBranch()
		let name = exists('*fugitive#head')
					\ ? fugitive#head()
					\ : ''
		return empty(name) ? '' : printf('(%s)', name)
	endfunction
	" }}}

	" svss {{{
	let g:svss_auto_source = 1
	" }}}

	" syntastic {{{
	let g:syntastic_mode_map = {
				\ 	'mode': 'passive',
				\ 	'active_filetypes': [
				\ 		'coffee',
				\ 		'html',
				\ 		'javascript',
				\ 		'python',
				\ 		'typescript',
				\ 		'vim',
				\ 	],
				\ 	'passive_filetypes': [],
				\ }
	let g:syntastic_auto_loc_list = 0

	" javascript
	let g:syntastic_javascript_checkers = ['eslint']

	" python
	" E501 ... line too long
	let g:syntastic_python_checkers = ['flake8']
	let g:syntastic_python_flake8_args = join([
				\ 	'--ignore=E501',
				\ ])

	" typescript
	let g:syntastic_typescript_checkers = ['tslint']
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
if s:plugins_activated
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
