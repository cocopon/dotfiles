"   __  __ _ _____ _____ _____ 
"   \ \ | |_|     |  ___| ____|
"  _ \ \| | | | | | |   | |___ 
" |_| \___|_|_|_|_|_|   |_____|


" Required Bundles {{{
let s:github_bundles = [
			\ 	'cocopon/colorswatch.vim',
			\ 	'cocopon/lightline-hybrid.vim',
			\ 	'cocopon/todo.vim',
			\ 	'davidhalter/jedi-vim',
			\ 	'h1mesuke/unite-outline',
			\ 	'itchyny/lightline.vim',
			\ 	'itchyny/thumbnail.vim',
			\ 	'kana/vim-arpeggio',
			\ 	'hynek/vim-python-pep8-indent',
			\ 	'mattn/calendar-vim',
			\ 	'mattn/emmet-vim',
			\ 	'pangloss/vim-javascript',
			\ 	'scrooloose/syntastic',
			\ 	'Shougo/neocomplcache',
			\ 	'Shougo/neosnippet',
			\ 	'Shougo/unite.vim',
			\ 	'Shougo/vesting',
			\ 	'Shougo/vimfiler',
			\ 	'Shougo/vimshell',
			\ 	'Shougo/vinarise',
			\ 	'thinca/vim-editvar',
			\ 	'thinca/vim-prettyprint',
			\ 	'thinca/vim-qfreplace',
			\ 	'thinca/vim-quickrun',
			\ 	'thinca/vim-ref',
			\ 	'tomtom/tcomment_vim',
			\ 	'tpope/vim-fugitive',
			\ 	'tpope/vim-surround',
			\ 	'tyru/open-browser.vim',
			\ 	'tyru/restart.vim',
			\ 	'ujihisa/camelcasemotion',
			\ 	'ujihisa/unite-colorscheme',
			\ 	'ujihisa/unite-font',
			\ 	'vim-jp/vital.vim',
			\ 	'vim-scripts/Align',
			\ 	'vim-scripts/matchit.zip',
			\ 	'vim-scripts/rest.vim',
			\ 	'yuratomo/w3m.vim',
			\
			\ 	'altercation/vim-colors-solarized',
			\ 	'jonathanfilip/vim-lucius',
			\ 	'jpo/vim-railscasts-theme',
			\ 	'nanotech/jellybeans.vim',
			\ 	'tomasr/molokai',
			\ 	'vim-scripts/proton',
			\ 	'vim-scripts/pyte',
			\ 	'vim-scripts/twilight',
			\ 	'w0ng/vim-hybrid',
			\ ]
let s:other_bundles = [
			\ ]
" }}}


" Utilities {{{
function! VimrcEnvironment()
	let env = {}
	let env.is_win = has('win32') || has('win64')

	return env
endfunction
let s:env = VimrcEnvironment()

function! s:mkdir_silently(dir)
	if isdirectory(a:dir)
		return 0
	endif

	call mkdir(a:dir)
	return 1
endfunction

function! VimrcDirectories()
	let runtime_path = s:env.is_win 
				\ ? expand('$VIM/vimfiles')
				\ : expand('~/.vim')
	return {
				\ 	'runtime': runtime_path,
				\ 	'neobundle': runtime_path . '/neobundle.vim',
				\ 	'bundle': runtime_path . '/bundle',
				\ 	'neosnippet': runtime_path . '/.neosnippet',
				\ }
endfunction
let s:dirs = VimrcDirectories()
" }}}


" Setup {{{
function! s:setup_dirs()
	let result = 0

	for dir in values(s:dirs)
		 let result = s:mkdir_silently(dir)

		 if result == 0
			 return 0
		 endif
	endfor

	return 1
endfunction

function! s:setup_neobundle()
	let neobundle_url = 'https://github.com/Shougo/neobundle.vim'
	if isdirectory(s:dirs.neobundle)
		return 0
	endif

	execute printf('!git clone %s %s', neobundle_url, s:dirs.neobundle)
	return 1
endfunction

function! s:setup_bundles()
	call s:mkdir_silently(s:dirs.bundle)
	let exists_bundle = (glob(s:dirs.bundle . '*') != '')

	filetype off
	if has('vim_starting')
		execute 'set runtimepath+=' . s:dirs.neobundle
		call neobundle#rc(s:dirs.bundle)
	endif

	" Vimproc
	NeoBundle 'https://github.com/Shougo/vimproc', {
				\ 	'build': {
				\ 		'cygwin': 'make -f make_cygwin.mak',
				\ 		'mac': 'make -f make_mac.mak',
				\ 		'unix': 'make -f make_unix.mak',
				\ 	}
				\ }

	" Github Bundles
	for bundle in s:github_bundles
		execute printf("NeoBundle 'https://github.com/%s'", bundle)
	endfor

	" Other Bundles
	for bundle in s:other_bundles
		execute printf("NeoBundle '%s'", bundle)
	endfor

	filetype plugin on
	filetype indent on

	if !exists_bundle
		NeoBundleInstall!
	endif
endfunction

function! s:setup()
	let funcnames = [
				\ 	'dirs',
				\ 	'neobundle',
				\ 	'bundles',
				\ ]

	for funcname in funcnames
		execute printf('let s:%s_executed = s:setup_%s()', funcname, funcname)
	endfor
endfunction

call s:setup()
" }}}


" Encoding {{{
set enc=utf-8
source $VIMRUNTIME/delmenu.vim
set langmenu=ja_JP.utf-8
source $VIMRUNTIME/menu.vim
" }}}


" Key {{{
" Turn off the IME when escaping Insert mode
set noimdisable
set iminsert=0 imsearch=-1
set noimcmdline
inoremap <silent> <ESC> <ESC>:<C-u>set iminsert=0<CR>
inoremap <silent> <C-[> <C-[>:<C-u>set iminsert=0<CR>

" Intuitive cursor movement when `:set wrap`
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

" Refer history in Command-line mode
cnoremap <C-p> <Up>
cnoremap <Up> <C-p>
cnoremap <C-n> <Down>
cnoremap <Down> <C-n>

" For US keyboard
nnoremap ; :

" Disable Ctrl-@ (for JIS keyboard)
inoremap <C-@> <Nop>

" File
nnoremap <C-n> :<C-u>e %:h<CR>
nnoremap <C-m> :<C-u>Unite file_mru<CR>

" Misc
nnoremap Y y$
" }}}


" Indent {{{
set autoindent
set list
set listchars=eol:¬,tab:▸\ 
set nosmartindent
set shiftround
set shiftwidth=4
set tabstop=4
" }}}


" Search {{{
set hlsearch
set ignorecase
set incsearch
set smartcase
" }}}


" Plugins {{{
" Disable archive plugins
let g:loaded_gzip = 1
let g:loaded_zip = 1
let g:loaded_zipPlugin = 1

" Disable unused kaoriya plugins
let plugin_dicwin_disable = 1

" CamelCaseMotion
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
map <silent> w <Plug>CamelCaseMotion_w

" matchit
runtime macros/matchit.vim
let b:match_words = "\<if\>:\<end\>,\<do\>:\<end\>,\<def\>:\<end\>"

" NeoComplCache
let g:neocomplcache_enable_at_startup = 1

" NeoSnippet
let g:neosnippet#snippets_directory = s:dirs.neosnippet
imap <C-Space> <Plug>(neosnippet_expand_or_jump)
smap <C-Space> <Plug>(neosnippet_expand_or_jump)
xmap <C-Space> <Plug>(neosnippet_expand_target)

" Netrw
let g:netrw_altv = 1
let g:netrw_preview = 1

" Unite
let g:unite_enable_start_insert = 0
let g:unite_split_rule = 'botright'
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
nnoremap <silent> ,ul :<C-u>Unite line<CR>
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
nnoremap <silent> ,uo :<C-u>Unite outline<CR>
nnoremap <silent> ,ut :<C-u>Unite todo/all<CR>

" Vimfiler
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0

" Syntastic
let g:syntastic_mode_map = {
			\ 	'mode': 'passive',
			\ 	'active_filetypes': ['javascript', 'python'],
			\ 	'passive_filetypes': [],
			\ }
let g:syntastic_auto_loc_list = 0

" --browser .... Tolerate standard browser globals
" --es5 ........ Tolerate ECMA Script 5 syntax (e.g. trailling commas)
" --nomen ...... Tolerate initial or trailing underbars in names
" --plusplus ... Tolerate '++' and '--'
" --sloppy ..... Tolerate missing 'use strict' pragma
" --vars ....... Tolerate many var statements per function
" --white ...... Tolerate messy white space
let g:syntastic_javascript_checkers = ['jslint']
let g:syntastic_javascript_jslint_args = join([
			\ 	'--browser',
			\ 	'--es5=false',
			\ 	'--nomen',
			\ 	'--plusplus',
			\ 	'--sloppy',
			\ 	'--vars',
			\ 	'--white',
			\ 	'--predef=goog',
			\ 	'--predef=jQuery',
			\ 	'--predef=$'
			\ ])

" E501 ... line too long
let g:syntastic_python_checkers = ['flake8']
let g:syntastic_python_flake8_args = join([
			\ 	'--ignore=E501'
			\ ])

" EasyMotion {{{
" let g:EasyMotion_keys = 'hfklasdfgyuiopqwertnmzxcvb'
" let g:EasyMotion_leader_key = '<Space><Space>'
" }}}

" Arpeggio {{{
" call arpeggio#load()
" let g:arpeggio_timeoutlen = 100
" Arpeggio nnoremap ef :<C-u>Unite file_mru<CR>
" Arpeggio nnoremap ew :<C-u>e %:h<CR>
" Arpeggio nnoremap fj :<C-u>Unite outline<CR>
" }}}

" quickrun.vim
let g:quickrun_config = {}
let g:quickrun_config['*'] = {'runner': 'vimproc'}
let g:quickrun_config['rst'] = {
			\ 	'command': 'rst2html.py',
			\ 	'outputter': 'browser',
			\ 	'runner': 'system'
			\ }

" lightline
let g:lightline = {
			\ 	'active': {
			\ 		'left': [
			\ 			['mode'],
			\ 			['filename', 'readonly', 'modified']
			\ 		],
			\ 		'right': [
			\ 			['lineinfo'],
			\ 			['percent'],
			\ 			['fileformat', 'fileencoding', 'filetype']
			\ 		]
			\ 	},
			\ 	'inactive': {
			\ 		'left': [
			\ 			['filename', 'modified']
			\ 		],
			\ 		'right': []
			\ 	},
			\ 	'colorscheme': 'hybrid',
			\ 	'mode_map': {
			\ 		'n': '-',
			\ 		'i': 'I',
			\ 		'R': 'R',
			\ 		'v': 'V',
			\ 		'V': 'V',
			\ 		'c': 'C',
			\ 		"\<C-v>": 'V',
			\ 		's': 'S',
			\ 		'S': 'S',
			\ 		"\<C-s>": 'S',
			\ 		'?': ' '
			\ 	}
			\ }
let g:lightline_hybrid_style = 'plain'

" jedi-vim
let g:jedi#auto_vim_configuration = 0
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first = 0
let g:jedi#rename_command = '<leader>R'

let g:neocomplcache_force_omni_patterns = {
			\ 	'python': '\h\w*\|[^. \t]\.\w*'
			\ }
let g:neocomplcache_omni_functions = {
			\ 	'python': 'jedi#completions'
			\ }

autocmd FileType python let b:did_ftplugin = 1

" openbrowser
nmap gW <Plug>(openbrowser-open)
" }}}


" FileType {{{
autocmd BufRead,BufNewFile *.as  setlocal ft=javascript
autocmd BufRead,BufNewFile *.pde setlocal ft=java sw=2 ts=2 expandtab
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
autocmd FileType xhtml      setlocal indentexpr&
autocmd FileType xml        setlocal sw=2 ts=2
autocmd FileType yaml       setlocal sw=2 ts=2
" }}}


" Misc {{{
set completeopt=menu,menuone
set display=lastline
set grepprg=grep\ -nH
set nobackup
set noswapfile
set nrformats-=octal
set number
set scrolloff=8
set shortmess=aTI
set sidescroll=1
set sidescrolloff=16
if has('virtualedit')
	set virtualedit=block
endif

" Disable archive plugins
let loaded_gzip = 1
let loaded_zip = 1
let loaded_zipPlugin = 1
" }}}


" Local Settings {{{
let s:local_vimrc = s:dirs.runtime . '.vimrc_local'
if filereadable(s:local_vimrc)
	execute 'source ' . s:local_vimrc
endif
" }}}
