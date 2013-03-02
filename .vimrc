"   __  __ _ _____ _____ _____ 
"   \ \ | |_|     |  ___| ____|
"  _ \ \| | | | | | |   | |___ 
" |_| \___|_|_|_|_|_|   |_____|
"
" Yay, it's fully automated vimrc!


" Required Bundles {{{
let s:github_bundles = [
			\ 	'cocopon/todo.vim',
			\ 	'h1mesuke/unite-outline',
			\ 	'kana/vim-arpeggio',
			\ 	'Lokaltog/vim-easymotion',
			\ 	'mattn/calendar-vim',
			\ 	'mattn/zencoding-vim',
			\ 	'OrgaChem/JavaScript-syntax',
			\ 	'scrooloose/syntastic',
			\ 	'Shougo/neocomplcache',
			\ 	'Shougo/neosnippet',
			\ 	'Shougo/unite.vim',
			\ 	'Shougo/vimfiler',
			\ 	'Shougo/vimproc',
			\ 	'Shougo/vimshell',
			\ 	'Shougo/vinarise',
			\ 	'thinca/vim-editvar',
			\ 	'thinca/vim-qfreplace',
			\ 	'thinca/vim-quickrun',
			\ 	'thinca/vim-ref',
			\ 	'tomtom/tcomment_vim',
			\ 	'tpope/vim-surround',
			\ 	'tyru/restart.vim',
			\ 	'ujihisa/camelcasemotion',
			\ 	'ujihisa/unite-colorscheme',
			\ 	'vim-jp/vital.vim',
			\ 	'vim-scripts/Align',
			\ 	'vim-scripts/matchit.zip',
			\
			\ 	'altercation/vim-colors-solarized',
			\ 	'jonathanfilip/vim-lucius',
			\ 	'jpo/vim-railscasts-theme',
			\ 	'nanotech/jellybeans.vim',
			\ 	'tomasr/molokai',
			\ 	'vim-scripts/proton',
			\ 	'vim-scripts/pyte',
			\ 	'vim-scripts/rdark',
			\ 	'vim-scripts/twilight',
			\ 	'vim-scripts/Wombat',
			\ 	'w0ng/vim-hybrid',
			\ ]
let s:other_bundles = [
			\ ]
" }}}


" Utilities {{{
" Environment
function! VimrcEnvironment()
	let env = {}
	let env.is_win = has('win32') || has('win64')
	let env.path_separator = env.is_win ? '\' : '/'

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

" Paths
function! s:join_path(comps)
	return join(a:comps, s:env.path_separator)
endfunction

function! VimrcDirectories()
	let runtime_path = s:env.is_win 
				\ ? s:join_path([$VIM, 'vimfiles'])
				\ : expand(s:join_path(['~', '.vim']))
	return {
				\ 	'runtime': runtime_path,
				\ 	'neobundle': s:join_path([runtime_path, 'neobundle.vim']),
				\ 	'bundle': s:join_path([runtime_path, 'bundle']),
				\ 	'neosnippet': s:join_path([runtime_path, '.neosnippet']),
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
	let exists_bundle = (glob(s:join_path([s:dirs.bundle, '*'])) != '')

	set nocompatible
	filetype off
	if has('vim_starting')
		execute 'set runtimepath+=' . s:dirs.neobundle
		call neobundle#rc(s:dirs.bundle)
	endif

	for bundle in s:github_bundles
		execute printf("NeoBundle 'https://github.com/%s'", bundle)
	endfor

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
" Disable Ctrl+@
imap <C-@> <Nop>

" Turn off the IME when escaping Insert mode
set noimdisable
set iminsert=0 imsearch=-1
set noimcmdline
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
inoremap <silent> <C-[> <C-[>:set iminsert=0<CR>

" Intuitive cursor movement when `:set wrap`
noremap j gj
noremap gj j
noremap k gk
noremap gk k

" Switch an active window
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

" For US Keyboard
noremap ; :
" }}}


" Indent {{{
set tabstop=4
set shiftwidth=4
set list
set listchars=eol:¬,tab:▸\ 
set autoindent
set nosmartindent
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
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e

" matchit
runtime macros/matchit.vim
let b:match_words="\<if\>:\<end\>,\<do\>:\<end\>,\<def\>:\<end\>"

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
nnoremap <silent> ,ub :<C-u>Unite bookmark -default-action=open<CR>
nnoremap <silent> ,uf :<C-u>Unite file<CR>
nnoremap <silent> ,um :<C-u>Unite file_mru<CR>
nnoremap <silent> ,uo :<C-u>Unite outline<CR>
nnoremap <silent> ,ut :<C-u>Unite todo/all<CR>

" Vimfiler
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0

" Syntastic
let g:syntastic_mode_map = {
			\ 	'mode': 'passive',
			\ 	'active_filetypes': ['javascript'],
			\ 	'passive_filetypes': [],
			\ }
let g:syntastic_auto_loc_list = 0
let g:syntastic_javascript_checkers = ['jslint']
" --browser ... Tolerate standard browser globals
" --es5 ....... Tolerate ECMA Script 5 syntax (e.g. trailling commas)
" --nomen ..... Tolerate initial or trailing underbars in names
" --sloppy .... Tolerate missing 'use strict' pragma
" --vars ...... Tolerate many var statements per function
" --white ..... Tolerate messy white space
let g:syntastic_javascript_jslint_args = '--browser --es5=false --nomen --sloppy --vars --white'

" EasyMotion
let g:EasyMotion_keys = 'hfklasdfgyuiopqwertnmzxcvb'
let g:EasyMotion_leader_key = '<Space><Space>'

" Arpeggio
call arpeggio#load()
Arpeggio nnoremap ef :Unite file_mru<CR>
Arpeggio nnoremap ew :<C-u>e %:h<CR>
" }}}


" FileType {{{
autocmd BufRead,BufNewFile *.as setlocal filetype=javascript
autocmd FileType css        setlocal sw=4 ts=4
autocmd FileType eruby      setlocal sw=2 ts=2
autocmd FileType html       setlocal sw=2 ts=2 indentexpr&
autocmd FileType javascript setlocal sw=4 ts=4
autocmd FileType php        setlocal sw=4 ts=4
autocmd FileType python     setlocal sw=4 ts=4 expandtab
autocmd FileType ruby       setlocal sw=2 ts=2
autocmd FileType scss       setlocal sw=4 ts=4
autocmd FileType text       setlocal tw=0
autocmd FileType vim        setlocal sw=2 ts=2
autocmd FileType xhtml      setlocal indentexpr&
autocmd FileType xml        setlocal sw=2 ts=2
" }}}


" Misc {{{
set completeopt=menu,menuone,preview
set display=lastline
set grepprg=grep\ -nH
set nobackup
set noswapfile
set nrformats-=octal
set number
set scrolloff=5
set shortmess=aTI
set sidescroll=1
if has('virtualedit')
	set virtualedit=block
endif

" Disable archive plugins
let loaded_gzip = 1
let loaded_zip = 1
let loaded_zipPlugin = 1
" }}}


" Local Settings {{{
let s:local_vimrc = s:join_path([s:dirs.runtime, '.vimrc_local'])
if filereadable(s:local_vimrc)
	execute 'source ' . s:local_vimrc
endif
" }}}


" vim: set fdm=marker:
