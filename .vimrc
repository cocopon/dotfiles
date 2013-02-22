" cocopon's vimrc
"
" This vimrc will setup automatically. yay!


" Required Bundles {{{
let s:github_bundles = [
			\ 	'h1mesuke/unite-outline',
			\ 	'kana/vim-arpeggio',
			\ 	'Lokaltog/vim-easymotion',
			\ 	'mattn/calendar-vim',
			\ 	'mattn/zencoding-vim',
			\ 	'pangloss/vim-javascript',
			\ 	'scrooloose/syntastic',
			\ 	'Shougo/neocomplcache',
			\ 	'Shougo/neosnippet',
			\ 	'Shougo/unite.vim',
			\ 	'Shougo/vimfiler',
			\ 	'Shougo/vimproc',
			\ 	'Shougo/vimshell',
			\ 	'Shougo/vinarise',
			\ 	't9md/vim-textmanip',
			\ 	'thinca/vim-editvar',
			\ 	'thinca/vim-qfreplace',
			\ 	'thinca/vim-quickrun',
			\ 	'thinca/vim-ref',
			\ 	'tomtom/tcomment_vim',
			\ 	'tpope/vim-surround',
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
	let is_win = has('win32') || has('win64')
	let path_separator = is_win ? '\' : '/'

	return {
				\ 	'is_win': is_win,
				\ 	'path_separator': path_separator,
				\ }
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
	let exists_bundle = isdirectory(s:dirs.bundle)
	if !exists_bundle
		call mkdir(s:dirs.bundle)
	endif

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
imap <C-a> <Home>
imap <C-b> <Left>
imap <C-d> <Del>
imap <C-e> <End>
imap <C-f> <Right>
imap <C-h> <Backspace>
imap <C-k> <C-o>D
imap <C-n> <Down>
imap <C-p> <Up>
" Refer history in Command-line mode
cnoremap <C-p> <Up>
cnoremap <Up> <C-p>
cnoremap <C-n> <Down>
cnoremap <Down> <C-n>
" }}}


" Indent {{{
set tabstop=4
set shiftwidth=4
set list
set listchars=eol:¬,tab:▸\ 
set autoindent
set nosmartindent
autocmd FileType html set indentexpr&
autocmd FileType js set indentexpr&
autocmd FileType xhtml set indentexpr&
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
nnoremap <silent> ,ub :<C-u>Unite -default-action=open bookmark<CR>
nnoremap <silent> ,uf :<C-u>Unite file<CR>
nnoremap <silent> ,um :<C-u>Unite -default-action=open file_mru<CR>
nnoremap <silent> ,uo :<C-u>Unite outline<CR>
nnoremap <silent> ,ut :<C-u>Unite -default-action=open todo/all<CR>

" Vimfiler
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0

" Syntastic
let g:syntastic_mode_map = {
			\ 	'mode': 'passive',
			\ 	'active_filetypes': ['javascript'],
			\ 	'passive_filetypes': [],
			\ }
let g:syntastic_javascript_checker = 'jslint'
let g:syntastic_javascript_jslint_conf = '--sloppy --white'

" EasyMotion
let g:EasyMotion_keys = 'hfklasdfgyuiopqwertnmzxcvb'
let g:EasyMotion_leader_key = '<Space><Space>'

" Arpeggio
call arpeggio#load()
Arpeggio nnoremap ew :<C-u>e %:h<CR>
" }}}


" FileType {{{
autocmd BufRead,BufNewFile *.as set filetype=javascript
autocmd FileType eruby setlocal sw=2 ts=2
autocmd FileType html  setlocal sw=2 ts=2
autocmd FileType text  setlocal tw=0
autocmd FileType ruby  setlocal sw=2 ts=2
autocmd FileType vim   setlocal sw=2 ts=2
" }}}


" Misc {{{
set completeopt=menu,menuone,preview
set display=lastline
set nobackup
set noswapfile
set nrformats-=octal
set number
if has('virtualedit')
	set virtualedit=block
endif
" }}}


" Local settings {{{
let s:local_vimrc = s:join_path([s:dirs.runtime, '.vimrc_local'])
if filereadable(s:local_vimrc)
	execute 'source ' . s:local_vimrc
endif
" }}}


" vim: set fdm=marker sw=2 ts=2:
