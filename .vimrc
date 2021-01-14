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
	let env.is_mac = has('mac')
	let env.is_win = has('win32')

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

let s:env = VimrcEnvironment()
" }}}


" Required plugins {{{
let s:plugins = [
			\ 	'AndrewRadev/linediff.vim',
			\ 	'Shougo/vimproc',
			\ 	'Vimjas/vim-python-pep8-indent',
			\ 	'andymass/vim-matchup',
			\ 	'cespare/vim-toml',
			\ 	'chaoren/vim-wordmotion',
			\ 	'cocopon/colorswatch.vim',
			\ 	'cocopon/iceberg.vim',
			\ 	'cocopon/inspecthi.vim',
			\ 	'cocopon/ntdcoco.vim',
			\ 	'cocopon/pgmnt.vim',
			\ 	'cocopon/shadeline.vim',
			\ 	'cocopon/snapbuffer.vim',
			\ 	'cocopon/vaffle.vim',
			\ 	'ctrlpvim/ctrlp.vim',
			\ 	'delphinus/vim-auto-cursorline',
			\ 	'editorconfig/editorconfig-vim',
			\ 	'flowtype/vim-flow',
			\ 	'iberianpig/tig-explorer.vim',
			\ 	'itchyny/thumbnail.vim',
			\ 	'jparise/vim-graphql',
			\ 	'junegunn/vim-easy-align',
			\ 	'kana/vim-textobj-indent',
			\ 	'kana/vim-textobj-user',
			\ 	'lambdalisue/gina.vim',
			\ 	'leafgarland/typescript-vim',
			\ 	'mattn/emmet-vim',
			\ 	'mattn/sonictemplate-vim',
			\ 	'mattn/vim-lsp-settings',
			\ 	'morhetz/gruvbox',
			\ 	'mxw/vim-jsx',
			\ 	'neovimhaskell/haskell-vim',
			\ 	'pangloss/vim-javascript',
			\ 	'prabirshrestha/asyncomplete-buffer.vim',
			\ 	'prabirshrestha/asyncomplete-file.vim',
			\ 	'prabirshrestha/asyncomplete-lsp.vim',
			\ 	'prabirshrestha/asyncomplete.vim',
			\ 	'prabirshrestha/vim-lsp',
			\ 	'prettier/vim-prettier',
			\ 	'previm/previm',
			\ 	'rking/ag.vim',
			\ 	'sophacles/vim-processing',
			\ 	'stephpy/vim-yaml',
			\ 	'thinca/vim-qfreplace',
			\ 	'thinca/vim-quickrun',
			\ 	'thinca/vim-ref',
			\ 	'thinca/vim-themis',
			\ 	'thinca/vim-zenspace',
			\ 	'tikhomirov/vim-glsl',
			\ 	'tomtom/tcomment_vim',
			\ 	'tpope/vim-markdown',
			\ 	'tpope/vim-surround',
			\ 	'tweekmonster/helpful.vim',
			\ 	'tyru/open-browser.vim',
			\ 	'w0ng/vim-hybrid',
			\ 	'w0rp/ale',
			\ ]
let s:colorscheme = 'iceberg'
" }}}


" Setup {{{
function! VimrcSetUp()
	call s:install_plugin_manager()
endfunction
" }}}


" Installation {{{
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


" Activation {{{
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
		" Plugin manager is not installed yet
		return 0
	endif

	let command = exists(':PresetPlug')
				\ ? 'PresetPlug'
				\ : 'Plug'

	for plugin in s:plugins
		execute printf("%s '%s'", command, plugin)
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
		" Plugin manager is not installed yet
		return 0
	endtry
endfunction
" }}}


" Initialization {{{
call s:mkdir_if_needed(s:env.path.tmp)
call s:mkdir_if_needed(s:env.path.undo)
let s:plugins_activated = s:activate_plugin_manager()
" }}}


" Mappings {{{
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

" Refer to history in command-line mode
cnoremap <C-p> <Up>
cnoremap <Up> <C-p>
cnoremap <C-n> <Down>
cnoremap <Down> <C-n>

" Open the parent directory, or the current directory if empty
nnoremap <silent> <C-u> :<C-u>call vaffle#init(expand('%'))<CR>

" Insert escaped '/' while inputting a search pattern
cnoremap <expr> / getcmdtype() == '/' ? '\/' : '/'

" For JIS keyboard
inoremap <C-@> <Nop>

" For US keyboard
noremap ; :

" Clear hlsearch
nnoremap <silent> <Esc><Esc> :<C-u>set nopaste<CR>:nohlsearch<CR>:diffoff!<CR>

" Misc
nnoremap Y y$
nnoremap K <Nop>
" }}}


" File types {{{
augroup vimrc_filetype
	autocmd!
	autocmd BufNewFile,BufRead *.as      setlocal filetype=javascript
	autocmd BufNewFile,BufRead *.gradle  setlocal filetype=groovy
	autocmd BufNewFile,BufRead *.pde     setlocal filetype=processing
	autocmd BufNewFile,BufRead *.podspec setlocal filetype=ruby
	autocmd BufNewFile,BufRead Podfile   setlocal filetype=ruby
	autocmd FileType gitcommit setlocal nocursorline spell spelllang=cjk,en
	autocmd FileType text      setlocal textwidth=0
	autocmd FileType vim       setlocal foldmethod=marker
augroup END
" }}}


" Terminal {{{
tnoremap <ESC><ESC> <C-\><C-n>

if exists('&termwinkey')
	set termwinkey=<C-_>
endif
" }}}


" Misc {{{
set completeopt=menu,menuone,noinsert,noselect
set display=lastline
set grepprg=grep\ -nH
set mouse=a
set nrformats-=octal
set shortmess=acTI
set splitright
set virtualedit=block
set wildmenu

" Appearance
set ambiwidth=double
set background=dark
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
let &undodir = s:env.path.undo
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
	" ag {{{
	let g:ag_prg = 'ag --vimgrep --smart-case'
	" }}}

	" ale {{{
	let g:ale_linters = {
				\ 	'javascript': ['eslint', 'flow'],
				\ 	'typescript': ['tsserver'],
				\ }
	let g:ale_open_list = 1
	let g:ale_set_loclist = 0
	let g:ale_set_quickfix = 0
	let g:ale_virtualtext_cursor = 1
	" }}}

	" asyncomplete.vim {{{
	call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
				\		'name': 'buffer',
				\ 	'allowlist': ['*'],
				\ 	'blocklist': ['go'],
				\ 	'completor': function('asyncomplete#sources#buffer#completor'),
				\ }))

	if !has('nvim')
		autocmd User lsp_float_opened
					\ call popup_setoptions(lsp#ui#vim#output#getpreviewwinid(), {
					\ 	'border': [0, 0, 0, 0],
					\ 	'padding': [0, 1, 0, 1],
					\ })
	end
	" }}}

	" ctrlp {{{
	let g:ctrlp_cache_dir = s:env.path.data . '/ctrlp'
	let g:ctrlp_clear_cache_on_exit = 0
	let g:ctrlp_custom_ignore = {
				\ 	'dir': '\v[\/](node_modules|\.git)$',
				\ }
	let g:ctrlp_map = ''
	let g:ctrlp_match_window = 'bottom,order:ttb'
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
	let g:ctrlp_tilde_homedir = 1

	nnoremap <C-g> :CtrlPMixed<CR>
	" }}}

	" flow {{
	let g:flow#enable = 0
	let g:flow#omnifunc = 0
	" }}

	" gina {{{
	let g:gina#command#blame#use_default_mappings = 0
	" }}}

	" go {{{
	let g:go_version_warning = 0
	" }}}

	" javascript {{{
	let g:javascript_plugin_flow = 1
	" }}}

	" linediff {{{
	let g:linediff_first_buffer_command = 'new'
	let g:linediff_further_buffer_command = 'vertical new'
	" }}}

	" matchup {{{
	let g:matchup_matchparen_offscreen = {
				\ 	'method': 'popup',
				\ }
	" }}}

	" ntdcoco {{{
	nmap <Bar> <Plug>(ntdcoco-toggle-cursor)
	" }}}

	" open-browser {{{
	nmap gW <Plug>(openbrowser-open)
	" }}}

	" pgmnt {{{
	let g:pgmnt_auto_source = 1
	" }}}

	" prettier
	" {{{
	let g:prettier#exec_cmd_async = 1
	let g:prettier#quickfix_enabled = 0
	" }}}

	" quickrun {{{
	let g:quickrun_config = {}
	let g:quickrun_config['_'] = {
				\ 	'runner': 'vimproc',
				\ 	'runner/vimproc/updatetime': 40,
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
		try
			let name = gina#component#repo#branch()
			return empty(name) ? '' : printf('(%s)', name)
		catch /:E117:/
			" E117: Unknown function
			return ''
		endtry
	endfunction
	" }}}

	" sonictemplate {{{
	let g:sonictemplate_vim_template_dir = s:env.path.plugins . '/sonictemplate-templates/template'
	" }}}

	" vaffle {{{
	let g:vaffle_auto_cd = 1
	" }}}
endif

" Disable unused plugins
let g:loaded_matchit = 1
let g:loaded_gzip = 1
let g:loaded_tar = 1
let g:loaded_tarPlugin = 1
let g:loaded_zip = 1
let g:loaded_zipPlugin = 1

" Disable unused kaoriya plugins
let g:plugin_dicwin_disable = 1
" }}}


" Local settings {{{
if filereadable(s:env.path.local_vimrc)
	execute 'source ' . s:env.path.local_vimrc
endif
" }}}


" Color scheme {{{
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

	if $COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit'
		set termguicolors
	endif
endif
" }}}
