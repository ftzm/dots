"------------------------------------------------------------
"   Plugin Initialization
"------------------------------------------------------------

call plug#begin('~/.local/share/nvim/plugged')
"--------------------------------------------------
"Session Managements
Plug 'tpope/vim-obsession'
"---File and Version Management--------------------
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdTree'
"Plug 'ctrlpvim/ctrlp.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
"---Language & Syntax------------------------------
Plug 'benekastah/neomake'
Plug 'Shougo/vimproc.vim'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
Plug 'bitc/vim-hdevtools'
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
"--Appearance--------------------------------------
Plug 'chriskempson/base16-vim'
Plug 'altercation/vim-colors-solarized'
Plug 'NLKNguyen/papercolor-theme'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'reedes/vim-pencil'
Plug 'morhetz/gruvbox'
"---Tmux-------------------------------------------
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
"---Mewvment--------------------------------------
Plug 'justinmk/vim-sneak'
"Plug 'easymotion/vim-easymotion'
Plug 'chaoren/vim-wordmotion' "CamelCase word objects
"---Text Manipulation------------------------------
Plug 'Valloric/YouCompleteMe'
Plug 'wellle/targets.vim'
Plug 'tpope/vim-surround'
call plug#end()

"------------------------------------------------------------
"   General Settings
"------------------------------------------------------------

set hidden " allow buffers to be hidden
set clipboard+=unnamedplus " yank and paste uses system clipboard
set cursorline " highlight the current line
set encoding=utf-8
set fenc=utf-8
set ignorecase smartcase "c matches C, not vice versa. sneak also.
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set incsearch
set nohlsearch " don't leave search matches highlighted
filetype indent plugin on " filetype appropriate indenting
syntax on " enable syntax highlighting

autocmd Filetype htmldjango setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2

"------------------------------------------------------------
"   Colorscheme
"------------------------------------------------------------

" vim sneak coloring that needs to be before colorscheme setting
augroup SneakPluginColors
   autocmd!
   autocmd ColorScheme * hi SneakPluginTarget guifg=white guibg=orange ctermfg=white ctermbg=160
   autocmd ColorScheme * hi SneakStreakTarget  guifg=white guibg=orange ctermfg=white ctermbg=160
   autocmd ColorScheme * hi SneakStreakMask  guifg=black guibg=orange ctermfg=160 ctermbg=160
augroup END

colorscheme gruvbox " set colorscheme
set background=dark " set background to dark

"------------------------------------------------------------
"   UI Stuff
"------------------------------------------------------------

" remove signcolmn bg
hi! SignColumn ctermbg=None
" make tildes blank
hi! EndOfBuffer ctermbg=bg ctermfg=bg guibg=bg guifg=bg
" personal solarized adjustments
" vertical split is a thin bg highlight color
hi! VertSplit ctermbg=bg ctermfg=0 guibg=bg
" little status line bit between airlines matches split color
hi! StatusLine ctermbg=4 ctermfg=0 guibg=bg guifg=None
hi! StatusLineNC ctermbg=bg ctermfg=0 guibg=bg guifg=None

"------------------------------------------------------------
"   Custom Keybindings
"------------------------------------------------------------

"navigate tabs if >1 tab, else navigate buffers.
function! NextTabOrBuffer()
    if tabpagenr('$') > 1
        tabnext
    else
        bnext
    endif
endfunction
function! PrevTabOrBuffer()
    if tabpagenr('$') > 1
        tabprevious
    else
        bprevious
    endif
endfunction

" buffer movement keys
map <silent>K :call PrevTabOrBuffer()<CR>
map <silent>J :call NextTabOrBuffer()<CR>

" putting in visual mode like ought to be the damn default
vmap P I<C-R>"<esc>
vmap p A<C-R>"<esc>

" leader mapping
let mapleader = " "

" easier split navigation
" nnoremap <C-J> <C-W><C-J>
" nnoremap <C-K> <C-W><C-K>
" nnoremap <C-L> <C-W><C-L>
" nnoremap <C-H> <C-W><C-H>

" resize current buffer by +/- 5
nnoremap <D-left> :vertical resize -5<cr>
nnoremap <D-down> :resize +5<cr>
nnoremap <D-up> :resize -5<cr>
nnoremap <D-right> :vertical resize +5<cr>

"toggle nerdtree
map <Leader>n :NERDTreeToggle<cr>

"function to move to previous error as defined in locations
function! <SID>LocationPrevious()
  try
    lprev
  catch /^Vim\%((\a\+)\)\=:E553/
    llast
  catch /^Vim\%((\a\+)\)\=:E42/
    echo "Location list empty"
  catch /^Vim\%((\a\+)\)\=:E776/
    echo "Write first to check for errors"
  catch /^Vim\%((\a\+)\)\=:E37/
    echo "Write your changes first"
  endtry
endfunction

"function to move to next error as defined in locations
function! <SID>LocationNext()
  try
    lnext
  catch /^Vim\%((\a\+)\)\=:E553/
    lfirst
  catch /^Vim\%((\a\+)\)\=:E42/
    echo "Location list empty"
  catch /^Vim\%((\a\+)\)\=:E776/
    echo "Write first to check for errors"
  catch /^Vim\%((\a\+)\)\=:E37/
    echo "Write your changes first"
  endtry
endfunction

"keybindings for error jumping functions
nnoremap <silent> <Plug>LocationPrevious    :<C-u>exe 'call <SID>LocationPrevious()'<CR>
nnoremap <silent> <Plug>LocationNext        :<C-u>exe 'call <SID>LocationNext()'<CR>
map <C-p>    <Plug>LocationPrevious
map <C-n>    <Plug>LocationNext

"write remap
map <Leader>w   :w<cr>

"remove trailing whitespaces before all writes
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

" Hack (or trick) to save a file using sudo after it's been opened
cmap w!! w ! sudo tee > /dev/null %


"tab movement keys
nnoremap tk :tabprevious<CR>
nnoremap tK :tabm -1<CR>
nnoremap tj :tabnext<CR>
nnoremap tJ :tabm +1<CR>
nnoremap th :tabfirst<CR>
nnoremap tl :tablast<CR>
nnoremap td :tabclose<CR>

" move to front or end of line
map H ^
map L $

"close buffer
map <Leader>c :bd<CR>

"------------------------------------------------------------
"   Language Specific Setting
"------------------------------------------------------------

"---Haskell----------------------------------------
autocmd Filetype haskell setlocal tabstop=2 shiftwidth=2 softtabstop=2

"------------------------------------------------------------
"   Airline
"------------------------------------------------------------

" airline stuff
let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
"let g:airline_left_sep = '⮀'
"let g:airline_left_alt_sep = '⮁'
"let g:airline_right_sep = '⮂'
"let g:airline_right_alt_sep = '⮃'
let g:airline_symbols = {}
"let g:airline_symbols.branch = '⭠'
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = 'R'
let g:airline_symbols.linenr = 'LN'
let g:airline_theme = 'base16'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#tab_min_count = 2
"let g:airline#extensions#tabline#buffer_min_count = 2

let g:airline_section_z = airline#section#create(['%{ObsessionStatus(''$'', '''')}', 'windowswap', '%3p%% ', 'linenr', ':%3v '])

"don't know what this is, but disabling gets rid of the yellow chunk right of
"the status line when a file has been modified
let g:airline_detect_modified=0

" always show airline
set laststatus=2


"------------------------------------------------------------
"   Obsession
"------------------------------------------------------------
map <Leader>o :Obsession<CR>

"------------------------------------------------------------
"   YouCompleteMe
"------------------------------------------------------------

" automatically confirm use of ycm conf
let g:ycm_confirm_extra_conf = 0

" make ycm use location list for errors
let g:ycm_always_populate_location_list = 1
"
" define triggers for semantic completion per-filetype
let g:ycm_semantic_triggers = {'haskell' : ['.']}

"automatically close the complete buffer split thing
autocmd CompleteDone * pclose

let g:ycm_python_binary_path = '/usr/bin/python3'

nnoremap <leader>d :split <bar> YcmCompleter GoToDefinition<CR>

"------------------------------------------------------------
"   neco-ghc
"------------------------------------------------------------
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

"------------------------------------------------------------
"   ghc-mod
"------------------------------------------------------------

map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>
map <silent> te :GhcMod<CR>

"------------------------------------------------------------
"   neomake
"------------------------------------------------------------

" run neomake on every write
autocmd! BufWritePost * Neomake

"let g:neomake_haskell_hdevtools_exe = '/home/matt/bin/hdevtools-stack'
"let g:neomake_haskell_ghcmod_exe = '/home/matt/bin/ghc-mod-stack'

"must be run in the stack project root directory
let g:neomake_haskell_enabled_makers = ['ghcmod']

let g:neomake_warning_sign = {
  \ 'text': 'W',
  \ 'texthl': 'WarningMsg',
  \ }
let g:neomake_error_sign = {
  \ 'text': 'E',
  \ 'texthl': 'ErrorMsg',
  \ }

let g:neomake_python_enabled_makers = ['pylint']

"------------------------------------------------------------
"   sneak
"------------------------------------------------------------

" easymotion style
let g:sneak#streak = 1
nmap <Leader>s <Plug>Sneak_S
let g:sneak#use_ic_scs = 1

"------------------------------------------------------------
"   EasyMotion
"------------------------------------------------------------
"let g:EasyMotion_do_mapping = 0 " Disable default mappings
"
"" Jump to anywhere you want with minimal keystrokes, with just one key binding.
"" `s{char}{label}`
"nmap s <Plug>(easymotion-overwin-f)
"" or
"" `s{char}{char}{label}`
"" Need one more keystroke, but on average, it may be more comfortable.
"nmap s <Plug>(easymotion-overwin-f2)
"
"" Turn on case insensitive feature
"let g:EasyMotion_smartcase = 1
"
"" JK motions: Line motions
"map <Leader>j <Plug>(easymotion-j)
"map <Leader>k <Plug>(easymotion-k)

"------------------------------------------------------------
"   tmuxsplitnavigator
"------------------------------------------------------------

" <c-h> is interpreted as <bs> in neovim
 nnoremap <silent> <bs> :TmuxNavigateLeft<cr>

"------------------------------------------------------------
"   Vimux Keys
"------------------------------------------------------------
 "
nnoremap <leader>tt <Esc>:call VimuxRunCommandInDir("/home/matt/bin/vim/testnear", 0)<CR>
nnoremap <leader>tc <Esc>:call VimuxCloseRunner()<CR>

"------------------------------------------------------------
"   CtrlP
"------------------------------------------------------------

" let g:ctrlp_show_hidden = 1

"------------------------------------------------------------
"   targets
"------------------------------------------------------------

let g:targets_quotes = '"d ''q `'

"------------------------------------------------------------
"   fzf
"------------------------------------------------------------
"let g:fzf_nvim_statusline = 0 "don't overwrite statusline

nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :Files<cr>
nnoremap <silent> <leader>A :execute 'Ag ' . input('Ag/')<CR>
nnoremap <silent> <leader>/ :BLines<CR>
nnoremap <silent> <leader>K :call SearchWordWithAg()<CR>
vnoremap <silent> <leader>K :call SearchVisualSelectionWithAg()<CR>

function! SearchWordWithAg()
  execute 'Ag' expand('<cword>')
endfunction

function! SearchVisualSelectionWithAg() range
  let old_reg = getreg('"')
  let old_regtype = getregtype('"')
  let old_clipboard = &clipboard
  set clipboard&
  normal! ""gvy
  let selection = getreg('"')
  call setreg('"', old_reg, old_regtype)
  let &clipboard = old_clipboard
  execute 'Ag' selection
endfunction

function! s:fzf_statusline()
  " Override statusline as you like
  setlocal statusline=fzf
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()
