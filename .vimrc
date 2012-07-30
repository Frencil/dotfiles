set nocompatible

" <VUNDLE> ... The Vim plugin bundler

    " Check that Vundle is cloned on this machine. If it isn't, clone it.
    " Credit: Erik Zaadi http://www.erikzaadi.com/2012/03/19/auto-installing-vundle-from-your-vimrc/
    let iCanHazVundle=1
    let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
    if !filereadable(vundle_readme)
        echo "Installing Vundle.."
        echo ""
        silent !mkdir -p ~/.vim/bundle
        silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
        let iCanHazVundle=0
    endif

    " Initialize Vundle
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()

    " ALL THE BUNDLES...
    " Required - let vundle manage vundle
    Bundle 'gmarik/vundle'

    " File tree
    Bundle 'scrooloose/nerdtree'

    " align text vertically on a string
    Bundle 'Align'

    " wrap common version control commands
    Bundle 'vcscommand.vim'
    Bundle 'tpope/vim-fugitive'

    " a bunch of colorschemes + a gui menu listing them
    Bundle 'flazz/vim-colorschemes'
    Bundle 'altercation/vim-colors-solarized'
    Bundle 'ColorSchemeMenuMaker'
    Bundle 'desert-warm-256'

    " match lots of things
    Bundle 'edsono/vim-matchit'

    Bundle 'L9'
    Bundle 'FuzzyFinder'
    Bundle 'ervandew/supertab'
    Bundle 'Syntastic'

    "...All the other bundles...
    if iCanHazVundle == 0
        echo "Installing Bundles, please ignore key map error messages"
        echo ""
        :BundleInstall
    endif
" </VUNDLE>

set title

syntax on
filetype plugin on
filetype indent on

" do not beep or flash at me
" vb is needed to stop beep
" t_vb sets visual bell action, we're nulling it out here)
set visualbell
set t_vb=

" enable mouse for (a)ll, (n)ormal, (v)isual, (i)nsert, or (c)ommand line
" mode -- seems to work in most terminals
set mouse=a

" let me delete stuff like crazy in insert mode
set backspace=indent,eol,start

" display commands as-typed + current position in file
set showcmd
set ruler

" add git status to statusline; otherwise emulate standard line with ruler
set statusline=%<%{fugitive#statusline()}\ %f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

" keep lots of command-line history
set history=3500

" search 
set incsearch
set ignorecase
set smartcase

" display tab characters as 8 spaces, indent 2 spaces,
" always use spaces instead of tabs
set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
"set smarttab
"set smartindent

" for gvim. no toolbar, otherwise these are the defaults:
set guioptions=aegimrLt

" Use + register (X Window clipboard) as unnamed register
" set clipboard=unnamedplus,autoselect

" turn off tab expansion for Makefiles
" au BufEnter ?akefile* set noexpandtab
" au BufLeave ?akefile* set expandtab
au FileType make setlocal noexpandtab

" -- TWEAKME --

" use comma for the leader key
let mapleader = ","

" treat p1k3 entries as HTML. lazy, but unlikely to hit many false positives:
au BufReadPost,BufNewFile */p1k3/*[0123456789]* call PikeHighlight()

" the ! means this can be freely redeclared - makes it easier to source
" ~/.vimrc after changes
fun! PikeHighlight()
  " make sure NERDTree windows don't get messed up
  if bufname("%") =~ "NERD_tree"
    return
  endif

  " the initial slash seems to be necessary to make \v work
  if bufname("%") =~ "\\v([0-9]{1,2}|[a-z]+)$"
    set filetype=html
  endif
endfun

" assume *.t files are perl - not actually much use for this
" au BufRead,BufNewFile *.t set filetype=perl

" <keybindings>

  " split lines under the cursor
  map K i<CR><Esc>g;

  " get a datestamp for a p1k3 entry
  " .-1 puts it on the current line, since :r reads onto the line below the
  " current one (or below the specified line - so here we're specifying the
  " one before the current one)
  nmap <leader>td :.-1r !today<CR><CR>

  " reformat a paragraph
  nmap <leader>q gqip

  " write all changed buffers
  nmap <leader>w :wa<CR>

  " pull up the last hundred git commits in a new window
  " TODO: put this in a function
  nmap <leader>l :vnew<CR>:r !git log -100<CR>:set ft=git<CR>gg<C-w>r<C-w>l

  map <F2> :NERDTreeToggle<CR>
  map <F3> :NERDTreeFind<CR>
  map <F4> :set invnumber<CR>
  imap <F4> <ESC>:set invnumber<CR>a
  " toggle search highlighting:
  map <F9> :set invhlsearch<CR>

  " tab navigation somewhat like firefox
  " http://vim.wikia.com/wiki/Alternative_tab_navigation
  nmap <C-S-Tab> :tabprevious<CR>
  nmap <C-Tab> :tabnext<CR>
  map <C-S-Tab> :tabprevious<CR>
  map <C-Tab> :tabnext<CR>
  imap <C-S-Tab> <Esc>:tabprevious<CR>i
  imap <C-Tab> <Esc>:tabnext<CR>i       
  " new tab:
  nmap <leader>tn :tabnew<CR>

  " fuzzyfinder
  nmap <leader>f :FufFile<CR>

  " display tabs - ,s will toggle (redraws just in case)
  nmap <silent> <leader>s :set nolist!<CR>:redr<CR>
  set listchars=tab:⇾\ ,trail:·
  set list

" </keybindings>

" retain view/folds
au BufWinLeave notes mkview
au BufWinEnter notes silent loadview

" read (unchanged) buffers when they're modified on filesystem
set autoread

