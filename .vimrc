"" .vimrc

""" タブ幅の設定
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

""" 自動インデント
set autoindent

""" 入力時の動き
" 対応する括弧に一次的にカーソル移動
set showmatch

""" 選択文字列をクリップボードに入れる
"set clipboard=unnamed

""" BackSpaceで削除出来るものを指定
set backspace=start,eol,indent

" 括弧のマッチング表示を停止
"let loaded_matchparen = 1

""" 行番号を表示
set number

""" 検索
"set smartcase
set ic
set incsearch
set nowrapscan

" 検索結果のハイライトを停止
set nohlsearch

" 検索結果の文字色を白に変更
highlight Search ctermfg=white

" swapファイルの場所
set directory=$HOME/.vim_swp

""" Filetype
filetype plugin indent on
"autocmd FileType html,xml,xsl, source ~/.vim/bundle/closetag.vim

""" 改行時のコメント自動挿入をオフ
autocmd Filetype * set formatoptions-=ro

""" ハイライト
syntax on

""" バックアップファイル
"set backupdir=$HOME/vim_backup

""" Statusbar caption
set statusline=%n:\ %<%f\ %m%r%h%w[%{&fileformat}][%{has('multi_byte')&&\ &fileencoding!=''?&fileencoding:&encoding}]\ 0x%B=%b%=%l,%c\ %P
set laststatus=2
highlight statusline term=NONE cterm=NONE ctermfg=black ctermbg=white

"" Tabs
nnoremap <silent> gn :<C-u>tabnext<CR>
nnoremap <silent> gp :<C-u>tabprevious<CR>
