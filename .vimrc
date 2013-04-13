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

" ファイルの種類に応じた処理を停止
"filetype off

""" ハイライト
syntax on

""" バックアップファイル
"set backupdir=$HOME/vim_backup

""" Statusbar caption
set statusline=%n:\ %<%f\ %m%r%h%w[%{&fileformat}][%{has('multi_byte')&&\ &fileencoding!=''?&fileencoding:&encoding}]\ 0x%B=%b%=%l,%c\ %P
set laststatus=2
highlight statusline term=NONE cterm=NONE ctermfg=black ctermbg=white

""" 文字コードの自動認識
if &encoding !=# 'utf-8'
	set encoding=japan
	set fileencoding=japan
endif

if has('iconv')
		let s:enc_euc = 'euc-jp'
		let s:enc_jis = 'iso-2022-jp'
	" iconvがeucJP-msに対応しているかをチェック
	if iconv("\x87\x64\x87\x6a", 'cp932', 'eucjp-ms') ==# "\xad\xc5\xad\xcb"
		let s:enc_euc = 'eucjp-ms'
		let s:enc_jis = 'iso-2022-jp-3'
	" iconvがJISX0213に対応しているかをチェック
	elseif iconv("\x87\x64\x87\x6a", 'cp932', 'euc-jisx0213') ==# "\xad\xc5\xad\xcb"
		let s:enc_euc = 'euc-jisx0213'
		let s:enc_jis = 'iso-2022-jp-3'
	endif
	" fileencodingsを構築
	if &encoding ==# 'utf-8'
		let s:fileencodings_default = &fileencodings
		let &fileencodings = s:enc_jis .','. s:enc_euc .',cp932'
		let &fileencodings = &fileencodings .','. s:fileencodings_default
		unlet s:fileencodings_default
	else
		let &fileencodings = &fileencodings .','. s:enc_jis
		set fileencodings+=utf-8,ucs-2le,ucs-2
		if &encoding =~# '^\(euc-jp\|euc-jisx0213\|eucjp-ms\)$'
			set fileencodings+=cp932
			set fileencodings-=euc-jp
			set fileencodings-=euc-jisx0213
			set fileencodings-=eucjp-ms
			let &encoding = s:enc_euc
			let &fileencoding = s:enc_euc
		else
			let &fileencodings = &fileencodings .','.  s:enc_euc
		endif
	endif
	"定数を処分
	unlet s:enc_euc
	unlet s:enc_jis
endif
" 日本語を含まない場合は fileencoding に encoding を使うようにする
if has('autocmd')
	function! AU_ReCheck_FENC()
		if &fileencoding =~# 'iso-2022-jp' && search("[^\x01-\x7e]", 'n') == 0
			let &fileencoding=&encoding
		endif
	endfunction
	autocmd BufReadPost * call AU_ReCheck_FENC()
endif
" 改行コードの自動認識
set fileformats=unix,dos,mac
" □とか○の文字があってもカーソル位置がずれないようにする
"if exists('&ambiwidth')
"	set ambiwidth=double
"endif

let g:neocomplcache_enable_at_startup = 1

call pathogen#runtime_append_all_bundles()

"" vim-ref
let g:ref_alc_cmd = 'w3m -dump %s'

"" FuzzyFinder
nnoremap <unique> <silent> <space>fb :FufBuffer!<CR>
nnoremap <unique> <silent> <space>ff :FufFile!<CR>
nnoremap <unique> <silent> <space>fm :FufMruFile!<CR>
nnoremap <unique> <silent> <space>fc :FufRenewCache<CR>
nnoremap <unique> <silent> <space>fd :FufDir<CR>
autocmd FileType fuf nmap <C-c> <ESC>
let g:fuf_patternSeparator = ' '
let g:fuf_modesDisable = ['mrucmd']
let g:fuf_mrufile_exclude = '\v\.DS_Store|\.git|\.swp|\.svn'
let g:fuf_mrufile_maxItem = 100
let g:fuf_enumeratingLimit = 20
let g:fuf_file_exclude = '\v\.DS_Store|\.git|\.swp|\.svn'

"" Tabs
nnoremap <silent> gn :<C-u>tabnext<CR>
nnoremap <silent> gp :<C-u>tabprevious<CR>

"" Colorscheme
colorscheme rdark
