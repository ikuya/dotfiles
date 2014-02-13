## .zshenv

# PATH
# path はPATHの内容と同期している配列変数。
# 末尾に(N-/)をつけると、存在しないパスの場合に空文字に置換される。
# echo ${(F)path} とやると配列要素を改行で連結するので見やすくなる
path=(
    $HOME/bin(N-/)
#    /usr/local/bin
#    /usr/bin
#    /bin
#    /usr/sbin
#    /sbin
    $path
)
