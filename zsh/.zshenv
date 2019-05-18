# =======
# zshenv
# =======

# -------
# Env
# -------

export EDITOR=$(which vi)

# -------
# PATH
# -------

# 環境固有のパス
path=(
    # $HOME/foo/bar/bin
    $path
)

# -------
# Alias
# -------

# -------
# Startup command
# -------

# -------
# Function
# -------

# --------------------------------------------------

# -------
# MacでUbuntu のVMを動かす際に、 xkb で Cmd -> Meta, Option -> Super にする
# -------
# ↓ Comment out
# xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mykbd $DISPLAY 2> /dev/null
#
# ↓ ~/.xkb/keymap/mykbd ファイルに書く
#xkb_keymap {
#    xkb_keycodes  { include "evdev+aliases(qwerty)"	};
#    xkb_types     { include "complete"	};
#    xkb_compat    { include "complete"	};
#    xkb_symbols   { include "pc+us+inet(evdev)+myswps(swapkeys)"	};
#    xkb_geometry  { include "pc(pc105)"	};
#};
#
# ↓ ~/.xkb/symbols/myswps ファイルに書く
#partial alphanumeric_keys
#xkb_symbols "swapkeys" {
#    key <LWIN> {[ Alt_L, Meta_L ]};
#    key <LALT> {[ Super_L ]};
#};
