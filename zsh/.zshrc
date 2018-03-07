# ------------------------------
# General Settings
# ------------------------------

autoload -U edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

bindkey -v               # キーバインドをviモードに設定
bindkey '^B' vi-backward-delete-char
bindkey "^T" history-beginning-search-backward-end
bindkey "^H" history-beginning-search-forward-end
bindkey -M viins 'hh' vi-cmd-mode
bindkey "^[OC" vi-forward-char
bindkey "^[OD" vi-backward-char
bindkey "^[OF" vi-end-of-line
bindkey "^[OH" vi-beginning-of-line
bindkey "^G" send-break
bindkey "^N" forward-char
bindkey "^D" backward-char
zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

umask 002
setopt no_beep           # ビープ音を鳴らさないようにする
setopt auto_cd           # ディレクトリ名の入力のみで移動する 
setopt correct           # コマンドのスペルを訂正する
setopt magic_equal_subst # =以降も補完する(--prefix=/usrなど)
setopt prompt_subst      # プロンプト定義内で変数置換やコマンド置換を扱う
setopt notify            # バックグラウンドジョブの状態変化を即時報告する
setopt equals            # =commandを`which command`と同じ処理にする

### Complement ###
autoload -U compinit; compinit # 補完機能を有効にする
setopt auto_list               # 補完候補を一覧で表示する(d)
setopt auto_menu               # 補完キー連打で補完候補を順に表示する(d)
setopt list_packed             # 補完候補をできるだけ詰めて表示する
setopt list_types              # 補完候補にファイルの種類も表示する
bindkey "^[[Z" reverse-menu-complete  # Shift-Tabで補完候補を逆順する("\e[Z"でも動作する)
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時に大文字小文字を区別しない

### Glob ###
setopt extended_glob # グロブ機能を拡張する
unsetopt caseglob    # ファイルグロブで大文字小文字を区別しない

### History ###
HISTFILE=~/.zsh_history   # ヒストリを保存するファイル
HISTSIZE=10000            # メモリに保存されるヒストリの件数
SAVEHIST=10000            # 保存されるヒストリの件数
setopt bang_hist          # !を使ったヒストリ展開を行う(d)
setopt extended_history   # ヒストリに実行時間も保存する
setopt hist_ignore_dups   # 直前と同じコマンドはヒストリに追加しない
setopt share_history      # 他のシェルのヒストリをリアルタイムで共有する
setopt hist_reduce_blanks # 余分なスペースを削除してヒストリに保存する

# マッチしたコマンドのヒストリを表示できるようにする
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# インクリメンタルからの検索
function peco-history-selection() {
    BUFFER=$(history -n 1 | tac | awk '!a[$0]++' | peco)
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection

# ------------------------------
# Other Settings
# ------------------------------

### Ls Color ###
eval $(dircolors $HOME/.dir_colors)
# 補完候補に色を付ける
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

function send_emacs(){
    emacsclient $1 > /dev/null &
}

### Aliases ###
alias v=nvim
alias e=send_emacs
alias ls="ls --color"
alias grep='grep --color=auto'

function extract() {
  case $1 in
    *.tar.gz|*.tgz) tar xzvf $1;;
    *.tar.xz) tar Jxvf $1;;
    *.zip) unzip $1;;
    *.lzh) lha e $1;;
    *.tar.bz2|*.tbz) tar xjvf $1;;
    *.tar.Z) tar zxvf $1;;
    *.gz) gzip -d $1;;
    *.bz2) bzip2 -dc $1;;
    *.Z) uncompress $1;;
    *.tar) tar xvf $1;;
    *.arj) unarj $1;;
  esac
}

# cdコマンド実行後、lsを実行する
function cd() {
    builtin cd $@ && ls;
}

# clipboard
if which xsel >/dev/null 2>&1 ; then 
    alias -g C='| xsel --input --clipboard'
    alias -g P='xclip -out -sel clip'
fi

# for opam
source ~/.zshrc

source ~/.zplug/init.zsh

zplug 'mafredri/zsh-async', from:github
zplug 'sindresorhus/pure', use:pure.zsh, from:github, as:theme
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# zplug check はインストールするものがないときに真を返す
# ゆえにそうでないとき zplug install する
if ! zplug check; then
    zplug install
fi

# プラグインを読み込み、コマンドを実行可能にする
zplug load
