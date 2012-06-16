#!/bin/sh

## ロードアベレージを表示
# http://d.hatena.ne.jp/yonchu/20120414/1334422075
uptime | awk '{print $(NF - 2),$(NF - 1),$NF}'
