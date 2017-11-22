#!/bin/sh

## ロードアベレージを表示
uptime | awk '{print $(NF - 2),$(NF - 1),$NF}'
