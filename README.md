# BulletPush #

[![Build Status](https://travis-ci.org/markus1189/BulletPush.png?branch=master)](https://travis-ci.org/markus1189/BulletPush)

Simple haskell client for Pushbullet.  Requires a file "~/.bulletpush"
with your token.

```
Haskell pushbullet client

Usage: bullet-push [-v|--verbose] [--token TOKEN] [--token-file FILE] COMMAND
  Push something with pushbullet.

Available options:
  -h,--help                Show this help text
  -v,--verbose             Enable verbose mode
  --token TOKEN            Use TOKEN for authentication
  --token-file FILE        Read authentication token from FILE, defaults to:
                           ~/.bulletpush

Available commands:
  note                     Push a note
  link                     Push a link`
```

Example:

`bullet-push note "My first note" "Hello World"`
