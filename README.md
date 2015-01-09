# BulletPush #

[![Build Status](https://travis-ci.org/markus1189/BulletPush.png?branch=master)](https://travis-ci.org/markus1189/BulletPush)

Simple haskell client for Pushbullet.  Requires a file "~/.bulletpush"
with your token.

```
$ bullet-push -h
Haskell pushbullet client

Usage: bullet-push COMMAND
  Push something with pushbullet.

Available options:
  -h,--help                Show this help text

Available commands:
  note                     Push a note
  link                     Push a link
```

Example:

`bullet-push note "My first note" "Hello World"`
