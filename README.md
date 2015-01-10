# BulletPush [![Build Status](https://travis-ci.org/markus1189/bullet-push.png?branch=master)](https://travis-ci.org/markus1189/bullet-push)

## Intro ##

BulletPush is a simple client for
[Pushbullet](https://www.pushbullet.com/) written in Haskell.

## Install ##

You can install it via `cabal install`.

In addition to the executable, you need your Pushbullet token.
Provide it either as a file `~/.bulletpush` with your token as the
sole content or the `--token` argument.  You can get a token on your
[Account Settings](https://www.pushbullet.com/account) page.

## Examples ##

```
bullet-push note "My first note" "Hello World"
bullet-push -e "email address" link "bullet-push rocks" "https://github.com/markus1189/bullet-push"
bullet-push list todo "todo item 1" "todo item 2"
```

## Arguments ##

```
bullet-push, the haskell pushbullet client

Usage: bullet-push [-v|--verbose] [-e|--email EMAIL] [--token TOKEN]
                   [--token-file FILE] COMMAND
  Push something with pushbullet.

Available options:
  -h,--help                Show this help text
  -v,--verbose             Enable verbose mode
  -e,--email EMAIL         Send push to EMAIL
  --token TOKEN            Use TOKEN for authentication
  --token-file FILE        Read authentication token from FILE, defaults to:
                           ~/.bulletpush

Available commands:
  note                     Push a note
  link                     Push a link
  list                     Push a checklist
  file                     Push a file
```
