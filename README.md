# BulletPush #

[![Build Status](https://travis-ci.org/markus1189/BulletPush.png?branch=master)](https://travis-ci.org/markus1189/BulletPush)

Simple haskell client for (Pushbullet)[https://www.pushbullet.com/].
Requires either a file `~/.bulletpush` with your token as the sole
content or the `--token` argument.  You can get a token on your
(Account Settings)[https://www.pushbullet.com/account] page.

```
Haskell pushbullet client

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
  list                     Push a checklist`
```

Examples:

* `bullet-push note "My first note" "Hello World"`
* `bullet-push -e "email address" link "bullet-push rocks" "https://github.com/markus1189/bullet-push"`
* `bullet-push list todo "todo item 1" "todo item 2"`
