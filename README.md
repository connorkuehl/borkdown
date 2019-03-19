# Markerel

The parser of choice for the discerning Markdown author.

## Features

Markerel currently supports:

* `atx` style headers (`## I'm a level 2 header`)
* **Bold text** (`**I'm a piece of bold text!**`)
* *Italic text* (`*I'm leaning over because I'm italic!`)
* [Links](#) (`[text](URL)`)
* ~~Strikethrough~~ (`~~cross this text off~~`)
* `Inline code` (``Markerel.hs``)
* Plain text :-)
* Unordered lists (like this one!)
* Ordered lists (enumerated lists, with numbers at the beginning)

## Usage

Markerel will only talk to `stdin` and `stdout` right now, so you'll probably
end up using it in a pipe-y IO-redirect-y kind of way.

```
$ cat a_markdown_file.md | ./Markerel.hs > document.html
```
