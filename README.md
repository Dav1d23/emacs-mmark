# Mmark: set and get marks in your buffers the Vim's way

This package introduces a way to get and set marks on Emacs buffers.
It allows you to quickly mark a location in a buffer, and jump back to that
exact location.

## Why should I use mmark instead of bookmark?

Out of the box, Emacs provides bookmarks to save/restore locations.
But this is not enough to me: usually, I mark several locations in the same
buffer and jump between them. AFAIK, bookmarks do not provide the ability to
mark several positions in the same buffer, which mmark does.

## Is mmark production ready?

This is quick: no.

## Why mmark?

The name is also chosen for quick access: there are few packages that are named
`mm*` and so pressing 3 keys will likely bring you to this package ;).

# How to use it

mmark has 3 main entry points:
* mmark-set, which reads a char from the minibuffer and records the position in
  the current buffer;
* mmark-get, which jumps to the defined position when getting the proper char;
* mmark-mode, that shows all saved marks in all the buffers in a tabulated-list
  mode.

In mmark-mode, one should select the interesting entry and then uses:
* `g` to refresh the buffer,
* `d` to remove a mark,
* `<return>` to jump to that mark.

Usually, you want to add this to your Emacs configuration:
```
(require 'mmark)
(global-set-key (kbd "C-c m s") 'mmark-set)
(global-set-key (kbd "C-c m g") 'mmark-get)
```
to make it quicker to set/get a mark.
