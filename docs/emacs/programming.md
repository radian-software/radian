# Programming in Emacs

Emacs supports programming in many different languages. Typically, the
appropriate language is detected automatically. For instance, if you
open a `.java` file, then `java-mode` will be enable automatically. In
some cases, though, you might open a file with an improper extension,
and Emacs would be unable to recognize it. In that case, you can
call—for instance—`M-x java-mode` manually.

Most programming modes provide syntax highlighting and indentation
rules. You can always indent the current line by pressing `TAB` and
indent the current [selection] by pressing `C-M-\`.

Some programming languages have very strict rules about indentation.
For some of those languages, *aggressive indentation* is enabled. This
means that Emacs will continually adjust the indentation as you edit,
so that it is always correct. You can disable aggressive indentation
with `M-x aggressive-indent`.

[selection]: editing.md

Emacs supports virtually every programming language. Radian does not
yet support every language, though.

You can find more information about the support for particular
languages here:

* [Lisps](lisps.md)
  * [Emacs Lisp](elisp.md)
  * [Clojure](clojure.md)
