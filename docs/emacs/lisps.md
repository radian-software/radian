# Lisps in Emacs

Lisp code has a very distinct format which lends itself well to
efficient navigation and structural editing. As such, Emacs has a
number of commands designed to make these tasks easier.

(If you are looking for information on Emacs' support for particular
Lisps, see the pages on [Emacs Lisp] and [Clojure].)

[emacs lisp]: elisp.md
[clojure]: clojure.md

## Commands for navigating Lisp code

| Command | Action                      |
| ------- | --------------------------- |
| `C-M-b` | Back one form               |
| `C-M-f` | Forward one form            |
| `C-M-p` | Descend backwards           |
| `C-M-n` | Ascend forwards             |
| `C-M-u` | Ascend backwards            |
| `C-M-d` | Descend forwards            |
| `C-M-a` | Beginning of top-level form |
| `C-M-e` | End of top-level form       |

The best way to explain these commands is by example. In the following
code snippets, the location of the cursor is shown as a `|`.

#### `C-M-b` (back one form)

    (reduce + (take-while (partial > 4000000) (filter ev|en? (fibonacci))))
    (reduce + (take-while (partial > 4000000) (filter |even? (fibonacci))))
    (reduce + (take-while (partial > 4000000) (|filter even? (fibonacci))))
    (reduce + (take-while (partial > 4000000) |(filter even? (fibonacci))))
    (reduce + (take-while |(partial > 4000000) (filter even? (fibonacci))))
    (reduce + (|take-while (partial > 4000000) (filter even? (fibonacci))))
    (reduce + |(take-while (partial > 4000000) (filter even? (fibonacci))))
    (reduce |+ (take-while (partial > 4000000) (filter even? (fibonacci))))
    (|reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
    |(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))

#### `C-M-f` (forward one form)

    (reduce + (take-while (par|tial > 4000000) (filter even? (fibonacci))))
    (reduce + (take-while (partial| > 4000000) (filter even? (fibonacci))))
    (reduce + (take-while (partial >| 4000000) (filter even? (fibonacci))))
    (reduce + (take-while (partial > 4000000|) (filter even? (fibonacci))))
    (reduce + (take-while (partial > 4000000)| (filter even? (fibonacci))))
    (reduce + (take-while (partial > 4000000) (filter even? (fibonacci))|))
    (reduce + (take-while (partial > 4000000) (filter even? (fibonacci)))|)
    (reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))|

#### `C-M-p` (descend backwards)

    (assoc (merge n (assoc (assoc m :a 1) :b 2)) :c 3)|
    (assoc (merge n (assoc (assoc m :a 1) :b 2)) :c 3|)
    (assoc (merge n (assoc (assoc m :a 1) :b 2)|) :c 3)
    (assoc (merge n (assoc (assoc m :a 1) :b 2|)) :c 3)
    (assoc (merge n (assoc (assoc m :a 1|) :b 2)) :c 3)

#### `C-M-n` (ascend forwards)

    (reduce + (take-while (par|tial > 4000000) (filter even? (fibonacci))))
    (reduce + (take-while (partial > 4000000)| (filter even? (fibonacci))))
    (reduce + (take-while (partial > 4000000) (filter even? (fibonacci)))|)
    (reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))|

#### `C-M-u` Ascend backwards

    (reduce + (take-while (partial > 4000000) (filter ev|en? (fibonacci))))
    (reduce + (take-while (partial > 4000000) |(filter even? (fibonacci))))
    (reduce + |(take-while (partial > 4000000) (filter even? (fibonacci))))
    |(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))

#### `C-M-d` (descend forwards)

    |(reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
    (|reduce + (take-while (partial > 4000000) (filter even? (fibonacci))))
    (reduce + (|take-while (partial > 4000000) (filter even? (fibonacci))))
    (reduce + (take-while (|partial > 4000000) (filter even? (fibonacci))))

#### `C-M-a` (beginning of top-level form)


    (when (member 'clojure-mode radian-packages)
      (add-hook 'clojure-mo|de-hook (lambda () (eldoc-mode 1))))

    |(when (member 'clojure-mode radian-packages)
      (add-hook 'clojure-mode-hook (lambda () (eldoc-mode 1))))

#### `C-M-e` (end of top-level form)

    (when (member 'clojure-mode radian-packages)
      (add-hook 'clojure-mo|de-hook (lambda () (eldoc-mode 1))))

    (when (member 'clojure-mode radian-packages)
      (add-hook 'clojure-mode-hook (lambda () (eldoc-mode 1))))
    |

## Commands for editing Lisp code

Balancing parentheses by hand is tedious and error-prone, so Emacs
does this automatically. This means that it is impossible to
accidentally unbalance parentheses, since Emacs always inserts them
and deletes them in pairs.

At first, this may seem like it makes editing difficult. For instance,
what if you want to remove the parentheses surrounding several forms?
You cannot simply delete one and then the other, since that would
unbalance them in the intermediate state. To compensate, Emacs
provides a number of useful commands to perform editing tasks on Lisp
forms:

| Command       | Action                                                  |
| ------------- | ------------------------------------------------------- |
| `M-(`         | Wrap next form in parentheses                           |
| `M-"`         | Wrap next form in double quotes                         |
| `<C-right>`   | Slurp from the right                                    |
| `<C-left>`    | Barf to the right                                       |
| `<C-M-left>`  | Slurp from the left                                     |
| `<C-M-right>` | Barf to the left                                        |
| `M-s`         | Splice current form                                     |
| `<M-up>`      | Kill to beginning of current form and splice            |
| `<M-down>`    | Kill to end of current form and splice                  |
| `M-r`         | Kill on either side of next form and splice (AKA raise) |
| `M-S`         | Split current form at cursor location                   |
| `M-J`         | Join forms on either side of cursor location            |
| `M-?`         | Reverse nesting of forms (AKA convolute)                |

The best way to explain these commands is by example. In the following
code snippets, the location of the cursor is shown as a `|`.

### `M-(` (wrap in parentheses)

    (foo |bar baz)
    (foo (|bar) baz)

### `M-"` (wrap in double quotes)

    (foo |bar baz)
    (foo "|bar" baz)

### `<C-right>` (slurp from right)

    (foo (bar |baz) quux zot)
    (foo (bar |baz quux) zot)

### `<C-left>` (barf to right)

    (foo (bar |baz quux) zot)
    (foo (bar |baz) quux zot)

### `<C-M-left>` (slurp from left)

    (foo bar (baz |quux zot))
    (foo (bar baz |quux zot))

### `<C-M-right>` (barf to left)

    (foo (bar baz |quux zot))
    (foo bar (baz |quux zot))

### `M-s` (splice)

    (foo (bar |baz) quux zot)
    (foo bar |baz quux zo)

### `<M-up>` (splice killing backwards)

    (foo (bar |baz) quux zot)
    (foo |baz quux zot)

### `M-down>` (splice killing forwards)

    (foo (bar| baz) quux zot)
    (foo bar| quux zot)

### `M-r` (raise)

    (foo (bar| (baz quux) zot) fum)
    (foo |(baz quux) fum)

### `M-S` (split)

    (foo (bar| baz quux) zot)
    (foo (bar|) (baz quux) zot)

### `M-J` (join)

    (foo (bar)| (baz quux) zot)
    (foo (bar| baz quux) zot)

### `M-?` (convolute)

The keybinding for s-expression convolution is probably quite
appropriate given most people's first reaction to it. However, it is
very useful in one particular case: reversing the nesting of forms.
Here is an example, where a `let` form is broadened in scope so that a
symbol it binds is available in a higher form:

    (apply +
           (concat
             (filter even? the-numbers)
             (let [the-numbers (range 50)]|
               (remove #(zero? (mod % 3))
                       the-numbers))))

    (apply +
           (let [the-numbers (range 50)]
             (concat
               (filter even? the-numbers)
               (remove #(zero? (mod % 3))
                       the-numbers))))

Note that I had to move a line break in order to make the formatting
look nice.

### Additional notes

Note that there are no keybindings analogous to `M-(` but for square
brackets or curly braces. As substitutes, you can use `[ C-right` and
`{ C-right`.

The best way to remember the slurp/barf keybindings, in my opinion, is
to keep in mind that the arrow key points in the direction in which
the parenthesis moves.

You may have to substitute `ESC C-left` and `ESC C-right` for
`C-M-left` and `C-M-right` in order for your terminal emulator to send
the correct key signals.

Remember that strings are also forms, so you can use most of these
keybindings to manipulate strings as well as parenthetical forms.

If you really need to bypass parenthesis balancing, you can disable it
with `M-x paredit-mode`.
