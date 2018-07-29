# Flute

Flute is a beautiful, easily composable HTML5 generation library in Common Lisp. It's

- Simple: the most simplistic syntax, for builtin and customized elements;
- Easy to debug: pretty print generated html snippet in REPL;
- Powerful: help you define reusable and composable components, like that in React
- Modern: focus only on HTML5

# Getting started

## Install and run tests

```lisp
(ql:quickload :flute)
(ql:quickload :flute-test)
```

Then define a new package specifically for HTML generation, in its definition:
```lisp
(defpackage flute-user
  (:use :cl :flute))
```
If you don't want to import all symbols, see [H Macro](#h-macro), which provide a similar interface as a tranditional Lisp HTML generation library.

## Using html elements
```
(html
  (head
    (link :rel "...")
    (script :src "..."))
  (body
    (div :id "a" :class "b"
      (p :style "color: red"
        "Some text")
      "Some text in div"
      (img :src "/img/dog.png")
      (a '(:href "/cat")
        (img '((:src . "/img/cat.png")))))))
```

These `html`, `div`, etc. are just functions. Element attribute can be given inline as the above example, or as alist/plist/attrs object as the first argument, like the last `a` and `img` in the above example. In this case they can be variables that calculated programmatically.

The remaining argument will be recognized as the children of this element. Each child can be:
1. string;
2. element, builtin or user defined;
3. list of 1, 2 and 3. Can also be NIL.
All children will be flattened as if they're given inline.

## Define new element
```lisp
(define-element dog (id size)
  (if (and (realp size) (> size 10))
      (div :id id :class "big-dog"
              children
              "dog")
      (div :id id :class "small-dog"
              children
              "dog")))
```
`dog` will be defined as a function that takes `:id` and `:size` keyword arguments. `dog` returns an user-defined element object. Inside it, `children` will be replaced with the children elements you provided when creating this `dog`:
```
FLUTE-USER> (defparameter *dog1* (dog :id "dog1" :size 20))
*DOG1*
FLUTE-USER> *dog1*
<div id="dog1" class="big-dog">dog</div>
FLUTE-USER> (dog :id "dog2" "I am a dog" *)
<div id="dog2" class="small-dog">
  I am a dog
  <div id="dog1" class="big-dog">dog</div>
  dog
</div>
```

All elements, both builtin and user defined ones are objects, although they're printed as html snippet in REPL. Their attribute can be accessed by `(element-attrs element)`. Their children can be accessed by `(element-children elements)` and tag name by `(element-tag element)`. You can modify an exising element's attrs and children. If you modify a user defined element, the body you defined in it's `define-element` also re-executed to take effect of the the attrs and children change:
```
FLUTE-USER> *dog1*
<div id="dog1" class="big-dog">dog</div>
FLUTE-USER> (setf (attr *dog1* :size) 10
                  ;; attr is a helper method to set (flute:element-attrs *dog1*)
                  (attr *dog1* :id) "dooooog1"
                  (element-children *dog1*) (list "i'm small now"))
("i'm small now")
FLUTE-USER> *dog1*
<div id="dooooog1" class="small-dog">
  i'm small now
  dog
</div>
```

By default user element is printed as what it expand to. If you have a lot of user defined element nested deeply, you probably want to have a look at the high level:
```
FLUTE-USER> (let ((*expand-user-element* nil))
              (print *dog1*)
              (values))

<dog id="dooooog1" size=10>i'm small now</dog>
; No value
FLUTE-USER>
```

## Generate HTML
To generate a piece of HTML string that probably used in a response of a backend service:
```lisp
(elem-str element)
```
To generate HTML string that has nice indent as that in REPL:
```lisp
(element-string element)
```
To generate that and write to file, just create a stream, then `(write element :stream stream)` for human or `(write element :stream stream :pretty nil)` for production.

## H macro
If you don't want to import all the symbols, you can use the `h` macro:
```lisp
(defpackage flute-min
  (:use :cl)
  (:import-from :flute
                :h
                :define-element))
```
Then just wrap `h` for all html generation part. In the same examples above, it becomes:
``` lisp
(in-package :flute-min)
(h (html
     (head
       (link :rel "...")
       (script :src "..."))
     (body
       (div :id "a" :class "b"
         (p :style "color: red"
           "Some text")
         "Some text in div"
         (img :src "/img/dog.png")
         (a '(:href "/cat")
           (img '((:src . "/img/cat.png"))))))))

(define-element dog (id size)
  (if (and (realp size) (> size 10))
      (h (div :id id :class "big-dog"
              flute:children
              "dog"))
      (h (div :id id :class "small-dog"
              flute:children
              "dog"))))

(defparameter *dog2* (dog :id "dog2" :size 20 "some children"))
```
From version 0.2 (available in Aug 2018 Quicklisp), flute supports css style id and class attribute for builtin elements. For example `div#id-name.class1.class2`, So you can also write:
```lisp
(h (div#a.b "..."))
;; Provide additional class and attributes
(h (div#a.b :class "c" :onclick "fun()"))
```

## Inline CSS and JavaScript
With help of [cl-css](https://github.com/Inaimathi/cl-css) (available in Quicklisp), You can write inline CSS for the `style` attribute, in a similar syntax like flute:
```lisp
(div :style (inline-css '(:margin 5px :padding 0px)))
```
`cl-css:inline-css` is a function taking plist and returns the result css string, so it can be safely used inside or outside of `H` macro and with variable arguments.

With help of [Parenscript](https://github.com/vsedach/Parenscript) (available in Quicklisp), You can write inline JavaScript for `onclick`, etc. attribute:
```lisp
(button :onclick (ps-inline (func)))
```

That's all you need to know to define elements and generate html. Please reference the [API Reference](#api-reference) Section for detailed API.

# Change Logs
## 2018/07/28 Version 0.2-dev
- Support `element#id.class1.class2` in `H` macro for builtin elements;
- Suggestions on inline CSS and JavaScript in lispy way;
- Jon Atack fix an error example in README.
## 2018/07/11 Version 0.1
- Current features, APIs and Tests.

# Motivation
Currently there're a few HTML generation library in Common Lisp, like [CL-WHO](https://edicl.github.io/cl-who/), [CL-MARKUP](https://github.com/arielnetworks/cl-markup) and [Spinneret](https://github.com/ruricolist/spinneret). They both have good features for generating standard HTML, but not very good at user element (components) that currently widely used in frontend: you need to define all of them as macros and to define components on top of these components, you'll have to make these components more complex macros to composite them. [Spinneret](https://github.com/ruricolist/spinneret) has a `deftag` feature, but `deftag` is still expand to a `defmacro`.

I'd also want to modify the customer component attribute after create it and incorporate it with it's own logic (like the dog size example above), this logic should be any lisp code. This requires provide all element as object, not plain HTML text generation. With this approach, all elements have a same name function to create it, and returns element that you can modify later. These objects are virtual doms and it's very pleasant to write html code and frontend component by just composite element objects as arguments in element creation function calls. Flute's composite feature inspired by [Hiccup](https://github.com/weavejester/hiccup) and [Reagent](https://github.com/reagent-project/reagent) but more powerful -- in flute, user defined elements is real object with attributes and it's own generation logic.

# Limitation
With the function name approach, it's not possible to support `div.id#class1#class2` style function names. I'm working on some tweak of reader macros in [illusion](https://github.com/ailisp/illusion) library to detect this and convert it to `(div :id "id" :class "class1 class2" ...)` call

The most and major limitation is we don't have a substential subset of Common Lisp in browser so flute can be used in frontend. [Parenscript](https://github.com/vsedach/Parenscript) is essentially a JavaScript semantic in Lisp like syntax. [JSCL](https://github.com/jscl-project/jscl) is promosing, but by now it seems focus on creating a CL REPL on Web and doesn't support `format` or CLOS. Also we lack enough infrastructure to build Common Lisp to JavaScript (might be an asdf plugin) and connect to a browser "Swank" via WebSocket from Emacs. I'll be working these: a full or at least substential subset of Common Lisp to JavaScript Compiler to eventually have a full frontend development environment in Common Lisp. Any help or contribution is welcome.

# API Reference
[API Reference](api.html)

# License
Licensed under MIT License.
Copyright (c) 2018, Bo Yao. All rights reserved.
