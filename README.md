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
    (script :src "...")
  (body
    (div :id "a" :class "b"
      (p :style "color: red"
        "Some text")
        "Some text in div"))))
```

These `html`, `div`, etc. are just functions. Element attribute can be given inline as the above example, or as alist/plist/attrs object as the first argument in the case they're calculated programmatically.

The remaining argument will be recognized as the children of this element, they can be a piece of text, other element object, or list of text/element/list of this kind of list of any depth. Children will be flattened as if they're given inline.

## Define new element
```lisp
(define-element dog (id size)
  (if (and (realp size) (> size 10))
      (div :id id :class "big-dog"
              children
              "dog")
      ((div :id id :class "small-dog"
              children
              "dog"))))
```
`DOG` will be defined as a function that takes `:ID` and `:SIZE` keyword arguments. `DOG` returns an user-defined element object. Inside it, `CHILDREN` will be replaced with the children elements you provided when creating this `DOG`:
```
CL-USER> (defparameter *dog1* (dog :id "dog1" :size 20))
*DOG1*
CL-USER> *dog1*
<div id="dog1" class="big-dog">dog</div>
CL-USER> (dog :id "dog2" "I am a dog" *)
<div id="dog2" class="small-dog">
  I am a dog
  <div id="dog1" class="big-dog">dog</div>
  dog
</div>
CL-USER>
```

All elements, both builtin and user defined ones are objects, although they're printed as html snippet in REPL. Their attribute can be accessed by `(element-attrs element)`. Their children can be accessed by `(element-children elements)` and tag name by `(element-tag element)`. You can modify an exising element's attrs and children. If you modify a user defined element, the body you defined in it's `define-element` also re-executed to take effect of the the attrs and children change:
```
FLUTE-USER> *dog1*
<div id="dog1" class="big-dog">dog</div>
CL-USER> (setf (attr *dog1* :size) 10
               (attr *dog1* :id) "dooooog1" ; attr is a helper method to set (flute:element-attrs *dog1*)
               (element-children *dog1*) (list "i'm small now"))
("i'm small now")
FLUTE-USER> *dog1*
<div id="dooooog1" class="small-dog">
  i'm small now
  dog
</div>
FLUTE-USER>
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
To generate that and write to file, just create a stream, then `(write element :stream stream)` or `(write element :stream stream :pretty nil)`

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
        (script :src "...")
      (body
        (div :id "a" :class "b"
          (p :style "color: red"
             "Some text")
          "Some text in div")))))

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


That's all you need to know to define elements and generate html. Please reference the [API Reference](#api-reference) Section for detailed API.

# Motivation
Currently there're a few HTML generation library in Common Lisp, like [CL-WHO](https://edicl.github.io/cl-who/), [CL-MARKUP](https://github.com/arielnetworks/cl-markup) and [Spinneret](https://github.com/ruricolist/spinneret). They both have good features for generating standard HTML, but not very good at user element (components) that currently widely used in frontend: you need to define all of them as macros and to define components on top of these components, you'll have to make these components more complex macros to composite them. [Spinneret](https://github.com/ruricolist/spinneret) has a `deftag` feature, but `deftag` is still expand to a `defmacro`.

I'd also want to modify the customer component attribute after create it and incorporate it with it's own logic (like the dog size example above), this logic should be any lisp code. This requires provide all element as object, not plain HTML text generation. With this approach, all elements have a same name function to create it, and returns element that you can modify later. These objects are virtual doms and it's very pleasant to write html code and frontend component by just composite element objects as arguments in element creation function calls. Flute's composite feature inspired by [Hiccup](https://github.com/weavejester/hiccup) and [Reagent](https://github.com/reagent-project/reagent) but more powerful -- in flute, user defined elements is real object with attributes and it's own generation logic.

# Limitation
With the function name approach, it's not possible to support `div.id#class1#class2` style function names. I'm working on some tweak of reader macros in [illusion](https://github.com/ailisp/illusion) library to detect this and convert it to `(div :id "id" :class "class1 class2" ...)` call

The most and major limitation is we don't have a substential subset of Common Lisp in browser so flute can be used in frontend. [Parenscript](https://github.com/vsedach/Parenscript) is essentially a JavaScript semantic in Lisp like syntax. [JSCL](https://github.com/jscl-project/jscl) is promosing, but by now it seems focus on creating a CL REPL on Web and doesn't support `format` or CLOS. Also we lack enough infrastructure to build Common Lisp to JavaScript (might be an asdf plugin) and connect to a browser "Swank" via WebSocket from Emacs. I'll be working these: a full or at least substential subset of Common Lisp to JavaScript Compiler to eventually have a full frontend development environment in Common Lisp. Any help or contribution is welcome.

# API Reference
Here is a draft version of API Reference, draft means it will be better organized and moved to a separate HTML doc, but it's content is already quite complete.

## Builtin HTML elements
```
    a abbr address area article aside audio b base bdi bdo blockquote
    body br button canvas caption cite code col colgroup data datalist
    dd del details dfn dialog div dl dt em embed fieldset figcaption
    figure footer form h1 h2 h3 h4 h5 h6 head header hr i iframe html
    img input ins kbd label legend li link main |map| mark meta meter nav
    noscript object ol optgroup option output p param picture pre progress
    q rp rt ruby s samp script section select small source span strong
    style sub summary sup svg table tbody td template textarea tfoot th
    thead |time| title tr track u ul var video wbr
```
All of above HTML5 elements are functions, which support same kinds of parameters, take `A` as example:
``` lisp
;; Function A &REST ATTRS-AND-CHILREN
;;
;; Create and return an <a> element object
;; ATTRS-AND-CHILDREN can be the following:

;; 1. an empty <a> tag
(a)

;; 2. attributes of alist, plist or ATTRS object
;; The following creates: <a id="aa" customer-attr="bb">
(a :id "aa" :customer-attr "bb")
(a '(:id "aa" :customer-attr "bb"))
(a '((:id . "aa") (:customer-attr . "bb")))
;; or assume we have the above one in variable a1
(a (element-attrs a1)) ; to share the same attrs with a1
(a (copy-attrs (element-attrs a1)))

;; 3. any of above format attributes with children
(a :id "aa" :customer-attr "bb"
  "Some children"
  (div '(:id "an element children"))
  ; list of any depth containing elements and texts, will be flattened
  (list a1 a2 (a '((:id . "aaa")) "some text")
        (list (h1 "aaa")))
  "some other text")
```
The `HTML` element is a little special, it's with `<!DOCTYPE html>` prefix to make sure browser recognize it correctly.

## User defined elements
```lisp
;; Macro DEFINE-ELEMENT NAME (&REST ARGS) &BODY BODY
;;
;; Define a user element with NAME as its tag name and function
;; NAME. After DEFINE-ELEMENT, a FUNCTION of NAME in current package
;; is defined. ARGS specified the possible keyword ARGS it can take as
;; it's ATTRS. You can either use these ARGS as Lisp arguments in the
;; BODY of its definition and plug in them to the BODY it expand to.
;; You can use FLUTE:CHILDREN to get or set it's children that you give
;; when call function NAME, FLUTE:ATTRS to get or set it's attributes
;; and FLUTE:TAG to get or set it's tag name.

;; Variable *EXPAND-USER-ELEMENT*
;;
;; Bind this variable to specify whether the user elements are print in
;; a high level (NIL), or expand to HTML elements (T). T by default.
```

## Attribute accessing utility
``` lisp
;; Function ATTRS-ALIST ATTRS
;; Function (SETF ATTRS-ALIST) ATTRS
;;
;; Return or set the attrs object in alist format

;; Function MAKE-ATTRS &KEYS ALIST
;;
;; Create a attrs aoject, given an alist of (:attr . "attr-value") pair.
;; Attribute values (cdr of each element in alist) will be escaped if
;; *ESCAPE-HTML* is not NIL

;; Function COPY-ATTRS ATTRS
;;
;; Make a copy and return the copy of ATTRS object

;; Method ATTR ATTRS KEY
;; Method (SETF ATTR) ATTRS KEY
;; Method ATTR ELEMENT KEY
;; Method (SETF ATTR) ELEMENT KEY
;;
;; Get or set the attribute value of given KEY. KEY should be an keyword.
;; If KEY does not exist, ATTR method will return NIL. (SETF ATTR) method
;; will create the (KEY . VALUE) pair. Don't use (SETF (ATTR ATTRS :key) NIL)
;; or (SETF (ATTR ELEMENT :key) NIL) to remove an attr, use DELETE-ATTR.

;; Method DELETE-ATTR ATTRS KEY
;; Method DELETE-ATTR ELEMENT KEY
;;
;; Delete the attribute key value pair from ATTRS or ELEMENT's ELEMENT-ATTRS,
;; will ignore if KEY doesn't exist.

```

## Element slots
```lisp
;; Method ELEMENT-TAG ELEMENT
;; Method (SETF ELEMENT-TAG) ELEMENT
;;
;; Get or set the ELEMENT-TAG STRING. For example <html>'s ELEMENT-TAG is "html"

;; Method ELEMENT-ATTRS ELEMENT
;; Method (SETF ELEMENT-ATTRS) ELEMENT
;;
;; Get or set the ELEMENT-ATTRS. When set this, must be an ATTRS object

;; Method ELEMENT-CHILDREN ELEMENT
;; Method (SETF ELEMENT-CHILDREN) ELEMENT
;;
;; Get or set element children. When set this manually, must given a flatten list
;; of ELEMENT or STRING.

;; Method USER-ELEMENT-EXPAND-TO USER-ELEMENT
;;
;; Get what this USER-ELEMENT-TO. Returns the root ELEMENT after it expands.
```

## The H macro
```lisp
;; Macro H &BODY CHILDREN
;;
;; Like a PROGN, except it will replace all html tag SYMBOLs with the same name one
;; in FLUTE PACKAGE, so you don't need to import all of them. As an alternative you
;; can import all or part of html element functions in FLUTE PACKAGE to use them
;; without H macro

```

## Escape utility
```lisp
;; Variable *ESCAPE-HTML*
;;
;; Specify the escape option when generate html, can be :UTF8, :ASCII, :ATTR or NIL.
;; If :UTF8, escape only #\<, #\> and #\& in body, and \" in attribute keys. #\' will
;; in attribute keys will not be escaped since flute will always use double quote for
;; attribute keys.
;; If :ASCII, besides what escaped in :UTF8, also escape all non-ascii characters.
;; If :ATTR, only #\" in attribute values will be escaped.
;; If NIL, nothing is escaped and programmer is responsible to escape elements properly.
;; When given :ASCII and :ATTR, it's possible to insert html text as a children, e.g.
;; (div :id "container" "Some <b>text</b>")
;; All the escapes are done in element creation time.

;; Function ESCAPE-STRING STRING TEST
;;
;; Escape the STRING if it's a STRING and escaping all charaters C that satisfied
;; (FUNCALL TEST C). Return the new STRING after escape.

;; Function UTF8-HTML-ESCAPE-CHAR-P CHAR
;;
;; Return T if CHAR is a CHARACTER that need to be escaped when HTML is UTF-8 encoded.
;; Return NIL otherwise.

;; Function ASCII-HTML-ESCAPE-CHAR-P CHAR
;;
;; Return T if CHAR is a CHARACTER that need to be escaped when HTML is ASCII encoded.
;; Return NIL otherwise.


;; Function ATTR-VALUE-ESCAPE-CHAR-P CHAR
;;
;; Return T if CHAR is a CHARACTER that need to be escaped when as an attribute value.
;; Return NIL otherwise.

```

## Generate HTML string

``` lisp
;; Method ELEMENT-STRING ELEMENT
;;
;; Return human readable, indented HTML string for ELEMENT

;; Method ELEM-STR ELEMENT
;;
;; Return minify HTML string for ELEMENT
```


# License
Licensed under MIT License.
Copyright (c) 2018, Bo Yao. All rights reserved.
