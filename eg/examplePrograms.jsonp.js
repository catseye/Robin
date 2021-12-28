examplePrograms = [
    {
        "contents": "(define a 1)\n(define b 1)\n(assert (equal? (subtract a b) 99))\n",
        "filename": "assertion.robin"
    },
    {
        "contents": ";''Example of a recursive fexpr.  It takes a list of bindings a la `let`\n   and returns a snippet of code `bind`ing those values.  This is actually\n   how `let` is now defined in the stdlib.''\n\n(define expand-let (fexpr (args env)\n  (bind body (head (tail args))\n    (bind expand-let-r (fexpr (iargs ienv)\n      (bind self (eval ienv (head iargs))\n        (bind items (eval ienv (head (tail iargs)))\n          (if (equal? items ())\n            body\n            (prepend (literal bind)\n              (prepend (head (head items))\n                (prepend (head (tail (head items)))\n                  (prepend (self self (tail items))\n                           ()))))))))\n      (expand-let-r expand-let-r (head args))))))\n\n(display (expand-let ((a 1) (b 2) (c 3)) foo))\n",
        "filename": "expand-let.robin"
    },
    {
        "contents": ";''Example of a recursive function.''\n\n(define fact (fun (self n)\n  (multiply n\n    (if (gt? n 1)\n      (self self (subtract n 1))\n      1))))\n(display (fact fact 5))\n",
        "filename": "fact.robin"
    }
];
