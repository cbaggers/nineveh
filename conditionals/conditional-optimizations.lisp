(in-package :nineveh.conditionals)

(v-defmacro mix-step-if ((op val threshold) then else)
  (assert (or (eq op '>=) (eq op '<)) (op)
          "Nineveh: Sorry < & >= are the only allowed operators for ms-if")
  `(mix ,(if (eq op '>=) else then)
        ,(if (eq op '>=) then else)
        (step ,threshold ,val)))
