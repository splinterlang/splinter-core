syntax checker
==============

* Flag unsupported syntax
    Example:
      1 = 9 --> cannot assign to literal
* Identify nonsense statements
    Example:
      foo():
        bar():
          pass --> doesn't support functions-in-functions yet
