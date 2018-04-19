# purescript-polyform-record-form-example

This scenario/problem was proposed by @thomashoneyman.

We want a registration form which validates password fields separately and after that it validates if they are equal. If the last valiadation failes it should attach error information to `password2` field.
We also want to represent a form as a record.


My proposition

We can use this strange monoid `Endo a` (it is really flipped `Join a`) as our monoidal "validation report" so finally we are going to get some transformation which will set appropriate fields of our form according to the validation result.

