
TODO:
* Try to resolve all of the todos I have left in the code. There are LOTS of them.
  * The following bullet points, though, try to summarize some of them.
* Continue with misses until steady state
* Handle max step
* Track inputs and solvers that lead to hits/misses -- i.e. payloads on branch statuses
* Port over logging (and figure out logging in general)
* Use input AST branches to customize output -- this is for use in the type checker
* Use variants instead of exceptions to make tracking easier
* Confirm that we only want int input


For the "max step" branch, I intend to keep a counter of which branches have caused us
to hit max step several times, and after so many times, we call that branch off limits.

I would like to think more intentionally about the representation before I get coding, but
I expect that it will just be a map from a branch to a count.