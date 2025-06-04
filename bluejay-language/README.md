
# Bluejay syntax highlighter

This highlighter attempts to mimic OCaml Platform but without a langauge server.

It makes approximations using regular expressions to highlight "function calls".

It has basically all been written with Claude 4, an AI chatbot. No person claims
credit for this syntax highlighter, besides any credit that is due for the effort
in managing chatbot failures and easing it into a solution that works alright.

Rough installation instructions:
1. Navigate to the `bluejay-language` directory (the one this README is in).
2. Run `npm install -g @vscode/vsce`. You may need to `--force` or use `sudo`.
3. Then do `vsce package --allow-missing-repository`. Answer `y`/`yes` to questions about missing licenses.
4. Run `code --install-extension bluejay-language-0.0.1.vsix`, or whatever version we're currently on (not just `0.0.1` every time).