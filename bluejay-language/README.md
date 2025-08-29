
# Bluejay syntax highlighter

This highlighter attempts to mimic OCaml Platform but without a langauge server.

It makes approximations using regular expressions to highlight "function calls".

## Installation

The following are a few very rough instructions to install the syntax highlighter
as a VS Code extension.

1. Navigate to the `bluejay-language` directory (the one this README is in).
2. Run `npm install -g @vscode/vsce`. You may need to `--force` or use `sudo`.
3. Then do `vsce package --allow-missing-repository`. Answer `y`/`yes` to questions about missing licenses.
4. Run `code --install-extension bluejay-language-0.0.2.vsix`, or whatever version we're currently on (not just `0.0.1` every time).

To make changes and reinstall, only run the last two commands. Note you either need to
uninstall the previous version, or increment the version number.

Uninstall with

```sh
code --list-extensions
```

Note the package name, which is probably `undefined_publisher.bluejay-language`, and then run

```sh
code --uninstall-extension undefined_publisher.bluejay-language
```

Also uninstall from npm with

```sh
npm uninstall bluejay-language@0.0.2
```

or whatever version(s) you have installed.

## Disclaimers

The icon is under the license found at [this](https://uxwing.com/license/) link.
It is the wing icon [here](https://uxwing.com/wing-icon/).
It has been recolored, resized, and mirrored, but it otherwise is not changed.

The icon has been set as a fallback to whatever icon theme you're using. As long
as your theme does not define an icon for `.bjy` files, the provided icon is used.

The code for this highlighter has basically all been written with Claude 4, an
AI chatbot. No person claims credit for this syntax highlighter, besides any credit
that is due for the effort in managing chatbot failures and easing it into a solution
that works alright.