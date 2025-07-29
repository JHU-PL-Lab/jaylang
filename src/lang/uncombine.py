#!/usr/bin/python3

"""
The purpose of this script is to derive the various Bluejay language
specifications from a single combined description.  Run with "--help-more" for
a description.
"""

import argparse
import dataclasses
import io
import os
import sys

from typing import List, Dict, Tuple

class MoreHelp(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        print(
"""
The purpose of this script is to derive the various Bluejay language
specifications from a single combined description.  This script is designed
to work as smoothly as possible with Dune but to be flexible to future
restructuring.

This script accepts a number of "combined" files and a prefix that they must all
share in common (e.g. combined files "zfoo" and "zbar" and the prefix "z").  It
also accepts a list of "targets": those files which it should produce.  Target
files must have names which are equal to some combined file with the prefix
removed and replaced (e.g. "yfoo", "ybar", and "xbar").  If called as

  python3 uncombine.py --prefix z --combined zfoo zbar --targets yfoo ybar xbar

then this script will uncombine zfoo into yfoo and will uncombine zbar into
both ybar and xbar.

When a file (e.g. "zbar") is uncombined into another (e.g. "ybar"), this script
will copy the combined file character by character to the target file.  This
behavior only deviates for uncombining commands appearing in the combined file.
These commands take the form of a start string "(*!" and an end string "!*)",
similar to OCaml comments.  The text between the start string and end string
must be a well-formatting uncombining command.  Recognized commands are:

    "scope <target_prefix> [target_prefix ...]"
        Copies non-newline characters after this command only if we are
        generating a file with one of the provided target prefixes.
    "endscope"
        Ends the most recent active scope.

The text from uncombining commands is elided from the target file.

For instance, if file "zbar" contains the text

    abc
    (*!scope y!*)
    def
    (*!endscope!*)
    ghi
    (*!scope x!*)
    jkl
    (*!endscope!*)
    mno

then uncombining into file "ybar" produces the text

    abc

    def

    ghi



    mno

Each uncombining operation is performed in isolation.  Whitespace within
uncombining commands is ignored.
""")
        sys.exit(0)

def parse_args():
    parser = argparse.ArgumentParser(
        description=("Generates lexer and parser files from a single combined "
                     "description.")
    )
    parser.add_argument(
        "--help-more", nargs=0, action=MoreHelp,
        help="Provides a more detailed description of this script.")
    parser.add_argument(
        "--prefix", required=True,
        help=("The common prefix of all combined filenames which is replaced "
              "in target filenames."))
    parser.add_argument(
        "--combined", nargs="+", required=True,
        help="The combined files to separate.")
    parser.add_argument(
        "--ignore-combined", nargs="+", required=False,
        help=("Combined files to ignore.  This is used to explicitly exclude "
              "dependencies included by Dune that we don't want to process."))
    parser.add_argument(
        "--targets", nargs="+", required=True,
        help="The target files to generate.")
    return parser.parse_args()

def uncombine_text(
        target_filename, combined_text: str, target_prefix: str
        ) -> str:
    COMMAND_START = "(*!"
    COMMAND_END = "!*)"
    scope_stack : List[List[str]] = []
    idx = 0
    linenum = 1
    def fail(msg):
        print("{:s} on line {:d} of {:s}".format(
            msg, linenum, target_filename))
        sys.exit(1)
    output = io.StringIO()
    while idx < len(combined_text):
        if combined_text[idx:idx+len(COMMAND_START)] == COMMAND_START:
            # Find the end of the uncombining command
            end_idx = combined_text.find(COMMAND_END, idx+len(COMMAND_END))
            if end_idx == -1:
                fail("Unterminated uncombining command")
            # Extract the command string
            command_string = combined_text[idx+len(COMMAND_START):end_idx]
            # Set next character to process after the command
            idx = end_idx+len(COMMAND_END)
            newlines = command_string.count("\n")
            # Parse and execute command
            command_parts = command_string.strip().split()
            if len(command_parts) == 0:
                fail("Empty uncombining command")
            elif command_parts[0] == "scope":
                scope_stack.append(command_parts[1:])
            elif command_parts[0] == "endscope":
                if len(command_parts) != 1:
                    fail("Extraneous tokens in endscope command")
                if len(scope_stack) == 0:
                    fail("No scope to end")
                scope_stack.pop()
            else:
                fail("Unrecognized uncombining command")
            # Place a number of newlines into the target to match those
            # appearing in the command.
            output.write("\n" * newlines)
            linenum += newlines
        else:
            # If we're in a scope that excludes this target, we'll only copy
            # newlines.
            in_scope = (len(scope_stack) == 0) or (target_prefix in scope_stack[-1])
            is_newline = combined_text[idx] == '\n'
            if is_newline: linenum += 1
            if in_scope or is_newline:
                output.write(combined_text[idx])
            idx += 1
    return output.getvalue()

@dataclasses.dataclass
class CombinedFile:
    suffix: str
    contents: str

@dataclasses.dataclass
class TargetTask:
    target_filename: str
    combined_contents: str
    target_prefix: str

def main():
    args = parse_args()
    # Check to ensure that combined files are properly prefixed.  Get the
    # contents of each combined file paired with its suffix.
    combined_filenames : List[str] = args.combined
    for ignore_filename in args.ignore_combined:
        if ignore_filename in combined_filenames:
            combined_filenames.remove(ignore_filename)
    combined_files : List[CombinedFile] = []
    for filename in combined_filenames:
        if not (os.path.basename(filename).startswith(args.prefix)):
            print(("Combined filename {:s} does not start with "
                   "prefix {:s}").format(filename, args.prefix))
        with open(filename) as f:
            contents = f.read()
        combined_files.append(CombinedFile(
            suffix=os.path.basename(filename)[len(args.prefix):],
            contents=contents))
    # For each target file, figure out the suffix to which it is matched and
    # pair it to its text.
    work : List[TargetTask] = []
    for filename in args.targets:
        found = False
        for combined_file in combined_files:
            if filename.endswith(combined_file.suffix):
                found = True
                target_prefix = \
                    os.path.basename(filename)[:-len(combined_file.suffix)]
                work.append(TargetTask(
                    target_filename=filename,
                    combined_contents=combined_file.contents,
                    target_prefix=target_prefix))
                break
        if not found:
            print("Target file {:s} does not match any combined files.".format(
                filename))
            sys.exit(1)
    # Now do the actual work
    for task in work:
        target_contents = uncombine_text(
            task.target_filename, task.combined_contents, task.target_prefix)
        with open(task.target_filename, 'w') as f:
            f.write(target_contents)

if __name__ == "__main__":
    main()