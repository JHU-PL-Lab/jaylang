#!/usr/bin/python

import sys

def read_dot_file(name):
    before_lines = []
    during_lines = []
    after_lines = []
    stage_lines = [before_lines, during_lines, after_lines]
    stage = 0
    with open(name) as f:
        for line in f:
            line = line.rstrip()
            if line[0:2] == "  " and stage == 0:
                stage = 1
            elif stage == 1 and line[0:2] != "  ":
                stage = 2
            stage_lines[stage].append(line)
    return stage_lines

def main():
    [source_header_lines, source_body_lines, source_footer_lines] = read_dot_file(sys.argv[1])
    [target_header_lines, target_body_lines, target_footer_lines] = read_dot_file(sys.argv[2])
    target_body_set = set([])
    for line in target_body_lines:
        target_body_set.add(line.replace(",style=invis",""))
    for line in source_body_lines:
        if line not in target_body_set and line[0:3] == "  \"":
            new_line = line[:-2] + ",style=invis" + line[-2:]
            target_body_lines.append(new_line)
    with open(sys.argv[2],'w') as f:
        f.write("\n".join(target_header_lines) + "\n" +
                "\n".join(target_body_lines) + "\n" +
                "\n".join(target_footer_lines) + "\n")

main()
