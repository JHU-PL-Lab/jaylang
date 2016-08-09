#!/usr/bin/env python

import argparse
import collections
import json
import sys

class Objdict(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)
    def __str__(self):
        return str(vars(self))

class ImmutableDict(collections.Mapping):
    def __init__(self, somedict):
        self._dict = dict(somedict)   # make a copy
        self._hash = None
    def __getitem__(self, key):
        return self._dict[key]
    def __len__(self):
        return len(self._dict)
    def __iter__(self):
        return iter(self._dict)
    def __hash__(self):
        if self._hash is None:
            self._hash = hash(frozenset(self._dict.items()))
        return self._hash
    def __eq__(self, other):
        return self._dict == other._dict
    def __repr__(self):
        return self._dict.__repr__()

class InvariantFailure(Exception): pass

class WorkOrder(object):
    """
    Represents the work that the user has ordered for a particular graph type.
    """
    def __init__(self):
        self.must_generate = set()
        self.generate_first = False
        self.generate_last = False
        self.generate_all = False
        self.generated = set()
        self.file_prefix = ''
    def should_generate(self, n, is_last):
        if n in self.generated:
            return False
        if self.generate_all:
            return True
        if is_last and self.generate_last:
            return True
        if n == 0 and self.generate_first:
            return True
        if n in self.must_generate:
            return True
        return False
    def should_generate_any_greater_than(self, n, is_last):
        if is_last:
            return False
        if n < 0 and (self.generate_first or self.generate_last or
                      self.generate_all or self.must_generate):
            return True
        if self.generate_all:
            return True
        if self.generate_last:
            return True
        if filter(lambda x: x>n, self.must_generate):
            return True
        return False
    def have_generated(self, n):
        self.generated.add(n)
    def get_missing(self):
        return self.must_generate.difference(self.generated)
    def __repr__(self):
        return str(vars(self))

def work_order_from_graph_type(cmd_args, graph_type):
    cmd_args = vars(cmd_args)
    w = WorkOrder()
    if cmd_args["last_" + graph_type]:
        w.generate_last = True
    if cmd_args["first_" + graph_type]:
        w.generate_first = True
    if cmd_args["all_" + graph_type + "s"]:
        w.generate_all = True
    if cmd_args[graph_type + "_by_number"]:
        w.must_generate = w.must_generate.union(
            cmd_args[graph_type + "_by_number"])
    w.file_prefix = cmd_args[graph_type + "_file_prefix"]
    return w

def parse_args():
    parser = argparse.ArgumentParser(
        description="Generate a DOT file from a DDPA JSON log.")
    parser.add_argument(
        '--last-pdr', action='store_true',
        help='generate a DOT file of the last PDR')
    parser.add_argument(
        '--last-cfg', action='store_true',
        help='generate a DOT file of the last CFG')
    parser.add_argument(
        '--first-pdr', action='store_true',
        help='generate a DOT file of the first PDR')
    parser.add_argument(
        '--first-cfg', action='store_true',
        help='generate a DOT file of the first CFG')
    parser.add_argument(
        '--pdr-by-number', action='append', type=int,
        help='generate a DOT file of the PDR with the specified work count')
    parser.add_argument(
        '--cfg-by-number', action='append', type=int,
        help='generate a DOT file of the CFG with the specified work count')
    parser.add_argument(
        '--all-pdrs', action='store_true',
        help='generate a DOT file for every PDR')
    parser.add_argument(
        '--all-cfgs', action='store_true',
        help='generate a DOT file for every CFG')
    parser.add_argument(
        '--pdr-file-prefix', nargs='?', type=str, default='ddpa_pdr_',
        help='the prefix used for PDR DOT files')
    parser.add_argument(
        '--cfg-file-prefix', nargs='?', type=str, default='ddpa_cfg_',
        help='the prefix used for CFG DOT files')
    parser.add_argument(
        'log_file', type=str,
        help='the JSON log file in which graphs are stored')

    args = parser.parse_args()

    return Objdict(
        pdrs=work_order_from_graph_type(args, "pdr"),
        cfgs=work_order_from_graph_type(args, "cfg"),
        log_file=args.log_file
        )

class VariantConvertor(object):
    def __init__(self, fs, post=None):
        self.fs = fs
        self.post = post

class VariantTypes:
    standard = 1
    singleton = 2

class BaseConversion(object):
    def __init__(self):
        self.singleton_variant_constructors = set()
        self.variant_post_translators = {}

    def add_singleton_variant_constructor(self, name):
        self.singleton_variant_constructors.add(name)
    def add_variant_post_translator(self, name, fn):
        self.variant_post_translators[name] = fn

    def convert(self, obj):
        t = type(obj)
        if t == type([]):
            if len(obj) > 0:
                obj = map(self.convert, obj)
                obj0 = obj[0]
                if obj[0] in self.singleton_variant_constructors:
                    if len(obj) == 2:
                        obj = obj[1]
                    else:
                        obj = tuple(obj[1:])
                else:
                    obj = tuple(obj)
                if obj0 in self.variant_post_translators:
                    obj = self.variant_post_translators[obj0](obj)
            else:
                obj = tuple(obj)
            return obj
        elif t == type({}):
            if obj.get("type") == "set":
                s = set()
                for x in obj["elements"]:
                    s.add(self.convert(x))
                return frozenset(s)
            elif obj.get("type") == "map":
                d = {}
                for x in obj["mappings"]:
                    k = self.convert(x["key"])
                    v = self.convert(x["value"])
                    d[k] = v
                return ImmutableDict(d)
            elif obj.get("type") == "multimap":
                d = {}
                for x in obj["mappings"]:
                    k = self.convert(x["key"])
                    vs = set()
                    for v in x["values"]:
                        vs.add(self.convert(v))
                    d[k] = frozenset(vs)
                return ImmutableDict(d)
            else:
                d = {}
                for k,v in obj.iteritems():
                    d[self.convert(k)] = self.convert(v)
                return ImmutableDict(d)
        else:
            return obj

    def sanity_check(self, value):
        if type(value) == type(()):
            for x in value: self.sanity_check(x)
        elif type(value) == type(frozenset([])):
            for x in value: self.sanity_check(x)
        elif type(value) == type(ImmutableDict({})):
            for k,v in value.iteritems():
                self.sanity_check(k)
                self.sanity_check(v)
        elif type(value) in map(type, [0,u"","",False,None]):
            pass
        else:
            raise InvariantFailure(
                "Sanity-checked structure contains value of type %s (%s)" %
                (type(value), value))

class GraphConversion(BaseConversion):
    def __init__(self):
        super(GraphConversion, self).__init__()
        self.add_singleton_variant_constructor("Ident")
        # Discard all the crap that Var constructors contain
        self.variant_post_translators["Var"] = lambda obj: obj[1]

class ClauseType:
    def __init__(self, name, color):
        self.name = name
        self.color = color
    @staticmethod
    def of_clause(clause):
        if clause[0] == "Start_clause" or clause[0] == "End_clause":
            return ClauseTypes.atomic
        elif clause[0] == "Enter_clause" or clause[0] == "Exit_clause":
            return ClauseTypes.wiring
        elif clause[0] == "Unannotated_clause":
            body_type = clause[1][2][0]
            if body_type in ["Abs_value_body",
                             "Abs_var_body",
                             "Abs_projection_body"
                            ]:
                return ClauseTypes.atomic
            elif body_type in ["Abs_appl_body",
                               "Abs_conditional_body"]:
                return ClauseTypes.control_flow
            else:
                raise InvariantFailure("Unrecognized clause body type: %s" %
                                       body_type)
        else:
            raise InvariantFailure("Unrecognized clause type: %s" % clause[0])
class ClauseTypes:
    atomic = ClauseType("atomic", "#44ff44")
    wiring = ClauseType("wiring", "#ff8844")
    control_flow = ClauseType("control flow", "gray")

def abbrv_clause(clause):
    if clause[0] == "Unannotated_clause":
        return clause[1][1]
    elif clause[0] == "Start_clause":
        return "start"
    elif clause[0] == "End_clause":
        return "end"
    elif clause[0] == "Enter_clause" or clause[0] == "Exit_clause":
        form = "+" if clause[0] == "Enter_caluse" else "-"
        return "%s=%s@%s%s" % (clause[1], clause[2], form, clause[3][1])
    else:
        raise InvariantFailure("Unrecognized clause for abbreviation: %s" %
                               str(clause))

def write_cfg_file(cfg, work_count, file_prefix):
    clauses = {}
    with open("%s_%d.dot" % (file_prefix, work_count), 'w') as f:
        f.write("strict digraph analysis {\n    rankdir=\"LR\"\n")
        g = cfg["ddpa_graph"][1]
        for edge in g:
            source = abbrv_clause(edge[1])
            target = abbrv_clause(edge[2])
            f.write("    \"%s\" -> \"%s\";\n" % (source,target))
            clauses[source] = ClauseType.of_clause(edge[1])
            clauses[target] = ClauseType.of_clause(edge[2])
        for k,v in clauses.iteritems():
            f.write("    \"%s\"[style=filled,fillcolor=\"%s\"];\n" %
                    (k,v.color))
        f.write("}\n")

def write_pdr_file(pdr, work_count, file_prefix):
    raise NotImplementedError()

def pdr_reachability_merge_delta(r, delta):
    x = {}
    for n in ["push_edges_by_source",
              "pop_edges_by_source",
              "nop_edges_by_source",
              "targeted_dynamic_pop_edges_by_source",
              "untargeted_dynamic_pop_actions_by_source"]:
        if n in delta:
            if n in r:
                m = {}
                for (k,vs) in r[n].iteritems():
                    m[k] = set(vs)
                for (k,vs) in delta[n].iteritems():
                    if k in m:
                        m[k] = m[k].union(vs)
                    else:
                        m[k] = vs
                for k in m:
                    m[k] = frozenset(m[k])
                x = m
            else:
                x = delta[n]
        elif n in r:
            x = r[n]
    return ImmutableDict(x)

def pdr_merge_delta(pdr, delta):
    node_awareness_map = dict(pdr["node_awareness_map"])
    node_awareness_map.update(delta.get("node_awareness_map", {}))
    node_awareness_map = ImmutableDict(node_awareness_map)

    known_states = frozenset(
        pdr["known_states"].union(delta.get("known_states", set())))

    start_nodes = frozenset(
        pdr["start_nodes"].union(delta.get("start_nodes", set())))

    if "reachability" in delta:
        reachability = pdr_reachability_merge_delta(pdr["reachability"],
                                                    delta["reachability"])
    else:
        reachability = pdr["reachability"]

    edge_function_count = delta.get("edge_function_count",
                                    pdr["edge_function_count"])

    untargeted_dpa_count = delta.get(
        "untargeted_dynamic_pop_action_function_count",
        pdr["untargeted_dynamic_pop_action_function_count"])

    work_collection = delta.get("work_collection", ())

    work_count = delta.get("work_count", pdr["work_count"])

    logging_function_present = pdr["logging_function_present"]

    x = {"node_awareness_map": node_awareness_map,
         "known_states": known_states,
         "start_nodes": start_nodes,
         "reachability": reachability,
         "edge_function_count": edge_function_count,
         "untargeted_dynamic_pop_action_function_count":
            untargeted_dpa_count,
         "work_collection": work_collection,
         "work_count": work_count,
         "logging_function_present": logging_function_present
        }
    return ImmutableDict(x)

def generate_graph_files(conversion, work, data):
    def check_generate_graph_file(
            graph, work_order, work_count, is_last, write_fn):
        if work_order.should_generate(work_count, is_last):
            write_fn(graph, work_count, work_order.file_prefix)
            work_order.have_generated(work_count)
    last_cfg = None
    last_pdr = None
    last_work_count = -1
    data.reverse()
    while (work.pdrs.should_generate_any_greater_than(last_work_count, False) \
        or work.cfgs.should_generate_any_greater_than(last_work_count, False)) \
      and len(data) > 0:
        # Pull the next item from the list
        event = data.pop()
        if event["work_count"] < last_work_count:
            raise InvariantFailure(
                "JSON objects not in order of work count: %s came before %s!" %
                (last_work_count, event["work_count"]))
        last_work_count = event["work_count"]
        # Depending on what type of element it contains, react accordingly.
        if event["element_type"] == "ddpa_graph":
            is_first_cfg = (last_cfg is None)
            last_cfg = conversion.convert(event["graph"])
            conversion.sanity_check(last_cfg)
            check_generate_graph_file(
                last_cfg, work.cfgs, last_work_count, False, write_cfg_file)
        elif event["element_type"] == "pds_reachability_graph":
            is_first_pdr = (last_pdr is None)
            last_pdr = conversion.convert(event["graph"])
            conversion.sanity_check(last_pdr)
            check_generate_graph_file(
                last_pdr, work.pdrs, last_work_count, False, write_pdr_file)
        elif event["element_type"] == "pds_reachability_graph_delta":
            if last_pdr is None:
                raise InvariantFailure("PDR delta appeared before first PDR")
            last_pdr = pdr_merge_delta(
                last_pdr, conversion.convert(event["graph"]))
            conversion.sanity_check(last_pdr)
            check_generate_graph_file(
                last_pdr, work.pdrs, last_work_count, False, write_pdr_file)
        else:
            raise InvariantFailure("Unrecognized element type: %s" %
                                   event["element_type"])
    check_generate_graph_file(
        last_cfg, work.cfgs, last_work_count, True, write_cfg_file)
    check_generate_graph_file(
        last_pdr, work.pdrs, last_work_count, True, write_pdr_file)

def main():
    work = parse_args()
    if not work.pdrs.should_generate_any_greater_than(-1, False) and \
       not work.cfgs.should_generate_any_greater_than(-1, False):
           print "You have not requested to generate any graphs."
           sys.exit(1)
    with open(work.log_file) as f:
        data = json.load(f)
    conversion = GraphConversion()
    generate_graph_files(conversion, work, data)

main()
