:- use_module(library(clpfd)).
:- use_module(load_note).
:- use_module(value_dict).
:- use_module(print_info).
:- use_module(dot_emitter).
:- use_module(json_emitter).
:- use_module(pl_emitter).

write_to_dot :-
  findall(X, value(X), Vals),
  dot_emitter(Vals).

write_to_pl :-
  findall(X, value(X), Vals),
  pl_emitter(Vals).

write_to_json :-
  findall(X, value(X), Vals),
  json_emitter(Vals).

write_to_text :-
  findall(X, value(X), Vals),
  print_info(Vals).

inspect_value_tensions(Value) :-
  findall(X, lookup(Value, X, tension(Value, X)), Tensions),
  format("Inspecting tensions for:"),
  format(Value),
  format("\n"),
  print_segment(tension, Value, Tensions, _).

inspect_value_supports(Value) :-
  findall(X, lookup(Value, X, support(Value, X)), Supports),
  format("Inspecting supports for:"),
  format(Value),
  format("\n"),
  print_segment(supports, Value, Supports, _).

inspect_value(Value) :-
  print_info([Value]).

inspect_discussion(DiscussionName) :-
  findall(X, lookup(DiscussionName, X, discussion(DiscussionName, X)), Examples),
  format("Discussion:"),
  format("\n"),
  format(DiscussionName),
  format("\n"),
  print_discussion(Examples).

inspect_dot_discussion(DiscussionName) :-
  findall(X, lookup(DiscussionName, X, discussion(DiscussionName, X)), Examples),
  format("digraph {"),
  format("\n"),
  get_dot_discussion(Examples),
  format("}").


get_dot_discussion([]).
get_dot_discussion([In|Ins]) :-
  findall([B, A, L], get_example(B, A, L, In), Out),
  print_dot_discussion(Out),
  get_dot_discussion(Ins).
