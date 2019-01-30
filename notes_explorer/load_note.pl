:- module(load_note, [load_note/1]).
:- use_module(value_dict).

load_note(Argv) :-
  string_concat(Argv, '.md', Input),
  open(Input, read, Str),
  read_file(Str),
  close(Str).

read_file(Stream) :-
  \+ at_end_of_stream(Stream),
  read_line_to_string(Stream,Tmp),
  test_string(Stream, Tmp),
  read_file(Stream).

read_file(Stream) :-
  at_end_of_stream(Stream).

test_string(Stream, String) :-
  \+ skip_line(String),
  parse_line(String, Group),
  next_line(Stream, Group2, Example),
  append(Group, Group2, Groups),
  build_relationship(Groups, _, Example).

test_string(Stream, String) :-
  is_discussion(String),
  set_c_discussion(Stream, String).

test_string(_, String) :-
  skip_line(String).

is_discussion(String) :-
  sub_string(String, 0, 11, _, TestString),
  TestString = "[Discussion".

set_c_discussion(Stream, _) :-
  at_end_of_stream(Stream).

set_c_discussion(Stream, String) :-
  \+ at_end_of_stream(Stream),
  read_line_to_string(Stream, Discussion),
  set_d_keywords(Discussion, String),
  set_current_discussion(Discussion).

set_d_keywords(Discussion, String) :-
  split_string(String, ":", "", [_, Keyword]),
  set_discussion_keyword(Discussion, Keyword).

skip_line(String) :-
  not(sub_string(String, 0, 7, _, _)).

skip_line("").
skip_line(String) :-
  sub_string(String, 0, 7, _, TestString),
  not(TestString = "[Value:").

parse_line(String, Groups) :-
  string_length(String, Length),
  Newlength is Length - 2,
  sub_string(String, 1, Newlength, _, SubString),
  split_string(SubString, " ", "", Groups).

next_line(Stream, Values, Out) :-
  \+ at_end_of_stream(Stream),
  read_line_to_string(Stream, String),
  check_line(Stream, String, Values, Out).

check_line(Stream, String, Values, Out) :-
  \+ skip_line(String),
  parse_line(String, Value),
  next_line(Stream, NextValues, Out),
  append(Value, NextValues, Values).

check_line(_, String, [], String) :-
  skip_line(String).

build_relationship([], _, _).
build_relationship([Group|Groups], _, Example) :-
  split_string(Group, ":", "", [Key,V]),
  Key = "Value",
  string_to_value(V, Value),
  value_lookup(Value),
  build_relationship(Groups, Value, Example).

build_relationship([Group|Groups], Ref, Example) :-
  split_string(Group, ":", "", [Key,V]),
  Key = "Tension",
  string_to_value(V, Value),
  lookup(Ref, Value, tension(Ref,Value)),
  set_example(tension, Ref, Value, Example),
  build_relationship(Groups, Ref, Example).

build_relationship([Group|Groups], Ref, Example) :-
  split_string(Group, ":", "", [Key,V]),
  Key = "Support",
  string_to_value(V, Value),
  lookup(Ref, Value, support(Ref, Value)),
  set_example(support, Ref, Value, Example),
  build_relationship(Groups, Ref, Example).

string_to_value(String, Value) :-
  atom_string(AtomValue, String),
  downcase_atom(AtomValue, Value).
