# Notes Explorer (Experimental)

## Starting

```console
# should be run from root of this repository
rlwrap swipl notes_explorer/notes_explorer.pl
```

## Loading Notes

```prolog
load_note("./es9/2018-11/nov-28").
```

## Printing Info

```prolog
% will write JSON to STDIO
write_to_json.
% will write text to STDIO
write_to_text.
% will write a dot graph source to STDIO 
write_to_dot.
```

## Inspecting

```prolog
inspect_value(interop).
inspect_value_supports(interop).
inspect_value_tensions(interop).
```
