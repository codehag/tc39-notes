# ECMA, TC39 Meeting Notes - Notes Explorer

Notes are Published at:

- http://tc39.github.io/tc39-notes
- http://rwaldron.github.io/tc39-notes

## Getting started

This project is hosted at https://notesexplorer.herokuapp.com. To get the password, please get in
touch with me.

## Setting up the project locally:

###Docker installation (local, no need to install prolog)

This is a fast way to get started, but people need to know how to use docker. Basic instructions
are as follows

1) Create the image
`docker build -t prolognotebook`

2) run it
`docker run -p 8888:8888 -e PORT=8888 -t prolognotebook`

once the notebook starts, you should navigate to `tc39-notes` inside you will find a file
`notebook_explorer_explorer.ipynb` -- click this and it will open an interactive environment
with instructions on how the project works.

these commands are very useful for people who haven't worked with docker before:

`docker ps` -- list all running containers, gives a list of process ids
`docker exec -it <process id> bash` -- start a bash shell inside of the container. You have access
to git / nano from here

`docker stop $(docker ps -a -q)` -- stop all containers
`docker rm $(docker ps -a -q)` -- remove all containers

You can play around in this environment.

### Local installation (full installation, might be more comfortable for those not familiar with
docker and wanting to play around with it more)
If you want freedom to look around and work as you like, then I recommend installing locally. If
you get stuck anywhere you can take a look at the docker build file.
To get the basic code running you just need to do the following:

1) install prolog:

`apt-get install -y git swi-prolog`

2)install jupyter notebook:

`apt install python3-notebook jupyter-core python-ipykernel`

2) Install jswipl:

`python3 -m pip install --upgrade --user jswipl

3) run the following command:

`mkdir /usr/local/share/jupyter/kernels/jswipl && cp jswipl/kernel.json /usr/local/share/jupyter/kernels/jswipl`

Then it should be all set to go.


## Notes Explorer API(Experimental)

### Starting

```console
# should be run from root of this repository
rlwrap swipl notes_explorer/notes_explorer.pl
```

### Loading Notes

```prolog
load_note("./es9/2018-11/nov-28").
```

### Printing Info

```prolog
% will write JSON to STDIO
write_to_json.
% will write text to STDIO
write_to_text.
% will write a dot graph source to STDIO
write_to_dot.
```

### Inspecting

```prolog
inspect_value(interop).
inspect_value_supports(interop).
inspect_value_tensions(interop).
```
