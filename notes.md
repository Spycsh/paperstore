create project
```
rebar3 new app name=paperstore
```

edit the rebar.config, install postgres

build the database file
```
escript src/paperstore_init.erl
```