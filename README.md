# paperstore

create project
```
rebar3 new app name=paperstore
```

edit the rebar.config, 

install postgres

build the database file
```
escript src/paperstore_init.erl
```

write your code in `/src` and `/test`

test
```
rebar3 proper
```

TODO:
- add rules

