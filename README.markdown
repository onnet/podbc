Pooled ODBC for Erlang
======================

podbc is a simple wrapper built on top of [poolboy](https://github.com/devinus/poolboy) and the odbc library bundled
with [Erlang/OTP](http://www.erlang.org) to pool ODBC connections in Erlang. In addition to the thin wrapper functions
around the functions found in Erlang's odbc module, there are functions to reduce boilerplate code and make your life
easier, when you use ODBC with Erlang. The project was formerly known as [poolboy_odbc](https://github.com/pannonia-technologies/poolboy_odbc).
The rename was made mainly to reduce typing.

Contributions are always very welcome! See [here](#contributors) who contributed in the past.

I try to keep the API as stable as possible, but things can still change, before we reach version 1. So please be aware
of that.

Usage:
------

**Starting:**

```erl-sh
1> podbc:start().
ok
```

**Adding a pool programmatically:**

```erl-sh
2> podbc_mgr:add_pool('odbc/test', [{size, 5}, {max_overflow, 5}],
        [{dsn, "DSN=test"}, {options, [{auto_commit, off}, {binary_strings, on}]}]).
{ok,<0.68.0>}
```

**Removing a pool programmatically:**

```erl-sh
3> podbc_mgr:remove_pool('odbc/test').
ok
```

**Sample configuration file:**

You can supply the configuration file with the *-config* option of *erl*. The example below will automatically configure
the pool *'odbc/test'* and start the *podbc* application.

```bash
$ erl -pa ebin -pa deps/*/ebin -config sample.config -s podbc start
```

```erl
%% sample.config
[
  {podbc, [
    {pools, [
      {
        'odbc/test',
        [{size, 5}, {max_overflow, 5}],
        [{dsn, "DSN=test"}, {options, [{auto_commit, off}, {binary_strings, on}]}]
      }
    ]},
    {mappings, [
      {'odbc/test', mysql}
    ]},
    {named_queries, [
      {mysql, [
        {last_insert_id, "SELECT CONVERT(LAST_INSERT_ID, UNSIGNED)", []},
        {'person.all', "SELECT id, last_name, first_name FROM person", []},
        {'person.insert', "INSERT INTO person (last_name, first_name) VALUES (?, ?)", [
            {sql_varchar, 64}, {sql_varchar, 64}]}
      ]}
    ]}
  ]}
].
```

All methods of Erlang's odbc module are supported. See [odbc](http://www.erlang.org/doc/man/odbc.html) for details. The functions
connect and disconnect work a little different.

**Connecting:**

```erl-sh
4> {ok, WorkerRef} = podbc:connect('odbc/test').
{ok,<0.44.0>}
```

**Disconnecting:**

```erl-sh
5> podbc:disconnect('odbc/test', WorkerRef).
ok
```

**do a way to reduce boilerplate and safely connect and disconnect:**

The code below will get a connection from the pool, then pass the worker reference to the *fun*, execute the *fun* and
then safely return the connection to the pool.

Inside the fun, we use the *named_query/3* function. See the example configuration file above for the sql.

```erl-sh
6> podbc:do('odbc/test', fun(WorkerRef) ->
        {selected, _, Result} = podbc:named_query(WorkerRef, 'person.all', []), Result end).
{ok,[{4,<<"Dampf">>,<<"Hans">>},
     {5,<<"Mustermann">>,<<"Erika">>}]}
```

Example with inserting data:

```erl-sh
7> podbc:do('odbc/nestor', fun(Worker) ->
        {updated, 1} = podbc:named_query(Worker, 'person.insert', [<<"John">>, <<"Doe">>]),
        {selected, _, [{Id}]} = podbc:named_query(Worker, last_insert_id, []),
        Id
   end).
{ok,13}
```

Road Map:
---------

- Complete podbc_qresolver, to be able to reload query mappings.
- Complete documentation.
- Write tests! At the moment there are no tests at all, which is really, really bad...
- Provide mock test library. So code using podbc can be easily tested.
- Add an asynchronus interface to named_query, param_query and sql_query.
- Add some kind of DDL generator, to automatically update the database schema on changes.

Authors:
--------

- Matthias Endler ([matthias-endler](https://github.com/matthias-endler)) <matthias.endler@pantech.at>

Contributors:
-------------

Here I just want to say thank you and give credit to people, who were contributing to the project in the past.

All contributors in chronological order of their first contribution.

- Erik Søe Sørensen ([eriksoe](https://github.com/eriksoe))

License:
--------

podbc is licensed under the MIT license. See [LICENSE](LICENSE) for details.