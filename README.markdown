Pooled ODBC for Erlang
======================

podbc is a simple wrapper built on top of [poolboy](https://github.com/devinus/poolboy) and the odbc library bundled
with [Erlang/OTP](http://www.erlang.org) to pool ODBC connections in Erlang. In addition to the thin wrapper functions
around the functions found in Erlang's odbc module, there are functions to reduce boilerplate code and make your life
easier, when you use ODBC with Erlang. The project was formerly known as [poolboy_odbc](https://github.com/pannonia-technologies/poolboy_odbc).
The rename was made mainly to reduce typing.

Contributions are always very welcome! See [here](#contributors) who contributed in the past.

Usage:
------

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