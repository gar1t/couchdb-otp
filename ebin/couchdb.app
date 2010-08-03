%%% -*-erlang-*-
{application, couchdb,
 [{description, "CouchDB OTP Application"},
  {vsn, "0.0"},
  {modules, [couchdb,
             couchdb_sup,
             couch_config]}, % TODO complete this list when finished
  {registered, [couchdb, couchdb_sup]}, % TODO complete when finished
  {applications, [kernel,
                  stdlib,
                  sasl,
                  crypto]},
  {mod, {couchdb_app, []}}]}.
