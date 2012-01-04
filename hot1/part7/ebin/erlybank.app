{application, erlybank,
 [{description, "ErlyBank system."},
  {vsn, "2.0"},
  {modules, [eb_app, eb_credit, eb_sup, eb_server, eb_atm, eb_event_manager, eb_withdrawal_handler]},
  {registered, [eb_sup, eb_credit, eb_server, eb_atm, eb_event_manager]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {eb_app, []}}
 ]
}.
