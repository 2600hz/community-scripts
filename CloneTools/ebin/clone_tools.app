{application, clone_tools,
 [
  {description, "Relicate phone numbers and audit account sanity"}
  ,{vsn, "0.0.1"}
  ,{modules, [audit, db_clone, hunt_account_id, number_replicator, props, view_rebuilder, wh_account, wh_binary, wh_json, wh_types]}
  ,{registered, []}
  ,{applications, [
                   kernel,
                   stdlib
                  ]}
  ,{mod, { clone_tools, []}}
  ,{env, []}
 ]}.
