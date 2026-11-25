open Pages

let security =
  [
    ("x-content-type-options", "nosniff");
    ("x-frame-options", "DENY");
    ("referrer-policy", "same-origin");
  ]

let routes =
  Router.make ~not_found ~error:error_page ~headers:security ~cache:no_store
    [
      Router.route Login ~path:"/login" ~page:login ~search:{ failed = optional bool };
      Router.post Login_post ~path:"/ui/login" ~handler:Handlers.login;
      Router.post Logout ~path:"/ui/logout" ~handler:Handlers.logout;
      Router.post Elevate_post ~path:"/ui/elevate" ~handler:Handlers.elevate;
      Router.post Theme ~path:"/ui/theme" ~handler:Handlers.theme;
      Router.post Password ~path:"/ui/password" ~handler:Handlers.password;
      Router.post App_create ~path:"/ui/applications" ~handler:Handlers.app_create;
      Router.post App_update ~path:"/ui/applications/:id<int>" ~handler:Handlers.app_update;
      Router.post Client_create ~path:"/ui/clients" ~handler:Handlers.client_create;
      Router.post Client_update ~path:"/ui/clients/:id<int>" ~handler:Handlers.client_update;
      Router.post User_create ~path:"/ui/users" ~handler:Handlers.user_create;
      Router.group ~path:"/" ~loader:Loaders.session ~layout:shell ~search:{ alert = optional string }
        [
          Router.route Messages ~path:"/" ~page:messages ~search:{ pages = default int 1 };
          Router.route App_messages ~path:"/messages/:app<int>" ~page:app_messages ~search:{ pages = default int 1 };
          Router.route Applications ~path:"/applications" ~page:applications ~search:{ reveal = optional string };
          Router.route App_edit ~path:"/applications/:id<int>/edit" ~page:app_edit ?overlay:app_edit_dialog;
          Router.route App_remove ~path:"/applications/:id<int>/delete" ~page:app_remove ?overlay:app_remove_dialog;
          Router.route App_token ~path:"/applications/:id<int>/token" ~page:app_token ?overlay:app_token_dialog;
          Router.route Clients ~path:"/clients" ~page:Pages.clients ~loader:Loaders.clients ~search:{ reveal = optional string };
          Router.route Client_edit ~path:"/clients/:id<int>/edit" ~page:client_edit ?overlay:client_edit_dialog
            ~loader:Loaders.client;
          Router.route Client_remove ~path:"/clients/:id<int>/delete" ~page:client_remove ?overlay:client_remove_dialog
            ~loader:Loaders.client;
          Router.route Users ~path:"/users" ~page:Pages.users ~loader:Loaders.users;
          Router.route User_edit ~path:"/users/:id<int>/edit" ~page:user_edit ?overlay:user_edit_dialog ~loader:Loaders.user;
          Router.route User_remove ~path:"/users/:id<int>/delete" ~page:user_remove ?overlay:user_remove_dialog
            ~loader:Loaders.user;
          Router.route Settings ~path:"/settings" ~page:settings;
          Router.route Elevate ~path:"/elevate" ~page:elevate ~search:{ next = optional string };
        ];
    ]
